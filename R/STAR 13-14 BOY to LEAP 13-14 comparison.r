library(ggplot2)
library(gdata)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Gather data and match STAR with LEAP
ds <- load_star_and_ps_data(y=2014)
ds$subject <- str_replace(ds$subject, 'reading', 'ela')
ds <- subset(ds, DateTaken < mdy('09/01/13') & grade == 3)
ds.s <- ds %>% select(StudentId, school, grade, subject, SS, GE, PR, NCE)
ds.g <- ds.s %>% group_by(StudentId, school, grade, subject) %>% summarise_each(funs(mean))

dl <- load_leap_data(y=2014)
dl <- subset(dl, test_name == 'L14' & achievement_level %in% c('U', 'AB', 'B', 'M', 'A'))
dl.s <- dl %>% select(subject, scaled_score, achievement_level, ai_points, on_level, student_number)

d <- merge(ds.g, dl.s, by.x=c("StudentId", "subject"), by.y=c("student_number", "subject"))


# Make text files of the LM summaries
d.e <- subset(d, subject == 'ela')
d.m <- subset(d, subject == 'math')
lm.e <- lm(d.e$NCE ~ d.e$scaled_score)
lm.m <- lm(d.m$NCE ~ d.m$scaled_score)

lm.file <- file("./../Output/STAR to iLEAP Linear Model Summary.txt", open = "wt")
sink(lm.file)
summary(lm.e)
summary(lm.m)
sink()
unlink(lm.file)


# Make threshold plots
calc_basic_threshes <- function(d){
	find_percent_basic <- function(cut, data){
		mean(data[data$NCE >= cut,]$achievement_level %in% c('B', 'M', 'A'), na.rm=TRUE)
	}
	NCEs <- unique(d$NCE)
	just.NCEs <- sapply(NCEs, find_percent_basic, data=d)
	data.frame(star.nces=NCEs, percent.cr=just.NCEs)
}

make_thresh_plot <- function(d, title) {
	ggplot(d, aes(x=star.nces, y=percent.cr))+
		geom_point()+
	  scale_y_continuous(breaks=seq(0,1,.05), label=percent)+
	  scale_x_continuous(breaks=seq(0,100,10))+
	  labs(title=title,
	        x="National NCE on BOY 3rd Grade STAR",
	        y="Percent of Students at or Above the NCE at BOY\nthat Scored Basic or Above on iLEAP in 3rd Grade at EOY"
	  )+
		theme_bw()
}

threshes <- d.e %>% do(calc_basic_threshes(.))
p <- make_thresh_plot(threshes, 'ELA BOY STAR to 3rd Grade LEAP Scores')
save_plot_as_pdf(p, 'Thresholds for BOY STAR to 3rd LEAP, ELA')

threshes <- d.m %>% do(calc_basic_threshes(.))
p <- make_thresh_plot(threshes, 'Math BOY STAR to 3rd Grade LEAP Scores')
save_plot_as_pdf(p, 'Thresholds for BOY STAR to 3rd LEAP, Math')


# Preciction Curves
# Maximize the percent of students for predicitons matching actual
find_star_leap_estimate_status <- function(r, cut.score) {
  s <- r['NCE']
  al <- r['achievement_level']
  if(is.na(s)){
    return(NA)
  }
  s.status <- as.numeric(s) >= cut.score
  al.status <- al %in% c('B', 'M', 'A')
 if(s.status & al.status){
   return('match')
 }else if(!s.status & !al.status){
   return('match')
 }else if(s.status & !al.status){
   return('over.estimate')
 }else if(!s.status & al.status){
   return('under.estimate')
 }
}
c <- d %>% group_by(subject) %>%
	do(make_match_percs(., seq(0, 100, 1), find_star_leap_estimate_status))

p <- ggplot(c, aes(x=cut, y=match, color=subject))+
  geom_line()+
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, .1), label=percent)+
  scale_x_continuous(limits=c(0,100), breaks=seq(0, 100, 5))+
  labs(title='3rd iLEAP Prediction Accuracy\nby NCE Cut Score on 3rd Grade BOY STAR',
    x="NCE Cut Score",
    y='Percent of Predictions that are Accurate'
  )+
  theme_bw()
save_plot_as_pdf(p, 'BOY STAR 3rd iLEAP Prediction Accuracy by NCE, Just Match, Both Subjects')

# Prediction curves
c <- d %>% group_by(subject) %>% do(make_match_percs(., seq(0, 100, 1), find_star_leap_estimate_status))
c <- melt(c, id.vars=c('cut', 'subject'))
p <- ggplot(c, aes(x=cut, y=value, color=variable))+
  geom_line()+
  scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, .1), label=percent)+
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
  scale_color_discrete(labels=c('match.perc'='Accurate', 'under.perc'='Under-Predicted', 'over.perc'='Over-Predicted'))+
  labs(title='3rd iLEAP Prediction Accuracy by BOY STAR NCE',
    x="Cut NCE on BOY STAR",
    y='Percent of Predictions on iLEAP Basic or Above Status'
  )+
  theme_bw()+
  theme(legend.title=element_blank(),
  axis.text.x=element_text(size=8)
  )+
  facet_wrap(~subject)
save_plot_as_pdf(p, 'BOY STAR iLEAP Prediction Percents by Subject')