library(ggplot2)
library(scales)
library(gdata)
library(dplyr)
library(reshape2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

find_star_leap_estimate_status <- function(r, cut.score) {
  s <- r['leap.modeled']
  al <- r['achievement_level']
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

# Get STAR and LEAP data together
df <- generate_student_model_info_with_ps_and_rti(include.rti=F)
ds <- subset(df, n > 2 & test.distance >= 150)

dl <- load_leap_data()
dl.s <- subset(dl, test_name == 'L14' & subject %in% c('ela', 'math'))
dl.s$subject <- as.character(dl.s$subject)
dl.s$subject[dl.s$subject == 'ela'] <- 'reading'
dl.s <- dl.s[, c('subject', 'achievement_level', 'ai_points', 'scaled_score', 'student_number')]

d <- merge(ds, dl.s, by.x=c('subject', 'StudentId'), by.y=c('subject', 'student_number'))
d <- subset(d, achievement_level %in% leap.als)

# Make a graph with all schools in one
c <- d %>% group_by(grade, subject) %>% do(make_match_percs(., seq(1, 8, .1), find_star_leap_estimate_status))
cm <- melt(c, id.vars=c('cut', 'grade', 'subject'))
p <- ggplot(cm, aes(x=cut, y=value, color=variable))+
  geom_line()+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.2), label=percent)+
  scale_x_continuous(breaks=seq(0,8,.5))+
  scale_color_discrete(labels=c('match.perc'='Accurate', 'under.perc'='Under-Predicted', 'over.perc'='Over-Predicted'))+
  labs(title='LEAP Prediction Accuracy of STAR Modeled LEAP Value Cut Score',
    x="Cut Score",
    y='Percent of Predictions'
  )+
  theme_bw()+
  theme(legend.title=element_blank(),
    axis.text.x=element_text(size=5),
    axis.text.y=element_text(size=6)
  )+
  facet_grid(subject ~ grade)
save_plot_as_pdf(p, 'LEAP Prediction Accuracy of STAR')


# Make a graph showing differences of schools
c <- d %>% group_by(grade, subject, school) %>% do(make_match_percs(., seq(1, 10, .1), find_star_leap_estimate_status))
cm <- melt(c, id.vars=c('cut', 'grade', 'subject', 'school'))
p <- ggplot(subset(cm, variable == 'match'), aes(x=cut, y=value, color=school))+
  geom_line()+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.2), label=percent)+
  scale_x_continuous(breaks=seq(0,10,.5))+
  labs(title='LEAP Prediction Accuracy of STAR by School',
    x="Cut Score",
    y='Percent of Predictions'
  )+
  theme_bw()+
  theme(legend.title=element_blank(),
    axis.text.x=element_text(size=5),
    axis.text.y=element_text(size=6)
  )+
  facet_grid(grade ~ subject)
save_plot_as_pdf(p, 'LEAP Prediction Accuracy of STAR by School')
