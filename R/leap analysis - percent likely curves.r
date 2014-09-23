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

calc_basic_threshes <- function(d){
	find_percent_basic <- function(cut, data){
		mean(data[data$leap.modeled >= cut,]$achievement_level %in% c('B', 'M', 'A'), na.rm=TRUE)
	}
	ges <- unique(d$leap.modeled)
	just.percs <- sapply(ges, find_percent_basic, data=d)
	data.frame(ge=ges, perc=just.percs)
}

threshes <- d %>% group_by(subject, grade) %>% do(calc_basic_threshes(.))

make_star_thresh_plot <- function(d, title){
	p <- ggplot(d, aes(x=ge, y=perc))+
	geom_point()+
  scale_y_continuous(breaks=seq(0,1,.05), label=percent)+
  scale_x_continuous(breaks=seq(0,13,.5))+
  labs(title=title,
        x="LEAP Modeled Grade Equivalent Value on STAR",
        y="Percent of Students at or Above the GE that Scored Basic or Above"
  )+
	theme_bw()
	return(p)
}
grades <- seq(3,8,1)
subjects <- c("reading", "math")
for(s in subjects){
	for(g in grades){
		d.sub <- subset(threshes, grade==g & subject==s)
		p <- make_star_thresh_plot(d.sub, paste0("STAR Percent Scoring Basic or Above ", simpleCap(s), " Grade ", g))
		save_plot_as_pdf(p, paste0('STAR Percent Scoring Basic or Above ', simpleCap(s), ' Grade ', g))
	}
}
