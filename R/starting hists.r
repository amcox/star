library(ggplot2)
library(scales)
library(gdata)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(gridExtra)
# library(car)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# STAR Starting Points...
boy_hist <- function(d, title, width=.5, ymax=.5, xmin=-6, xmax=3){
	p <- ggplot(d, aes(x=gap))+
		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=width)+
		geom_vline(xintercept=mean(d$gap, na.rm=T), color="blue")+
		geom_vline(xintercept=median(d$gap, na.rm=T), color="blue", linetype="longdash")+
		scale_y_continuous(labels=percent, limits=c(0,ymax), breaks=seq(0,ymax,.1))+
		scale_x_continuous(breaks=seq(xmin,xmax,1), limits=c(xmin, xmax))+
		theme_bw()+
		labs(title=paste0(title, " n=",nrow(d)))+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					title=element_text(size=5),
					axis.text.y=element_text(size=5),
					axis.text.x=element_text(size=5)
					)
	return(p)
}

df <- load_star_and_ps_data()
d <- df %>% group_by(StudentId, school, grade, grade.category, small.school, subject) %>% summarize(ge=mean(GE))
d$gap <- apply(d, 1, function(r){
	return(as.numeric(r['ge']) - as.numeric(r['grade']))
})

# ...by school-grade
make_school_grade_boy_hists <- function(d, subj) {
  grades <- seq(3,8,1)
  schools <- c("RCAA", "STA", "DTA", "SCH")
  plots <- sapply(levels(interaction(grades, schools)), function(x){NULL})
  for(g in grades){
  	for(s in schools){
  		d.sub <- subset(d, grade==g & school==s & subject==subj)
  		plots[[paste(g,s,sep=".")]] <- boy_hist(d.sub, paste(s, g, sep=" "), .5, .5)
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main=paste0("\nSTAR ", simpleCap(subj), " Starting Gap 2014-15"),
  												left="\nPercent of Students",
  												sub="STAR Gap to Grade Level (negative means below level)\n
  dashed line = median, solid line = mean\n",
  												ncol=6)
  )
  save_plot_as_pdf(p, paste0('STAR ', simpleCap(subj), ' Starting Gap 2014-15, by Grade-School'))
}
make_school_grade_boy_hists(d, 'reading')
make_school_grade_boy_hists(d, 'math')


# ...by small school
make_small_school_boy_hists <- function(d, subj) {
  grade.categories <- c("35", "68")
  schools <- c("RCAA", "STA", "DTA", "SCH")
  plots <- sapply(levels(interaction(schools, grade.categories)), function(x){NULL})
  for(g in grade.categories){
  	for(s in schools){
  		d.sub <- subset(d, grade.category==g & school==s & subject==subj)
  		plots[[paste(s,g,sep=".")]] <- boy_hist(d.sub, paste(s, g, sep=" "), .5, .5)
  	}
  }
  p <- do.call(arrangeGrob, c(plots, main=paste0("\nSTAR ", simpleCap(subj), " Starting Gap 2014-15"),
  												left="\nPercent of Students",
  												sub="STAR Gap to Grade Level (negative means below level)\n
  dashed line = median, solid line = mean\n",
  												ncol=4)
  )
  save_plot_as_pdf(p, paste0('STAR ', simpleCap(subj), ' Starting Gap 2014-15, by Small School'))
}
make_small_school_boy_hists(d, 'reading')
make_small_school_boy_hists(d, 'math')