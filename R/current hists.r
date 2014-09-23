library(ggplot2)
library(scales)
library(gdata)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(gridExtra)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# SCHR hists...
growth_hist <- function(d, title, width=.1, ymax=.1, xmin=-4, xmax=4){
	p <- ggplot(d, aes(x=modeled.year.growth))+
		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=width)+
		geom_vline(xintercept=mean(d$modeled.year.growth, na.rm=T), color="blue")+
		geom_vline(xintercept=median(d$modeled.year.growth, na.rm=T), color="blue", linetype="longdash")+
		scale_y_continuous(labels=percent, limits=c(0,ymax), breaks=seq(0,ymax,.1))+
		scale_x_continuous(breaks=seq(xmin,xmax,.5), limits=c(xmin, xmax))+
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

gap_growth_hist <- function(d, title, width=.1, ymax=.1, xmin=-4, xmax=4){
	p <- ggplot(d, aes(x=modeled.year.gap.growth))+
		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=width)+
		geom_vline(xintercept=mean(d$modeled.year.gap.growth, na.rm=T), color="blue")+
		geom_vline(xintercept=median(d$modeled.year.gap.growth, na.rm=T), color="blue", linetype="longdash")+
		scale_y_continuous(labels=percent, limits=c(0,ymax), breaks=seq(0,ymax,.1))+
		scale_x_continuous(breaks=seq(xmin,xmax,.5), limits=c(xmin, xmax))+
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

df <- generate_student_model_info_with_ps_and_rti(include.rti=F)
ds <- subset(df, n > 2 & test.distance >= 150)

# reading growth
make_growth_plots_by_school <- function(d, subj) {
  grades <- seq(3,8,1)
  for(s in schools){
    plots <- sapply(levels(interaction(grades, s)), function(x){NULL})
    for(g in grades){
    		d.sub <- subset(d, grade==g & school==s & subject==subj)
    		plots[[paste(g,s,sep=".")]] <- growth_hist(d.sub, paste(s, g, sep=" "), width=.25, ymax=.3)
    }
    p <- do.call(arrangeGrob, c(plots, main=paste0("\n", s, " ", simpleCap(subj), " Growth for 2013-14\n",
                              "Based on Linear Model of Students' Scores"
                            ),
    												left="\nPercent of Students",
    												sub=paste0("Growth in Years.Months\n",
                              "dashed line = median, solid line = mean\n"
                            ),
    												ncol=3)
    )
    save_plot_as_pdf(p, paste0('STAR ', simpleCap(subj), ' Growth Hist by Grade for ', s))
  }
}
make_growth_plots_by_school(ds, 'reading')
make_growth_plots_by_school(ds, 'math')


make_gap_growth_plots_by_school <- function(d, subj) {
  grades <- seq(3,8,1)
  for(s in schools){
    plots <- sapply(levels(interaction(grades, s)), function(x){NULL})
    for(g in grades){
    		d.sub <- subset(d, grade==g & school==s & subject==subj)
    		plots[[paste(g,s,sep=".")]] <- gap_growth_hist(d.sub, paste(s, g, sep=" "), width=.25, ymax=.3)
    }
    p <- do.call(arrangeGrob, c(plots, main=paste0("\n", s, " ", simpleCap(subj), " Gap Growth for 2013-14\n",
                              "Based on Linear Model of Students' Scores"
                            ),
    												left="\nPercent of Students",
    												sub=paste0("Gap Growth in Years.Months (Negative is Closing the Gap)\n",
                            "dashed line = median, solid line = mean\n"),
    												ncol=3)
    )
    save_plot_as_pdf(p, paste0("STAR ", simpleCap(subj), " Gap Growth Hist by Grade for ", s))
  }
}
make_gap_growth_plots_by_school(ds, 'reading')
make_gap_growth_plots_by_school(ds, 'math')