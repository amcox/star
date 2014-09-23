library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# PS data was last updated on 1/16, but pulled on 5/1
ds <- generate_student_model_info_with_ps_and_rti(include.rti=T)
ds$tier <- apply(ds, 1, find_tier)


# Based on tests taken up until 5/1, this is how students are predicted to perform at the end of the year.

modeled_year_growth_hist <- function(d, title, y.max=.2, x.breaks=.5) {
  p <- ggplot(d, aes(x=modeled.year.growth))+
  		geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=.5)+
  		geom_vline(xintercept=mean(d$modeled.year.growth, na.rm=T), color="blue")+
  		geom_vline(xintercept=median(d$modeled.year.growth, na.rm=T), color="blue", linetype="longdash")+
  		scale_y_continuous(labels=percent, limits=c(0, y.max), breaks=seq(0, y.max, .1))+
  		scale_x_continuous(breaks=seq(-5, 5, x.breaks), limits=c(-5, 5))+
  		theme_bw()+
  		labs(title=paste0(title, " n=",nrow(d)))+
  		theme(axis.title.x=element_blank(),
  					axis.title.y=element_blank(),
  					title=element_text(size=7),
  					axis.text.y=element_text(size=7),
  					axis.text.x=element_text(size=7)
  					)
  return(p)
}
# Get just sound data
df <- subset(ds, n >= 3 & range <= 3 & abs(modeled.year.growth) < 5)

# Make hist pair of math and reading, all schools and grades
d <- subset(df, subject == 'reading')
p.r <- modeled_year_growth_hist(d, "Reading")
d <- subset(df, subject == 'math')
p.m <- modeled_year_growth_hist(d, "Math")
do.call(grid.arrange, c(list(p.r, p.m), main=paste0("\nModeled STAR Growth for Year 2013-14\n",
                          "Based on Linear Model of Students' Scores"
                        ),
												left="\nPercent of Students",
												sub=paste0("Growth in Years.Months\n",
                          "dashed line = median, solid line = mean\n"
                        ),
												ncol=2)
)

# Make hist set of reading, by small school
plots <- sapply(levels(interaction(schools, grade.categories)), function(x){NULL})
for(s in schools){
  for(g in grade.categories){
  		d.sub <- subset(df, grade.category==g & school==s & subject=="reading")
  		plots[[paste(s,g,sep=".")]] <- modeled_year_growth_hist(d.sub, paste(s, g, sep=" "), y.max=.32, x.breaks=1)
  }
}
do.call(grid.arrange, c(plots, main=paste0("\nModeled Reading Growth for Year 2013-14\n",
                          "Based on Linear Model of Students' Scores"
                        ),
												left="\nPercent of Students",
												sub=paste0("Growth in Years.Months\n",
                          "dashed line = median, solid line = mean\n"
                        ),
												ncol=4, as.table=F)
)

# Make hist set of math, by small school
plots <- sapply(levels(interaction(schools, grade.categories)), function(x){NULL})
for(s in schools){
  for(g in grade.categories){
  		d.sub <- subset(df, grade.category==g & school==s & subject=="math")
  		plots[[paste(s,g,sep=".")]] <- modeled_year_growth_hist(d.sub, paste(s, g, sep=" "), y.max=.32, x.breaks=1)
  }
}
do.call(grid.arrange, c(plots, main=paste0("\nModeled Math Growth for Year 2013-14\n",
                          "Based on Linear Model of Students' Scores"
                        ),
												left="\nPercent of Students",
												sub=paste0("Growth in Years.Months\n",
                          "dashed line = median, solid line = mean\n"
                        ),
												ncol=4, as.table=F)
)

# Hists by starting gap
ds$starting.gap.tier <- cut(ds$boy.modeled.gap, c(-999, -2, -1, 999),
  labels=c("T3", "T2", "T1"), right=FALSE
)
tiers <- c("T3", "T2", "T1")
subs <- c("reading", "math")
plots <- sapply(levels(interaction(tiers, subs)), function(x){NULL})
for(t in tiers){
  for(s in subs){
  		d.sub <- subset(df, starting.gap.tier==t & subject==s)
  		plots[[paste(t,s,sep=".")]] <- modeled_year_growth_hist(d.sub, paste(s, t, sep=" "), y.max=.32, x.breaks=1)
  }
}
do.call(grid.arrange, c(plots, main=paste0("\nModeled Growth by Tier for Year 2013-14\n",
                          "Based on Linear Model of Students' Scores"
                        ),
												left="\nPercent of Students",
												sub=paste0("Growth in Years.Months\n",
                          "dashed line = median, solid line = mean\n"
                        ),
												ncol=3, as.table=F)
)

# Gap growth status plot
make_ggs_plot(subset(df),
              c("subject", "starting.gap.tier"),
							"modeled.year.ggs", "Predicted Gap Closure at EOY, By Tier"
)

# Example of individual student models
d.t <- load_star_and_ps_data()
d.t <- subset(d.t, subject == 'reading' & StudentId %in% subset(df, subject == 'reading')$StudentId)
ggplot(subset(d.t, StudentId %in% unique(d.t$StudentId)[sample(length(unique(d.t$StudentId)), 4)]), aes(x=DateTaken, y=GE))+
	geom_point()+
	stat_smooth(method="lm", se=FALSE, color="blue")+
	scale_y_continuous()+
  theme_bw()+
  labs(
    title="STAR Reading Scores and Linear Models",
    x="Date",
    y="Grade Equivalent"
  )+
	facet_wrap(~StudentId)
  
# Make a single student plot for an example
ggplot(subset(d.t, StudentId == '101803'), aes(x=DateTaken, y=GE))+
	geom_point(size=4)+
	stat_smooth(method="lm", se=FALSE, color="blue", size=2)+
	scale_y_continuous(breaks=seq(0,10,.5), minor_breaks=seq(0,10,.1))+
  scale_x_date(limits=c(as.Date("2013-7-1"), as.Date("2014-6-1")),
    breaks=date_breaks('months'), labels=date_format("%b")
  )+
  theme_bw()+
  labs(
    title="STAR Reading Scores and Linear Models",
    x="Date",
    y="Grade Equivalent"
  )+
	facet_wrap(~StudentId)