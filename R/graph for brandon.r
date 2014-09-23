library(gdata)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- generate_student_model_info_with_ps_and_rti(include.rti=T)
df <- subset(df, grade %in% grades)
df <- subset(df, n >= 2 & range <= 3 & abs(modeled.year.growth) < 5)

d.means <- df %.%
  group_by(grade) %.%
  summarize(ge.gap.mean=mean(moy.modeled.gap, na.rm=T),
    ge.gap.median=median(moy.modeled.gap, na.rm=T)
  )

df.p <- df %.%
  group_by(grade, subject) %.%
  mutate(percentile=ecdf(moy.modeled.gap)(moy.modeled.gap))
  
df.top <- subset(df.p, percentile > .45)

d.means.top <- df.top %.%
  group_by(grade) %.%
  summarize(ge.gap.mean=mean(moy.modeled.gap, na.rm=T),
    ge.gap.median=median(moy.modeled.gap, na.rm=T)
  )

d.means$group <- rep("all", nrow(d.means))
d.means.top$group <- rep("top 55%", nrow(d.means.top))
d.means.all <- rbind(d.means, d.means.top)

save_df_as_csv(d.means.all, 'star data for brandon graph')