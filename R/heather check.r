library(plyr)
library(dplyr)
library(lubridate)
update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

ds <- generate_student_model_info()

d <- subset(ds, school == 'DTA' & grade %in% 6:8)

save_df_as_csv(d, 'export for heather')

d <- subset(d, first.date <= '2014-08-15' & last.date >= '2014-09-01')

d <- d %>% mutate(first.last.actual.growth = last.actual - first.actual)

d %>% group_by(subject) %>% summarize(mean.actual.growth = mean(first.last.actual.growth, na.rm=T))

d.avgs <- d %>% group_by(subject, grade) %>% summarize(mean.first = mean(first.actual, na.rm=T),
  mean.last = mean(last.actual, na.rm=T)
)

save_df_as_csv(d.avgs, 'export for heather avgs')