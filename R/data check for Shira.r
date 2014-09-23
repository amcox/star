library(plyr)
library(dplyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

ds <- generate_student_model_info_with_ps_and_rti(include.rti=F)
ds <- subset(ds, school == 'SCH' & grade.category == '35')

ds %>% group_by(school, grade.category, subject) %>% summarize(
  growth.avg=mean(modeled.year.growth, na.rm=T),
  gap.growth.avg=mean(modeled.year.gap.growth, na.rm=T)
)