library(plyr)
library(dplyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_star_and_ps_data()
df <- df[, c('StudentId', 'GE', 'subject', 'last.name', 'first.name', 'grade', 'school')]
names(df)[names(df) == 'GE'] <- 'current.boy.ge'

d.old <- load_old_star_summary()
d.old <- d.old[, c('subject', 'StudentId', 'last.date', 'last.actual', 'last.modeled', 'eoy.modeled')]

d <- merge(df, d.old)

d <- d %>% mutate(last.actual.dif = current.boy.ge - last.actual,
  last.modeled.dif = current.boy.ge - last.modeled,
  eoy.modeled.dif = current.boy.ge - eoy.modeled
)

d <- d %>% mutate(last.actual.dif.abs = abs(last.actual.dif),
  last.modeled.dif.abs = abs(last.modeled.dif),
  eoy.modeled.dif.abs = abs(eoy.modeled.dif)
)

save_df_as_csv(d, 'STAR Difference Between 13-14 EOY and 14-15 BOY')