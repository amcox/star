library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

db <- students_with_tiers()

# Find the number of students at each tier for each small school
dc <- db %>% group_by(small.school, subject) %>%
  summarize(total.students=n(),
    total.tiered.students=sum(!is.na(tier.num)),
    t2.students=sum(tier.num == 2, na.rm=T),
    t3.students=sum(tier.num == 3, na.rm=T),
    perc.t1=round((sum(is.na(tier.num)) / length(tier.num)), digits=2),
    perc.t2=round((sum(tier.num == 2, na.rm=T) / length(tier.num)), digits=2),
    perc.t3=round((sum(tier.num == 3, na.rm=T) / length(tier.num)), digits=2)
  )  
save_df_as_csv(dc, 'RTI Tier Percentages')

# Save file for tier lookup
d <- db %>% select(student_number, subject, tier.num)
d$tier.num[is.na(d$tier.num)] <- 1
dw <- d %>% spread(subject, tier.num)
save_df_as_csv(dw, 'tiers')
