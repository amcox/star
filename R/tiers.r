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

df <- load_intervention_data()
ds <- load_student_data(all=T)

# Get just the tier interventions
dt <- subset(df, grepl("Tier", type))
dt <- dt %>% separate(type, c('type', 'subject'), ' - ')
dt$tier.num <- extract_numeric(dt$level)
dt$start <- ymd(dt$start)

# Select only active entries
dt <- subset(dt, active == 1)

# Check for duplicate tier entries and save a file with dupes for correction
day.dupes <- dt %>% group_by(ps.student.number, subject, start) %>%
  summarize(n=n()) %>% subset(., n > 1)
day.dupes <- merge(day.dupes, select(ds, student_number:grade),
  by.x='ps.student.number', by.y='student_number'
)
save_df_as_csv(
  arrange(day.dupes, school, grade, ps.student.number, subject, start),
  'duplicate tier entries'
)

# Select only the most recent tier for each student-subject
dt <- dt %>% group_by(ps.student.number, subject) %>% arrange(start) %>% filter(row_number() == 1) 

# Dupe students for subjects
ds.e <- ds
ds.e$subject <- rep('ELA', nrow(ds.e))
ds.m <- ds
ds.m$subject <- rep('Math', nrow(ds.m))
dsb <- rbind(ds.e, ds.m)
dsb$small.school <- apply(dsb, 1, make_small_school, grade.col='grade')

# Merge students and tiers, keeping students
db <- merge(dsb, select(dt, ps.student.number, subject, tier.num),
  by.x=c('student_number', 'subject'),
  by.y=c('ps.student.number', 'subject'), all.x=T
)

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