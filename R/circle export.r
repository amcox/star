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

# Load star data, with student data so that we can calculate gaps
df <- load_star_and_student_data()

# Rename 'true_grade' to 'grade'
names(df)[names(df) == 'true_grade'] <- 'grade'

# Just grab the data we need
df <- subset(df, subject == 'reading')
df <- subset(df, school == 'DTA' | school == 'RCAA' | (school == 'SCH' & grade == 3))

# Find the gap to grade level for each test
df$goal <- apply(df, 1, goal_level_from_row)
df$gap <- apply(df, 1, gap)

# For each student find their average GE and gap
d.means <- df %>% group_by(StudentId) %>% summarize(
  ge = mean(GE),
  gap = mean(gap)
)

# Load in the student data and join it onto the star data
ds <- load_student_data()
d <- merge(d.means, ds, by.x='StudentId', by.y='student_number')

# Load in the school code inforamtionm and join into the rest of the data
school.codes <- data.frame(school=c('DTA', 'RCAA', 'SCH'),
  school.long=c('ReNEW Dolores T. Aaron Elementary School',
    'ReNEW Cultural Arts Academy', 'ReNEW Schaumburg Elementary School'
  ),
  school.code=c('369003', '369001', '369006')
)
d <- merge(d, school.codes)

# Fill in some information that's needed on the template
d$year <- rep('2014', nrow(d))
d$district.code <- rep('369', nrow(d))
d$district.name <- rep('ReNEW Schools', nrow(d))
d$assessment.period <- rep('Beginning', nrow(d))
d$measure <- rep('Grade Equivalent', nrow(d))

# Calculate whether each student is on level based on their mean gap
d$on.level <- apply(d, 1, function(r) {
  if(as.numeric(r['gap']) > 0.2){
    return('Above Level')
  }else if(as.numeric(r['gap']) < -0.2){
    return('Below Level')
  }else {
    return('On Level')
  }
})

# Select the columns in the right order
de <- d %>% select(year, district.code, district.name, school.code,
  school.long, grade, home_room, last_name, first_name, StudentId,
  dob, assessment.period, measure, ge, on.level
)

# Save data
save_df_as_csv(de, 'circle export boy')