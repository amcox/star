# Make a file that has all of the student predictions
library(dplyr)
update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Generate the STAR models by student, tied to US2
  ds <- generate_student_model_info_for_teachers()

# Write result to the spreadsheets folder
  file.name <- "student STAR models data for teachers"
  save_df_as_csv(ds, file.name)

# Roll-up scores by teacher
  # Get data from masterdash with one row per enrollment-score. Run the file 'export enrollments.r' in masterdash and copy the resulting file ('enrollments data.csv') into the Data folder of this directory.
  d.se <- load_enrollments_data()
  
  # Just grab some columns from the student models, and rename others
  d.s <- select(ds, StudentId, subject, us1.modeled.gap:modeled.year.ggs)
  names(d.s)[names(d.s)=='StudentId'] <- 'student_number'
  names(d.s)[names(d.s)=='subject'] <- 'star.subject'

  # Merge the enrollments and the star data
  d <- merge(d.se, d.s)
  
  # Roll-up the data by teacher to find averages of those star columns
  d.r <- d %>% group_by(teacher_name, subject, star.subject) %>%
    summarize(n=n(),
      us1.modeled.gap.avg = mean(us1.modeled.gap, narm=T),
      us2.modeled.gap.avg = mean(us2.modeled.gap, narm=T),
      eoy.modeled.gap.avg = mean(eoy.modeled.gap, narm=T),
      modeled.us1.us2.gap.growth.avg = mean(modeled.us1.us2.gap.growth, narm=T),
      modeled.year.gap.growth.avg = mean(modeled.year.gap.growth, narm=T),
      modeled.us1.us2.ggs.perc.good = mean(modeled.us1.us2.ggs != 'opened', na.rm=T),
      modeled.year.ggs.perc.good = mean(modeled.year.ggs != 'opened', na.rm=T)
    )
    
  # Save as a csv
  save_df_as_csv(d.r, 'star roll-up by teacher for US2')
