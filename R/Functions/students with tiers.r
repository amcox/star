students_with_tiers <- function(output_dupes=T) {
  df <- load_intervention_data()
  ds <- load_student_data(all=T)

  # Get just the tier interventions
  dt <- subset(df, grepl("Tier", type))
  dt <- dt %>% separate(type, c('type', 'subject'), ' - ')
  dt$tier.num <- extract_numeric(dt$level)
  dt$start <- ymd(dt$start)

  # Select only active entries
  dt <- subset(dt, active == 1)

  if(output_dupes){
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
  }

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
  
  return(db)
}