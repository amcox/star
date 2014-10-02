# Make a file that has all of the student predictions
library(dplyr)
update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

export_student_model_info <- function(include.rti=F){
  ds <- generate_student_model_info()
	# Write result to the spreadsheets folder
	file.name <- "student STAR models data"
	save_df_as_csv(ds, file.name)
}

export_student_model_info()