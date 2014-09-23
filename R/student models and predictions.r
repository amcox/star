# Make a file that has all of the student predictions
library(plyr)
library(dplyr)
update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

export_student_model_info <- function(include.rti=F){
  ds <- generate_student_model_info_with_ps_and_rti(include.rti=T)
	# Write result to the spreadsheets folder
	if(include.rti){
		file.name <- "student STAR data with RTI info and LEAP prediction"
	}else{
		file.name <- "student STAR data with LEAP prediction"
	}
	save_df_as_csv(ds, file.name)
}

export_student_model_info(include.rti=F)