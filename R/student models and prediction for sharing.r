# Make a file that has all of the student predictions wihout personaly
# identifying information.

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

library(digest)

ds <- generate_student_model_info_with_ps_and_rti(include.rti=T)

ds$hash.id <- apply(ds, 1, function(r){
	digest(paste0(r['StudentId'],"7d2X=R?k{8"), algo="md5")
})

drops <- c("first.name", "last.name", "StudentId")
ds <- ds[,!(names(ds) %in% drops)]

file.name <- "student data for sharing"
write.csv(ds, paste0("./../Spreadsheets/", file.name, ".csv"), 
				row.names=F, na=""
)