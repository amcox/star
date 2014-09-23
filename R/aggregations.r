# Make aggregation files by school, grade, and small school of average STAR
# and percents on track to meet LEAP cut scores

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

ds <- generate_student_model_info_with_ps_and_rti()

export_predicted_leap_cr_percents <- function(d) {	
  library(reshape2)
	d.on.level.percs <- ddply(d, .(school, grade, subject), summarize,
				on.perc=length(subset(leap.cut.prediction, leap.cut.prediction == 'on.track')) / length(leap.cut.prediction)
	)
	d.on.level.percs.sg <- dcast(d.on.level.percs, school + grade ~ subject)
	d.on.level.percs <- ddply(d, .(school, grade.category, subject), summarize,
				on.perc=length(subset(leap.cut.prediction, leap.cut.prediction == 'on.track')) / length(leap.cut.prediction)
	)
	d.on.level.percs.gc <- dcast(d.on.level.percs,
															school + grade.category ~ subject
	)
	names(d.on.level.percs.gc) <- c("school", "grade", "math", "reading")
	write.csv(rbind(d.on.level.percs.sg, d.on.level.percs.gc),
						"./../Output/STAR Percent of Students Predicted to be CR at LEAP.csv",
						na="", row.names=F
	)
}

export_average_star_info <- function(d) {
	# By grade-school
	d.g.s <- ddply(d, .(school, grade, subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	
	# By small school
	d.gc.s <- ddply(d, .(school, grade.category, subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	names(d.gc.s) <- c("school", "grade", "subject", "avg.ge", "avg.gap")
	
	# Whole school
	d.s <- ddply(d, .(school, subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	d.s$grade <- rep(NA, nrow(d.s))
	
	# Whole grades without school
	d.g <- ddply(d, .(grade, subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	d.g$school <- rep(NA, nrow(d.g))
	
	# Grade categories without school
	d.gc <- ddply(d, .(grade.category, subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	names(d.gc) <- c("grade", "subject", "avg.ge", "avg.gap")
	d.gc$school <- rep(NA, nrow(d.gc))
	
	# All network
	d.all <- ddply(d, .(subject), summarize,
										avg.ge=mean(last.modeled, na.rm=T),
										avg.gap=mean(last.modeled.gap, na.rm=T)
	)
	d.all$school <- rep(NA, nrow(d.all))
	d.all$grade <- rep(NA, nrow(d.all))
	
	d.averages <- rbind(d.g.s, d.gc.s, d.s, d.g, d.gc, d.all)
	write.csv(d.averages, "./../Output/STAR Average Levels by Grade and School.csv",
						na="", row.names=F
	)
}

export_star_prediction_info <- function(d) {
  library(dplyr)
	# By grade-school
  d.gs <- d %.% group_by(school, grade) %.%
    summarize(perc.on.track.1.yr = length(modeled.year.growth[modeled.year.growth >= 1]) / length(modeled.year.growth))	
	
	save_df_as_csv(d.gs, 'percent of students on track to grow a year')
}

export_predicted_leap_cr_percents(ds)
export_average_star_info(ds)
export_star_prediction_info(subset(ds, n >= 2 & modeled.year.growth <= 4 & subject == 'reading'))


