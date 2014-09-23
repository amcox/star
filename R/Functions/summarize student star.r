# For each student, make a linear model of their test scores, and find the
# first and last score date y-values of that model, then evaluate growth based
# on those values
summarize_student_star <- function(d) {
	nr <- list()
	d <- d[order(d$DateTaken),]
	lm.d <- lm(GE ~ DateTaken, data=d)
	nr$n <- nrow(d)
	nr$range <- max(d$GE) - min(d$GE)
	nr$sd <- sd(d$GE)
  nr$r2 <- summary(lm.d)$r.squared
	nr$first.date <- d[1, ]$DateTaken
	nr$last.date <- d[nrow(d), ]$DateTaken
	nr$first.actual <- d[1, ]$GE
	nr$last.actual <- d[nrow(d), ]$GE
  nr$all.actual.avg <- mean(d$GE)
	nr$first.modeled <- predict.lm(lm.d, data.frame(DateTaken=c(nr$first.date)))
	nr$last.modeled <- predict.lm(lm.d, data.frame(DateTaken=c(nr$last.date)))
	nr$leap.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2014-4-7")))
	)
  nr$boy.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2013-8-1")))
	)
  nr$moy.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2014-2-1")))
	)
  nr$eoy.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2014-6-1")))
	)
	nr$modeled.growth <- nr$last.modeled - nr$first.modeled
  nr$modeled.year.growth <- nr$eoy.modeled - nr$boy.modeled
	if("grade" %in% colnames(d)){
		grade <- as.numeric(d[1,]$grade)
		nr$first.actual.gap <- nr$first.actual - goal_level(nr$first.date, grade)
		nr$last.actual.gap <- nr$last.actual - goal_level(nr$last.date, grade)
    nr$all.actual.avg.gap <- mean(d$gap)
		nr$first.modeled.gap <- nr$first.modeled - goal_level(nr$first.date, grade)
		nr$last.modeled.gap <- nr$last.modeled - goal_level(nr$last.date, grade)
		nr$leap.modeled.gap <- nr$leap.modeled - goal_level(as.Date("2014-4-7"), grade)
    nr$boy.modeled.gap <- nr$boy.modeled - goal_level(as.Date("2013-8-1"), grade)
    nr$moy.modeled.gap <- nr$boy.modeled - goal_level(as.Date("2014-2-1"), grade)
    nr$eoy.modeled.gap <- nr$eoy.modeled - goal_level(as.Date("2014-6-1"), grade)
		nr$modeled.gap.growth <- nr$first.modeled.gap - nr$last.modeled.gap
    nr$modeled.year.gap.growth <- nr$boy.modeled.gap - nr$eoy.modeled.gap
		nr$modeled.ggs <- cut_gg_status(nr$modeled.gap.growth)
    nr$modeled.year.ggs <- cut_gg_status(nr$modeled.year.gap.growth)
	}
	nr$test.distance <- abs(as.numeric(nr$first.date - nr$last.date))
	
	return(as.data.frame(nr))
}

generate_student_summaries <- function(d.star, d.ps){
  # library(plyr)
	# Make a df with just grades from the PS info, join onto STAR
	d.grades <- select(d.ps, student_number, true_grade)
  names(d.grades) <- c('StudentId', 'grade')
	d.star.grades <- merge(d.star, d.grades)
  
  # Find the gap for each test
  d.star.grades$goal <- apply(d.star.grades, 1, goal_level_from_row)
  d.star.grades$gap <- apply(d.star.grades, 1, gap)

	# Create model and get info for each student
  # return(ddply(d.star.grades, .(StudentId, subject), summarize_student_star))
  d.star.grades %>% group_by(StudentId, subject) %>% do(summarize_student_star(.))
}

generate_student_model_info_with_ps_and_rti <- function(include.rti=F) {
	# If include.rti is T, then the export will include all the RTI fields
	# from the PS data, otherwise it will only include grade and school info
		
	# Load data and make summary
	df <- load_star_data()
	df.ps <- load_student_data()

	ds <- generate_student_summaries(df, df.ps)

	# Join on the PS data, including RTI info
	if(include.rti){
		ds <- merge(ds, df.ps)
	}else{
		df.ps.sub <- df.ps[, 1:7]
		ds <- merge(ds, df.ps.sub)
	}
	
	# Add the leap prediction information
	ds <- add_leap_predictions(ds)
	return(ds)
}