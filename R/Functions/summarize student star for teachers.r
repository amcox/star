# For each student, make a linear model of their test scores, and find the
# first and last score date y-values of that model, then evaluate growth based
# on those values
summarize_student_star_for_teachers <- function(d) {
	nr <- list()
	d <- d[order(d$DateTaken),]
	lm.d <- lm(GE ~ DateTaken, data=d)
	nr$n <- nrow(d)
	nr$range <- max(d$GE) - min(d$GE)
	nr$first.date <- d[1, ]$DateTaken
	nr$last.date <- d[nrow(d), ]$DateTaken
	nr$test.distance <- abs(as.numeric(nr$first.date - nr$last.date))
	nr$first.actual <- d[1, ]$GE
	nr$last.actual <- d[nrow(d), ]$GE

  nr$us1.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2014-8-1")))
	)
  nr$us2.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2014-11-1")))
	)
  # Add more US dates here when needed.
	nr$eoy.modeled <- predict.lm(lm.d,
																data.frame(DateTaken=c(as.Date("2015-6-1")))
	)
	nr$modeled.us1.us2.growth <- nr$us2.modeled - nr$us1.modeled
  nr$modeled.year.growth <- nr$eoy.modeled - nr$us1.modeled
	if("grade" %in% colnames(d)){
		grade <- as.numeric(d[1,]$grade)
    nr$us1.modeled.gap <- nr$us1.modeled - goal_level(as.Date("2014-8-1"), grade)
    nr$us2.modeled.gap <- nr$us2.modeled - goal_level(as.Date("2014-11-1"), grade)
    nr$eoy.modeled.gap <- nr$eoy.modeled - goal_level(as.Date("2015-6-1"), grade)

		nr$modeled.us1.us2.gap.growth <- nr$us1.modeled.gap - nr$us2.modeled.gap
    nr$modeled.year.gap.growth <- nr$us1.modeled.gap - nr$eoy.modeled.gap
		nr$modeled.us1.us2.ggs <- cut_gg_status(nr$modeled.us1.us2.gap.growth)
    nr$modeled.year.ggs <- cut_gg_status(nr$modeled.year.gap.growth)
	}
	
	return(as.data.frame(nr))
}

generate_student_summaries_for_teachers <- function(d.star, d.ps){
  # library(plyr)
	# Make a df with just grades from the PS info, join onto STAR
	d.grades <- select(d.ps, student_number, grade)
  names(d.grades) <- c('StudentId', 'grade')
	d.star.grades <- merge(d.star, d.grades)
  
  # Find the gap for each test
  d.star.grades$goal <- apply(d.star.grades, 1, goal_level_from_row)
  d.star.grades$gap <- apply(d.star.grades, 1, gap)

	# Create model and get info for each student
  # return(ddply(d.star.grades, .(StudentId, subject), summarize_student_star))
  d.star.grades %>% group_by(StudentId, subject) %>% do(summarize_student_star_for_teachers(.))
}

generate_student_model_info_for_teachers <- function() {
	# Load data and make summary
	df <- load_star_data()
	df.ps <- load_student_data()

	ds <- generate_student_summaries_for_teachers(df, df.ps)

	ds <- merge(ds, df.ps, by.x='StudentId', by.y='student_number')
  
	return(ds)
}