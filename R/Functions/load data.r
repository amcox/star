clean_combine_star_data <- function(df.math, df.reading) {
	library(lubridate)
	df.math$DateTaken <- mdy_hm(df.math$DateTaken)
	df.reading$DateTaken <- mdy_hm(df.reading$DateTaken)
	df.math <- df.math[,c("StudentId", "DateTaken", "GP", "SS", "GE", "PR",
										"NCE")]
	df.reading <- df.reading[,c("StudentId", "DateTaken", "GP", "SS", "GE", "PR",
														"NCE", "IRL", "LowerZPD", "UpperZPD")]
	df.math$subject <- rep("math", nrow(df.math))
	df.reading$subject <- rep("reading", nrow(df.reading))
	df <- merge(df.math, df.reading, all=T)
	df$subject <- factor(df$subject)
	return(df)
}

load_star_data <- function(y=2015) {
	# setwd("./../../Data")
	df.m <- read.csv(file=paste0("./../Data/", y, "/math_summary.csv"), head=TRUE,
		na.string=c("", " ", "  "), stringsAsFactors=F
	)
	df.r <- read.csv(file=paste0("./../Data/", y, "/reading_summary.csv"), head=TRUE,
		na.string=c("", " ", "  "), stringsAsFactors=F
	)
	df <- clean_combine_star_data(df.math=df.m, df.reading=df.r)
	return(df)
}

load_ps_data <- function(y=2015) {
	df.ps <- read.csv(file=paste0("./../Data/", y, "/ps info.csv"), head=TRUE,
		na.string=c("", " ", "  "), stringsAsFactors=F
	)
	names(df.ps) <- c("StudentId", "last.name", "first.name", "school", "grade",
										"entrydate", "home.room", "state.test", "tier.1", "tier.2",
										"tier.3", "not.star", "teacher.reading", "guided.reading",
										"reading.mastery", "boost", "blitz", "amplify", "ffw",
										"corrective.reading", "novel", "compass.ela", "compass.math",
										"teacher.math", "st.math", "accelerated.math", "math.flash",
										"dtm", "aleks", "algebra", "ixl", "compass.sci", "compass.ss"
	)
	df.ps$grade.category <- cut(df.ps$grade, c(2, 6, 9),
														labels=c("35", "68"), right=FALSE
	)
	df.ps$small.school <- apply(df.ps, 1, function(r){
		paste0(r['school'],r['grade.category'])
	})
	ps.col.order <- c("StudentId", "last.name", "first.name", "school", "grade",
										"grade.category", "small.school",
										"entrydate", "home.room", "state.test", "tier.1", "tier.2",
										"tier.3", "not.star", "teacher.reading", "guided.reading",
										"reading.mastery", "boost", "blitz", "amplify", "ffw",
										"corrective.reading", "novel", "compass.ela", "compass.math",
										"teacher.math", "st.math", "accelerated.math", "math.flash",
										"dtm", "aleks", "algebra", "ixl", "compass.sci", "compass.ss")
	df.ps <- df.ps[, ps.col.order]
	return(df.ps)
}

load_student_data <- function(y=2015, all=F) {
  if(all){
    d <- read.csv(file=paste0("./../Data/", y, "/students all.csv"), head=TRUE, na.string=c("", " ", "  "),
      stringsAsFactors=F
    )
  }else{
    d <- read.csv(file=paste0("./../Data/", y, "/students.csv"), head=TRUE, na.string=c("", " ", "  "),
      stringsAsFactors=F
    )
  }
  names(d) <- tolower(names(d))
  names(d)[names(d) == 'true_grade'] <- 'grade'
  return(d)
}

load_star_and_ps_data <- function(y=2015) {
	df.star <- load_star_data(y=y)
	df.ps <- load_ps_data(y=y)
	df <- merge(df.star, df.ps)
	return(df)
}

load_star_and_student_data <- function(y=2015) {
	df.star <- load_star_data(y=y)
	df.student <- load_student_data(y=y)
	df <- merge(df.star, df.student, by.x='StudentId', by.y='student_number')
	return(df)
}

load_leap_data <- function(y=2015) {
  d <- read.csv(file=paste0("./../Data/", y, "/benchmark and leap data.csv"), head=TRUE, na.string=c(""))
  names(d) <- tolower(names(d))
  return(d)
}

load_old_star_summary <- function() {
  read.csv(file="./../Data/13-14 eoy star summary.csv", head=TRUE, na.string=c("", " ", "  ")) 
}

load_intervention_data <- function(y=2015) {
  d <- read.csv(file=paste0("./../Data/", y, "/interventions.csv"), head=TRUE, na.string=c("", " ", "  "),
    stringsAsFactors=F
  )
  names(d) <- tolower(names(d))
  return(d)
}

load_enrollments_data <- function(y=2015) {
  d <- read.csv(file=paste0("./../Data/", y, "/enrollments data.csv"), head=TRUE, na.string=c("", " ", "  "),
    stringsAsFactors=F
  )
  names(d) <- tolower(names(d))
  return(d)
}

