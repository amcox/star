clean_combine_star_data <- function(df.math, df.reading) {
	df.math$DateTaken <- as.Date(df.math$DateTaken, format="%Y-%m-%d")
	df.reading$DateTaken <- as.Date(df.reading$DateTaken, format="%Y-%m-%d")
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

load_star_data <- function() {
	# setwd("./../../Data")
	df.m <- read.csv(file="./../Data/math_summary.csv", head=TRUE, na.string=c("", " ", "  "))
	df.r <- read.csv(file="./../Data/reading_summary.csv", head=TRUE, na.string=c("", " ", "  "))
	df <- clean_combine_star_data(df.math=df.m, df.reading=df.r)
	return(df)
}

load_ps_data <- function() {
	df.ps <- read.csv(file="./../Data/ps info.csv", head=TRUE, na.string=c("", " ", "  "))
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

load_student_data <- function(all=F) {
  if(all){
    d <- read.csv(file="./../Data/students all.csv", head=TRUE, na.string=c("", " ", "  "),
      stringsAsFactors=F
    )
  }else{
    d <- read.csv(file="./../Data/students.csv", head=TRUE, na.string=c("", " ", "  "),
      stringsAsFactors=F
    )
  }
  names(d) <- tolower(names(d))
  names(d)[names(d) == 'true_grade'] <- 'grade'
  return(d)
}

load_star_and_ps_data <- function() {
	df.star <- load_star_data()
	df.ps <- load_ps_data()
	df <- merge(df.star, df.ps)
	return(df)
}

load_star_and_student_data <- function() {
	df.star <- load_star_data()
	df.student <- load_student_data()
	df <- merge(df.star, df.student, by.x='StudentId', by.y='student_number')
	return(df)
}

load_leap_data <- function() {
  read.csv(file="./../Data/leap data.csv", head=TRUE, na.string=c("", " ", "  ")) 
}

load_old_star_summary <- function() {
  read.csv(file="./../Data/13-14 eoy star summary.csv", head=TRUE, na.string=c("", " ", "  ")) 
}

load_intervention_data <- function() {
  d <- read.csv(file="./../Data/interventions.csv", head=TRUE, na.string=c("", " ", "  "),
    stringsAsFactors=F
  )
  names(d) <- tolower(names(d))
  return(d)
}

