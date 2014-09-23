goal_level <- function(date, grade) {
	month <- as.numeric(format.Date(date,"%m"))
	school.month <- switch(month,0.6,0.7,0.8,0.9,1.0,1.0,0,0.1,0.2,0.3,0.4,0.5)
	return(as.numeric(grade) + school.month)
}

goal_level_from_row <- function(row){
	goal_level(row['DateTaken'], row['grade'])
}

gap <- function(row){
	return(as.numeric(row['GE']) - as.numeric(row['goal']))
}

cut_gg_status <- function(vec){
	cut(vec, c(-999, -0.1, 0.1, 999),
													labels=c("closed", "none", "opened"), right=FALSE
													)
}

find_tier <- function(r){
	if (!is.na(r[['tier.3']]) & as.numeric(r[['tier.3']])==1){
		return(3)
		} else {
			if (!is.na(r[['tier.2']]) & as.numeric(r[['tier.2']])==1) {
				return(2)
				} else {
					return(1)
				}
		}
}

make_small_school <- function(r, grade.col='grade') {
  if(r['school'] == 'SCH'){
    gc <- cut(as.numeric(r[grade.col]), c(-5, 4, 7, 9),
      labels=c("K-3", "4-6", "7-8"), right=FALSE
    )
  }else{
    gc <- cut(as.numeric(r[grade.col]), c(-5, 3, 6, 9),
      labels=c("K-2", "3-5", "6-8"), right=FALSE
    )
  }
  return(paste(r['school'], gc, sep=' '))
}