add_leap_predictions <- function(d) {
	df.star.cuts <- read.csv(file="./../Data/star cuts.csv", head=TRUE,
														na.string=c("", " ", "  ")
	)
	# Join on the STAR LEAP cut scores
	d <- merge(d, df.star.cuts)

	# Use the cut scores to predict LEAP performance
	d$leap.cut.gap <- apply(d, 1, function(r){
		as.numeric(r[['leap.modeled']]) - as.numeric(r[['cut.score']])
	})
	d$leap.cut.prediction <- apply(d, 1, function(r){
		if(as.numeric(r[['leap.cut.gap']]) >= 0){
			return("on.track")
		}else{
			return("off.track")
		}
	})
	return(d)
}