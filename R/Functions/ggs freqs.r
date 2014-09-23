ggs_freqs <- function(data, dimensions, count.column){
	ggs_sums <- function(d, column){
		r <- list()
		r['closed'] <- length(d[[column]][d[[column]]=="closed"])
		r['none'] <- length(d[[column]][d[[column]]=="none"])
		r['opened'] <- length(d[[column]][d[[column]]=="opened"])
		return(data.frame(r))
	}
	return(ddply(data, dimensions, ggs_sums, column=count.column))
}