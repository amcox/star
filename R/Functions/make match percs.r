make_match_percs <- function(d, cuts, status.func) { 
  all.props <- sapply(cuts, function(c) {
    d$estimate.status <- apply(d, 1, status.func, c)
    d$estimate.status <- factor(d$estimate.status, levels=c('match', 'over.estimate', 'under.estimate'))
    round(prop.table(table(d$estimate.status)),2)
  })
  all.props <- t(all.props)
  data.frame(cut=cuts, all.props)
}