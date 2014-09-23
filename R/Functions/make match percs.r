make_match_percs <- function(d, cuts, status.func) {
  # out.frame <- data.frame(cut=cuts)
  # # all.props <-
  #
  # data.frame(t(sapply(cuts, function(c) {
  #   c + prop.table(table(c('a', 'a', 'b', 'b', 'b')))
  # })))
  
  all.props <- sapply(cuts, function(c) {
    d$estimate.status <- apply(d, 1, status.func, c)
    d$estimate.status <- factor(d$estimate.status, levels=c('match', 'over.estimate', 'under.estimate'))
    round(prop.table(table(d$estimate.status)),2)
  })
  all.props <- t(all.props)
  data.frame(cut=cuts, all.props)

  # out.frame$match.perc <- apply(out.frame, 1, function(r) {
  #   d$estimate.status <- apply(d, 1, status.func, as.numeric(r['cut']))
  #   props <- round(prop.table(table(d$estimate.status)),2)
  #   return(
  #     tryCatch(props[['match']], error = function(e){NA})
  #   )
  # })
  # out.frame$over.perc <- apply(out.frame, 1, function(r) {
  #   d$estimate.status <- apply(d, 1, status.func, as.numeric(r['cut']))
  #   props <- round(prop.table(table(d$estimate.status)),2)
  #   return(
  #     tryCatch(props[['over.estimate']], error = function(e){NA})
  #   )
  # })
  # out.frame$under.perc <- apply(out.frame, 1, function(r) {
  #   d$estimate.status <- apply(d, 1, status.func, as.numeric(r['cut']))
  #   props <- round(prop.table(table(d$estimate.status)),2)
  #   return(
  #     tryCatch(props[['under.estimate']], error = function(e){NA})
  #   )
  # })
  # return(out.frame)
}