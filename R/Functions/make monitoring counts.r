make_monitoring_counts <- function(d, us.date='2014-07-01', pm.date='2014-07-01') {
  summarize(
    d,
    us.reading = sum(subject == 'reading' & date >= us.date),
    us.math = sum(subject == 'math' & date >= us.date),
    pm.reading = sum(subject == 'reading' & date >= pm.date),
    pm.math = sum(subject == 'math' & date >= pm.date)
  )
}