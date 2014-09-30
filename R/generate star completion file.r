library(dplyr)
library(reshape2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_star_data()

names(df) <- c("id", "date", "GP", "SS", "GE", "PR", "NCE", "subject", "IRL",
								"LowerZPD", "UpperZPD"
)

make_monitoring_counts <- function(d, us.date='2014-07-01', pm.date='2014-07-01') {
  summarize(
    d,
    us.reading = sum(subject == 'reading' & date >= us.date),
    us.math = sum(subject == 'math' & date >= us.date),
    pm.reading = sum(subject == 'reading' & date >= pm.date),
    pm.math = sum(subject == 'math' & date >= pm.date)
  )
}

ds <- df %>% group_by(id) %>% do(make_monitoring_counts(., pm.date='2014-09-07'))

save_df_as_csv(ds, 'completion counts')