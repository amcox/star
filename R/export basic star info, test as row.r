update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_star_and_student_data()
names(df)[names(df) == 'true_grade'] <- 'grade'
df$goal <- apply(df, 1, goal_level_from_row)
df$gap <- apply(df, 1, gap)

save_df_as_csv(df, 'star data, tests as rows')