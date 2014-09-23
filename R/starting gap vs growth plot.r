# Plot of starting gap vs. growth by schools
library(ggplot2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

starting_gap_vs_gap_growth_plot <- function(school.name, d) {
	d.school <- subset(d, school == school.name)
	p <- ggplot(d.school, aes(x=first.modeled.gap, y=modeled.year.gap.growth))+
		geom_point(alpha=.3)+
		geom_smooth(method="lm")+
		labs(title=paste0("STAR Starting Gap vs. Gap Growth 2013-14 ", school.name),
		x="Starting Gap from Grade Level in Years.Months\n(negative is below level)",
		y="Gap Growth for the Full Year, Based on Linear Model, in Years.Months\n(negative is gap closure)"
		)+
		theme_bw()+
		facet_grid(grade.category ~ subject)
	save_plot_as_pdf(p, paste0('STAR Starting Gap vs Gap Growth ', school.name))
}

ds <- generate_student_model_info_with_ps_and_rti(include.rti=F)
ds <- subset(ds, n > 2 & test.distance >= 150)

sapply(schools, starting_gap_vs_gap_growth_plot, d=ds)