# Make PDFs of GGS by tier for each school
library(ggplot2)
library(scales)
library(reshape2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

create_tiered_ggs_plots <- function(school.name, d) {
	d.school <- subset(d, school==school.name)
	p.r <- make_ggs_plot(subset(d.school, subject == "reading"),
											c("tier", "grade.category"), "modeled.year.ggs",
											"Reading"
	)
	p.m <- make_ggs_plot(subset(d.school, subject == "math"),
											c("tier", "grade.category"), "modeled.year.ggs",
											"Math"
	)
	p <- arrangeGrob(p.r, p.m, ncol=2,
							main=paste0("\nSTAR Gap Closure by Tier for 2013-14, ", school.name)
	)
  save_plot_as_pdf(p, paste0('STAR Gap Growth Status by Tier ', school.name))
}

# Get data
ds <- generate_student_model_info_with_ps_and_rti(include.rti=T)
ds <- subset(ds, n > 2 & test.distance >= 150)

# Output GGS plot for reading
p <- make_ggs_plot(subset(ds, subject == "reading"),
              c("school", "grade.category"),
							"modeled.year.ggs", "Reading STAR Gap Closure for 2013-14"
)
save_plot_as_pdf(p, 'STAR Reading Gap Growth Status by Small School', wide=F)

# Output GGS plot for math
p <- make_ggs_plot(subset(ds, subject == "math"),
              c("school", "grade.category"),
							"modeled.year.ggs", "Math STAR Gap Closure for 2013-14"
)
save_plot_as_pdf(p, 'STAR Math Gap Growth Status by Small School', wide=F)

# Find tiers and make ggs by tier graph
ds$tier <- apply(ds, 1, find_tier)
sapply(schools, create_tiered_ggs_plots, d=ds)