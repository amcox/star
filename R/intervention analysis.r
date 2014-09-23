# Use STAR data to test the effectiveness of interventions
library(ggplot2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

reading.cols <- c("teacher.reading", "guided.reading", "reading.mastery",
									"boost", "blitz", "amplify", "ffw", "corrective.reading",
									"novel", "compass.ela"
)
math.cols <- c("compass.math", "teacher.math", "st.math", "accelerated.math",
							"math.flash", "dtm", "aleks", "algebra", "ixl"
)
logicalize_intervention_cols <- function(d) {
	# Change interventon columns to logical, with NA as F
	d$reading.mastery <- as.integer(d$reading.mastery)
	d$aleks <- as.integer(d$aleks)
	d[, reading.cols] <- lapply(d[, reading.cols], as.logical)
	d[, reading.cols][is.na(d[, reading.cols])] <- FALSE
	d[, math.cols] <- lapply(d[, math.cols], as.logical)
	d[, math.cols][is.na(d[, math.cols])] <- FALSE
	return(d)
}
intervention_gap_growth_plot <- function(d, title) {
	# Needs data with just modeled.gap.growth, tier, and interventions as cols
	d.m <- melt(d, id.vars=c("modeled.gap.growth", "tier"),
							variable.name="intervention", value.name="received"
	)
	ggplot(d.m, aes(x=intervention, y=modeled.gap.growth, fill=received))+
		geom_boxplot(notch=T, outlier.size=0)+
		labs(title=title,
					x="Intervention",
					y="Gap Growth in Years.Months (Negative is Closing)"
		)+
		theme_bw()+
		facet_wrap(~ tier, ncol=1, scales="free")
}

# Load data and make summary
df <- load_star_data()
df.ps <- load_ps_data()

ds <- generate_student_summaries(df, df.ps)

# Join on the PS data, including RTI info
ds <- merge(ds, df.ps)

ds <- logicalize_intervention_cols(ds)
ds$tier <- apply(ds, 1, find_tier)

# Check whether any intervention is true and make a new col with that info
ds$any.reading.intervention <- apply(ds[, reading.cols], 1, sum)
ds$any.reading.intervention <- ds$any.reading.intervention > 0
ds$any.math.intervention <- apply(ds[, math.cols], 1, sum) > 0

# Reading plot, whole network
d.sub <- subset(ds, subject == "reading")
d.sub <- subset(d.sub, test.distance > 45)
d.sub <- d.sub[, c("modeled.gap.growth", "tier", reading.cols)]
intervention_gap_growth_plot(d.sub,
	title=paste0("Impact of Interventions on Reading Growth, Whole Network\n",
	"Only Students with at Least Two Tests More Than 45 Days Apart\n",
	"Grouped by RTI Tier"
	)
)

# Math plot, whole network
d.sub <- subset(ds, subject == "math")
d.sub <- subset(d.sub, test.distance > 45)
d.sub <- d.sub[, c("modeled.gap.growth", "tier", math.cols)]
intervention_gap_growth_plot(d.sub,
	title=paste0("Impact of Interventions on Math Growth, Whole Network\n",
	"Only Students with at Least Two Tests More Than 45 Days Apart\n",
	"Grouped by RTI Tier"
	)
)