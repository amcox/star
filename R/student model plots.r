library(ggplot2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_star_and_ps_data()

df$goal <- apply(df, 1, goal_level)
df$gap <- apply(df, 1, gap)

# How many students have each number of tests?
t <- table(ddply(df, .(StudentId, subject), summarize, c=length(DateTaken))[,2:3])

# Pull a radom sample of students with a range number of tests and plot
# their scores with a set of smoothing models
b <- ddply(df, .(StudentId, subject, school), summarize, c=length(DateTaken))
b.s <- subset(b, c>=3 & subject=="reading" & school=="DTA")
b.r <- b.s[sample(nrow(b.s), min(nrow(b.s), 36)), ]
# a <- subset(df, subject=="reading" & StudentId %in% b.r$StudentId)
a <- subset(df, subject=="math" & StudentId %in% c("100002"))
ggplot(a, aes(x=DateTaken, y=GE))+
	geom_point()+
	# stat_smooth(method="loess", se=FALSE, span=2, color="red", family="symmetric")+
	# stat_smooth(method="loess", se=FALSE, color="orange", degree=1, enp.target=2)+
	# stat_smooth(method="loess", se=FALSE, span=2, color="green")+
	stat_smooth(method="lm", se=FALSE, color="blue")+
	scale_y_continuous()+
	facet_wrap(~StudentId)