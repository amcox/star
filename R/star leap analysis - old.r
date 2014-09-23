# STAR level vs LEAP scores
leap.als <- c("U", "AB", "B", "M", "A")
# d.s <- d.sum[!d.sum$first.last.same,]
# d.s <- d.s[d.s$first.last.day.dif>119,]
d.s <- d.sum[d.sum$leap.comparison.distance < 31,]
d.s <- drop.levels(d.s)

df.cutoffs <- read.csv(file="basic_cutoffs.csv", head=TRUE, na.string=c("", " ", "  "))
df.master.dash <- read.csv(file="masterdash.csv", head=TRUE, na.string=c("", " ", "  "))
df.md.e <- df.master.dash[,c("id", "L13_ELA_SS", "L13_ELA_AL")]
df.md.m <- df.master.dash[,c("id", "L13_Math_SS", "L13_Math_AL")]

d.a <- merge(d.s, df.md.e)
d.a <- merge(d.a, df.md.m)

d.e <- d.a[d.a$subject=="reading" & d.a$L13_ELA_AL %in% leap.als,c("id", "subject", "grade", "leap.comparison.ge", "leap.comparison.gap", "L13_ELA_SS")]
d.e <- drop.levels(d.e)
names(d.e) <- c("id", "subject", "grade", "ge", "gap", "ss")
d.m <- d.a[d.a$subject=="math" & d.a$L13_Math_AL %in% leap.als,c("id", "subject", "grade", "leap.comparison.ge", "leap.comparison.gap", "L13_Math_SS")]
d.m <- drop.levels(d.m)
names(d.m) <- c("id", "subject", "grade", "ge", "gap", "ss")
d.c <- rbind(d.e, d.m)

gap.models <- dlply(d.e, .(grade), lm, formula="leap.comparison.gap~L13_ELA_SS")
ge.models <- dlply(d.e, .(grade), lm, formula="leap.comparison.ge~L13_ELA_SS")
gap.models <- dlply(d.m, .(grade), lm, formula="leap.comparison.gap~L13_Math_SS")
ge.models <- dlply(d.m, .(grade), lm, formula="leap.comparison.ge~L13_Math_SS")
# GE does slightly better in F-stat than the gap, so use that
# In general, the R squareds were in the .3 range

ggplot(d.a[d.a$subject=="reading" & d.a$L13_ELA_AL %in% leap.als,], aes(x=leap.comparison.ge, y=L13_ELA_SS))+
	geom_hline(data=df.cutoffs[df.cutoffs$subject=="ela",], aes(yintercept=ss))+
	geom_point()+
	geom_smooth(method="lm")+
	scale_x_continuous(breaks=seq(0, 10, 1))+
	theme_bw()+
	facet_wrap(~grade)

# Make plots of each grade-subject that show percent scoring basic at or above each ge
calc.basic.threshes <- function(d){
	find.percent.basic <- function(cut.ge, data, cut.ss){
		mean(data[data$ge >= cut.ge,]$ss >= cut.ss, na.rm=TRUE)
	}
	g <- unique(d$grade)
	s <- unique(d$subject)
	ss.cut <- subset(df.cutoffs, grade==g & subject==s)[1,"ss"]
	ges <- unique(d$ge)
	just.percs <- sapply(ges, find.percent.basic, data=d, cut.ss=ss.cut)
	new.df <- data.frame(ge=ges, perc=just.percs)
	return(new.df)
}

threshes <- ddply(d.c, .(subject, grade), calc.basic.threshes)

make.star.thresh.plot <- function(d, title){
	p <- ggplot(d, aes(x=ge, y=perc))+
	geom_point()+
	scale_y_continuous(breaks=seq(0,1,.05), label=percent)+
	scale_x_continuous(breaks=seq(0,13,.5))+
	labs(title=title,
				x="Grade Equivalent on STAR",
				y="Percent of Students at or Above the Grade Equivalent that Scored Basic or Above"
	)+
	theme_bw()
	return(p)
}
save.plot <- function(plot, subject, grade){
	pdf(paste0("Output/", "STAR Cutoffs ", subject, " Grade ", grade, ".pdf"), width=10.5, height=8)
	print(plot)
	dev.off()
}
grades <- seq(3,8,1)
subjects <- c("reading", "math")
for(s in subjects){
	for(g in grades){
		d.sub <- subset(threshes, grade==g & subject==s)
		p <- make.star.thresh.plot(d.sub, paste0("STAR Cutoffs ", s, " Grade ", g))
		save.plot(p, s, g)
	}
}

# Distribution of STAR GE at each LEAP AL
leap.als <- c("U", "AB", "B", "M", "A")
d.s <- d.sum[d.sum$leap.comparison.distance < 31,]
d.s <- drop.levels(d.s)

df.master.dash <- read.csv(file="masterdash.csv", head=TRUE, na.string=c("", " ", "  "))
df.md.e <- df.master.dash[,c("id", "L13_ELA_AL")]
df.md.m <- df.master.dash[,c("id", "L13_Math_AL")]

d.a <- merge(d.s, df.md.e)
d.a <- merge(d.a, df.md.m)

d.e <- d.a[d.a$subject=="reading" & d.a$L13_ELA_AL %in% leap.als,c("id", "subject", "grade", "leap.comparison.ge", "L13_ELA_AL")]
d.e <- drop.levels(d.e)
names(d.e) <- c("id", "subject", "grade", "ge", "al")
d.m <- d.a[d.a$subject=="math" & d.a$L13_Math_AL %in% leap.als,c("id", "subject", "grade", "leap.comparison.ge", "L13_Math_AL")]
d.m <- drop.levels(d.m)
names(d.m) <- c("id", "subject", "grade", "ge", "al")
d.c <- rbind(d.e, d.m)

d.c$al <- reorder(d.c$al, new.order=c("U", "AB", "B", "M", "A"))

ggplot(d.c, aes(x=al, y=ge))+
	geom_boxplot(outlier.size=0, notch=T)+
	geom_jitter(position=position_jitter(height=0), alpha=.15)+
	scale_y_continuous(breaks=seq(0,13,1))+
	labs(title="Distribution of STAR Grade Levels by LEAP Achievement Levels",
			x="2013 LEAP Achievement Level",
			y="STAR Test Grade Equivalent Closes to LEAP"
	)+
	theme_bw()+
	facet_grid(subject~grade)

# By CR
d.babove <- d.c
d.babove$al <- as.character(d.babove$al)
d.babove[d.babove=="A"] = "CR"
d.babove[d.babove=="M"] = "CR"
d.babove[d.babove=="B"] = "CR"
d.babove$al <- factor(d.babove$al)
d.babove$al <- reorder(d.babove$al, new.order=c("U", "AB", "CR"))

ggplot(d.babove, aes(x=al, y=ge))+
	geom_boxplot(outlier.size=0, notch=T)+
	geom_jitter(position=position_jitter(height=0), alpha=.15)+
	scale_y_continuous(breaks=seq(0,13,1))+
	labs(title="Distribution of STAR Grade Levels by LEAP Achievement Levels",
			x="2013 LEAP Achievement Level",
			y="STAR Test Grade Equivalent Closes to LEAP"
	)+
	theme_bw()+
	facet_grid(subject~grade)