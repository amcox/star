# Identify the students with high variance (sd and range)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_star_and_ps_data()

d.sum <- ddply(df, .(StudentId, school, grade, subject), summarize, n=length(DateTaken),
								sd=sd(GE, na.rm=T), range=max(GE, na.rm=T) - min(GE, na.rm=T)
)
d.sum$grade <- factor(d.sum$grade)
d.mult <- subset(d.sum, n > 1)

d.m <- subset(d.mult, subject=="math")
d.hr.m <- subset(d.m, range >= 3)
t.hr.m <- addmargins(table(d.hr.m$grade, d.hr.m$school))
t.all.m <- addmargins(table(d.m$grade, d.m$school))
t.prop.m <- (t.hr.m/t.all.m)
write.csv(t.prop.m, "./../Spreadsheets/props high range math.csv")
d.hr.m <- d.hr.m[,c("StudentId", "grade")]
d.hr.m$math.high.range <- rep(T, times=nrow(d.hr.m))

d.r <- subset(d.mult, subject=="reading")
d.hr.r <- subset(d.r, range >= 3)
t.hr.r <- addmargins(table(d.hr.r$grade, d.hr.r$school))
t.all.r <- addmargins(table(d.r$grade, d.r$school))
t.prop.r <- (t.hr.r/t.all.r)
write.csv(t.prop.r, "./../Spreadsheets/props high range reading.csv")
d.hr.r <- d.hr.r[,c("StudentId", "grade")]
d.hr.r$reading.high.range <- rep(T, times=nrow(d.hr.r))

d.m <- subset(d.mult, subject=="math")
d.hsd.m <- subset(d.m, sd >= 1.5)
t.hsd.m <- addmargins(table(d.hsd.m$grade, d.hsd.m$school))
t.all.m <- addmargins(table(d.m$grade, d.m$school))
t.prop.m <- (t.hsd.m/t.all.m)
write.csv(t.prop.m, "./../Spreadsheets/props high sd math.csv")
d.hsd.m <- d.hsd.m[,c("StudentId", "grade")]
d.hsd.m$math.high.sd <- rep(T, times=nrow(d.hsd.m))

d.r <- subset(d.mult, subject=="reading")
d.hsd.r <- subset(d.r, sd >= 1.5)
t.hsd.r <- addmargins(table(d.hsd.r$grade, d.hsd.r$school))
t.all.r <- addmargins(table(d.r$grade, d.r$school))
t.prop.r <- (t.hsd.r/t.all.r)
write.csv(t.prop.r, "./../Spreadsheets/props high sd reading.csv")
d.hsd.r <- d.hsd.r[,c("StudentId", "grade")]
d.hsd.r$reading.high.sd <- rep(T, times=nrow(d.hsd.r))

d.hv.r <- merge(d.hr.r, d.hsd.r, all=T)
d.hv.m <- merge(d.hr.m, d.hsd.m, all=T)
d.hv <- merge(d.hv.r, d.hv.m, all=T)
d.hv <- d.hv[,c("StudentId", "reading.high.sd", "reading.high.range",
								"math.high.sd", "math.high.range"
								)
]
d.hv <- merge(d.hv, df.ps[,c("StudentId", "last.name", "first.name", "grade", "school")])
write.csv(d.hv, "./../Spreadsheets/high variance students.csv", row.names=F)