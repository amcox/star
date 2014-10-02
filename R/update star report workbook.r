options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Open workbook and set default style to none
wb <- loadWorkbook('./../Spreadsheets/STAR Testing Rate.xlsx')
setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

# Write tiers
db <- students_with_tiers()
d <- db %>% select(student_number, subject, tier.num)
d$tier.num[is.na(d$tier.num)] <- 1
d.tiers <- d %>% spread(subject, tier.num)

clearRangeFromReference(wb, reference = "tiers!A1:C9999")
writeWorksheet(wb, d.tiers, 'tiers', header = TRUE)

# Save tiers summary file for pasting
dc <- db %>% group_by(small.school, subject) %>%
  summarize(total.students=n(),
    total.tiered.students=sum(!is.na(tier.num)),
    t2.students=sum(tier.num == 2, na.rm=T),
    t3.students=sum(tier.num == 3, na.rm=T),
    perc.t1=round((sum(is.na(tier.num)) / length(tier.num)), digits=2),
    perc.t2=round((sum(tier.num == 2, na.rm=T) / length(tier.num)), digits=2),
    perc.t3=round((sum(tier.num == 3, na.rm=T) / length(tier.num)), digits=2)
  )  
save_df_as_csv(dc, 'RTI Tier Percentages')

# Write raw data
df.m <- read.csv(file="./../Data/math_summary.csv", head=TRUE, na.string=c("", " ", "  "))
df.r <- read.csv(file="./../Data/reading_summary.csv", head=TRUE, na.string=c("", " ", "  "))

writeWorksheet(wb, df.r, 'ela', header = TRUE)
writeWorksheet(wb, df.m, 'math', header = TRUE)

# Write counts
df <- load_star_data()
names(df) <- c("id", "date", "GP", "SS", "GE", "PR", "NCE", "subject", "IRL",
								"LowerZPD", "UpperZPD"
)
ds <- df %>% group_by(id) %>% do(make_monitoring_counts(., pm.date='2014-09-07'))
ds$id <- extract_numeric(ds$id)
ds <- subset(ds, !is.na(id))

clearRangeFromReference(wb, reference = "counts!A1:E9999")
writeWorksheet(wb, ds, 'counts', header = TRUE)

# Write students
ds <- load_student_data()
clearRangeFromReference(wb, reference = "Students!A1:K9999")
writeWorksheet(wb, ds, 'Students', header = TRUE)

saveWorkbook(wb)

# Then convert Students!G to date, fill down students formulas from top, paste values into the Tier Summary from file exported, mabually set dates on headers