schools <- c("RCAA", "STA", "DTA", "SCH", 'RSP')
grades <- c("3", "4", "5", "6", "7", "8")
small.schools <- c("RCAA35", "RCAA68", "STA35", "STA35", "DTA35", "DTA68",
									"SCH35", "SCH68"
)
grade.categories <- c("35", "68")

achievement.levels.with.laa <- c("A", "M", "B", "AB", "U", "B2", "AB2", "F", "PF", "ES", "MS", "WTS")
al.order.low.high <- c("WTS", "MS", "ES", "PF", "F", "AB2", "B2", "U", "AB", "B", "M", "A")
leap.als <- c("U", "AB", "B", "M", "A")
laa2.als <- c("PF", "F", "AB2", "B2")
laa1.als <- c("WTS", "MS", "ES")
leap.al.nums <- list("A"=5, "M"=4, "B"=3, "AB"=2, "U"=1)
laa2.al.nums <- list("B2"=4, "AB2"=3, "F"=2, "PF"=1)
laa1.al.nums <- list("ES"=3, "MS"=2, "WTS"=1)