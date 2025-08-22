library(readxl)
d <- read_xlsx("data/vercors-25-metadata.xlsx")

summary(d)


unique(d$saison)
d$date1 <- as.character(d$date-debut)