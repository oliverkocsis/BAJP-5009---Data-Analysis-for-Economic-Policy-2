library(data.table)
source("constant.R")


for (location in locations) {
  data <- data.table()
  for (year in 1996:2016) {
    url <- sprintf(URL_TEMPLATE, location[2], year, year)
    data <- rbind(data, read.csv(url, strip.white = TRUE, as.is = TRUE))
  }
  setDT(data)
  data[, CET := as.Date(CET)]
  data[, Events := as.factor(Events)]
  data[, WindDirDegrees := as.integer(gsub("<br />", "", WindDirDegrees.br...))]
  data[, WindDirDegrees.br... := NULL]
  write.csv(data, sprintf("%s.csv", location[1]))
}