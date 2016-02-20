library(data.table)

URL_TEMPLATE <- "https://www.wunderground.com/history/%s/%i/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=%i&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"
locations <- list(
  c("Luxembourg", "airport/ELLX"),
  c("Brussels", "airport/EBCI"),
  c("Paris", "airport/LFPO"),
  c("Frankfurt", "airport/EDDF"),
  c("Zurich", "airport/LSZH")
)


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