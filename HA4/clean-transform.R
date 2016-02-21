library(data.table)
library(xts)
source("constant.R")

read.weather.as.ts <- function(location) {
  # Read weather dat into a data.table
  data <- read.csv(sprintf("%s.csv", location), as.is = TRUE)
  setDT(data)
  data[, CET := as.Date(CET)]
  data[, Events := as.factor(Events)]
  setkey(data, CET)
  dates <- data.table(CET = as.Date(min(data$CET):max(data$CET), origin = "1970-01-01"))
  setkey(dates, CET)
  data <- merge(dates, data, all.x = TRUE)
  data[, na.approx := is.na(Max.TemperatureC)]
  data[, Max.TemperatureC := na.approx(Max.TemperatureC)]
  data[, Max.TemperatureC.Diff := Max.TemperatureC - shift(Max.TemperatureC, type = "lag")]
  data[, Month := factor(format(CET, "%m1"))]
  data[, Feb := Month == "02"]
  data[, Mar := Month == "03"]
  data[, Apr := Month == "04"]
  data[, May := Month == "05"]
  data[, Jun := Month == "06"]
  data[, Jul := Month == "07"]
  data[, Aug := Month == "08"]
  data[, Sep := Month == "09"]
  data[, Oct := Month == "10"]
  data[, Nov := Month == "11"]
  data[, Dec := Month == "12"]
}

weather.as.ts <- function(data) {
  ts <- ts(data[CET >= as.Date("2005-01-01") & CET < as.Date("2016-01-01")]$Max.TemperatureC, start = c(2005,1), frequency = 365.25)
}

ln.diff.as.ts <- function(data) {
  ts <- ts(data[CET >= as.Date("2005-01-01") & CET < as.Date("2016-01-01")]$Max.TemperatureC.Diff[-1], start = c(2005,1), frequency = 365.25)
}

luxembourg.dt <- read.weather.as.ts(locations[[1]][1])
# brussels.dt <- read.weather.as.ts(locations[[2]][1])
# paris.dt <- read.weather.as.ts(locations[[3]][1])
# frankfurt.dt <- read.weather.as.ts(locations[[4]][1])
# zurich.dt <- read.weather.as.ts(locations[[5]][1])

luxembourg.ts <- weather.as.ts(luxembourg.dt)
# brussels.ts <- weather.as.ts(brussels.dt)
# paris.ts <- weather.as.ts(paris.dt)
# frankfurt.ts <- weather.as.ts(frankfurt.dt)
# zurich.ts <- weather.as.ts(zurich.dt)

luxembourg.diff.ts <- ln.diff.as.ts(luxembourg.dt)
# brussels.diff.ts <- ln.diff.as.ts(brussels.dt)
# paris.diff.ts <- ln.diff.as.ts(paris.dt)
# frankfurt.diff.ts <- ln.diff.as.ts(frankfurt.dt)
# zurich.diff.ts <- ln.diff.as.ts(zurich.dt)

