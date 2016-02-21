library(data.table)
library(zoo)
library(vars)
library(forecast)
library(tseries)
source("clean-transform.R")

# Random Walk Forecast
tsdisplay(luxembourg.ts)
PP.test(luxembourg.ts, lshort = FALSE)
adf.test(luxembourg.ts, k = 365)
kpss.test(luxembourg.ts, lshort = FALSE)
luxembourg.stl <- stl(luxembourg.ts, s.window = 365.25 / 12, t.window = 365.25 * 4, robust = TRUE)
plot(luxembourg.stl)
luxembourg.fcast <- forecast(luxembourg.stl, h = 15, method="naive")
plot(luxembourg.fcast, include = 60, ylim = c(min(luxembourg.dt$Max.TemperatureC),max(luxembourg.dt$Max.TemperatureC)))

luxembourg.dt[CET >= as.Date("2016-01-01")]

# Monthly Average


# AR(2)

# Other Model
luxembourg.arima <- auto.arima(luxembourg.ts)
summary(luxembourg.arima)
luxembourg.fcast <- forecast(luxembourg.arima, h = 15)
plot(luxembourg.fcast, include = 60, ylim = c(min(luxembourg.dt$Max.TemperatureC),max(luxembourg.dt$Max.TemperatureC)))