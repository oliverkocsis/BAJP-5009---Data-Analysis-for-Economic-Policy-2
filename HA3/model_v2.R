library(XML)
library(data.table)
library(xts)
library(vars)
library(forecast)

############## A Tompa Hülyeségei - start #####

set.seed(73)

Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

setwd("D:/Google Drive/Mikike/BusinessAnalytics/Tananyag/Data Analysis 2/HA/HA3/R/")
source("D:/Google Drive/Mikike/BusinessAnalytics/Tananyag/Data Analysis 2/HA/HA2/NASDAQ/da_helper_functions.R")

############## A Tompa Hülyeségei - stop #####

#### Data ####
#### USD/MXN Exchnage Rate ####
usd_mxn <- read.csv("usdmxn.csv", stringsAsFactors = FALSE, strip.white=TRUE)
setDT(usd_mxn)
str(usd_mxn)
summary(usd_mxn)
# Cleaning and transformation
usd_mxn[, Date := as.Date(usd_mxn$Date, "%m/%d/%Y")]
usd_mxn <- usd_mxn[Date >= as.Date("1995-01-01") & Date < as.Date("2015-09-01")]
usd_mxn[, lnRate := log(Rate)]
usd_mxn[, Year := as.numeric(format(usd_mxn$Date,'%Y'))]
usd_mxn[, Quarter := as.yearqtr(Date)]
usd_mxn[, Year_Close_Date := max(Date), by = Year]
usd_mxn[, Quarter_Close_Date := max(Date), by = Quarter]
usd_mxn[, Year_Close := Date == Year_Close_Date]
usd_mxn[, Quarter_Close := Date == Quarter_Close_Date]
summary(usd_mxn)


# Time series
# First without ln...
usd_mxn_quarter_bas <- ts(usd_mxn[Quarter_Close == TRUE]$Rate, start = c(1995, 1), frequency = 4)
PP.test(usd_mxn_quarter_bas)
str(PP.test(usd_mxn_quarter_bas))

# as it was...

usd_mxn_quarter <- ts(usd_mxn[Quarter_Close == TRUE]$lnRate, start = c(1995, 1), frequency = 4)
PP.test(usd_mxn_quarter)
plot(usd_mxn_quarter, 
     main = "Quarterly Exchnage Rate (USD/MXN)",
     ylab = "Exchnage Rate (USD/MXN)")
fit <- stl(usd_mxn_quarter, s.window=4)
plot(usd_mxn_quarter, col="gray",
     main="Trend of Quarterly Exchnage Rate (USD/MXN)",
     ylab="Exchnage Rate (USD/MXN)")
lines(fit$time.series[,2],col="red",ylab="Trend")

PP.test(diff(usd_mxn_quarter))
plot(diff(usd_mxn_quarter), 
     main = "Difference in Quarterly Exchnage Rate (USD/MXN)",
     ylab = "Exchnage Rate (USD/MXN)")
PP.test(diff(usd_mxn_quarter, lag = 4))
plot(diff(usd_mxn_quarter, lag = 4), 
     main = "Year on Year differene in Quarterly Exchnage Rate (USD/MXN)",
     ylab = "Exchnage Rate (USD/MXN)")

plot(fit)
plot(forecast(fit, method="naive"),
     main="Forecast of Quarterly Exchnage Rate (USD/MXN)",
     ylab="Exchnage Rate (USD/MXN)")

#### GDP (Quarterly) ####
oecd <- read.csv("oecd_mexico.csv", stringsAsFactors = FALSE, strip.white=TRUE)
setDT(oecd)
str(oecd)
summary(oecd)
oecd <- oecd[LOCATION == "MEX" & SUBJECT == "B1_GA" & MEASURE == "CQR" & FREQUENCY == "Q", .(TIME, Unit, Unit.Code,  PowerCode, PowerCode.Code, Value)]
oecd[, TIME := as.yearqtr(TIME, format = "%Y-Q%q")]
oecd[, lnValue := log(Value)]
oecd <- oecd[as.Date(TIME) >= as.Date("1995-01-01")]
str(oecd)
summary(oecd)
# Time series
# First without ln...
oecd_quarter_bas <- ts(oecd$Value, start = c(1995, 1), frequency = 4)
PP.test(oecd_quarter_bas)

# as it was...

oecd_quarter <- ts(oecd$lnValue, start = c(1995, 1), frequency = 4)
PP.test(oecd_quarter)
plot(oecd_quarter, 
     main = "Quarterly GDP (million Pesos)",
     ylab = "Exchnage Rate (USD/MXN)")
fit <- stl(oecd_quarter, s.window=4)
plot(oecd_quarter, col="gray",
     main="Trend of Quarterly GDP (million Pesos)",
     ylab="Exchnage Rate (USD/MXN)")
lines(fit$time.series[,2],col="red",ylab="Trend")

PP.test(diff(oecd_quarter))
plot(diff(oecd_quarter), 
     main = "Difference in Quarterly GDP (million Pesos)",
     ylab = "Exchnage Rate (USD/MXN)")
PP.test(diff(oecd_quarter, lag = 4))
plot(diff(oecd_quarter, lag = 4), 
     main = "Year on Year differene in Quarterly GDP (million Pesos)",
     ylab = "Exchnage Rate (USD/MXN)")

plot(fit)
plot(forecast(fit, method="naive"),
     main="Forecast of Quarterly Exchnage Rate (USD/MXN)",
     ylab="Exchnage Rate (USD/MXN)")

PPtest_dt=as.data.table(NULL)
attr(PPtest_dt,"row.names") <- c("Data", "Ln_Data", "LogDiff_Data", "LogDiffLag4_Data")
colnames(PPtest_dt) <- c("GDP", "Exchange_rate")




#### Dynamic Lag Analysis ####
data <- merge(usd_mxn[Quarter_Close == TRUE, .(Quarter, lnRate)], oecd[, .(TIME, lnValue)], by.x = "Quarter", by.y = "TIME")
timeseries <- ts(data[, .(lnRate, lnValue)], start = c(1995, 1), frequency = 4)
plot(timeseries)
plot(diff(timeseries))
VARselect(diff(timeseries), lag.max=16, type="const")$selection

#### Vector Autoregression ####
var <- VAR(diff(timeseries), p=1, type="const")
serial.test(var, lags.pt=1, type="PT.asymptotic")
summary(var)
fcst <- forecast(var)
plot(fcst, xlab="Year")

#### IRF #### 
plot(irf(var, impulse = 'lnRate', response = 'lnValue', ortho = FALSE))
plot(irf(var, impulse = 'lnValue', response = 'lnRate', ortho = FALSE))
gdp_var_rate <- data[, .(dlnRate = diff(lnRate), dlnValue = diff(lnValue))]

# lag1
var1 <- VAR(gdp_var_rate, p = 1)
plot(irf(var1, impulse = 'dlnRate', response = 'dlnValue', ortho = FALSE))
plot(irf(var1, impulse = 'dlnValue', response = 'dlnRate', ortho = FALSE))

#lag4
var4 <- VAR(gdp_var_rate, p = 4)
plot(irf(var4, impulse = 'dlnRate', response = 'dlnValue', ortho = FALSE))
plot(irf(var4, impulse = 'dlnValue', response = 'dlnRate', ortho = FALSE))

causality(var1, cause = "dlnRate")
 

