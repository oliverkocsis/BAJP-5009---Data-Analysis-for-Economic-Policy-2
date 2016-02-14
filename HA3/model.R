library(XML)
library(data.table)
library(xts)
library(vars)
library(forecast)

diff(shift(1:10, type = "lag"))

#### Functions
explore.ts <- function(ts, lag = 0, difference = 0, title) {
  if (lag > 0) {
    ts <- diff(x = ts, lag = lag, differences = difference)
  }
  print(PP.test(ts))
  fit <- stl(usd_mxn_quarter, t.window=4, s.window="periodic", robust=TRUE)
  plot(fit,  main = title)
  plot(usd_mxn_quarter, col="gray", main = title)
  lines(fit$time.series[,2],col="red", ylab="Trend", main = title)
  monthplot(fit$time.series[,"seasonal"], main="", ylab=title)
  plot(forecast(fit, method="naive"), main = title)
  Acf(ts, main = title)
  Pacf(ts, main = title)
}

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
usd_mxn_quarter <- ts(usd_mxn[Quarter_Close == TRUE]$lnRate, start = c(1995, 1), frequency = 4)
for (lag in c(0, 1, 2, 4)) {
  title <- paste("% Exchange Rate of USD/MXN (Δ on ", lag ,")", sep = "")
  print(title)
  explore.ts(usd_mxn_quarter, lag = lag, difference = 1, title = title)
}

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
oecd_quarter <- ts(oecd$lnValue, start = c(1995, 1), frequency = 4)
for (lag in c(0, 1, 2, 4)) {
  title <- paste("% GDP (Δ on ", lag ,")", sep = "")
  print(title)
  explore.ts(oecd_quarter, lag = lag, difference = 1, title = title)
}

#### Dynamic Lag Analysis ####
data <- merge(usd_mxn[Quarter_Close == TRUE, .(Quarter, lnRate)], oecd[, .(TIME, lnValue)], by.x = "Quarter", by.y = "TIME")
data[, lnRate.Diff := lnRate - shift(lnRate, n=1, fill=NA, type="lag")]
data[, lnValue.Diff := lnValue - shift(lnValue, n=1, fill=NA, type="lag")]
data[, lnRate.Diff.Diff := lnRate.Diff - shift(lnRate.Diff, n=1, fill=NA, type="lag")]
data[, lnValue.Diff.Diff := lnValue.Diff - shift(lnValue.Diff, n=1, fill=NA, type="lag")]


lag.max <- 8
# Standard
summary <- data.table(i = 1:(lag.max + 2))
formula <- "lnValue.Diff ~ lnRate.Diff"
for (lag in 1:lag.max) {
  formula <- paste(formula, " + shift(lnRate.Diff, n = ", lag, ", fill=NA, type='lag')", sep = "")
}
print(formula)
fit <- lm(as.formula(formula), data = data)
Coefficients <- data.table(i = 1:(lag + 2), Coefficients = data.frame(summary(fit)["coefficients"])[,1])
NeweyWest.SE <- data.table(i = 1:(lag + 2), NeweyWest.SE = sqrt(diag(NeweyWest(fit, lag = lag))))
s <- merge(Coefficients, NeweyWest.SE, by = "i")
colnames(s) <- c("i", paste("Coefficients (", lag, ")", sep = ""), paste("NeweyWest SE (", lag, ")", sep = ""))
summary <- merge(summary, s, by = "i", all.y  = TRUE)
rownames(summary) <- rownames(data.frame(summary(fit)["coefficients"]))
summary


# Commulative
summary <- data.table(i = 1:(lag.max + 2))
for (lag in 0:lag.max) {
  formula <- paste("lnValue.Diff ~ shift(lnRate.Diff, n = ", lag, ", fill=NA, type='lag')", sep = "")
  print(formula)
  fit <- lm(as.formula(formula), data = data)
  Coefficients <- data.table(i = 1:(lag + 2), Coefficients = data.frame(summary(fit)["coefficients"])[,1])
  NeweyWest.SE <- data.table(i = 1:(lag + 2), NeweyWest.SE = sqrt(diag(NeweyWest(fit, lag = lag))))
  s <- merge(Coefficients, NeweyWest.SE, by = "i")
  colnames(s) <- c("i", paste("Coefficients (", lag, ")", sep = ""), paste("NeweyWest SE (", lag, ")", sep = ""))
  summary <- merge(summary, s, by = "i", all.y  = TRUE)
}
rownames(summary) <- rownames(data.frame(summary(fit)["coefficients"]))
summary
write.csv(summary, "dynamic.lags.csv")



#### Vector Autoregression ####
data <- merge(usd_mxn[Quarter_Close == TRUE, .(Quarter, lnRate)], oecd[, .(TIME, lnValue)], by.x = "Quarter", by.y = "TIME")
timeseries <- ts(data[, .(lnRate, lnValue)], start = c(1995, 1), frequency = 4)
plot(timeseries)
plot(diff(timeseries))
VARselect(diff(timeseries), lag.max=16, type="const")$selection
var <- VAR(diff(timeseries), p=4, type="const")
serial.test(var, lags.pt=4, type="PT.asymptotic")
summary(var)
fcst <- forecast(var)
plot(fcst, xlab="Year")
irf <- irf(var)
summary(irf)
plot(irf)
acf(diff(timeseries))
pacf(diff(timeseries))
