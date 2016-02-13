library(XML)
library(data.table)
library(xts)
### !!! ###
library(urca)


############## A Tompa Hülyeségei - start #####

set.seed(73)

Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

setwd("D:/Google Drive/Mikike/BusinessAnalytics/Tananyag/Data Analysis 2/HA/HA3/R/")
source("D:/Google Drive/Mikike/BusinessAnalytics/Tananyag/Data Analysis 2/HA/HA2/NASDAQ/da_helper_functions.R")

############## A Tompa Hülyeségei - stop #####


############## Loading, Eyeballing, Engineering 

# Banco de Mexico
usd_mxn <- read.csv("usdmxn.csv", stringsAsFactors = FALSE, strip.white=TRUE)
setDT(usd_mxn)
usd_mxn[, Date := as.Date(usd_mxn$Date, "%m/%d/%Y")]
summary(usd_mxn)

# NY.GDP.MKTP.KD	GDP (constant 2005 US$)
# NY.GDP.PCAP.KD	GDP per capita (constant 2005 US$)
gdp <- read.csv("mexico_data.csv", stringsAsFactors = FALSE, strip.white=TRUE)
setDT(gdp)
summary(gdp)

# Merge yearly data
usd_mxn[, Year := as.numeric(format(usd_mxn$Date,'%Y'))]
usd_mxn[, Year_Close_Date := max(Date) ,by = Year]
usd_mxn[Date == Year_Close_Date]

data <- merge(usd_mxn[Date == Year_Close_Date, .(Year, Date, Rate)], gdp[, .(Year, NY.GDP.MKTP.KD, NY.GDP.PCAP.KD)], by = "Year")
data <- data[Year < 2015]
timeseries <- ts(data[, 3:5, with = FALSE], start = min(data$Year))
### !!! ###
data$NY.GDP.MKTP.KD <- as.numeric(data$NY.GDP.MKTP.KD)
data$NY.GDP.PCAP.KD <- as.numeric(data$NY.GDP.PCAP.KD)

plot(timeseries)
plot(diff(timeseries))


############## Unit roots, stationarity, etc. 
############## 1) Rate

pptRate <- ur.pp(data[,Rate], type = "Z-tau")

pptRate
pptRate@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptRate@teststat))

# taking log

data[,lnRate := log(Rate)]

pptLnRate <- ur.pp(data[,lnRate], type = "Z-tau")

pptLnRate
pptLnRate@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptLnRate@teststat))


# take log diffs
data[, dlnRate := lnRate - lag(lnRate)]
data$dlnRate[1] = 0

pptDlnRate <- ur.pp(data[,dlnRate], type = "Z-tau")

pptDlnRate
pptDlnRate@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptDlnRate@teststat))



############## 2) GDP MKTP

pptGDPA <- ur.pp(data[,NY.GDP.MKTP.KD], type = "Z-tau")

pptGDPA
pptGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptGDPA@teststat))

# taking log

data[,lnNY.GDP.MKTP.KD := log(NY.GDP.MKTP.KD)]

pptLnGDPA <- ur.pp(data[,lnNY.GDP.MKTP.KD], type = "Z-tau")

pptLnGDPA
pptLnGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptLnGDPA@teststat))
### !!! log taking is satisfactory - no unit root

# take log diffs
data[, dlnNY.GDP.MKTP.KD := lnNY.GDP.MKTP.KD - lag(lnNY.GDP.MKTP.KD)]
data$dlnNY.GDP.MKTP.KD[1] = 0

pptDlnGDPA <- ur.pp(data[,dlnNY.GDP.MKTP.KD], type = "Z-tau")

pptDlnGDPA
pptDlnGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptDlnGDPA@teststat))



############## 3) GDP PCAP

pptGDPA <- ur.pp(data[,NY.GDP.PCAP.KD], type = "Z-tau")

pptGDPA
pptGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptGDPA@teststat))

# taking log

data[,lnNY.GDP.PCAP.KD := log(NY.GDP.PCAP.KD)]

pptLnGDPA <- ur.pp(data[,lnNY.GDP.PCAP.KD], type = "Z-tau")

pptLnGDPA
pptLnGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptLnGDPA@teststat))

# take log diffs
data[, dlnNY.GDP.PCAP.KD := lnNY.GDP.PCAP.KD - lag(lnNY.GDP.PCAP.KD)]
data$dlnNY.GDP.PCAP.KD[1] = 0

pptDlnGDPA <- ur.pp(data[,dlnNY.GDP.PCAP.KD], type = "Z-tau")

pptDlnGDPA
pptDlnGDPA@cval
cat("MacKinnon p-value for Z-tau: ", punitroot(pptDlnGDPA@teststat))
 
