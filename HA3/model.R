library(XML)
library(data.table)
library(xts)

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

plot(timeseries)
plot(diff(timeseries))