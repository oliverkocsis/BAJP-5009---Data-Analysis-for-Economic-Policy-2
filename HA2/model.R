library(data.table)
library(ggplot2)

# Load and clean
# Download from web
# cisco <- read.csv("http://real-chart.finance.yahoo.com/table.csv?s=CSCO&d=0&e=31&f=2016&g=d&a=2&b=26&c=1990&ignore=.csv", stringsAsFactors = FALSE)
# Local copy
cisco <- read.csv("csco.csv", stringsAsFactors = FALSE)
setDT(cisco)
cisco[, Date := as.Date(Date)]
setkey(cisco, Date)

# Explore
str(cisco)
summary(cisco)
ggplot(melt(cisco, id.vars = c("Date"), measure.vars = colnames(cisco)[2:5]), aes(x = variable, y = value)) + geom_boxplot()
ggplot(cisco, aes(x = Date, y = Close)) + geom_line()



# First and second ordered difference
cisco[, Close.Log := log(Close)]
ggplot(cisco, aes(x = Date, y = Close.Log)) + geom_line()
cisco[, Close.Log.Diff := Close.Log - shift(Close.Log, n=1, fill=NA, type="lag")]
ggplot(cisco, aes(x = Date, y = Close.Log.Diff)) + geom_line()
cisco[which.min(Close.Log.Diff)]
cisco[Date > as.Date("1993-03-10") & Date < as.Date("1993-03-25")]

# Find the dot.com maximum and add dummy
cisco[Date > as.Date("2000-01-01") & Date < as.Date("2005-01-01")][which.max(Close)]
cisco[, Dotcom.Before := Date <= as.Date("2000-03-22")][,Dotcom.After := Date > as.Date("2000-03-22")]
ggplot(cisco, aes(x = Dotcom.Before, y = Close)) + geom_boxplot()