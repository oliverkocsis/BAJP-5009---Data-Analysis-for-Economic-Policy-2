library(readr)
library(dplyr)
library(dummies)
library(ggplot2)
library(lmtest)  # for Breush-Godfrey
library(stargazer)

source('../da_helper_functions.R')

# First data manipulation ------------------------------------------------------

nasdaq <- read_csv('NASDAQ.csv')

nasdaq <- nasdaq %>%
    mutate(
        year = as.numeric(format(date, '%Y')),
        month = format(date, '%b'),
        day_of_week = weekdays(date)
    ) %>%
    arrange(date) %>%
    mutate(gap = factor(as.numeric(date - lag(date)) - 1))

# generate dummies for gap, month and day_of_week
nasdaq <- dummy.data.frame(as.data.frame(nasdaq))
names(nasdaq) <- gsub('month|day_of_week', '', names(nasdaq))

nasdaq %>% select(gap2, Monday) %>% table(.)

nasdaq <- nasdaq %>%
    rename(after_weekend = gap2, after_911 = gap6) %>%
    mutate(
        after_sandy = as.numeric(date == '2012-10-31'),
        after_holiday = as.numeric((gap3 > 0 | gap4 > 0) & after_sandy == 0),
        before_holiday = lead(after_holiday)
    )


# Exploratory part -------------------------------------------------------------

nasdaq %>% ggplot(aes(x = date, y = close)) + geom_line(size = 1)
nasdaq %>% ggplot(aes(x = date, y = volume)) + geom_line(size = 1)

# take logs and plot
nasdaq <- nasdaq %>%
    mutate(
        ln_close = log(close),
        ln_volume = ifelse(volume == 0, NA, log(volume))  # to have NA instead of Inf
    )

nasdaq %>% ggplot(aes(x = date, y = ln_close)) + geom_line(size = 1)
nasdaq %>% ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)
nasdaq %>% filter(year < 2001) %>% 
    ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)
nasdaq %>% filter(year >= 2001) %>% 
    ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)


# unit root tests

pperron(nasdaq$ln_close, model = 'trend')
pperron(nasdaq$ln_volume)
pperron(filter(nasdaq, year < 2001)$ln_volume)
pperron(filter(nasdaq, year < 2001)$ln_volume, 'trend')
pperron(filter(nasdaq, year >= 2001)$ln_volume)


# take log diffs
nasdaq <- nasdaq %>%
    mutate(
        return = ln_close - lag(ln_close),        
        dln_volume = ln_volume - lag(ln_volume)
    )

pperron(nasdaq$return)
pperron(nasdaq$dln_volume)

nasdaq %>% ggplot(aes(x = date, y = return)) + geom_line(size = 1)

# correlograms

acf(nasdaq$return)
pacf(nasdaq$return)

# make spline
nasdaq <- mutate(nasdaq, t = row_number())
knot <- which(nasdaq$year == 2001)[1]  # first day in 2001
nasdaq$t1 <- c(1:knot, rep(knot, nrow(nasdaq) - knot))
nasdaq$t2 <- c(rep(0, knot), 1:(nrow(nasdaq) - knot))

# Regressions ------------------------------------------------------------------

# return

days <- c('Monday', 'Tuesday', 'Wednesday', 'Friday')
x_vars <- c(
    'Jan', days, 
    'before_holiday', 'after_holiday', 'after_911', 'after_sandy', 
    't1', 't2'
)

myregs <- lapply(c(2, 12), function(max_lag) {
    lm(
        as.formula(paste(
            'return ~', 
            lags(return, 1:max_lag), 
            '+', paste(x_vars, collapse = '+')        
        )),
        data = nasdaq       
    )
})

arimareg <- Arima(nasdaq$return, order = c(2, 0, 1), xreg = nasdaq[x_vars])

resid <- na.omit(arimareg$residuals)
acf(resid)
pacf(resid)

myregs <- c(myregs, list(arimareg))
stargazer_r(
    myregs, se = 'newey-west', max_lag = 17,
    dep.var.labels = 'return'
)


# volume

myreg1 <- lm(
    as.formula(paste(
        'ln_volume ~', 
        lags(ln_volume, 1:15), 
        '+', paste(x_vars, collapse = '+')
    )),
    data = nasdaq   
)

arimareg1 <- Arima(nasdaq$ln_volume, order = c(2, 0, 1), xreg = nasdaq[x_vars])

arimareg2 <- Arima(nasdaq$ln_volume, order = c(10, 0, 10), xreg = nasdaq[x_vars])

resid <- na.omit(arimareg2$residuals)
acf(resid)
pacf(resid)


stargazer_r(
    list(myreg1, arimareg1, arimareg2), se = 'newey-west', max_lag = 17,
    dep.var.labels = "log volume"
)

# three periods: 1971-84, 1985-2000, 2001-16

x_vars <- x_vars[1:(length(x_vars)-2)]  # exclude spline terms

myreg1 <- lm(
    as.formula(paste(
        'return ~', 
        lags(return, 1:2), 
        '+', paste(x_vars, collapse = '+')
    )),
    data = filter(nasdaq, year <= 1984)   
)

myreg2 <- lm(
    as.formula(paste(
        'return ~', 
        lags(return, 1:2), 
        '+', paste(x_vars, collapse = '+')
    )),
    data = filter(nasdaq, year > 1984, year <= 2000)   
)

myreg3 <- lm(
    as.formula(paste(
        'return ~', 
        lags(return, 1:2), 
        '+', paste(x_vars, collapse = '+')
    )),
    data = filter(nasdaq, year > 2000)   
)


nasdaq8500 <- filter(nasdaq, year > 1984, year <= 2000)
arimareg1 <- Arima(
    nasdaq8500$ln_volume, order = c(2, 0, 1), 
    xreg = nasdaq8500[c(x_vars[1:7], 't')]  # exclude sandy and 9/11
)

nasdaq0116 <- filter(nasdaq, year > 2000)
arimareg2 <- Arima(
    nasdaq0116$return, order = c(2, 0, 1), xreg = nasdaq0116[c(x_vars, 't')]
)

stargazer_r(
    list(myreg1, myreg2, myreg3, arimareg1, arimareg2),
    se = 'newey-west', max_lag = 10,
    dep.var.labels = 'return',
    column.labels = c('71-84', '85-00', '01-16', '85-00', '01-16')
)

