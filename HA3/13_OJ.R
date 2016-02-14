library(readr)
library(dplyr)
library(ggplot2)
library(zoo)  # easier work with dates
library(gridExtra)  # to combine graphs
library(urca)  # for unit root and cointegration tests
library(sandwich)  # for robust estimation of covariance matrix
library(stargazer)  # for summarizing the models in a table


source('../da_helper_functions.R')

# convert Stata date formats to standard yyyy-mm-dd format
oj_price <- read_csv('oj_price_fdd.csv')
oj_price$time <- as.yearmon(oj_price$time, '%Ym%m')



# Graphs -----------------------------------------------------------------------

oj_price <- oj_price %>%
    mutate(
        ln_price = log(ppioj), 
        dpct_price = 100*(ln_price - lag(ln_price)),
        month = as.numeric(format(time, '%m'))
    )

# graphs with copy paste 
oj_price %>% ggplot(aes(x = time, y = ppioj)) + geom_line(size = 1) + scale_x_yearmon()
oj_price %>% ggplot(aes(x = time, y = ln_price)) + geom_line(size = 1) + scale_x_yearmon()
oj_price %>% ggplot(aes(x = time, y = dpct_price)) + geom_line(size = 1) + scale_x_yearmon()
oj_price %>% ggplot(aes(x = time, y = fdd)) + geom_line(size = 1) + scale_x_yearmon()


# graphs with function on the same plot
monthly_plot <- function(variable) { 
    oj_price %>%
        ggplot(aes_string(x = "time", y = variable)) +
        geom_line(size = 1) +
        scale_x_yearmon()
}

variables_of_interest <- c('ppioj', 'ln_price', 'dpct_price', 'fdd')

list_of_graphs <- lapply(variables_of_interest, monthly_plot)
do.call(grid.arrange, list_of_graphs)


# Use zoo package for plotting as well
orange_juice <- zoo(select(oj_price, -time, -month), order.by = oj_price$time)
plot(orange_juice)

# Unit root tests --------------------------------------------------------------

pperron(oj_price$ppioj)
pperron(oj_price$ln_price)
pperron(oj_price$dpct_price)
pperron(oj_price$fdd)

# within a for loop
for (variable in variables_of_interest) {
    print(variable)
    pperron(oj_price[[variable]])
}

# Regressions ------------------------------------------------------------------

# table1

myreg1 <- lm(d(dpct_price) ~ 1, oj_price)
summary_r(myreg1, se = 'newey-west', max_lag = 25)

myreg2 <- lm(d(fdd) ~ 1, oj_price)
summary_r(myreg2, se = 'newey-west', max_lag = 25)

myreg3 <- lm(dpct_price ~ factor(month), oj_price)
summary_r(myreg3, se = 'newey-west', max_lag = 25)

myreg4 <- lm(fdd ~ factor(month), oj_price)
summary_r(myreg4, se = 'newey-west', max_lag = 25)

stargazer_r(
    list(myreg1, myreg2, myreg3, myreg4), 
    se = 'newey-west', max_lag = 25, digits = 2,
    omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "rsq")
)

# table2

myreg5 <- lm(dpct_price ~ fdd, oj_price)
myreg6 <- lm(dpct_price ~ fdd + factor(month), oj_price)

stargazer_r(
    list(myreg5, myreg5, myreg5, myreg5, myreg6), 
    se = list('robust', 'newey-west', 'newey-west', 'newey-west', 'newey-west'),
    max_lag = list(NA, 2, 13, 25, 25),
    digits = 2,
    omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "rsq"),
    dep.var.caption = 'LHS: % change in price',
    dep.var.labels.include = FALSE,
    column.labels = c('robust SE', 'NW SE, lags 2', 'NW SE, lags 13', 'NW SE, lags 25', 'NW SE, lags 25')
)

# table3

myreg7 <- lm(dpct_price ~ fdd + lag(fdd), oj_price)
# with automatic lag inclusion
myreg7 <- lm(
    as.formula(
        paste("dpct_price ~ ", lags(fdd, 0:1))
    ), 
    oj_price
)

# everything together
max_lag <- list(1, 4, 7, 13)
myregs <- lapply(
    max_lag, function(l) {
        lm(
            as.formula(paste("dpct_price ~", lags(fdd, 0:l))),
            oj_price
        )
    }
)
myreg8 <- lm(
    as.formula(
        paste("dpct_price ~", lags(fdd, 0:13), "+ factor(month)")
    ),
    oj_price    
)
myregs[[5]] <- myreg8

stargazer_r(
    myregs, 
    se = 'newey-west',
    max_lag = 25,
    digits = 2,
    omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "rsq"),
    dep.var.caption = 'LHS: % change in price',
    dep.var.labels.include = FALSE,
    column.labels = c(sapply(max_lag, function(l) paste0("L(", l, ")")), "L(13)")
)

# table4

myreg9 <- lm(
    as.formula(
        paste("dpct_price ~", lags(fdd, c(1, 4, 7, 13)))
    ),
    oj_price
)
myreg9

fdd_lag <- c(1, 4, 7, 13)
dfdd_max_lag <- c(0, 3, 6, 12)
myregs2 <- lapply(
    seq_along(fdd_lag), function(i) {
        lm(
            as.formula(
                paste(
                    "dpct_price ~", 
                    lags(fdd, fdd_lag[i]), "+", lags(d(fdd), 0:dfdd_max_lag[i])
                )
            ),
            oj_price
        )
    }
)
myregs2 <- c(list(myreg2), myregs2)
stargazer_r(
    myregs2, 
    se = 'newey-west',
    max_lag = 25,
    digits = 2,
    omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "rsq"),
    dep.var.caption = 'LHS: % change in price',
    dep.var.labels.include = FALSE,
    column.labels = c(sapply(fdd_lag, function(l) paste0("L(", l, ")")))
)

