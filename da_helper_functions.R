
calculate_se <- function(lm_model, se = 'robust', max_lag) {
    if (!se %in% c('traditional', 'robust', 'newey-west')) stop("se should be one of traditional, robust or newey-west (default is robust).")
    if (!require(sandwich)) stop("Required sandwich package is missing.")

    if (se == 'robust') {
        sqrt(diag(vcovHC(lm_model, type="HC1")))
    } else if (se == 'newey-west') {
        sqrt(diag(NeweyWest(lm_model, lag = max_lag, prewhite = FALSE)))
    } else {
        sqrt(diag(vcov(lm_model)))
    }
}

summary_r <- function(model, se = 'robust', max_lag = 0, ...) {

    sumry <- summary(model)
    table <- coef(sumry)
    table[, 2] <- calculate_se(model, se, max_lag)
    table[, 3] <- table[,1]/table[, 2]
    table[, 4] <- 2*pt(abs(table[, 3]), df.residual(model), lower.tail=FALSE)

    sumry$coefficients <- table
    p <- nrow(table)
    if (p > 1) {
        if (se == 'robust') {
            hyp <- cbind(0, diag(p - 1))    
            sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust="hc1")[2, "F"]
        } else if (se == 'newey-west') {
            sumry$fstatistic[1] <- NA
        }
    }

    print(sumry)
    cat("Number of observations:", length(residuals(model)), "\n\n")

    if (se == 'robust') {
        cat("Note: Heteroscedasticity-consistent standard errors (adjustment HC1)\n")
    } else if (se == 'newey-west') {
        cat("Note: Newey-West standard errors - maximum lag:", max_lag, "\n")
    }
    

}

stargazer_r <- function(list_of_models, type="text", align=TRUE, no.space=TRUE,
                        omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "sigma2"), 
                        se = 'robust', max_lag = 0, ...) {
    if (!require(stargazer)) stop("Required stargazer package is missing.")
    
    if (class(type) != "character") stop("Different models should be given in a list.")
    if (class(list_of_models) == "lm") list_of_models <- list(list_of_models)
    if (!length(se) %in% c(1, length(list_of_models))) stop("For parameter se you should give one string (if you want to apply it to all models) or a list of strings (if you want to apply different types of standard error for the different models). The string could take traditional, robust, and newey-west (default is robust).")

    if (length(se) == 1) {
        note <- paste(capwords(se[[1]]), "standard errors in parentheses")
        se <- as.list(rep(se[[1]], length(list_of_models)))
    } else {
        note <- "Standard errors in parentheses"
    }

    if (length(max_lag) == 1) {
        max_lag <- as.list(rep(max_lag[[1]], length(list_of_models)))
        if (all(se == 'newey-west')) {
            note <- paste(note, "- max lag:", max_lag[[1]])
        }
    }

    if (any(se == 'newey-west')) omit.stat <- c(omit.stat, 'rsq')

    list_se_robust <- lapply(
        seq_along(list_of_models), 
        function(j) {
            if (class(list_of_models[[j]]) == 'lm') {
                calculate_se(list_of_models[[j]], se = se[[j]], max_lag = max_lag[[j]])
            } else {
                NULL
            }
        }
    )
    
    args <- list(...)
    if (!is.null(args[['out']])) type="html"
    
    stargazer(
        list_of_models,
        se = list_se_robust,
        report ="vcs*",
        notes = note,
        type = type, align = align, omit.stat = omit.stat, no.space = no.space,
        ...
    )
}


pperron <- function(x, model = c('constant', 'trend'), type = "Z-tau") {
    if (!require(urca)) stop("Required urca package is missing.")

    results <- ur.pp(x, type = type, model = model)
    print(results)

    model <- match.arg(model)
    if (model == 'trend') trend = 'ct' else trend = 'c' 
    cat(
        "MacKinnon approximate p-value for Z-tau:", 
        punitroot(results@teststat, trend = trend), 
        "\n\n"
    )
}

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = "-" )
    sapply(strsplit(s, split = "-"), cap, USE.NAMES = !is.null(names(s)))
}

lags <- function(variable, lags) {
    var_string <- deparse(substitute(variable))
    paste(
        lapply(
            lags, 
            function(i) {
                paste0("lag(", var_string, ",", i, ")")
            }
        ),
        collapse = "+"
    )
}

d <- function(x) {
    c(NA, diff(x))
}

Arima <- function(..., transform.pars = FALSE) {
    model <- arima(...)

    # rename to be consistent with lm
    names(model$coef) <- gsub('intercept', '(Intercept)', names(model$coef))
    row.names(model$var.coef) <- gsub('intercept', '(Intercept)', row.names(model$var.coef))
    colnames(model$var.coef) <- gsub('intercept', '(Intercept)', colnames(model$var.coef))

    model
}
