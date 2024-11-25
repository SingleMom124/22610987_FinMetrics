

##########################################################################################
# Re-balancing Portfolio
##########################################################################################

estimatePORT <- function(data,
                         size = 1000,
                         months = c(1, 7),
                         weights)

    {

    library(pacman)
    p_load(tidyverse, rmsfuns, xts)

    #----------------------------------------------------------------------------
    # Checking if there is a column of class "Date"
    #----------------------------------------------------------------------------

    date_col <- names(data)[sapply(data, inherits, "Date")]

    if (length(date_col) > 0){

        names(data)[names(data) == date_col] <- "Date"

    } else {

        stop("No column of class date found")

    }

    #----------------------------------------------------------------------------
    # Put in long format so the function always know that column names
    #----------------------------------------------------------------------------

    data <- data %>%
        pivot_longer(cols = -Date,
                     names_to = "Index",
                     values_to = "Returns") %>%
        arrange(Date)


    #----------------------------------------------------------------------------
    # Estimating the portfolio
    #----------------------------------------------------------------------------

    rebalance_months <- months

    num_col <- names(weights)[sapply(weights, inherits, "numeric")]

    if (length(num_col) > 0){

        names(weights)[names(weights) == num_col] <- "Weights"

    } else {

        stop("No column of numeric date found in weights")

    }

    name_col <- names(weights)[sapply(weights, inherits, "character")]

    if (length(name_col) > 0){

        names(weights)[names(weights) == name_col] <- "Index"

    } else {

        stop("No column of character date found in weights to attach weights")

    }

    weights <- data %>%
        mutate(m = as.numeric(format(x = Date,
                                     format = "%m")),
               ym = as.numeric(format(x = Date,
                                      format = "%Y%m"))) %>%
        dplyr::filter(m %in% rebalance_months) %>%
        group_by(ym, m, Index) %>%
        filter(Date == last(Date)) %>%
        ungroup() %>%
        dplyr::select(-m, -ym) %>%
        inner_join(weights,
                   by = "Index")

    weights_test <- weights %>%
        group_by(Date) %>%
        summarise(Fully_Invested = sum(Weights)) %>%
        filter(Fully_Invested > 1.000001 | Fully_Invested < 0.9999999 ) %>%
        nrow() > 0

    if(weights_test > 0){

        stop("Bruh, weights do not sum to one. Im not mad, just disappointed")

    }

    # Now lets calculate the portfolio

    Fund_Size_at_Start <- size

    # Convert data into wide format and xts format...

    weights_xts <- weights %>%
        dplyr::select(Date, Index, Weights) %>%
        pivot_wider(names_from = Index,
                    values_from = Weights) %>%
        tbl_xts()

    indices_xts <- data %>%
        pivot_wider(names_from = Index,
                    values_from = Returns)
    indices_xts[is.na(indices_xts)] <- 0
    indices_xts <- indices_xts %>%
        tbl_xts()

    # Pass to the rmsfuns function

    portfolio <- rmsfuns::Safe_Return.portfolio(R = indices_xts,
                                                weights = weights_xts,
                                                lag_weights = T,
                                                verbose = T,
                                                contribution = T,
                                                value = Fund_Size_at_Start,
                                                geometric = T)


    # Clean and save portfolio returns and weights:

    contribution <- portfolio$"contribution" %>%
        xts_tbl() %>%
        pivot_longer(cols = -date,
                     names_to = "Index",
                     values_to = "Contribution")

    bp_weight <- portfolio$"BOP.Weight" %>%
        xts_tbl() %>%
        pivot_longer(cols = -date,
                     names_to = "Index",
                     values_to = "Weights")
    bp_value <- portfolio$"BOP.Value" %>%
        xts_tbl() %>%
        pivot_longer(cols = -date,
                     names_to = "Index",
                     values_to = "Value_Held")

    # Now bind all of these columns together

    portfolio <- data %>%
        rename(date = Date) %>%
        left_join(bp_weight, by = c("date", "Index")) %>%
        left_join(bp_value, by = c("date", "Index")) %>%
        left_join(contribution, by = c("date", "Index"))

    # Now we can calculate the portfolio returns...

    portfolio_returns <- portfolio %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Returns * Weights,
                                        na.rm = T)) %>%
        filter(PortfolioReturn !=0)

    return(portfolio_returns)

}