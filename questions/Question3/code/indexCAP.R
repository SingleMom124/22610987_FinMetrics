indexCAP <- function(data,       # Returns data. Need to be only three columns, date, return, and tickers
                     rebalance,  # Re-balance data. Also only three columns, date, tickers, and weight
                     cap)        # The size of the cap
    {

    # First using another function to determine what need to be rebalanced and when

    rebalancer <- function(data,
                           cap)

    {

        # First establishing a specific format for the data

        if( !"weight" %in% names(data)) stop("... for Calc capping to work, provide weight column called 'weight' in the rebalance argument")

        if( !"date" %in% names(data)) stop("... for Calc capping to work, provide date column called 'date' in the rebalance argument")

        if( !"tickers" %in% names(data)) stop("... for Calc capping to work, provide id column called 'tickers' in the rebalance argument")

        # Now identify the indices breaching the cap

        breachers <- data %>% filter(weight > cap) %>% pull(tickers)

        if(length(breachers) > 0) {

            while(data %>% filter(weight > cap) %>% nrow() > 0 ) {

                data <-bind_rows(data %>% filter(tickers %in% breachers) %>% mutate(weight = cap),
                                 data %>% filter(!tickers %in% breachers) %>% mutate(weight = (weight / sum(weight, na.rm = T)) * (1 - length(breachers) * cap)))

                breachers <- c(breachers, data %>% filter(weight > cap) %>% pull(tickers))

            }

            if( sum(data$weight, na.rm=T) > 1.001 | sum(data$weight, na.rm=T) < 0.999 | max(data$weight, na.rm = T) > cap) {

                stop("Issue with summing the weights")

            }

        } else {

        }

        data

    }

    if( !"rebalance_time" %in% names(rebalance)) stop("... for Calc capping to work, provide a rebalance dates column called 'rebalance_time' in the rebalance argument")

    Capped_df <- rebalance  %>%
        group_split(rebalance_time) %>%
        map_df(~rebalancer(.,
                           cap = cap) ) %>%
        dplyr::select(-rebalance_time)

    # Now use this to create index returns
    # First getting the weights

    wts <- Capped_df %>%
        tbl_xts(cols_to_xts = weight,
                spread_by = tickers)
    wts[is.na(wts)] <- 0

    if( !"return" %in% names(data)) stop("... for Calc capping to work, provide return column called 'return' in the data argument")

    if( !"date" %in% names(data)) stop("... for Calc capping to work, provide date column called 'date' in the data argument")

    if( !"tickers" %in% names(data)) stop("... for Calc capping to work, provide id column called 'tickers' in the data argument")

    if(ncol(data) > 3) stop("... too many columns in the data argument, supply only `date`, `tickers`, and `return` columns")

    # Then the returns

    rts <- data %>%
        tbl_xts(cols_to_xts = return,
                spread_by = tickers)
    rts[is.na(rts)] <- 0

    # Putting both into the portfolio function

    index <- rmsfuns::Safe_Return.portfolio(R = rts,
                                            weights = wts,
                                            lag_weights = T) %>%
        xts_tbl() %>%
        rename(portfolio = portfolio.returns)

}