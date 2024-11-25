
#################################################################################################
# Function to calculate returns
#################################################################################################

calculateRETURNS <- function(data,                       # Financial data of interest
                             type = c("log", "simple"),  # Type of returns we want
                             freq = "standard",          # Return frequency
                             start = NULL,               # Start date
                             end = NULL,                 # End date
                             stat = F)                   # Additional data we might want to see

    {

    library(pacman)
    p_load(tidyverse)

    #----------------------------------------------------------------------------
    # Checking if there is a column of class "Date"
    #----------------------------------------------------------------------------

    date_col <- names(data)[sapply(data, inherits, "Date")]

    if (length(date_col) > 0){

        names(data)[names(data) == date_col] <- "DATE"

    } else {

        stop("No column of class date found")

    }

    #----------------------------------------------------------------------------
    # Establishing and applying the time scale
    #----------------------------------------------------------------------------

    time_scale <- data$DATE

    if (is.null(start)){

        start <- first(time_scale)

    }

    if (is.null(end)){

        end <- last(time_scale)

    }

    data <- data %>%
        dplyr::filter(DATE >= start & DATE <= end)

    #----------------------------------------------------------------------------
    # Calculating log returns
    #----------------------------------------------------------------------------

    if ("log" %in% type){

        # Returns according to the original structure of the data (Use for daily or for if the data is already monthly for example)

        if (freq == "standard"){

            log <- data %>%
                arrange(DATE) %>%
                pivot_longer(cols = -DATE,
                             names_to = "Type",
                             values_to = "Return") %>%
                group_by(Type) %>%
                mutate(Log_Return = log(Return) - log(lag(Return))) %>%
                filter(DATE > first(DATE)) %>%
                ungroup()%>%
                dplyr::select(DATE, Type, Log_Return)

        }

        # Monthly returns

        if (freq == "month"){

            log <- data %>%
                arrange(DATE) %>%
                mutate(ym = format(DATE, "%Y-%m")) %>%
                group_by(ym) %>%
                dplyr::filter(DATE == dplyr::last(DATE)) %>%
                ungroup() %>%
                dplyr::select(-ym) %>%
                pivot_longer(cols = -DATE,
                             names_to = "Type",
                             values_to = "Return") %>%
                group_by(Type) %>%
                mutate(Log_Monthly = log(Return) - log(lag(Return))) %>%
                filter(DATE > first(DATE)) %>%
                ungroup() %>%
                dplyr::select(DATE, Type, Log_Monthly)

        }

        # Annual returns

        if (freq == "annual"){

            log <- data %>%
                mutate(ym = format(DATE, "%Y-%m")) %>%
                arrange(DATE) %>%
                group_by(ym) %>%
                dplyr::filter(DATE == dplyr::last(DATE)) %>%
                ungroup() %>%
                dplyr::select(-ym) %>%
                pivot_longer(cols = -DATE,
                             names_to = "Type",
                             values_to = "Return") %>%
                group_by(Type) %>%
                mutate(Log_Monthly = log(Return) - log(lag(Return))) %>%
                filter(DATE > dplyr::first(DATE)) %>%
                ungroup() %>%
                mutate(y = format(DATE, "%Y")) %>%
                group_by(y, Type) %>%
                mutate(Log_Annual = sum(Log_Monthly)) %>%
                ungroup() %>%
                dplyr::select(DATE, Type, Log_Annual)

        }

    }

    #----------------------------------------------------------------------------
    # Calculating simple returns
    #----------------------------------------------------------------------------

    # Simple returns requires geometric chaining
    # We do this because we are summing across time dimensions

    if ("simple" %in% type){

        # Returns according to the original structure of the data (Use for daily or for if the data is already monthly for example)

        if (freq == "standard"){

            simple <- data %>%
                arrange(DATE) %>%
                pivot_longer(cols = -DATE,
                             names_to = "Type",
                             values_to = "Return") %>%
                group_by(Type) %>%
                mutate(Simple_Return = Return / lag(Return) - 1) %>%
                filter(DATE > first(DATE)) %>%
                ungroup() %>%
                dplyr::select(DATE, Type, Simple_Return)

        }

        # Monthly returns

        if (freq == "month"){

            simple <- data %>%
                arrange(DATE) %>%
                mutate(ym = format(DATE, "%Y-%m")) %>%
                pivot_longer(cols = data %>% select_if(is.numeric) %>% names(),
                             names_to = "Type",
                             values_to = "Return") %>%
                group_by(Type) %>%
                mutate(Simple_Return = Return / lag(Return) - 1) %>%
                mutate(Simple_Return = coalesce(Simple_Return, 0)) %>%
                mutate(Month_Index = cumprod(1 + Simple_Return)) %>%
                group_by(ym) %>%
                dplyr::filter(DATE == dplyr::last(DATE)) %>%
                ungroup() %>%
                dplyr::select(-ym) %>%
                group_by(Type) %>%
                mutate(Simple_Monthly = Month_Index / lag(Month_Index) - 1) %>%
                arrange(Type) %>%
                ungroup() %>%
                dplyr::select(DATE, Type, Simple_Monthly)

        }

        # Still working on yearly simple returns

    }

        returns <- inner_join(log, simple, by = c("DATE", "Type"))

    #----------------------------------------------------------------------------
    # If you want to see additional statistical info
    #----------------------------------------------------------------------------

    returns <- returns %>%
        arrange(Type)

    if(stat == T){

        returns <- returns %>%
            mutate(across(.cols = c(starts_with("Simple"), starts_with("Log")),
                          .fns = list(Max = ~ max(.),
                                      Min = ~ min(.),
                                      Med = ~ median(.)),
                          .names = "{fn}_{col}"))

    }

    return(returns)

}

##########################################################################################
# Re-balancing Portfolio
##########################################################################################

estimatePORT <- function(data,
                         type = c("random", "ew"),
                         size = 100,
                         months = NULL,
                         max = NULL,
                         min = NULL)

    {

    library(pacman)
    p_load(tidyverse, rmsfuns, xts)

    #----------------------------------------------------------------------------
    # Checking if there is a column of class "Date" and applying time scale
    #----------------------------------------------------------------------------

    date_col <- names(data)[sapply(data, inherits, "Date")]

    if (length(date_col) > 0){

        names(data)[names(data) == date_col] <- "DATE"

    } else {

        stop("No column of class date found")

    }

    #----------------------------------------------------------------------------
    # Put in long format so the function always know that column names
    #----------------------------------------------------------------------------

    data <- data %>%
        pivot_longer(cols = -DATE,
                     names_to = "Stock",
                     values_to = "Price") %>%
        arrange(DATE) %>%
        group_by(Stock) %>%
        mutate(Return = Price / lag(Price) - 1) %>%
        ungroup() %>%
        dplyr::filter(DATE > first(DATE)) %>%
        dplyr::select(-Price)

    #----------------------------------------------------------------------------
    # Random Weight Portfolio
    #----------------------------------------------------------------------------

    if (type == "random"){

        rebalance_months <- months

        random_weights <- data %>%
            mutate(Month = as.numeric(format(x = DATE,
                                             format = "%m")),
                   YearMonth = as.numeric(format(x = DATE,
                                                 format = "%Y%m"))) %>%
            filter(Month %in% rebalance_months) %>%
            group_by(YearMonth, Month, Stock) %>%
            filter(DATE == last(DATE)) %>%
            ungroup()

        # Now create a random weighting vector for each stock.
        # All weights must sum to 1, this is a must, no matter the context
        # An additional constraint just for complexities sake is that each stock may have a maximum exposure of up to 20% of the equal weight.
        # Then let make minimum exposure of 2%

        no_stocks <- length(unique(random_weights$Stock))

        max_exposure <- (1 / no_stocks) * (max + 1)
        min_exposure <- min

        # Now to append the weight vector...
        # To do this we use the "random.bounded" function which I ripped from the "rportfolios" package
        # What it does is generate one random portfolio in which the asset allocations sum to the given total x.t and are...
        # ...constrained to be between the given vectors of lower and upper bounds

        random.bounded <- function(n = 2,             # A positive integer value which is the number of assets in the portfolio
                                   x.t = 1,           # A numeric value which is the sum of the allocations across all assets
                                   x.l = rep(0,n),    # A numeric vector of lower bounds for each of the assets in the portfolio
                                   x.u = rep(x.t,n),  # A numeric vector of upper bounds for each of the assets in the portfolio
                                   max.iter = 1000)   # A positive integer which is the maximum number of iterations in the rejection method loop
        {

            if ( n < 1 ) {
                stop( "argument n is not a positive integer" )
            }
            if ( n == 1 ) {
                x <- c(x.t)
                return( x )
            }
            if ( !is.vector( x.l ) ) {
                stop( "argument x.l is not a vector" )
            }
            if ( !is.vector( x.u ) ) {
                stop( "argument x.u is not a vector" )
            }
            if ( length( x.l ) != n ) {
                stop( "the length of argument x.l does not equal the number of assets n")
            }
            if ( length( x.u ) != n ) {
                stop( "the length of argument x.u does not equal the number of assets n" )
            }
            if ( sum ( x.l ) >= x.t ) {
                stop( "the sum of the lower bounds x.u is greater than or equal to x.t" )
            }
            if ( sum( x.u ) <= x.t ) {
                stop( "the sum of the upper bounds x.u is less than or equal to x.t" )
            }
            if ( any( x.l >= x.u ) ) {
                stop( "at least one of the lower bounds in x.l is greater than or equal to an upper bound in x.u" )
            }

            # compute the surplus allocation and allocation range

            x.s <- x.t - sum( x.l )
            x.r <- x.u - x.l

            # initial run parameters

            nm1 <- n - 1
            iter <- 0
            more <- TRUE
            while ( more ) {
                indices <- sample( 1:n, n, replace = FALSE )
                z <- rep( 0, n )
                U <- runif( n )
                thisIndex <- indices[1]
                lambda <- min( x.s, x.r[thisIndex] )
                z[thisIndex] <- lambda * U[thisIndex]
                x.s <- x.s - z[thisIndex]
                iter <- iter + 1
                if ( n > 2 ) {
                    for ( i in 2:nm1 ) {
                        thisIndex <- indices[i]
                        lambda <- min( x.s, x.r[thisIndex] )
                        z[thisIndex] <- lambda * U[thisIndex]
                        x.s <- x.s - z[thisIndex]
                    }
                }
                thisIndex <- indices[n]
                z[thisIndex] <- x.s
                z[thisIndex] <- min( x.s, x.r[thisIndex] )
                x <- x.l + z
                if ( x[thisIndex] <= x.u[thisIndex] ) {

                    # determine the un-allocated surplus

                    x.s <- x.t - sum( x )

                    # allocate the surplus if necessary

                    if ( x.s > 0 ) {

                        # determine which investments have slack relative to the upper bounds

                        x.g <- x.u - x

                        # select the order in which the investments are selected for assigning the slack

                        indices <- sample( 1:n, n, replace = FALSE )

                        # loop over the investments

                        for ( i in 1:n ) {
                            thisIndex <- indices[i]
                            if ( x.g[thisIndex] > 0 ) {
                                amount <- min( x.g[thisIndex], x.s )
                                x[thisIndex] <- x[thisIndex] + amount
                                x.s <- x.s - amount
                            }
                        }
                    }
                    return( x )
                }
                if ( iter > max.iter ) {
                    stop( "maximum number of iterations exceeded in random.bounded" )
                }
            }
            return( x )
        }

        weight_adjust <- bind_cols(random_weights %>%
                                       arrange(DATE),

                                   random_weights %>%
                                       group_by(DATE) %>%
                                       do(random_weights = random.bounded(n = nrow(.),
                                                                          x.t = 1,
                                                                          x.l = rep(x = min_exposure,
                                                                                    times = nrow(.)),
                                                                          x.u = rep(x = max_exposure,
                                                                                    times = nrow(.)),
                                                                          max.iter = 1000)) %>%
                                       ungroup() %>%
                                       unnest(random_weights) %>%
                                       select(-DATE)

        )

        # Make sure all of the weights sum to one

        weights_test <- weight_adjust %>%
            group_by(DATE) %>%
            summarise(Fully_Invested = sum(random_weights)) %>%
            filter(Fully_Invested > 1.000001 | Fully_Invested < 0.9999999 ) %>%
            nrow() > 0

        if(weights_test > 0){

            stop("Bruh, weights do not sum to one. Im not mad, just disappointed")

        }

        # Now lets calculate the portfolio
        # Lets say we start our fund with 1000 rand

        Fund_Size_at_Start <- size

        # Convert data into wide format and xts format...

        random_weights_xts <- weight_adjust %>%
            select(DATE, Stock, random_weights) %>%
            pivot_wider(names_from = Stock,
                        values_from = random_weights) %>%
            tbl_xts()

        stock_returns <- data %>%
            pivot_wider(names_from = Stock,
                        values_from = Return)
        stock_returns[is.na(stock_returns)] <- 0
        stock_returns_xts <- stock_returns %>%
            tbl_xts()

        # Pass the weights and returns data into the portfolio estimation function

        random_weights_portfolio <- rmsfuns::Safe_Return.portfolio(R = stock_returns_xts,
                                                                   weights = random_weights_xts,
                                                                   lag_weights = T,
                                                                   verbose = T,
                                                                   contribution = T,
                                                                   value = Fund_Size_at_Start,
                                                                   geometric = T)

        # Clean and save portfolio returns and weights...
        # ... making them data frames from xts and putting back into long format...
        # ...to join with the original data we made long:

        random_contribution <- random_weights_portfolio$"contribution" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Contribution")
        random_bp_weight <- random_weights_portfolio$"BOP.Weight" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Weight")
        random_bp_value <- random_weights_portfolio$"BOP.Value" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Value_Held")

        # Binding with the original data

        random_weights_portfolio <- data %>%
            rename(date = DATE) %>%
            left_join(random_bp_weight, by = c("date", "Stock")) %>%
            left_join(random_bp_value, by = c("date", "Stock")) %>%
            left_join(random_contribution, by = c("date", "Stock"))


        # Calculating the portfolio returns

        random_weight_portfolio_returns <- random_weights_portfolio %>%
            group_by(date) %>%
            summarise(PortfolioReturn = sum(Return * Weight,
                                            na.rm = T)) %>%
            filter(PortfolioReturn != 0)


        return(random_weight_portfolio_returns)

    }

    #----------------------------------------------------------------------------
    # Equal Weight Portfolio
    #----------------------------------------------------------------------------

    if(type == "ew"){

        # Establishing the equal weights

        rebalance_months <- months

        weights <- data %>%
            mutate(Month = as.numeric(format(x = DATE,
                                             format = "%m")),
                   YearMonth = as.numeric(format(x = DATE,
                                                 format = "%Y%m"))) %>%
            filter(Month %in% rebalance_months) %>%
            group_by(YearMonth, Month, Stock) %>%
            filter(DATE == last(DATE)) %>%
            ungroup()

        weight_adjust <- weights %>%
            group_by(DATE) %>%
            mutate(equal_weight = 1 / n()) %>%
            ungroup() %>%
            select(-Month, -YearMonth)

        # Now lets calculate the portfolio
        # Lets say we start our fund with 1000 rand

        Fund_Size_at_Start <- size

        # Convert data into wide format and xts format...

        equal_weights_xts <- weight_adjust %>%
            select(DATE, Stock, equal_weight) %>%
            pivot_wider(names_from = Stock,
                        values_from = equal_weight) %>%
            tbl_xts()

        stock_returns <- data %>%
            pivot_wider(names_from = Stock,
                        values_from = Return)
        stock_returns[is.na(stock_returns)] <- 0
        stock_returns_xts <- stock_returns %>%
            tbl_xts()

        # Pass to the rmdfun function

        equal_weights_portfolio <- rmsfuns::Safe_Return.portfolio(R = stock_returns_xts,
                                                                  weights = equal_weights_xts,
                                                                  lag_weights = T,
                                                                  verbose = T,
                                                                  contribution = T,
                                                                  value = Fund_Size_at_Start,
                                                                  geometric = T)


        # Clean and save portfolio returns and weights:

        equal_contribution <- equal_weights_portfolio$"contribution" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Contribution")
        equal_bp_weight <- equal_weights_portfolio$"BOP.Weight" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Weight")
        equal_bp_value <- equal_weights_portfolio$"BOP.Value" %>%
            xts_tbl() %>%
            pivot_longer(cols = -date,
                         names_to = "Stock",
                         values_to = "Value_Held")

        # Now bind all of these columns together

        equal_weight_portfolio <- data %>%
            rename(date = DATE) %>%
            left_join(equal_bp_weight, by = c("date", "Stock")) %>%
            left_join(equal_bp_value, by = c("date", "Stock")) %>%
            left_join(equal_contribution, by = c("date", "Stock"))

        # Now we can calculate the portfolio returns...

        equal_weights_portfolio_returns <- equal_weight_portfolio %>%
            group_by(date) %>%
            summarise(PortfolioReturn = sum(Return * Weight,
                                            na.rm = T)) %>%
            filter(PortfolioReturn !=0)

        return(equal_weights_portfolio_returns)

    }


}
