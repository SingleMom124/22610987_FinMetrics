##############################################################################################
# More informative names for DCC model pairs
##############################################################################################

renameMGARCH <- function(series,   # Original data set
                         pairs,    # Pairs from the DCC model
                         long = T) # Whether you want it in long format or not

{

    no_series <- ncol(series)
    names_series <- colnames(series)
    paste(names_series, collapse = "_")

    nam <- c()

    xx <- mapply(rep, times = no_series:1, x = names_series)

    # Designing a nested for loop to save the names corresponding to the columns of interest..

    nam <- c()

    for (j in 1:(no_series)) {

        for (i in 1:(no_series)) {

            nam[(i + (j - 1) * (no_series))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")

        }

    }

    colnames(pairs) <- nam

    # So to plot all the time-varying correlations wrt SBK:
    # First append the date column that has (again) been removed...

    pairs <- data.frame(cbind(date = index(series), pairs)) %>%
        mutate(date = as.Date(date)) %>%
        tbl_df()

    if (long == T){

        pairs <- pairs %>%
            pivot_longer(cols = -date,
                         names_to = "Pairs",
                         values_to = "Rho")

    }

    return(pairs)

}

