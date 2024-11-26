
#########################################################################################
# Determine the best Uni-variate GARCH model
#########################################################################################

bestGARCH <- function(data,                                               # Series to run the models on
                      comp = c("sGARCH","gjrGARCH","eGARCH","apARCH"),    # Models to compare
                      garchOrder = c(1, 1),                               # Order of the GARCH
                      armaOrder = c(1, 0),                                # Order of the ARMA process
                      fit = "Akaike")                                     # Selection criteria
    {

    # There are three steps to this function
    # Loading the necessary packages

    library(pacman)
    pacman::p_load(rugarch, huxtable, tidyverse)

    #--------------------------------------------------------------------------------------
    # STEP 1: Estimate the models
    #--------------------------------------------------------------------------------------

    models = 1:length(comp)
    model_list = list()

    for (p in models){

        garch_spec <- rugarch::ugarchspec(variance.model = list(model = comp[p],
                                                                garchOrder = garchOrder),
                                          mean.model = list(armaOrder = armaOrder,
                                                         include.mean = TRUE),
                                          distribution.model = "norm")

        garch_fit <- rugarch::ugarchfit(spec = garch_spec,
                                        data = data)

        model_list[[p]] = garch_fit
    }

    # Putting the model criteria into a matrix to evaluate

    names(model_list) <- comp

    fit_mat <- sapply(model_list, infocriteria)

    Model_Fits <- data.frame(Criteria = rownames(infocriteria(model_list[[1]])),
                            fit_mat)

    #--------------------------------------------------------------------------------------
    # STEP 2: Estimate the News Impact Curves
    #--------------------------------------------------------------------------------------

    nic_list <- list()

    for (i in seq_along(model_list)) {

        model <- model_list[[i]]

        nic <- rugarch::newsimpact(z = NULL,
                                   object = model)

        nic_df <- cbind(nic$zx, nic$zy)

        model_name <- names(model_list)[i]

        colnames(nic_df) <- c("Epsilon", "Sigma")

        nic_df <- data.frame(nic_df)
        nic_df$Model <- rep(model_name, nrow(nic_df))

        nic_list[[model_name]] <- nic_df
    }

    nic <- do.call(rbind, nic_list) %>%
        data.frame() %>%
        ggplot() +
        aes(x = Epsilon,
            y = Sigma,
            color = Model) +
        geom_line(size = 0.5) +
        labs(title = "News Impact Curves") +
        theme_bw()

    #--------------------------------------------------------------------------------------
    # STEP 3: Select the model with the best fit according the chosen selection criteria
    #--------------------------------------------------------------------------------------

    optimal <- Model_Fits %>%
        pivot_longer(cols = -Criteria,
                     names_to = "Model",
                     values_to = "Value") %>%
        filter(Criteria == fit) %>%
        filter(Value == min(Value)) %>%
        pull(Model)

    Best_Model <- model_list[[optimal]]

    # Return the three steps in a list

    output <- list(Model = Best_Model,
                   Criteria = Model_Fits,
                   NIC = nic)

    names(output)[names(output) == "Model"] <- optimal

    return(output)

}
