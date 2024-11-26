#################################################################################
# Estimate a GOGARCH Model
#################################################################################

estimateGOGARCH <- function(data,                   # Data to investigate
                            type,                   # Type of GARCH you want to apply across the series
                            garchOrder = c(1, 1),
                            armaOrder = c(1, 0)){


    library(pacman)
    p_load(rugarch, rmgarch, xts, tidyverse, ggplot2)

    # First specify a univariate GARCH

    uni_specifications <- rugarch::ugarchspec(variance.model = list(model = type,
                                                                    garchOrder = garchOrder),
                                              mean.model = list(armaOrder = armaOrder,
                                                                include.mean = T),
                                              distribution.model = "sstd")

    # Second, repeat it n times so each series is fitted with the specified GARCH

    multi_specifications <- rugarch::multispec(replicate(ncol(data), uni_specifications))

    # Third, push this through to a Go-GARCH specification

    go_specification <- rmgarch::gogarchspec(multi_specifications,
                                             distribution.model = "mvnorm",
                                             ica = "fastica")

    # Fourth, fit a multi-variable model

    multiple <- rugarch::multifit(multi_specifications,
                                  data,
                                  cluster = makePSOCKcluster(10))

    # Can now fit the Go-GARCH

    gogarch <- rmgarch::gogarchfit(spec = go_specification,
                                   data = data,
                                   solver = "hybrid",
                                   cluster = makePSOCKcluster(10),
                                   gfun = "tanh",
                                   maxiter1 = 40000,
                                   epsilon = 1e-08,
                                   rseed = 100)

    # From this we can extract the sigmas and time varying correlations

    Sigma <- sigma(gogarch)

    TV_Cor <- rcor(gogarch)
    TV_Cor <- aperm(TV_Cor, c(3,2,1))
    dim(TV_Cor) <- c(nrow(TV_Cor), ncol(TV_Cor)^2)

    # This has kak naming though so fix it with the following function

    output <- list(Sigma = Sigma,
                   TV_Cor = TV_Cor)

    return(output)

}