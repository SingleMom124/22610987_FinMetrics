
###########################################################################################
# Adjust fund returns for fees
###########################################################################################

# Create a function that converts and applies the bps

adjustFEE <- function(returns,  # Returns we want to adjust
                      fee)      # Basis points being charged

    {

    # Convert basis points to percentage

    fee_rate <- fee / 10000

    # Annual rate compounded monthly

    monthly_fee_rate <- (1 + fee_rate) ^ (1/12) - 1

    # Apply monthly fee rate cumulatively using compound interest

    adjusted_return <- return - monthly_fee_rate

    return(adjusted_return)

    }

