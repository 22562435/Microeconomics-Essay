source("code/gradual_adjustment_strategy.R")
gradual_adjustment_strategy_inc_200 <- function(player_history,
                                                opponent_history,
                                                increasing = TRUE,
                                                adjustment_period = 200) {
    gradual_adjustment_strategy_inc_200 <- gradual_adjustment_strategy(
        player_history,
        opponent_history,
        increasing = TRUE,
        adjustment_period = 200
    )
}