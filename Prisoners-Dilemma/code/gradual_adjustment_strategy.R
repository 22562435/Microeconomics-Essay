# Strategy that adjusts its probability of cooperating over a set adjustment period.
gradual_adjustment_strategy <- function(player_history, opponent_history, increasing = TRUE, adjustment_period = 200) {
    # Get the current round number
    current_round <- length(player_history) + 1

    # Calculate the proportion of the game that has elapsed (0 at the start, 1 at the end)
    progress <- min(current_round / adjustment_period, 1)

    # Determine the probability of cooperation based on progress
    if (increasing) {
        # Start with a low probability and increase to high as the game progresses
        coop_probability <- progress  # Linearly scales from 0 to 1
    } else {
        # Start with a high probability and decrease to low as the game progresses
        coop_probability <- 1 - progress  # Linearly scales from 1 to 0
    }

    # Randomly choose to cooperate or defect based on the calculated probability
    if (runif(1) < coop_probability) {
        return("Cooperate")
    } else {
        return("Defect")
    }
}
