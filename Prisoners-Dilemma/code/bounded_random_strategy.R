bounded_random_strategy <- function(player_history, opponent_history) {
    # Initialize defaults
    if (length(opponent_history) == 0) {
        return("Cooperate")  # Cooperate if there's no history
    }

    # Count the number of cooperations and defections in the opponent's history
    cooperations <- sum(opponent_history == "Cooperate")
    defections <- sum(opponent_history == "Defect")

    # Calculate total actions
    total_actions <- cooperations + defections

    # Calculate the probability of defecting
    if (total_actions == 0) {
        defect_probability <- 0  # No history, default to cooperation
    } else {
        defect_probability <- defections / total_actions
    }

    # Randomly choose to cooperate or defect based on the calculated probability
    if (runif(1) < defect_probability) {
        return("Defect")
    } else {
        return("Cooperate")
    }
}
