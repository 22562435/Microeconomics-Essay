random_strategy <- function(player_history, opponent_history,p = 0.5) {
    # Randomly choose "Cooperate" with probability p or "Defect" with probability 1-p
    if (runif(1) < p) {
        return("Cooperate")
    } else {
        return("Defect")
    }
}
