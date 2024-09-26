tit_for_tat_forgiveness <- function(player_history, opponent_history, forgiveness_prob = 0.1) {
    if (length(opponent_history) == 0) {
        return("Cooperate")
    } else if (opponent_history[length(opponent_history)] == "Defect" && runif(1) > forgiveness_prob) {
        return("Defect")  # Forgive with probability `forgiveness_prob`
    } else {
        return("Cooperate")  # Return to cooperate
    }
}
