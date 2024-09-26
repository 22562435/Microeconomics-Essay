tit_for_tat_randomisation <- function(player_history, opponent_history, random_prob = 0.1) {
    if (length(opponent_history) == 0) {
        return("Cooperate")
    } else if (opponent_history[length(opponent_history)] == "Defect" && runif(1) < random_prob) {
        return("Defect")  # Randomly defect with probability `random_prob`
    } else {
        return(opponent_history[length(opponent_history)])  # Mimic opponent's last action
    }
}
