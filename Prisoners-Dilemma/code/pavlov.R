pavlov <- function(player_history, opponent_history) {
    if (length(player_history) == 0) {
        return("Cooperate")
    } else if (player_history[length(player_history)] == opponent_history[length(opponent_history)]) {
        return(player_history[length(player_history)])  # Stay with the last action
    } else {
        return("Defect")
    }
}
