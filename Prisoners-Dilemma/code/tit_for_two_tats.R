tit_for_two_tats <- function(player_history, opponent_history) {
    if (length(opponent_history) < 2) {
        return("Cooperate")
    } else if (opponent_history[length(opponent_history)] == "Defect" &&
               opponent_history[length(opponent_history) - 1] == "Defect") {
        return("Defect")
    } else {
        return("Cooperate")
    }
}
