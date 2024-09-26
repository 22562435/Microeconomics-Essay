grim_trigger <- function(player_history, opponent_history) {
    if (length(opponent_history) == 0) {
        return("Cooperate")
    } else if ("Defect" %in% opponent_history) {
        return("Defect")
    } else {
        return("Cooperate")
    }
}
