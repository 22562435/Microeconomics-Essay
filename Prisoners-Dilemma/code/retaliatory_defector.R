retaliatory_defector <- function(player_history, opponent_history, retaliation_length = 2) {
    if (length(opponent_history) == 0) {
        return("Cooperate")
    } else if (opponent_history[length(opponent_history)] == "Defect") {
        # Check the length of defection streak
        defection_streak <- sum(rev(opponent_history) == "Defect")
        if (defection_streak <= retaliation_length) {
            return("Defect")
        } else {
            return("Cooperate")
        }
    } else {
        return("Cooperate")
    }
}
