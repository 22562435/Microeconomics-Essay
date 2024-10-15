adaptive_defector <- function(player_history, opponent_history, threshold = 0.4, look_back = 5) {
    if (length(opponent_history) == 0) {
        return("Cooperate")
    }

    recent_opponent_moves <- tail(opponent_history, look_back)
    defect_rate <- sum(recent_opponent_moves == "Defect") / length(recent_opponent_moves)

    if (defect_rate > threshold) {
        return("Defect")
    } else {
        return("Cooperate")
    }
}
