bully <- function(opponent_actions, my_actions) {
    # If the opponent has defected more than 3 times in a row, cooperate
    if (length(opponent_actions) > 3 && all(tail(opponent_actions, 3) == "Defect")) {
        return("Cooperate")
    }

    return("Defect")  # Otherwise, defect
}
