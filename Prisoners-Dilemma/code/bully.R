bully <- function(opponent_actions, my_actions) {
    defect_count <- sum(opponent_actions == "Defect")

    # Cooperate if the opponent has defected more than 3 times in a row
    if (length(opponent_actions) > 3 && tail(opponent_actions, 3) == c("Defect", "Defect", "Defect")) {
        return("Cooperate")
    }

    return("Defect")
}
