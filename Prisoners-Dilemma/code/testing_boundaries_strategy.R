#It attempts to gauge the opponent's response to a defection by either continuing to defect if the opponent is cooperative or returning to cooperation if the opponent retaliates.

probing_adjuster <- function(player_history, opponent_history) {
    # Check if there's any history. If not, start with "Cooperate".
    if (length(player_history) == 0) {
        return("Cooperate")
    }

    # Get the last action of both the player and the opponent
    last_player_action <- tail(player_history, 1)
    last_opponent_action <- tail(opponent_history, 1)

    # Check how many times the opponent has defected consecutively
    consecutive_defections <- 0
    for (i in seq_along(opponent_history)) {
        if (opponent_history[length(opponent_history) - i + 1] == "Defect") {
            consecutive_defections <- consecutive_defections + 1
        } else {
            break
        }
    }

    # Rule 1: If the opponent has defected three times consecutively, continue to defect.
    if (consecutive_defections >= 3) {
        return("Defect")
    }

    # Rule 2: If the last time the player defected, the opponent also defected, go back to cooperating.
    if (last_player_action == "Defect" && last_opponent_action == "Defect") {
        return("Cooperate")
    }

    # Rule 3: If the last time the player defected, the opponent cooperated, continue to defect.
    if (last_player_action == "Defect" && last_opponent_action == "Cooperate") {
        return("Defect")
    }

    # Rule 4: If the player cooperated last round, try defecting to test the opponent.
    if (last_player_action == "Cooperate") {
        return("Defect")
    }

    # Default to cooperating if no other rules apply.
    return("Cooperate")
}
