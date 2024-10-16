#This strategy is designed to forgive quickly after probing with a defection, aiming to maintain cooperation but punishing persistent defections.
forgiving_tester <- function(player_history, opponent_history) {
    # Start with cooperation if no history exists
    if (length(player_history) == 0) {
        return("Cooperate")
    }

    # Get the last action of both the player and the opponent
    last_player_action <- tail(player_history, 1)
    last_opponent_action <- tail(opponent_history, 1)

    # Count consecutive defections by the opponent
    consecutive_defections <- 0
    for (i in seq_along(opponent_history)) {
        if (opponent_history[length(opponent_history) - i + 1] == "Defect") {
            consecutive_defections <- consecutive_defections + 1
        } else {
            break
        }
    }

    # Rule 1: If the opponent defected three times consecutively, switch to defection until they cooperate.
    if (consecutive_defections >= 3) {
        return("Defect")
    }

    # Rule 2: Defect once every 4 rounds as a test.
    if (length(player_history) %% 4 == 0) {
        return("Defect")
    }

    # Rule 3: If the last action was a probe (defect) and the opponent cooperated, continue cooperating.
    if (last_player_action == "Defect" && last_opponent_action == "Cooperate") {
        return("Cooperate")
    }

    # Rule 4: If the opponent matched a defection, forgive by returning to cooperate.
    if (last_player_action == "Defect" && last_opponent_action == "Defect") {
        return("Cooperate")
    }

    # Default: Cooperate if no other rules apply.
    return("Cooperate")
}
