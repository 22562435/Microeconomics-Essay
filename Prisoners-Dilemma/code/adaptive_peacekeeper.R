#This strategy adapts to the opponent's behavior with a probing mechanism that tests the opponent’s willingness to cooperate and adjusts based on their response.

adaptive_peacekeeper <- function(player_history, opponent_history) {
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

    # Rule 1: Defect if opponent has defected more than two times after a probe.
    if (consecutive_defections > 2) {
        return("Defect")
    }

    # Rule 2: Defect every 6 rounds as a probe.
    if (length(player_history) %% 6 == 0) {
        return("Defect")
    }

    # Rule 3: If the opponent matched a defection, return to cooperating.
    if (last_player_action == "Defect" && last_opponent_action == "Defect") {
        return("Cooperate")
    }

    # Default: Continue cooperating if no other rules apply.
    return("Cooperate")
}
