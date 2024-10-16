#This strategy builds trust through initial cooperation, occasionally probes with a defection, and attempts to rebuild cooperation if the probe results in a breakdown.


cautious_rebuilder <- function(player_history, opponent_history) {
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

    # Rule 1: If opponent defected three times in a row, continue defecting until they cooperate.
    if (consecutive_defections >= 3) {
        return("Defect")
    }

    # Rule 2: If the opponent matched a defection, cooperate again to repair.
    if (last_player_action == "Defect" && last_opponent_action == "Defect") {
        return("Cooperate")
    }

    # Rule 3: Occasionally defect to test the opponent's tolerance (every 5th round).
    if (length(player_history) %% 5 == 0) {
        return("Defect")
    }

    # Default: Cooperate if no other rules apply.
    return("Cooperate")
}
