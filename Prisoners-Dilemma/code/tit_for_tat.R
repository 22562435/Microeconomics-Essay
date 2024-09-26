tit_for_tat <- function(previous_actions, opponent_actions) {
    if (length(opponent_actions) == 0) {
        return("Cooperate")  # Cooperate if it's the first round
    } else {
        # Mimic the opponent's last action
        return(opponent_actions[length(opponent_actions)])
    }
}
