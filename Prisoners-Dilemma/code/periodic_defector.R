periodic_defector <- function(player_history, opponent_history, cooperate_for = 2, defect_for = 1) {
    total_rounds <- length(opponent_history)
    if (total_rounds < cooperate_for) {
        return("Cooperate")
    } else if (total_rounds < cooperate_for + defect_for) {
        return("Defect")
    } else {
        return(ifelse(((total_rounds - cooperate_for - defect_for) %% (cooperate_for + defect_for)) < cooperate_for,
                      "Cooperate", "Defect"))
    }
}
