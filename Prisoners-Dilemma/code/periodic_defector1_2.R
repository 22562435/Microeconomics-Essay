source("code/periodic_defector.R")
periodic_defector1_2 <- function(player_history,
                                 opponent_history,
                                 cooperate_for = 1,
                                 defect_for = 2) {
    return(
        periodic_defector(
            player_history,
            opponent_history,
            cooperate_for = 1,
            defect_for = 2
        )
    )
}