source("code/periodic_defector.R")
periodic_defector1_4 <- function(player_history,
                                 opponent_history,
                                 cooperate_for = 1,
                                 defect_for = 4) {
    return(
        periodic_defector(
            player_history,
            opponent_history,
            cooperate_for = 1,
            defect_for = 4
        )
    )
}