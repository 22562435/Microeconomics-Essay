source("code/periodic_defector.R")
periodic_defector2_1 <- function(player_history,
                                 opponent_history,
                                 cooperate_for = 2,
                                 defect_for = 1) {
    return(
        periodic_defector(
            player_history,
            opponent_history,
            cooperate_for = 2,
            defect_for = 1
        )
    )
}