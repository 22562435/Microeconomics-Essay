#This strategy adjusts its probability of cooperating based on recent behavior of the opponent. It emphasizes maintaining a high cooperation rate initially, but becomes more cautious if the opponent defects frequently.


gradient_cooperation <- function(opponent_actions, my_actions) {
    cooperate_prob <- 1  # Start with full cooperation
    rounds <- length(opponent_actions)

    if (rounds == 0) {
        return("Cooperate")
    }

    # Adjust cooperation probability based on the last 5 actions
    last_actions <- tail(opponent_actions, 5)
    defect_count <- sum(last_actions == "Defect")

    # Decrease cooperation probability based on the number of defections
    cooperate_prob <- max(0, 1 - (defect_count / 5))

    return(ifelse(runif(1) < cooperate_prob, "Cooperate", "Defect"))
}
