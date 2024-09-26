head_to_head <- function(Strategy1, Strategy2, rounds, payoffs = c(3, 5, 0, 1), p = 0) {
    # Initialize history vectors for actions
    history1 <- character(rounds)
    history2 <- character(rounds)

    # Initialize total payoffs
    total_payoff1 <- 0
    total_payoff2 <- 0

    # Play the game for the specified number of rounds
    for (round in 1:rounds) {
        # Get actions from both strategies
        action1 <- if (round == 1) {
            Strategy1(character(0), character(0))  # No history in the first round
        } else {
            Strategy1(history1[1:(round-1)], history2[1:(round-1)])
        }

        action2 <- if (round == 1) {
            Strategy2(character(0), character(0))  # No history in the first round
        } else {
            Strategy2(history2[1:(round-1)], history1[1:(round-1)])
        }

        # Record the actions
        history1[round] <- action1
        history2[round] <- action2

        # Get the payoffs from the prisoners_dilemma function
        result <- prisoners_dilemma(action1, action2, payoffs)

        # Update total payoffs using p
        total_payoff1 <- total_payoff1 + result["Player.1.payoff"] * (1 - p) + result["Player.2.payoff"] * p
        total_payoff2 <- total_payoff2 + result["Player.2.payoff"] * (1 - p) + result["Player.1.payoff"] * p
    }

    # Return a data frame with the results
    return(data.frame(
        "Strategy 1" = deparse(substitute(Strategy1)),
        "Strategy 2" = deparse(substitute(Strategy2)),
        "Total Player 1 Payoff" = total_payoff1,
        "Total Player 2 Payoff" = total_payoff2
    ))
}
