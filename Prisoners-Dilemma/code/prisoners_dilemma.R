# Function to calculate payoffs in a Prisoner's Dilemma
prisoners_dilemma <- function(action1, action2, payoffs = c(3, 5, 0, 1)) {
    # Unpack the payoffs vector (R, T, S, P)
    R <- payoffs[1]
    T <- payoffs[2]
    S <- payoffs[3]
    P <- payoffs[4]

    # Create a payoff matrix where:
    # Rows = Player 1's actions, Columns = Player 2's actions
    payoff_matrix <- list(
        "Cooperate_Cooperate" = c(R, R),  # Both cooperate
        "Cooperate_Defect" = c(S, T),     # Player 1 cooperates, Player 2 defects
        "Defect_Cooperate" = c(T, S),     # Player 1 defects, Player 2 cooperates
        "Defect_Defect" = c(P, P)         # Both defect
    )

    # Concatenate the action strings to access the correct payoffs
    key <- paste(action1, action2, sep = "_")

    # Return a list with the actions and payoffs for both players
    return(data.frame(
        "Player 1 action" = action1,
        "Player 2 action" = action2,
        "Player 1 payoff" = payoff_matrix[[key]][1],
        "Player 2 payoff" = payoff_matrix[[key]][2]
    ))
}

# Example usage with default payoffs
#prisoners_dilemma("Defect", "Cooperate")

# Example usage with different payoffs (R = 2, T = 6, S = -1, P = 0)
#prisoners_dilemma("Defect", "Cooperate", payoffs = c(2, 6, -1, 0))
