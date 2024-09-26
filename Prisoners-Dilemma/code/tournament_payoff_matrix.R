tournament_payoff_matrix <- function(strategies, rounds = 50, payoffs = c(3, 5, 0, 1), p = 0) {
    # Get the number of strategies
    num_strategies <- length(strategies)

    # Initialize a matrix to store the results
    results_matrix <- matrix(0, nrow = num_strategies, ncol = num_strategies)





    # Extract unique strategy names
    strategy_names <- names(strategies)

    # Check for unique names
    if (length(unique(strategy_names)) != length(strategy_names)) {
        stop("Strategy names must be unique.")
    }

    # # Define strategy names
    # strategy_names <- sapply(strategies, function(x) deparse(substitute(x)))
    #
    # # Set row and column names for clarity
     rownames(results_matrix) <- strategy_names
     colnames(results_matrix) <- strategy_names

    # Play the tournament
    for (i in 1:num_strategies) {
        for (j in 1:num_strategies) {
            if (i != j) {
                # Get the payoffs from the head-to-head function
                result <- head_to_head(strategies[[i]], strategies[[j]], rounds, payoffs, p)

                # Store the payoff for strategy i against strategy j
                results_matrix[i, j] <- result[[1]] # Player 1's payoff is for the row strategy
            } else {
                # Same strategy against itself
                results_matrix[i, j] <- head_to_head(strategies[[i]], strategies[[j]], rounds, payoffs, p)[[1]] # Should reflect expected payoff against itself
            }
        }
    }

    # Convert to data frame for better readability
    results_df <- as.data.frame(results_matrix)

    # Add a total row for each strategy
    #results_df <- rbind(results_df, Total = rowSums(results_df))

    # Add a total column for each strategy
    results_df <- cbind(results_df, Total = c(rowSums(results_df)))
    results_df$Rank <- rank(-results_df$Total, ties.method = "min")
    return(results_df)
}


# # Ensure your strategies are defined
# strategies <- list(
#     Always_Cooperate = always_cooperate,
#     Always_Defect = always_defect,
#     Tit_for_Tat = tit_for_tat
# )
#
# # Run the tournament payoff matrix function
# symmetric_results <- tournament_payoff_matrix(strategies)
# print(symmetric_results)

