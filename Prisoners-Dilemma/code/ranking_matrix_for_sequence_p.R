ranking_matrix_for_sequence_p <- function(strategies, rounds = 50, payoffs = c(3, 5, 0, 1), p = seq(-0.1, 0.5, 0.05), rank = TRUE) {
    # Initialize an empty list to store the results for each p value
    results_list <- list()
source("code/tournament_payoff_df.R")
    # Iterate over each p value in the sequence
    for (current_p in p) {
        # Use the tournament_payoff_df function to get the results for this p
        tournament_results <- tournament_payoff_df(strategies, rounds, payoffs, current_p)

        # Extract the rank or total column name based on the current p
        if (rank) {
            col_name <- paste0("Rank p", current_p)
        } else {
            col_name <- paste0("Total p", current_p)
        }

        # Extract the relevant column and store it in the list with its p value as the name
        results_list[[as.character(current_p)]] <- tournament_results[[col_name]]
    }

    # Combine the results into a single data frame
    results_df <- do.call(cbind, results_list)

    # Add the strategy names as the leftmost column
    results_df <- cbind(Strategy = rownames(tournament_results), results_df)

    # Convert to a data frame for better readability
    results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)

    return(results_df)
}
