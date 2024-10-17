library(kableExtra)

create_tournament_table2 <- function(symmetric_results, p_value) {
    # Identify the column names for "Total" and "Rank"
    total_col_name <- grep("^Total", colnames(symmetric_results), value = TRUE)
    rank_col_name <- grep("^Rank", colnames(symmetric_results), value = TRUE)

    # Ensure only one "Total" and one "Rank" column is used
    total_col_index <- which(colnames(symmetric_results) == total_col_name)
    rank_col_index <- which(colnames(symmetric_results) == rank_col_name)

    # Update column names with abbreviations
    new_colnames <- c(
        "AC", "AD", "TfT", "Tf2T", "TfTF", "TfTR", "P", "G/T", "B", "RD",
        "ADe", "APe", "PA", "FT", "P", "CR", "PC", "DC", "BG", "RG",
        "R0.1", "R0.25", "R0.5", "R0.75", "R0.9", " ", " "
    )
    colnames(symmetric_results) <- new_colnames

    # Create a kable with nice formatting
    symmetric_results %>%
        kable(caption = paste("Tournament Payoff Matrix for p=", p_value)) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      latex_options = c("scale_down", "hold_position"),
                      font_size = 5) %>%
        column_spec(1, bold = TRUE) %>%  # Bold the row names (strategy names)
        column_spec(2:(ncol(symmetric_results) - 1), width = "0.25cm") %>%  # Adjust columns for strategy payoffs
        column_spec(total_col_index+1, bold = TRUE, color = "blue",width = "0.2cm") %>%  # Highlight Total column
        column_spec(rank_col_index+1, bold = TRUE, color = "red",width = "0.2cm") %>%  # Highlight Rank column
        add_header_above(c(" " = 1, "Payoff Against Other Strategies" = ncol(symmetric_results) - 2,
                           "Total" = 1, "Rank" = 1))  # Add a header for better readability
}
