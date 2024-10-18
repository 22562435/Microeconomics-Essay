library(kableExtra)

format_table <- function(data, title = "Strategy Rankings Across Different p Values") {
    data %>%
        kable("latex", align = 'c', caption = title, booktabs = TRUE, linesep = "") %>%
        kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            latex_options = c("scale_down", "hold_position"),
            font_size = 5
        ) %>%
        column_spec(1, bold = TRUE, width = "4cm") %>%  # Bold the strategy names
        column_spec(2:(ncol(data)), width = "0.7cm") %>%  # Adjust the width of payoff columns
        add_header_above(c(" " = 1, "p Values" = ncol(data) - 1)) %>%
        row_spec(seq(5, nrow(data)-5, by = 5), extra_latex_after = "\\midrule")  # Add a horizontal line every 5 rows for better readability
}
