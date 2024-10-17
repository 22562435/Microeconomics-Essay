library(kableExtra)

format_table <- function(data, title = "Strategy Rankings Across Different p Values") {
    data %>%
        kable("latex", align = 'c', caption = title, booktabs = TRUE, linesep = "") %>%
        kable_styling(
            font_size = 8,  # Reduce the font size for a more compact table
            latex_options = c("hold_position", "condensed")
        ) %>%
        column_spec(1, bold = TRUE, width = "3cm") %>%
        column_spec(2:ncol(data), width = "0.7cm")  # Adjust the width of other columns
}

