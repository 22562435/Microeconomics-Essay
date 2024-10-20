

library(dplyr)
library(knitr)
library(kableExtra)

# Define the function to calculate rankings
create_ranking_table <- function(data) {
  # Convert all columns (except Strategy) to numeric
  data[,-1] <- data[,-1] %>% lapply(as.numeric)
  
  # Calculate the average for each strategy across the p-values
  data <- data %>%
    rowwise() %>%
    mutate(Average = mean(c_across(-Strategy), na.rm = TRUE)) %>%
    ungroup()
  
  # Rank the strategies based on their average, descending order
  data <- data %>%
    arrange(desc(Average)) %>%
    mutate(Ranking = row_number())
  
  # Select the desired columns and reorder them
  result <- data %>%
    select(Ranking, Strategy, Average)
  
  # Return the result in a kable format
  kable(result, "latex", align = 'c', caption = "Strategy Rankings Across All p-Values", booktabs = TRUE, linesep = "") %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      latex_options = c("scale_down", "hold_position"),
      font_size = 8
    ) %>%
    column_spec(1, bold = TRUE, width = "2cm") %>%  # Bold the ranking numbers
    column_spec(2, width = "5cm") %>%  # Adjust the width of strategy names
    column_spec(3, width = "3cm")  # Adjust the width of average column
}



