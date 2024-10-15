library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dplyr)



custom_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#E9D100",
                   "#A65628", "#984EA3", "#999999", "#FF69B4", "#8B4513",
                   "#20B2AA", "#FF6347", "#4682B4", "#FFD700", "#4B3D31",
                   "#F08080", "#00CED1","black","wheat","wheat3")





create_bump_chart <- function(ranking_df) {
    # Reshape the data from wide to long format
    ranking_long <- ranking_df %>%
        pivot_longer(cols = -Strategy, names_to = "p", values_to = "Rank") %>%
        mutate(p = as.numeric(p),   # Convert p to numeric for proper ordering
               Rank = as.numeric(Rank))  # Ensure Rank is numeric

    # Create the bump chart
    ggplot(ranking_long, aes(x = p, y = Rank, color = Strategy)) +
        geom_line(aes(group = Strategy), size = 1) +  # Different line types for each strategy
        geom_point(aes(shape = Strategy), size = 2) +  # Different point shapes for each strategy
        scale_y_reverse(breaks = seq(1, max(ranking_long$Rank, na.rm = TRUE))) +  # Reverse the y-axis for ranks
        scale_color_manual(values = custom_colors) +  # Use a color palette for better distinction
        labs(title = "Strategy Ranks Across Different Values of p",
             x = "p-value",
             y = "Rank",
             color = "Strategy",
             shape = "Strategy") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 9),
              legend.text = element_text(size = 7))
}
