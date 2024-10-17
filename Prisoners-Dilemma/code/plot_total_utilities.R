library(ggplot2)
library(reshape2)

# Function to create a line graph of total utilities for different strategies
plot_total_utilities <- function(data) {
    # Reshape the data frame to long format for ggplot
    long_data <- melt(data, id.vars = "Strategy", variable.name = "p_value", value.name = "total_utils")

    # Define the color palette
    color_palette <- c(
        "#CCCCCC", "#666666",  # Greyscale: Always Cooperate, Always Defect
        "#1EAD69", "#33A02C", "#66C2A5", "#B2DF8A",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
        "#4B9CD3",  # Blue: Pavlov
        "#D64545", "#BA2D2D", "#E57373",  # Reds: Grim Trigger, Bully, Retaliatory Defector
        "#B57EDC", "#9C27B0", "#CE93D8", "#8E44AD", "#B39DDB", "#76448A",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
        "#FFA726", "#FF7043", "#FFB74D", "#FF8A65",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
        "#FFEB3B", "#FDD835", "#FFEE58", "#FFD600", "#FFFF00"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    )

    # Plot the data
    ggplot(long_data, aes(x = as.numeric(p_value), y = total_utils, color = Strategy, group = Strategy)) +
        geom_line(size = 1) +
        scale_color_manual(values = color_palette) +
        labs(x = "Values of p", y = "Total Utilities", title = "Total Utilities for Different Strategies") +
        theme_minimal() +
        theme(legend.title = element_blank())
}

# Example of calling the function with your result_df_total data
# plot_total_utilities(result_df_total)
