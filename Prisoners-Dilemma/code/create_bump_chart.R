# Define the bump chart function
create_bump_chart <- function(data, limit = 10) {
    # Check if the first column is named 'Strategy'
    if (!"Strategy" %in% colnames(data)) {
        stop("Data frame must contain a 'Strategy' column.")
    }

    # Convert rank columns to numeric
    numeric_data <- data %>%
        mutate(across(-Strategy, as.numeric))  # Convert all columns except 'Strategy' to numeric

    # Transform the data into long format for easier plotting
    long_data <- numeric_data %>%
        pivot_longer(cols = -Strategy, names_to = "p_value", values_to = "rank") %>%
        mutate(p_value = as.numeric(p_value))  # Ensure p_value is numeric

    # Filter to keep only the top N strategies for each p-value
    top_strategies <- long_data %>%
        group_by(p_value) %>%
        filter(rank <= limit) %>%
        ungroup()

    # Check if top_strategies is empty after filtering
    if (nrow(top_strategies) == 0) {
        stop("No strategies meet the rank limit. Please adjust the limit.")
    }

    # Get the unique strategies that appear in the top N ranks for legend purposes
    unique_strategies <- unique(top_strategies$Strategy)

    # Determine the number of unique strategies for color assignment
    num_strategies <- length(unique_strategies)
    color_palette <-  c(
        "#CCCCCC", "#666666",  # Greyscale: Always Cooperate, Always Defect
        "#1EAD69", "#33A02C", "#66C2A5", "#B2DF8A",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
        "#4B9CD3",  # Green: Pavlov
        "#D64545", "#BA2D2D", "#E57373",  # Reds: Grim Trigger, Bully, Retaliatory Defector
        "#B57EDC", "#9C27B0", "#CE93D8", "#8E44AD", "#B39DDB", "#76448A",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
        "#FFA726", "#FF7043", "#FFB74D", "#FF8A65",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
        "#FFEB3B", "#FDD835", "#FFEE58", "#FFD600", "#FFFF00"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    )#RColorBrewer::brewer.pal(n = min(12, num_strategies), name = "Set3")  # Use only up to 12 colors

    # Create a named vector for colors corresponding to unique strategies
    named_colors <- setNames(color_palette, unique_strategies)

    # Plot the bump chart
    ggplot(top_strategies, aes(x = p_value, y = rank, color = Strategy, group = Strategy)) +
        geom_line(size = 1.2) +  # Adjust line thickness as needed
        geom_point(size = 2) +  # Add points for better visibility
        scale_y_reverse(breaks = 1:limit) +  # Reverse the y-axis to show rank 1 at the top
        scale_x_continuous(breaks = sort(unique(top_strategies$p_value))) +  # Set x-axis breaks for each p-value
        theme_minimal() +
        labs(title = paste("Top", limit, "Strategies Ranks Across Different Values of p"),
             x = "p-value",
             y = "Rank") +
        theme(legend.position = "right") +
        scale_color_manual(values = named_colors)  # Use the color palette with only shown strategies
}

# Example usage remains the same
# Call the function to create the bump chart
#create_bump_chart(result_df, limit = 10)
