library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)



bump_chart <- function(ranking_df) {

    custom_colors <- c(
        "#a16928", "#a16928",  # Greyscale: Always Cooperate, Always Defect
        "#bd925a", "#bd925a", "#bd925a", "#bd925a",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
        "#d6bd8d",  # Green: Pavlov
        "#edeac2", "#edeac2", "#edeac2",  # Reds: Grim Trigger, Bully, Retaliatory Defector
        "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
        "#79a7ac", "#79a7ac", "#79a7ac", "#79a7ac",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
        "#2887a1", "#2887a1", "#2887a1", "#2887a1", "#2887a1"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    )


    # Reshape the data from wide to long format
    ranking_long <- ranking_df %>%
        pivot_longer(cols = -Strategy, names_to = "p", values_to = "Rank") %>%
        mutate(p = as.numeric(p),   # Convert p to numeric for proper ordering
               Rank = as.numeric(Rank))  # Ensure Rank is numeric

    # Get the order of strategies based on the first and last ranks
    first_rank_order <- ranking_long %>%
        filter(p == min(p)) %>%
        arrange(Rank) %>%
        pull(Strategy)

    last_rank_order <- ranking_long %>%
        filter(p == max(p)) %>%
        arrange(Rank) %>%
        pull(Strategy)

    # Create the bump chart
    bump_chart <- ggplot(ranking_long, aes(x = p, y = Rank, color = Strategy)) +
        geom_line(aes(group = Strategy), alpha=0.75,linewidth=1.5) +  # Different line types for each strategy
       # geom_point(aes(shape = Strategy), size = 2) +  # Different point shapes for each strategy
        scale_y_reverse(breaks = seq(1, max(ranking_long$Rank, na.rm = TRUE))) +  # Reverse the y-axis for ranks
        scale_color_manual(values = custom_colors) +  # Use a color palette for better distinction
        labs(title = "",
             x = "p-value",
             y = "Rank",
             color = "Strategy",
             shape = "Strategy") +
        theme_bw() +
        theme(legend.position = "none",  # Hide default legend
              plot.title = element_text(hjust = 0.5))  # Center title



    return(bump_chart)
}

# Example usage
# ranking_df should be your data frame containing the strategy ranks
# final_plot <- create_bump_chart(ranking_df)
# print(final_plot)
