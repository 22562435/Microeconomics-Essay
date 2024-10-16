create_bump_chart2 <- function(ranking_df) {
    # Reshape the data from wide to long format
    ranking_long <- ranking_df %>%
        pivot_longer(cols = -Strategy, names_to = "p", values_to = "Rank") %>%
        mutate(p = as.numeric(p), Rank = as.numeric(Rank)) %>%
        drop_na(p, Rank)  # Drop any rows with NA values for p or Rank

    # Get the order of strategies based on the first and last ranks
    first_rank_order <- ranking_long %>%
        filter(p == min(p)) %>%
        arrange(Rank) %>%
        pull(Strategy)

    last_rank_order <- ranking_long %>%
        filter(p == max(p)) %>%
        arrange(Rank) %>%
        pull(Strategy)

    # Create the bump chart without using shapes for strategies
    bump_chart <- ggplot(ranking_long, aes(x = p, y = Rank, color = Strategy)) +
        geom_line(aes(group = Strategy), size = 1) +  # Different line types for each strategy
        geom_point(size = 2) +  # Remove shape mapping
        scale_y_reverse(breaks = seq(1, max(ranking_long$Rank, na.rm = TRUE))) +  # Reverse the y-axis for ranks
        scale_color_manual(values = custom_colors) +  # Use a color palette for better distinction
        labs(title = "Strategy Ranks Across Different Values of p",
             x = "p-value",
             y = "Rank",
             color = "Strategy") +
        theme_bw() +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

    # Create the legends separately for left and right sides
    left_legend <- get_legend(
        ggplot(ranking_long %>% filter(Strategy %in% first_rank_order),
               aes(x = p, y = Rank, color = factor(Strategy, levels = first_rank_order))) +
            geom_line(size = 1) +
            scale_color_manual(values = custom_colors) +
            theme_void() +
            theme(legend.position = "right",
                  legend.text = element_text(size = 8),  # Adjust legend text size
                  legend.key.size = unit(0.5, "lines"),
                  legend.title = element_blank())  # Position legend on the right
    )

    right_legend <- get_legend(
        ggplot(ranking_long %>% filter(Strategy %in% last_rank_order),
               aes(x = p, y = Rank, color = factor(Strategy, levels = last_rank_order))) +
            geom_line(size = 1) +
            scale_color_manual(values = custom_colors) +
            theme_void() +
            theme(legend.position = "right",
                  legend.text = element_text(size = 8),  # Adjust legend text size
                  legend.key.size = unit(0.5, "lines"),
                  legend.title = element_blank())  # Position legend on the right
    )

    final_plot <- ggdraw() +
        draw_plot(left_legend, x = 0, y = 0, width = 0.2, height = 1) +  # Adjusted size for better alignment
        draw_plot(bump_chart, x = 0.2, y = 0, width = 0.6, height = 1) +
        draw_plot(right_legend, x = 0.8, y = 0, width = 0.2, height = 1)

    return(final_plot)
}
