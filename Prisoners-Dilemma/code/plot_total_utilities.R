library(ggplot2)
library(reshape2)

# Function to create a line graph of total utilities for different strategies
plot_total_utilities <- function(data) {
    # Reshape the data frame to long format for ggplot
    long_data <- melt(data, id.vars = "Strategy", variable.name = "p_value", value.name = "total_utils")

    # Convert total utilities to numeric
    long_data$total_utils <- as.numeric(long_data$total_utils)
    long_data$p_value <- as.numeric(as.character(long_data$p_value))

    # Order the Strategy factor levels by the order in which they appear in the original data
    long_data$Strategy <- factor(long_data$Strategy, levels = unique(data$Strategy))
    # color_palette <-  c(
    #     "#CCCCCC", "#666666",  # Greyscale: Always Cooperate, Always Defect
    #     "#1EAD69", "#33A02C", "#66C2A5", "#B2DF8A",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
    #     "#4B9CD3",  # Green: Pavlov
    #     "#D64545", "#BA2D2D", "#E57373",  # Reds: Grim Trigger, Bully, Retaliatory Defector
    #     "#B57EDC", "#9C27B0", "#CE93D8", "#8E44AD", "#B39DDB", "#76448A",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
    #     "#FFA726", "#FF7043", "#FFB74D", "#FF8A65",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
    #     "#FFEB3B", "#FDD835", "#FFEE58", "#FFD600", "#FFFF00"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    # )
    # # Define the color palette (example with 25 colors)
    # color_palette <- c(
    #     "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
    #     "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
    #     "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
    #     "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5",
    #     "#393B79", "#637939", "#8C6D31", "#843C39", "#7B4173"
    # )



    # color_palette<- c(
    #     "#a16928", "#a16928",  # Greyscale: Always Cooperate, Always Defect
    #     "#bd925a", "#bd925a", "#bd925a", "#bd925a",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
    #     "#d6bd8d",  # Green: Pavlov
    #     "#edeac2", "#edeac2", "#edeac2",  # Reds: Grim Trigger, Bully, Retaliatory Defector
    #     "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8", "#b5c8b8",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
    #     "#79a7ac", "#79a7ac", "#79a7ac", "#79a7ac",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
    #     "#2887a1", "#2887a1", "#2887a1", "#2887a1", "#2887a1"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    # )

    color_palette<- c(
        "#000000", "#000000",  # Greyscale: Always Cooperate, Always Defect
        "#bd925a", "#bd925a", "#bd925a", "#bd925a",  # Greens: Tit for Tat, Tit for Two Tats, Tit for Tat with Forgiveness, Tit for Tat with Randomisation
        "#800000",  # Green: Pavlov
        "#000075", "#000075", "#000075",  # Reds: Grim Trigger, Bully, Retaliatory Defector
        "#dcbeff", "#dcbeff", "#dcbeff", "#dcbeff", "#dcbeff", "#dcbeff",  # Purples: Adaptive Defector, Adaptive Peacekeep, Probing Adjuster, Forgiving Tester, Prober, Cautious Rebuilder
        "#bfef45", "#bfef45", "#bfef45", "#bfef45",  # Oranges: Progressive Cooperator, Deminishing Cooperator, Bounded Gradient, Recent Gradient
        "#2887a1", "#2887a1", "#2887a1", "#2887a1", "#2887a1"  # Yellows: Random 10%, Random 25%, Random 50%, Random 75%, Random 90%
    )

    linetypes <- c(
        "solid", "dashed", "solid", "dashed", "dotted", "dotdash",
        "solid", "solid", "dashed", "dotted", "solid", "dashed",
        "dotted", "dotdash", "longdash", "twodash", "solid", "dashed",
        "dotted", "dotdash", "solid", "dashed", "dotted", "dotdash",
        "longdash"
    )

    point_shapes <- c(
        16, 15, 16, 15, 3, 4,
        16, 16, 15, 3, 16, 15,
        3, 4, 17, 18, 16, 15,
        3, 4, 16, 15, 3, 4,
        17
    )




    # Plot the data
    ggplot(long_data, aes(x = p_value, y = total_utils, color = Strategy, group = Strategy, shape = Strategy)) +
        geom_line(size = 1,alpha=0.75) +
        geom_point(size = 2,,alpha=0.75) +
        scale_color_manual(values = color_palette) +
        scale_shape_manual(values = point_shapes) +
        #scale_linetype_manual(values = linetypes) +
        scale_x_continuous(breaks = seq(-0.1, 0.5, by = 0.05), limits = c(-0.1, 0.5)) +
        scale_y_continuous(breaks = seq(7000, 14500, by = 500)) +
        labs(x = "Values of p", y = "Total Utilities", title = "") +
        theme_bw() +
        theme(
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 5),
            legend.key.size = unit(0.5, "lines"),
            legend.spacing.x = unit(0.5, 'cm'),
            legend.spacing.y = unit(0.2, 'cm'),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10),
            panel.grid.minor = element_blank()
        )
    #+
        #guides(color = guide_legend(nrow = 3, byrow = TRUE),
        #       shape = guide_legend(override.aes = list(size = 2)))
}






