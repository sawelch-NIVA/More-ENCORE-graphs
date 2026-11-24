# Figure 09a: point and line chart of prob of chem group SumRQ exceeding threshold against use reduction (%)

make_fig09 <- function(month_abb) {
  fig09_data <- data_long_pretty_merged_all |>
    filter(
      Month_abb == month_abb,
      comparison_operation == "exceeds",
      sum_operation == "SumRQ"
    )

  p <- ggplot(
    fig09_data,
    aes(
      x = use_reduction,
      y = Probability_perc_merged,
      shape = stressor_group_name,
      colour = stressor_group_name
    )
  ) +
    geom_point() +
    geom_line() +
    scale_colour_manual(
      values = c("#050505", "#e48f8f", "#99e48f", "#8fade4"),
      name = "Substance Group"
    ) +
    scale_shape_discrete(name = "Substance Group") +
    facet_grid(
      rows = vars(sum_operation_threshold),
      cols = vars(RBD),
      labeller = label_value
    ) +
    labs(
      title = "Probability that group SumRQ exceeds given threshold",
      subtitle = month_abb,
      x = "Use Reduction (%)",
      y = "P(SumRQ > threshold)"
    ) +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))

  month_num <- sprintf("%02d", match(month_abb, month.abb))
  filename <- glue("images/fig09_{month_num}_{str_to_lower(month_abb)}.png")
  ggsave(filename = filename, plot = p, width = 30, height = 20, units = "cm")
  message(paste0("saved ", filename))
  invisible(p)
}

walk(month.abb, make_fig09)
