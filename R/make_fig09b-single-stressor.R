# Figure 09b: point and line chart of prob of individual stressor exceeding threshold against use reduction (%)

make_fig09_ss <- function(month_abb) {
  fig09_ss_data <- data_long_pretty_merged_all |>
    filter(
      Month_abb == month_abb,
      comparison_operation == "exceeds",
      sum_operation == "RQ"
    ) |>
    group_by(stressor_name, Month_abb, sum_operation_threshold, RBD) |>
    mutate(
      chem_colour = sd(Probability_perc_merged) /
        mean(Probability_perc_merged) >
        0.1
    ) |>
    ungroup()

  highlight <- fig09_ss_data |>
    filter(chem_colour) |>
    distinct(stressor_name) |>
    pull(stressor_name)

  others <- setdiff(unique(fig09_ss_data$stressor_name), highlight)

  # one colour per highlighted stressor, grey for everything else
  colours <- c(
    setNames(glasbey(length(highlight)), highlight),
    setNames(rep("grey80", length(others)), others)
  )

  p <- ggplot(
    fig09_ss_data,
    aes(
      x = use_reduction,
      y = Probability_perc_merged,
      group = stressor_name,
      colour = stressor_name
    )
  ) +
    geom_point() +
    geom_line() +
    scale_colour_manual(
      values = colours,
      breaks = highlight,
      name = "Substance"
    ) +
    facet_grid(
      rows = vars(sum_operation_threshold),
      cols = vars(RBD),
      labeller = label_value
    ) +
    labs(
      title = "Probability that any group SumRQ exceeds given threshold",
      subtitle = paste0(
        month_abb,
        ": substances where Z-score > 0.1 highlighted"
      ),
      x = "Use Reduction (%)",
      y = "P(SumRQ > threshold)"
    ) +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))

  month_num <- sprintf("%02d", match(month_abb, month.abb))
  filename <- glue("images/fig09_ss_{month_num}_{str_to_lower(month_abb)}.png")
  ggsave(filename = filename, plot = p, width = 30, height = 20, units = "cm")
  message(paste0("saved ", filename))
  invisible(p)
}

make_fig09_ss("Jan")

walk(month.abb, make_fig09_ss)
