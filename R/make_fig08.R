# Figure 08: how the RQ distribution of a stressor changes under each
# use-reduction scenario (one panel per scenario), walked across all stressors. Uses every reduction level,
# so reads data_long_pretty_merged_all rather than the reduction-filtered data.

# One figure per stressor (every code in stressor_names$stressor_code), for this RBD
fig08_rbd <- default_rbd

make_fig08 <- function(stressor, rbd) {
  stopifnot(stressor %in% stressor_names$stressor_code)

  plot_data <- data_long_pretty_merged_all |>
    filter(
      stressor_code == stressor,
      RBD == rbd,
      comparison_operation == "interval"
    )
  stopifnot(nrow(plot_data) > 0)

  # interval probabilities sum to 100% per month/reduction scenario
  check_sums <- plot_data |>
    summarise(
      total = sum(Probability_perc_merged),
      .by = c(Month_abb, use_reduction)
    ) |>
    filter(abs(total - 100) > 0.5)
  stopifnot(nrow(check_sums) == 0)

  reduction_levels <- sort(unique(plot_data$use_reduction))
  plot_data <- plot_data |>
    mutate(
      reduction_label = factor(
        use_reduction,
        levels = reduction_levels,
        labels = glue("{reduction_levels}% use reduction")
      )
    )

  rbd_full_name <- first(plot_data$rbd_name)
  stressor_label <- first(plot_data$stressor_name)
  group_label <- first(
    stressor_names$stressor_group_name[stressor_names$stressor_code == stressor]
  )

  p <- plot_data |>
    ggplot(aes(
      y = fct_rev(Month_abb),
      x = Probability_perc_merged,
      fill = RQ_range_merged
    )) +
    geom_col(position = "fill", width = geom_col_width) +
    facet_wrap(vars(reduction_label), ncol = 4, axes = "all_x") +
    scale_x_continuous_probability(limits = NULL, expand = FALSE) +
    scale_y_discrete_months() +
    labs(
      x = "Probability of RQ in interval",
      y = "Month",
      title = glue(
        "Effect of use reduction on Risk Quotient: {stressor_label}"
      ),
      subtitle = glue("{rbd_full_name}, {group_label} (predicted)")
    ) +
    set_fill_scale(name = "RQ interval") +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(hjust = 0, face = "bold")
    )

  filename <- glue("images/fig08_{str_to_lower(stressor)}_rbd{rbd}.png")
  ggsave(filename = filename, plot = p, width = 30, height = 20, units = "cm")
  message(paste0("saved ", filename))
  invisible(p)
}

walk(stressor_names$stressor_code, \(code) make_fig08(code, fig08_rbd))
