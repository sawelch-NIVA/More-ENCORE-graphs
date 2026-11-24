# Check: interval probabilities sum to 100% per node per month/RBD
check_sums <- data_long_pretty_merged |>
  filter(comparison_operation == "interval") |>
  summarise(
    total = sum(Probability_perc_merged),
    .by = c(Month_abb, rbd_name, stressor_group, stressor_code, sum_operation)
  ) |>
  filter(abs(total - 100) > 0.5)
stopifnot(nrow(check_sums) == 0)

# One figure per RBD with every stressor (35 panels), ordered by group then code.
# Panel letters run a-z then A-Z so they are unique across groups; the group
# code is shown in each strip.
panel_labels <- c(letters, LETTERS)

individual_stressors <- data_long_pretty_merged |>
  filter(!is.na(stressor_code), comparison_operation == "interval")

stressor_panel_order <- individual_stressors |>
  distinct(stressor_group, stressor_code, stressor_name) |>
  arrange(stressor_group, stressor_code) |>
  mutate(
    panel_label = glue(
      "{panel_labels[row_number()]}) {stressor_name} ({stressor_group})"
    )
  )
stopifnot(nrow(stressor_panel_order) <= length(panel_labels))

fig05_rbds <- individual_stressors |>
  distinct(RBD, rbd_name) |>
  arrange(rbd_name)

walk(seq_len(nrow(fig05_rbds)), function(i) {
  rbd_code <- fig05_rbds$RBD[i]
  rbd_full_name <- fig05_rbds$rbd_name[i]

  plot_data <- individual_stressors |>
    filter(RBD == rbd_code) |>
    left_join(
      stressor_panel_order |> select(stressor_code, panel_label),
      by = "stressor_code"
    ) |>
    mutate(panel_label = factor(panel_label, levels = stressor_panel_order$panel_label))

  stopifnot(nrow(plot_data) > 0)

  n_panels <- n_distinct(plot_data$stressor_code)
  n_cols <- 5
  n_rows <- ceiling(n_panels / n_cols)

  p <- plot_data |>
    ggplot(aes(
      y = fct_rev(Month_abb),
      x = Probability_perc_merged,
      fill = RQ_range_merged
    )) +
    geom_col(position = "fill", width = geom_col_width) +
    facet_wrap(vars(panel_label), ncol = n_cols, axes = "all_x") +
    scale_x_continuous_probability(limits = NULL, expand = FALSE) +
    scale_y_discrete_months() +
    labs(
      x = "Probability of RQ in interval",
      y = "Month",
      title = "Probability distributions for Risk Quotient by stressor and month",
      subtitle = glue("{rbd_full_name}, all stressors (n = {n_panels}) (predicted)")
    ) +
    set_fill_scale(name = "RQ interval") +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(hjust = 0, face = "bold", size = 9),
      axis.text = element_text(size = 7),
      panel.spacing.x = unit(1, "lines")
    )

  filename <- glue("images/fig05_rbd{rbd_code}.png")
  ggsave(
    filename = filename,
    plot = p,
    width = 36,
    height = 5 + 4.2 * n_rows,
    units = "cm"
  )
  message(paste0("saved ", filename))
})
