data <- arrow::read_parquet(data_path) |> as_tibble()

# Warn if unexpected shape (12 months x 4 RBDs x 11 reductions x 5 thresholds
# = 2640 scenarios, 556 output rows each)
n_scenarios <- data |>
  distinct(Month, RBD_pop, Use_reduction_by, Threshold_RQ) |>
  nrow()
if (n_scenarios != 2640 | nrow(data) != 2640 * 556) {
  warning(
    "Imported data dimensions different to expected values. Stuff may break."
  )
}

# RBD_pop and RBD_geo are assumed identical; we only keep one
stopifnot(all(data$RBD_pop == data$RBD_geo))

# check the threshold columns are always identical
thresholds_identical <- with(
  data,
  all(Threshold_RQ == Threshold_SumRQ, Threshold_RQ == Threshold_SumSumRQ)
)
if (!thresholds_identical) {
  warning(
    "Code assumes Threshold_ columns are always identical. Stuff may break."
  )
}

# Parse node columns and validate ----
data_long_raw <- data |>
  parse_node_columns() |>
  validate_parsed_nodes() |>
  transmute(
    metric = OutputName,
    Month = as.integer(Month),
    RBD = as.integer(RBD_pop),
    use_reduction = as.integer(Use_reduction_by),
    group_code,
    subst_code,
    rq_op,
    is_exceeds,
    exceeds_val,
    interval_val,
    threshold_merged = Threshold_RQ,
    value = Probability
  )

# Interval rows appear 5x (once per threshold state) but the probability should
# be (near-)identical. Collapse explicitly by averaging across copies rather
# than relying on distinct(), which would keep rows differing by float noise.
data_long <- data_long_raw |>
  mutate(
    threshold_merged = if_else(is_exceeds, threshold_merged, NA_real_)
  ) |>
  summarise(
    value = mean(value),
    .by = c(
      metric,
      Month,
      RBD,
      use_reduction,
      group_code,
      subst_code,
      rq_op,
      is_exceeds,
      exceeds_val,
      interval_val,
      threshold_merged
    )
  )

# check we don't have any weird groups
# none of these groups' summed probabilities should be > 1 +/- floating point
summed_probs_gt_1 <- data_long |>
  summarise(
    value_sum = sum(value),
    .by = c(Month, RBD, use_reduction, threshold_merged, rq_op, group_code, subst_code)
  ) |>
  filter(value_sum > 1.05) # close enough

# if this triggers your grouping variables are messed up :(
stopifnot(nrow(summed_probs_gt_1) == 0)

# Validation checks
stopifnot(!all(is.na(data_long$value)))
stopifnot(nrow(data_long) > 0)
stopifnot(all(!is.na(data_long$Month)))
stopifnot(all(!is.na(data_long$RBD)))
# RQ rows (substance-level) must have both a group and a substance code
stopifnot(
  nrow(filter(data_long, rq_op == "RQ", is.na(group_code) | is.na(subst_code))) == 0
)
# Exceeds rows must have a resolved TRUE/FALSE value (not NA)
stopifnot(nrow(filter(data_long, is_exceeds, is.na(exceeds_val))) == 0)
# Non-exceeds (interval) rows must have an interval value
stopifnot(nrow(filter(data_long, !is_exceeds, is.na(interval_val))) == 0)

# For each node, interval probabilities must sum to 1 (within tolerance)
bad_sums <- data_long |>
  filter(!is_exceeds) |>
  summarise(
    p_summed = sum(value),
    .by = c(Month, RBD, use_reduction, group_code, subst_code, rq_op)
  ) |>
  filter(abs(p_summed - 1) > 0.05)

if (nrow(bad_sums) > 0) {
  stop(
    nrow(bad_sums),
    " node-month-RBD-reduction combination(s) have interval probabilities not summing to 1.",
    call. = FALSE
  )
}

# Rename and transform columns ----
# Drop P(exceeds = 0) rows — P(exceeds = 1) is all that is needed downstream,
# and keeping both would cause them to be summed together in merge_intervals.R
data_long <- data_long |>
  filter_out(is_exceeds, !exceeds_val) |>
  mutate(
    stressor_group = coalesce(group_code, "all"),
    stressor_code = subst_code,
    sum_operation = rq_op,
    exceedence_boolean = is_exceeds,
    comparison_operation = if_else(is_exceeds, "exceeds", "interval"),
    RQ_level = interval_val
  ) |>
  select(
    metric,
    Month,
    RBD,
    use_reduction,
    stressor_group,
    stressor_code,
    sum_operation,
    exceedence_boolean,
    comparison_operation,
    RQ_level,
    value,
    threshold_merged
  )

# quick data report
message(glue(
  "-- Data Loaded --
  Stressors ({n_distinct(na.omit(data_long$stressor_code))}): {toString(na.omit(unique(data_long$stressor_code)))}
  Months: ({n_distinct(data_long$Month)})
  RBDs: ({n_distinct(data_long$RBD)}): {toString(sort(unique(data_long$RBD)))}
  Use reductions: {toString(sort(unique(data_long$use_reduction)))}
  Stressor Groups: {toString(unique(data_long$stressor_group))}
  Sum Operations: {toString(unique(data_long$sum_operation))}"
))
