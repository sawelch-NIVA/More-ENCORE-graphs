# Merge split intervals back together as follows

rq_level_ranges_merged <- tribble(
    ~from            , ~to          ,
    "0 - 0"          , "0 - 0"      , # keep
    "0 - 0.001"      , "0 - 0.01"   ,
    "0.001 - 0.0032" , "0 - 0.01"   ,
    "0.0032 - 0.01"  , "0 - 0.01"   ,
    "0.01 - 0.032"   , "0.01 - 0.1" ,
    "0.032 - 0.1"    , "0.01 - 0.1" ,
    "0.1 - 0.32"     , "0.1 - 1"    ,
    "0.32 - 1"       , "0.1 - 1"    ,
    "1 - 3.2"        , "1 - 10"     ,
    "3.2 - 10"       , "1 - 10"     ,
    "10 - 32"        , "10 - Inf"   ,
    "32 - Inf"       , "10 - Inf"
) |>
    mutate(
        lower_bound = case_when(
            to == "0 - 0" ~ NA_character_,
            TRUE ~ (str_extract(to, "^[0-9.]+"))
        )
    )


# some quick back of the envelope maths: every node has 12 intervals per
# month x RBD x use-reduction scenario. Nodes = individual stressors + groups
# (SumRQ) + the top-level SumSumRQ.
n_scenarios_interval <- n_distinct(data_long_pretty$Month_abb) *
    n_distinct(data_long_pretty$RBD) *
    n_distinct(data_long_pretty$use_reduction)
n_interval_nodes <- nrow(stressor_names) + n_distinct(stressor_names$stressor_group) + 1
stopifnot(
    data_long_pretty |> filter(comparison_operation == "interval") |> nrow() ==
        n_scenarios_interval * n_interval_nodes * 12
)

data_long_pretty_merged <- data_long_pretty |>
    mutate(
        RQ_range_merged = recode_values(
            RQ_range,
            to = rq_level_ranges_merged$to,
            from = rq_level_ranges_merged$from
        )
    ) |>
    reframe(
        Probability_perc_merged = sum(Probability_perc),
        .by = c(
            "Month_abb",
            "RBD",
            "rbd_name",
            "use_reduction",
            "stressor_group",
            "stressor_group_name",
            "stressor_name_group_md",
            "stressor_code",
            "stressor_name",
            "sum_operation",
            "sum_operation_threshold",
            "comparison_operation",
            "RQ_range_merged",
            "exceedence_boolean"
        )
    ) |>
    distinct()

# once we merge 12 to 6 rows, we expect half as many interval rows
stopifnot(
    data_long_pretty_merged |>
        filter(comparison_operation == "interval") |>
        nrow() ==
        n_scenarios_interval * n_interval_nodes * 6
)


# Check data, ish
stopifnot(!all(is.na(data_long_pretty_merged$Probability_perc_merged)))
stopifnot(!all(is.na(data_long_pretty_merged$RQ_range_merged)))
stopifnot(all(!is.na(data_long_pretty_merged$Month_abb)))
stopifnot(all(!is.na(data_long_pretty_merged$rbd_name)))
stopifnot(all(data_long_pretty_merged$Probability_perc_merged >= 0))
stopifnot(nrow(data_long_pretty_merged) > 0)
stopifnot(nrow(data_long_pretty_merged) < nrow(data_long_pretty))

# more floating point nonsense
stopifnot(all(data_long_pretty_merged$Probability_perc_merged <= 100.1))
