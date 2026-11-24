stressor_group_names <- tribble(
  ~stressor_group , ~stressor_group_name ,
  "PESTI"         , "Pesticides"         ,
  "PHARM"         , "Pharmaceuticals"    ,
  "REACH"         , "REACH chemicals"    ,
  "all"           , "All stressors"
)

rbd_names <- tibble(
  rbd_code = 1:4,
  rbd_name = c("BEMEUSE_RW", "BEMAAS_VL", "BEESCAUT_RW", "BESCHELDE_VL")
)

# Stressor names are just the (truncated) codes for now. Letters restart within
# each group so panels are labelled a), b), c) ... per group.
stressor_names_lookup <- read_excel(
  "data/lookups/ChemicalInfo_20261010.xlsx"
) |>
  select(SubstanceName, SubstanceCode, MixtureGroup)
# note that some names have been fixed (proper case, reduced to first name)

stressor_names <- data_long |>
  filter(!is.na(stressor_code)) |>
  distinct(stressor_code, stressor_group) |>
  arrange(stressor_group, stressor_code) |>
  left_join(
    stressor_names_lookup,
    by = join_by(stressor_code == SubstanceCode)
  ) |>
  mutate(
    label_letter = letters[row_number()],
    .by = stressor_group
  ) |>
  left_join(stressor_group_names, by = join_by(stressor_group))

# Number of stressors per group (used for panel labels)
stressor_group_n <- stressor_names |>
  count(stressor_group, name = "n_stressors")

rq_level_ranges <- tribble(
  ~RQ_level , ~RQ_range        , ~RQ_lower_bound ,
          0 , "0 - 0"          , NA_integer_     ,
          1 , "0 - 0.001"      ,  0              ,
          2 , "0.001 - 0.0032" ,  0.001          ,
          3 , "0.0032 - 0.01"  ,  0.0032         ,
          4 , "0.01 - 0.032"   ,  0.01           ,
          5 , "0.032 - 0.1"    ,  0.032          ,
          6 , "0.1 - 0.32"     ,  0.1            ,
          7 , "0.32 - 1"       ,  0.32           ,
          8 , "1 - 3.2"        ,  1              ,
          9 , "3.2 - 10"       ,  3.2            ,
         10 , "10 - 32"        , 10              ,
         11 , "32 - Inf"       , 32
)

data_long_pretty <- data_long |>
  mutate(
    Month_abb = factor(Month, levels = 1:12, labels = month.abb),
    Probability_perc = value * 100
  ) |>
  left_join(rbd_names, by = join_by(RBD == rbd_code)) |>
  left_join(
    stressor_names |> select(stressor_code, SubstanceName, label_letter),
    by = join_by(stressor_code)
  ) |>
  left_join(
    stressor_group_names,
    by = join_by(stressor_group),
    unmatched = "error"
  ) |>
  left_join(rq_level_ranges, by = join_by(RQ_level)) |>
  mutate(
    stressor_name = SubstanceName,
    stressor_name_group_md = glue("{label_letter}) {SubstanceName}"),
    # threshold equality already verified in load_data.R
    sum_operation_threshold = if_else(
      exceedence_boolean,
      as.double(threshold_merged),
      NA_real_
    )
  ) |>
  select(
    Month_abb,
    RBD,
    rbd_name,
    use_reduction,
    stressor_group,
    stressor_group_name,
    stressor_name_group_md,
    stressor_code,
    stressor_name,
    sum_operation,
    sum_operation_threshold,
    RQ_level,
    RQ_range,
    comparison_operation,
    metric,
    exceedence_boolean,
    Probability_perc
  ) |>
  distinct()

# check we haven't added (or removed) any rows in the process
stopifnot(nrow(data_long) == nrow(data_long_pretty))
