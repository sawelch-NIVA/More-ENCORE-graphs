# Parse node columns ----
# The parquet files already carry structured columns (NodeName, Inst_Group,
# Inst_Substance, StateNo), so no regex on metric names is needed beyond
# stripping the Group_/Subst_ prefixes and _1 suffixes.
parse_node_columns <- function(df) {
  df |>
    mutate(
      group_code = str_extract(Inst_Group, "^Group_([A-Z0-9]+)_[0-9]+$", group = 1),
      subst_code = str_extract(Inst_Substance, "^Subst_([A-Z0-9]+)_[0-9]+$", group = 1),
      # Anything not listed here becomes NA and fails validation
      rq_op = case_when(
        NodeName %in% c("SumSumRQ", "SumSumRQ_exceeds") ~ "SumSumRQ",
        NodeName %in% c("SumRQ", "SumRQ_exceeds", "Any_SumRQ_exceeds") ~ "SumRQ",
        NodeName == "Any_RQ_exceeds" ~ "Any_RQ",
        NodeName %in% c("RQ", "RQ_exceeds") ~ "RQ"
      ),
      is_exceeds = str_ends(NodeName, "_exceeds"),
      exceeds_val = if_else(is_exceeds, StateNo == 1, NA),
      interval_val = if_else(!is_exceeds, as.integer(StateNo), NA_integer_)
    )
}

# Validate parsed nodes ----
validate_parsed_nodes <- function(parsed_df) {
  # Known valid values
  valid_groups <- c("PESTI", "PHARM", "REACH")
  valid_substs <- c(
    # PESTI
    "AZOXYS", "BROMOX", "CHLORP", "CLETHO", "DIMETO", "DIURON", "ETHOPR",
    "FLUROX", "FLUTOL", "MCPAAA", "METRIB", "TEBUCO",
    # PHARM
    "ARIPIP", "BISOPR", "FENTAN", "ISOTRE", "LANSOP", "MESALA", "PANTOP",
    "RAMIPR", "URSODE", "VALPRO",
    # REACH
    "ACETIC", "AMETRY", "ANILIN", "BENZEN", "BISETH", "CHLORO", "DICBEN",
    "DICETH", "DICMET", "DICPRO", "DINITR", "NAPHTA", "NONYLP"
  )
  valid_intervals <- 0:11

  problems <- character()
  add_problem <- function(msg, bad) {
    if (length(bad)) {
      problems <<- c(problems, paste0(msg, ": ", toString(unique(bad))))
    }
  }

  add_problem(
    "Unexpected group codes",
    parsed_df |> filter(!is.na(Inst_Group), !group_code %in% valid_groups) |> pull(Inst_Group)
  )
  add_problem(
    "Unexpected substance codes",
    parsed_df |> filter(!is.na(Inst_Substance), !subst_code %in% valid_substs) |> pull(Inst_Substance)
  )
  add_problem(
    "Could not parse rq_op for node",
    parsed_df |> filter(is.na(rq_op)) |> pull(NodeName)
  )
  add_problem(
    "Exceeds rows with state other than 0/1",
    parsed_df |> filter(is_exceeds, !StateNo %in% 0:1) |> pull(OutputName)
  )
  add_problem(
    "Interval values out of range 0-11",
    parsed_df |> filter(!is_exceeds, !interval_val %in% valid_intervals) |> pull(OutputName)
  )
  add_problem(
    "Substance rows missing group",
    parsed_df |> filter(!is.na(subst_code), is.na(group_code)) |> pull(OutputName)
  )

  if (length(problems)) {
    stop(
      "Node parsing validation failed:\n",
      paste0("  - ", problems, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(parsed_df)
}
