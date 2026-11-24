# Readme ----
# This dataset describes predicted chemical stressor Risk Quotients in 4 river basin
# districts (RBD 1-4) across a 12-month period, under 11 use-reduction scenarios
# (0-100% in steps of 10). A Bayesian Network (not included) has been used to calculate
# the probability of both individual stressors and groups (PESTI, PHARM, REACH) and combined.
#
# Raw HUGIN exports are text files (data/df.prob.long.txt, data/df.stat.long.txt).
# R/convert_to_parquet.R converts them to parquet once; the pipeline reads the parquet.
#
# Columns of df.prob.long (one row per node state per scenario):
#   Month: 1-12
#   RBD_pop, RBD_geo: RBD 1-4 (identical; RBD_pop is used)
#   Use_reduction_by: use reduction scenario, 0-100 (%)
#   Threshold_RQ / _SumRQ / _SumSumRQ: RQ thresholds for the *_exceeds nodes (always identical)
#   OOBN_Level: Top | Group | Substance
#   Inst_Group: Group_PESTI_1 (pesticides), Group_PHARM_1 (pharmaceuticals), Group_REACH_1 (REACH)
#   Inst_Substance: Subst_XXXXXX_1 (35 substances; names are truncated codes)
#   NodeName: SumSumRQ, SumSumRQ_exceeds, Any_SumRQ_exceeds, Any_RQ_exceeds,
#             SumRQ, SumRQ_exceeds (group level), RQ, RQ_exceeds (substance level)
#   StateNo: interval 0..11 for RQ-type nodes; 0/1 (does NOT / DOES exceed) for *_exceeds nodes
#   Probability: P(node = state)
#
# df.stat.long holds MEAN / MEDIAN / MAX.P / MODAL per node; it is converted but not plotted.

# Setup ----
# Install these packages if you don't have them
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggtext)
  library(glue)
  library(grid)
  library(patchwork)
  library(pals)
})

# set the latest version of the datafile here
# ! WARNING: This script is very fragile to changes in node names
# ! This is unavoidable, but expect stuff to break if they change
data_path <- "data/df.prob.long_2026-11-18.parquet"
source("R/convert_to_parquet.R")

# Requires the source text files, not included in the repo due to size
convert_txt_to_parquet(
  "data/df.prob.long_2026-11-18.txt",
  "data/df.prob.long_2026-11-18.parquet"
)
convert_txt_to_parquet(
  "data/df.stat.long_2026-11-18.txt",
  "data/df.stat.long_2026-11-18.parquet"
)
# Defaults: figs 05-07 use no use reduction; poster figs and fig 08 use RBD 1
default_reduction <- 0
default_rbd <- 1L
dir.create("images", showWarnings = FALSE)

# Pipeline ----
## Load ----
# Load raw data file. Pivot, generate tidy columns with regex
source("R/fct_parse_nodes.R")
source("R/load_data.R")
## Transform ----
# Replace terse computer names with pretty human names
source("R/format_data.R")
# Merge from 12 intervals to 6 (inc 0-0), and sum probabilities
source("R/merge_intervals.R")
# Keep every reduction scenario for fig 08; everything else uses the default one
data_long_pretty_merged_all <- data_long_pretty_merged
data_long_pretty_merged <- data_long_pretty_merged_all |>
  filter(use_reduction == default_reduction)

## Plot ----
# Set colour scheme, axis, and fonts
source("R/themes.R")

# some more defaults
geom_col_width <- 0.9 # 0 (none) to 1 (full)


set_manuscript_theme()
# Generate Figure(s) 05: P(RQ=Z) (x) for each month (y), stressor (facet), and RBD (plot, all 35 stressors); RQ interval (fill)
source("R/make_fig05.R")
# Generate Figure 06: P(Group SumRQ=Z) (x) for each month (y), group (facet-x), and RBD (facet-y); RQ interval (fill)
source("R/make_fig06.R")
# Generate Figure 07: For each RBD, Group="All", compare P(SumSumRQ=Z) to P(Any_SumRQ_exceeds) and P(Any_RQ_exceeds) (facet-x) at threshold_SumRQ and threshold_SumSumRQ %in% c(0.1, 1); RQ interval (fill), month (x), P(of group in interval, or threshold exceeded) (y). Bars use fill for intervals, line colour for thresholds
source("R/make_fig07.R")
# Generate Figure 08: effect of the use-reduction scenarios on each stressor's RQ distribution (one figure per stressor)
source("R/make_fig08.R")
# Figure 09a: line chart of SumRQ > Threshold (y) against use reduction (x)
source("R/make_fig09a.R")
# Figure 09b: same as 9a, but single stressors
source("R/make_fig09b-single-stressor.R")
# Figure 10: VoI heatmap, depends on sep. data file
source("R/make_fig10.R")

set_poster_theme()
# Generate figures for posters
source("R/make_poster_figs.R")
