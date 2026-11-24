# Figure 10. 3 experimental versions of Value of Information plots.
# needs rbd_names from format_data.R

data_voi <- read_excel("data/VOI_20261118.xlsx") |>
  left_join(rbd_names, by = join_by(RBD_geo == rbd_code))


data_voi_factor <- data_voi |>
  mutate(
    Threshold_SumRQ = fct_inorder(as.character(Threshold_SumRQ)) |>
      fct_rev(),
    # TODOØ Order stressors pesti pharma reach
    mutual_info_pretty = round(`Mutual info (%)`, digits = 2),
    class_pretty = gsub(pattern = "Group_", replacement = "", x = Class) |>
      ordered(c("PESTI", "PHARM", "REACH"))
  )

voi_heatmap <- ggplot(
  data_voi_factor,
  mapping = aes(
    x = fct_rev(Threshold_SumRQ),
    y = class_pretty,
    fill = mutual_info_pretty
  )
) +
  geom_tile() +
  geom_text(aes(
    label = mutual_info_pretty,
    colour = ifelse(mutual_info_pretty > 0.5, "black", "white")
  )) +
  scale_fill_viridis_c(name = "Mutual Information") +
  scale_colour_identity() +
  facet_grid(rows = vars(rbd_name)) +
  labs(
    x = "Substance Class",
    y = "SumRQ > Threshold",
    title = "Value of Information for substance groups across RBDs and RQ thresholds"
  )

ggsave(
  "images/fig10_voi.png",
  voi_heatmap,
  width = 20,
  height = 30,
  units = "cm"
)

# ggplot(
#   data_voi_factor,
#   mapping = aes(
#     fill = Class,
#     y = mutual_info_pretty,
#     x = fct_rev(Threshold_SumRQ)
#   )
# ) +
#   geom_col(position = "dodge") +
#   facet_grid(cols = vars(RBD_geo), labeller = "label_both") +
#   labs(
#     x = "Threshold",
#     y = "VOI",
#     title = "Value of Information for Stressor Groups Across Different Scenarios"
#   ) +
#   theme_minimal()

# data_voi_factor_table <- data_voi_factor |>
#   mutate(group_pretty = gsub(pattern = "Group_", "", Class))

# ggplot(
#   data_voi_factor_table,
#   mapping = aes(x = mutual_info_pretty, y = group_pretty, fill = group_pretty)
# ) +
#   geom_col(position = "dodge") +
#   facet_grid(
#     rows = vars(RBD_geo, Threshold_SumRQ),
#     as.table = TRUE,
#     switch = "y"
#   ) +
#   theme(
#     strip.text.y.left = element_text(angle = 0, face = "bold"),
#     legend.position = "none",
#     plot.subtitle = element_text(face = "bold", margin = margin(0, 0, 0, -75))
#   ) +
#   labs(subtitle = "Class RBD RQ")
