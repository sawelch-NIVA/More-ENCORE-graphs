library(tidyverse)
library(ggtext)

spatial_info <- readxl::read_excel("data/lookups/SpatialInfo_20260924.xlsx")

# population props are very left-skewed but with a long right tail
# lots of places without much
p1 <- ggplot(
  spatial_info,
  aes(x = Population_prop_BE, fill = RBD_Code)
) +
  geom_histogram() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Histogram of Population Proportion by RBD subcatchment",
    subtitle = "population props are very left-skewed but with a long right tail (all Schelde)"
  ) +
  theme(
    plot.subtitle = element_textbox_simple(margin = margin(b = 8)),
    plot.title.position = "plot"
  )

# important point - this is cumulative area, not actual area
p2 <- ggplot(
  spatial_info,
  aes(x = CUMAREA_m2, fill = RBD_Code)
) +
  geom_histogram() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Histogram of Cumulative Area (m2) by RBD subcatchment",
    subtitle = "Slightly fatter tail on cumulative area, some subcatchments are pretty big but with low population"
  ) +
  theme(
    plot.subtitle = element_textbox_simple(margin = margin(b = 8)),
    plot.title.position = "plot"
  )

p1 + p2

ggplot(
  spatial_info,
  aes(x = CUMAREA_m2, y = Population_prop_BE, colour = RBD_Code)
) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_brewer(type = "qual", palette = "Set2") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") +
  labs(
    title = "Relationship between cumulative area and population proportion",
    subtitle = "No actual statistics performed here, but it doesn't look like there's much of a relationship between cumulative area and population proportion",
    x = "Log10 Cumulative Area (m2)",
    y = "Log10 Population Proportion"
  )
