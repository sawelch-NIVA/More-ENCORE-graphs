# Poster variants of figs 05-07 for the default RBD (default_rbd).
# Source after _RUNME.R has run through merge_intervals.R (data_long_pretty_merged must exist).

# ---- Poster presets --------------------------------------------------------
poster_width <- 7 # inches
poster_height <- 5 # inches
poster_dpi <- 300
poster_rbd <- default_rbd
poster_font_scale <- 22

# set a new column width to account for different plot scaling

poster_save <- function(p, stem) {
    filename <- glue("images/poster/{stem}.png")
    ggsave(
        filename = filename,
        plot = p,
        width = poster_width,
        height = poster_height,
        units = "in",
        dpi = poster_dpi,
        device = ragg::agg_png
    )
    message(paste0("saved ", filename))
}


# ---- Fig 05: one plot per stressor, the poster RBD --------------------------------
fig05_data <- data_long_pretty_merged |>
    filter(
        !is.na(stressor_code),
        RBD == poster_rbd,
        comparison_operation == "interval"
    )

poster_rbd_name <- fig05_data |> pull(rbd_name) |> first()

fig05_stressors <- fig05_data |>
    distinct(stressor_code, stressor_name_group_md) |>
    arrange(stressor_code)

poster_fig05 <- map(
    fig05_stressors$stressor_code,
    function(code) {
        md_label <- fig05_stressors$stressor_name_group_md[
            fig05_stressors$stressor_code == code
        ]
        fig05_data |>
            filter(stressor_code == code) |>
            ggplot(aes(
                y = fct_rev(Month_abb),
                x = Probability_perc_merged,
                fill = RQ_range_merged
            )) +
            geom_col(position = "fill", width = geom_col_width) +
            scale_x_continuous_probability(limits = NULL, expand = FALSE) +
            scale_y_poster_months() +
            set_fill_scale(name = "RQ interval") +
            labs(
                x = "Probability of RQ in Interval",
                y = "Month",
                title = md_label,
                subtitle = glue("{poster_rbd_name} (predicted)")
            ) +
            theme(
                plot.title = element_markdown(face = "bold"),
                legend.position = "none",
                plot.margin = margin(10, 30, 10, 10), # needed to prevent the trailing % from being clipped by setting expand = FALSE
                text = element_text(size = poster_font_scale)
            )
    }
) |>
    set_names(fig05_stressors$stressor_code)

iwalk(poster_fig05, function(p, code) {
    poster_save(
        p,
        glue("poster_fig05_{str_to_lower(str_replace_all(code, '_', '-'))}")
    )
})

# ---- Fig 06: one plot per stressor group, the poster RBD -------------------------
fig06_data <- data_long_pretty_merged |>
    filter(
        RBD == poster_rbd,
        sum_operation == "SumRQ",
        stressor_group != "all",
        comparison_operation == "interval"
    )

fig06_groups <- fig06_data |>
    distinct(stressor_group, stressor_group_name) |>
    arrange(stressor_group_name)

poster_fig06 <- map(
    fig06_groups$stressor_group,
    function(grp) {
        grp_name <- fig06_groups$stressor_group_name[
            fig06_groups$stressor_group == grp
        ]
        fig06_data |>
            filter(stressor_group == grp) |>
            ggplot(aes(
                y = fct_rev(Month_abb),
                x = Probability_perc_merged,
                fill = RQ_range_merged
            )) +
            geom_col(position = "fill", width = geom_col_width) +
            scale_x_continuous_probability(limits = NULL, expand = FALSE) +
            scale_y_poster_months() +
            set_fill_scale(name = "RQ interval", drop = TRUE) +
            labs(
                x = "Probability of SumRQ in Interval",
                y = "Month",
                title = grp_name,
                subtitle = glue("{poster_rbd_name} (predicted)")
            ) +
            theme(
                legend.position = "none",
                text = element_text(size = poster_font_scale),
                plot.margin = margin(0, 30, 0, 0) # needed to prevent the trailing % from being clipped by setting expand = FALSE
            )
    }
) |>
    set_names(fig06_groups$stressor_group)

iwalk(poster_fig06, function(p, grp) {
    poster_save(p, glue("poster_fig06_{grp}"))
})

# ---- Fig 07: individual panels per threshold × approach, the poster RBD ---------
fig07_data <- data_long_pretty_merged |>
    filter(
        stressor_group == "all",
        RBD == poster_rbd,
        ((exceedence_boolean & sum_operation != "SumSumRQ") |
            (sum_operation == "SumSumRQ" & !is.na(RQ_range_merged)))
    )

make_poster_fig07_panels <- function(data, threshold) {
    p_sumsum_data <- data |>
        filter(sum_operation == "SumSumRQ", comparison_operation == "interval")

    p_CA <- p_sumsum_data |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged,
            fill = RQ_range_merged
        )) +
        geom_col(position = "fill", width = geom_col_width) +
        geom_intervals_outlined(p_sumsum_data, threshold) +
        scale_x_continuous_probability(limits = NULL) +
        scale_y_poster_months() +
        set_fill_scale(name = "RQ interval") +
        labs(
            x = glue("Probability of SumSumRQ > {threshold}"),
            y = "Month",
            title = glue("Concentration Addition (CA), threshold = {threshold}")
        ) +
        coord_cartesian(expand = FALSE) +
        theme(
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            legend.position = "none",
            text = element_text(size = poster_font_scale),
            plot.margin = margin(0, 30, 0, 0) # needed to prevent the trailing % from being clipped by setting expand = FALSE
        )

    p_IA <- data |>
        filter(
            sum_operation == "Any_RQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            fill = as.character(threshold)
        )) +
        geom_col(colour = "#888888") +
        set_fill_threshold_scale(
            threshold = as.character(threshold),
            lighten = 0.2
        ) +
        scale_x_continuous_probability() +
        scale_y_poster_months() +
        coord_cartesian(expand = FALSE) +
        labs(
            x = glue("Joint probability of any RQ > {threshold}"),
            y = "Month",
            title = glue("Independent Action (IA), threshold = {threshold}")
        ) +
        theme(
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            plot.margin = margin(0, 30, 0, 0), # needed to prevent the trailing % from being clipped by setting expand = FALSE
            text = element_text(size = poster_font_scale)
        )

    p_CA_IA <- data |>
        filter(
            sum_operation == "SumRQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            fill = as.character(threshold)
        )) +
        geom_col(colour = "#777") +
        set_fill_threshold_scale(
            threshold = as.character(threshold),
            lighten = 0.2
        ) +
        scale_x_continuous_probability() +
        scale_y_poster_months() +
        coord_cartesian(expand = FALSE) +
        labs(
            x = glue("Joint probability of any SumRQ > {threshold}"),
            y = "Month",
            title = glue("CA+IA, threshold = {threshold}")
        ) +
        theme(
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            text = element_text(size = poster_font_scale),
            plot.margin = margin(0, 30, 0, 0), # needed to prevent the trailing % from being clipped by setting expand = FALSE
        )

    list(CA = p_CA, IA = p_IA, CA_IA = p_CA_IA)
}

fig07_ranges <- c(0.1, 1)

poster_fig07 <- map(fig07_ranges, \(t) make_poster_fig07_panels(fig07_data, t)) |>
    set_names(as.character(fig07_ranges)) |>
    list_flatten(name_spec = "{outer}_{inner}")

iwalk(poster_fig07, function(p, name) {
    poster_save(p, glue("poster_fig07_{str_replace_all(name, '\\\\.', '-')}"))
})
