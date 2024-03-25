
#' Create stats table for salary data
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |> filter(title_general == 'Scientist') |>   gt_salary_stats()
#'
gt_salary_stats <- function(.df, .return_data = FALSE, .gt = TRUE) {

  l <- list(
    average = mean(.df$salary_total),

    base_min = quantile(.df$salary_base, 0.05),
    total_min = quantile(.df$salary_total, 0.05),
    bonus_min = quantile(.df$bonus, 0.05),

    base_max = quantile(.df$salary_base, 0.95),
    total_max = quantile(.df$salary_total, 0.95),
    bonus_max = quantile(.df$bonus, 0.95)
  ) |>
    purrr::map(~scales::dollar(.x, accuracy = 1))

  if (.return_data) { return(l) }

  .color <- pal_salary()['total']
  .color_base <- pal_salary()['base']
  .color_bonus <- pal_salary()['bonus']
  title <- tibble(
    left = c('Average Total Pay', 'Total Pay Range', 'Base Pay', 'Bonus'),
    right = c(
      glue::glue(
        '<b><a style="color:{.color}">{l$average}</b></a>'
      ),
      glue::glue(
        '<b><a style="color:{.color}">{l$total_min}</b></a>',
        ' - ',
        '<b><a style="color:{.color}">{l$total_max}</b></a>'
      ),
      '<b><a style="color:{.color_base}">{l$base_min} - {l$base_max}</a></b>',
      '<b><a style="color:{.color_bonus}">{l$bonus_min} - {l$bonus_max}</a></b>')
  ) |>
    mutate(right = purrr::map_chr(right, ~glue::glue(.x)))

  if (.gt) {
    title <- title |>
      gt::gt() |>
      gt::cols_align(align = 'right', columns = right) |>
      gt::tab_options(
        data_row.padding = gt::px(0),
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        column_labels.hidden = TRUE) |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = gt::px(1.5),
          style = "solid"
        ),
        locations = gt::cells_body()
      ) |>
      gt::tab_header(title = gt::md("Yearly salary")) |>
      gt::fmt_markdown()
  }

  return(title)
}


#' table of summarized salary info
#'
#' @return plot
#' @export
#'
#' @examples
#' gt_career_progression(salaries)
gt_career_progression <- function(.df) {
  .df |>
    arrange(title_general) |>
    group_by(title_category, title_general) |>
    dplyr::summarize(
      total = median(salary_total, na.rm = TRUE),
      base = median(salary_base, na.rm = TRUE),
      bonus = median(bonus, na.rm = TRUE),
      bonus_pct = mean(bonus_pct, na.rm = TRUE)
    ) |>
    #ungroup() |>
    mutate(base_p = base/total) |>
    relocate(base_p, .after = total) |>
    select(-base) |>
    gt::gt() |>

    # adjust content
    gt::cols_label(
      title_general = '',
      total = 'Total',
      base_p = 'Base/Bonus',
      bonus = 'Bonus'
    ) |>

    gt::fmt_currency(columns = c(total, bonus), decimals = 0) |>
    gt::fmt_percent(bonus_pct, decimals = 0) |>
    gt::cols_merge(
      columns = c(bonus, bonus_pct),
      pattern = "{1} ({2})"
    )|>
    gtExtras::gt_plt_bar_pct(
      base_p, fill = pal_salary()['base'], background = pal_salary()['bonus']
    ) |>
    gt::cols_align(title_general, align = 'right') |>
    gt::cols_align(bonus_pct, align = 'left') |>

    # theming
    gt::tab_style(
      style = gt::cell_borders(style = 'hidden'),
      locations = gt::cells_body(
        columns = c(everything(), -title_category))
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(color = pal_salary()['total'])
      ),
      locations = gt::cells_body(
        columns = total
      )
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(color = pal_salary()['bonus_2'])
      ),
      locations = gt::cells_body(
        columns = bonus
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = 'xx-small',
                            color = "white",
      ),
      location = gt::cells_row_groups()
    ) |>

    gt::tab_options(
      column_labels.border.top.style = 'hidden',
      column_labels.border.bottom.style = 'hidden',
      row_group.padding = gt::px(0),
      data_row.padding = gt::px(1)
    )
}
