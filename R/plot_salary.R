#' @rdname plot_salary
#' @export
calculate_salary_stats <- function(.df, x) {
  x_mean <- .df |>  pull({{x}}) |> mean()
  percentiles <- .df |>  pull({{x}}) |>  quantile(c(0.05, 0.95))
  stats <- c(
    'Average' = x_mean,
    setNames(percentiles, nm = c('5th percentile', '95th percentile'))) |>
    tibble::enframe(name = 'stat', 'x') |>
    mutate(stat = forcats::fct(
      stat, levels = c('5th percentile', 'Average', '95th percentile')))
}

#' plot salary histogram
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' ggiraph::girafe(plot_salary(salaries_sub))
#' print(x)
#'
#' @param .df salary data
#' @export
plot_salary <- function(
    .df, x = salary_total, fill = title_general, title = '',
    .interactive = TRUE) {

  .df <- .df |>  filter(!is.na({{x}}), !is.na({{fill}}))

  stats <- calculate_salary_stats(.df, {{x}})

  p <- ggplot(.df, aes(
    x = {{x}}, fill = {{fill}})) +
    geom_vline(
      data = stats, aes(xintercept = x, color = stat), linetype = 'twodash') +
    ggiraph::geom_histogram_interactive(
      bins = 25,
      aes(
        y = after_stat(count / sum(count)),
        tooltip =  glue::glue(
          "{.f}: {.p}",
          .f = fill,
          .p = scales::percent(after_stat(count / sum(count)))
          ),
        group = {{fill}}))  +
    theme_minimal() +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom') +
    paletteer::scale_fill_paletteer_d('ggthemes::Tableau_20', guide ='none') +
    paletteer::scale_color_paletteer_d('khroma::highcontrast') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(title = title, color = '', x = '', y = '% of jobs')

  suppressWarnings({print(p) })
}
#' Make the title for salary histogram
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |>  plot_salary_title()
#'
plot_salary_title <- function(.df, .return_data = FALSE, .gt = TRUE) {

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


  title <- tibble(
    left = c('Average is', 'Total Pay Range', 'Base Pay', 'Bonus'),
    right = c(
      '<b><a style="color:#DDAA33FF">{l$average}</b></a> /yr',
      glue::glue(
        '<b><a style="color:#004488FF">{l$total_min}</b></a>',
        ' - ',
        '<b><a style="color:#BB5566FF">{l$total_max}</b></a> /yr'
      ),
      "{l$base_min} - {l$base_max} /yr", "{l$bonus_min} - {l$bonus_max} /yr")
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


#' Plot salary against years of experience
#'
#' @param .df  salary data
#'
#' @return ggplot
#' @export
#'
#' @examples
#' data(salaries)
#' salaries_sci <- salaries |>
#'   filter(title_general == 'Scientist', location_country == 'United States Of America')
#' plot_experience(salaries_sci)
plot_experience <- function(.df, fill = title_general, .plotly =TRUE) {

  p <- .df |>
    arrange(years_of_experience) |>
    mutate(exp_binned = cut(
      years_of_experience,
      breaks = c(-Inf, 2, 5, 9, 14, Inf),
      labels = c('1-2', '3-5', '6-9', '10-14', '15+'))) |>
    ggplot(aes(x = exp_binned, y = salary_total, fill = {{fill}})) +
    geom_boxplot(outlier.shape = NA, position = position_dodge()) +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.title.y = element_blank(),
      panel.grid.minor.y = element_line(),
      panel.grid.major.x = element_blank()) +
    paletteer::scale_fill_paletteer_d('ggthemes::Tableau_20', guide ='none') +
    scale_y_continuous(
      labels = scales::dollar, limits = c(0, NA),
      expand = expansion(c(0, 0.1))
    ) +
    labs(
      title = 'Total Compensation by Years of Experience',
      x = 'Years of Experience')

  if (.plotly) {
    p <- plotly::ggplotly(p, height = 250) |> plotly::layout(boxmode = "group")
  }

  suppressWarnings({ p })
}
