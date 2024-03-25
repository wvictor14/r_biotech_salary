
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
#' plot_salary(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_salary <- function(
    .df, x = salary_total, fill = title_general, title = '',
    height = 275,
    .type = 'plotly') {

  .df <- .df |>  filter(!is.na({{x}}), !is.na({{fill}}))
  stats <- calculate_salary_stats(.df, {{x}})

  p <- ggplot(.df, aes(
    x = {{x}}, fill = {{fill}})) +
    geom_vline(
      xintercept = stats$x[1],
      linetype = 'twodash',
      color = 'black'
    ) +
    geom_vline(
      xintercept = stats$x[2],
      linetype = 'twodash',
      color = 'black'
    ) +
    geom_vline(
      xintercept = stats$x[3],
      linetype = 'twodash',
      color = 'black'
    ) +
    geom_histogram(bins = 25, aes(y = after_stat(count / sum(count))))  +
    theme_minimal() +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom') +
    paletteer::scale_fill_paletteer_d(pal_paletteer(), guide ='none') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(title = title, color = '', x = '', y = '% of jobs')

  stopifnot(.type %in% c('plotly', 'ggplot2'))
  if (.type == 'plotly') {
    p <- plotly::ggplotly(p, height = height)
  }

  suppressWarnings({ p })
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
plot_experience <- function(.df, fill = title_general, height = 275, .plotly =TRUE) {

  .cut <- .df |>
    arrange(years_of_experience) |>
    mutate(
      exp_binned = cut(
        years_of_experience,
        breaks = c(-Inf, 2, 5, 9, 14, Inf),
        labels = c('1-2', '3-5', '6-9', '10-14', '15+')
      ) |> forcats::fct_rev()
    )

  .summarized <- .cut |>
    summarize(
      .by = c(title_general, exp_binned),
      n_obs = n(),
      med = median(salary_total, na.rm = TRUE),
      me = mean(salary_total, na.rm = TRUE),
      q25 = quantile(salary_total, c(0.25), na.rm = TRUE),
      q75 = quantile(salary_total, c(0.75), na.rm = TRUE)
    )
  p <- .summarized |>
    ggplot(aes(y = exp_binned, x = med, fill = {{fill}})) +
    geom_bar(stat = 'identity') +
    geom_text(
      aes(
        label = stringr::str_c(scales::dollar(med/1000, accuracy = 1), 'K'),
        x = med*0.4
      ),
      color = 'white', hjust = 0,
      size = 4, size.unit = 'pt'
      ) +
    #geom_jitter(data = .cut, aes(x = salary_total), width = 0.5, alpha = 0.5) +
    #geom_boxplot(outlier.shape = NA, position = position_dodge()) +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      axis.line.y = element_line(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
      panel.grid.minor.x = element_line(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank()
    ) +
    facet_grid(cols = vars({{fill}})) +
    paletteer::scale_fill_paletteer_d(pal_paletteer(), guide = 'none') +
    scale_x_continuous(
      labels =  scales::dollar_format(scale = .001, suffix = "K"),
      limits = c(0, NA),
      breaks = ifelse(
        length(unique(.df$title_general)) == 1,
        scales::breaks_pretty(n = 3),
        scales::breaks_pretty(n = 2)
      ),
      expand = expansion(c(0, 0.1))
    ) +
    labs(
      title = 'Total Compensation by Years of Experience',
      y = 'Years\nof\nExperience')

  if (.plotly) {
    p <- plotly::ggplotly(p, height = height)
  }

  suppressWarnings({ p })
}

#' plot career progression
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' plot_salary(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_career_progression <- function(
    .df, x = date, fill = title_general, title = '',
    .type = 'plotly') {
  .df %>%
    count({{x}}) |>
    plotly::plot_ly(x = ~{{x}}) |>
    plotly::add_lines(y = ~n) |>
    plotly::layout(
      xaxis = list(rangeslider = (list(type = 'date')))
    )
}


