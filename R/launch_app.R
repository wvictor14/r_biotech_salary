#' @export
#' @import shiny shiny.fluent
launch_app <- function(..., salaries = NULL) {

  message('start')
  tic('initalize done')
  if (is.null(data)) {
    data(salaries)
    salaries |>  glimpse()
    message('salaries loaded')
  }

  version <- paste0('v', as.character(utils::packageVersion('r_biotech_salary')))
  #shiny::addResourcePath("www", "www")

  rb_ui <- rb_ui()
  rb_server <- rb_server
  toc()

  shiny::shinyApp(rb_ui, rb_server, options = list(launch.browser = TRUE), ...)
}

#' app ui
#'
#' @export
rb_ui <- function() {

  filters <- tagList(
    shiny.fluent::Dropdown.shinyInput(
      'title',
      multiSelect = TRUE,
      value = 'Scientist',
      label = NULL,
      options = salaries |>
        mutate(key = title_general, text = title_general) |>
        select(key, text) |>
        distinct()  |>
        arrange(key) |>
        mutate(across(everything(), ~ifelse(is.na(.x), '(Missing)', .x)))
    ),
    shiny.fluent::Dropdown.shinyInput(
      "location",
      placeHolder = "Select location",
      multiSelect = TRUE,
      value = salaries$location_country |> unique() |>  sort(),
      options =  salaries |>
        mutate(key = location_country, text = location_country) |>
        select(key, text) |>
        distinct()  |>
        arrange(key) |>
        mutate(across(everything(), ~ifelse(is.na(.x), '(Missing)', .x)))
    ),
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      shiny.fluent::DatePicker.shinyInput(
        "fromDate", value = as.Date('2022/01/01')),
      shiny.fluent::DatePicker.shinyInput(
        "toDate", value = Sys.time() |> lubridate::date())
    )
  )

  shiny.fluent::fluentPage(
    shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!"),
    tags$style(".card { padding: 28px; margin-bottom: 28px; }"),

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(content = shiny.fluent::Stack(
        filters,
        gt::gt_output('salary_stats_text')
      ),
      size = 4,
      style = "max-height: 250px;"),
      makeCard(content = shiny.fluent::Stack(
        ggiraph::girafeOutput("plot", height = '250px')
      ),
      size = 8,
      style = "max-height: 250px")
    ),
    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(
        content = plotly::plotlyOutput('plot_experience'),
        size = 6,
        style = "max-height: 250px"
      ),
      makeCard(
        content = div(
          style="max-height: 250px; overflow: auto",
          gt::gt_output("analysis")
        ),
        size = 6,
        style = "max-height: 250px"
      )
    )
  )
}

#' app server
#' @param input input
#' @param output output
#' @param session session
#' @export
#' @import dplyr ggplot2
#' @importFrom tictoc tic toc
rb_server <- function(input, output, session) {

  .salaries <- reactive({
    message('being data filter')
    tic('data filtered')
    req(input$fromDate)
    .df <-salaries |>
      filter(
        date >= input$fromDate,
        date <= input$toDate,
        title_general %in% input$title,
        location_country %in% input$location
      )
    toc()
    return(.df)
  })

  #plot
  output$salary_stats_text <- gt::render_gt({
    message('stats start')
    tic('stats end')
    gt <- plot_salary_title(.salaries())
    toc()
    return(gt)
  })
  #output$plot <- ggiraph::renderGirafe({
  #  plot_salary(.salaries(), title = 'Total Compensation (Base + Bonus)') |>
  #    ggiraph::girafe(width_svg = 10)
  #})

  #output$plot_experience <- plotly::renderPlotly({

    # suppress warnings for app session
  #  storeWarn<- getOption("warn")
  #  options(warn = -1)

  #  plot_experience(.salaries())
  #})

  # render the table + other components
  output$analysis <- gt::render_gt({
    items_list <- if(nrow(.salaries()) > 0){

      message('gt start')
      tictoc::tic('gt done')
      selected_cols <- .salaries() |>
        select(
          title_general,
          location_country,
          salary_base,
          bonus_pct,
          date
        )

      out <- selected_cols |>
        gt::gt() |>
        gt::cols_label(
          title_general = "Job title",
          location_country = "Location",
          salary_base = "Salary (Base)",
          bonus_pct = "Bonus %",
          date = "Date"
        ) |>
        gt::fmt_date(date) |>
        gt::fmt_percent(columns = bonus_pct) |>
        gt::fmt_currency(columns = salary_base) |>
        gt::opt_interactive()

      toc()
      return(out)
    } else {
      p("No matching salary data.")
    }
    return(items_list)
  })

}

#' @export
makeCard <- function(title = NULL, content, size = 12, style = "") {


  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
