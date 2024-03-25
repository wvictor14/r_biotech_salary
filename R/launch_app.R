#' @export
#' @import shiny shiny.fluent
launch_app <- function(..., salaries = NULL) {

  if (is.null(data)) {
    data(salaries)
    salaries |>  glimpse()
    message('salaries loaded')
  }

  version <- paste0('v', as.character(utils::packageVersion('r_biotech_salary')))
  #shiny::addResourcePath("www", "www")

  rb_ui <- rb_ui()
  rb_server <- rb_server

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
      options = salaries |> make_grouped_options(title_category, title_general)
    ),
    shiny.fluent::Dropdown.shinyInput(
      "location_country",
      placeHolder = "Select location",
      multiSelect = FALSE,
      value = 'United States Of America',
      options =  salaries |>
        mutate(key = location_country, text = location_country) |>
        select(key, text) |>
        distinct()  |>
        arrange(key)
    ),
    shiny.fluent::Dropdown.shinyInput(
      inputId = "location_granular",
      placeHolder = "Select sub-location",
      multiSelect = TRUE
    ),

    shiny.fluent::Stack(
      horizontal = TRUE,
      shiny.fluent::DefaultButton.shinyInput(
        "select_all", text = "Select all"
      ),
      shiny.fluent::DefaultButton.shinyInput(
        "deselect_all", text = "Deselect all"
      )
    ),
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(
        'date',
        options = c('All' , '2024', '2023', '2022') %>%
          tibble(key = ., text = .),
        styles = list(flexContainer = list(display = "flex", gap = 10)),
        value = 'All')
    )
  )

  shiny.fluent::fluentPage(
    shiny.fluent::Stack(
      horizontal = TRUE,
      shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!"),
      tags$pre(tags$span('\t')),
      shiny.fluent::Text(variant = "xLarge", 'Showing salary data for:')),
    tags$style(".card { padding: 14px; margin-bottom: 14px; }"),

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(content = shiny.fluent::Stack(
        filters,
        gt::gt_output('salary_stats_text')
      ),
      size = 4,
      style = "max-height: 300px;"),
      makeCard(content = shiny.fluent::Stack(
        plotly::plotlyOutput("plot")
      ),
      size = 8,
      style = "max-height: 300px")
    ),
    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(
        content = div(
          style="max-height: 275px",
          gt::gt_output('table_career_progression')
        ),
        size = 4,
        style = "max-height: 275px"
      ),
      makeCard(
        content = plotly::plotlyOutput('plot_experience'),
        size = 8,
        style = "max-height: 275px"
      )
    ),
    makeCard(
      content = div(
        style="max-height: 600px; overflow: scroll",
        DT::dataTableOutput("table_raw", height = '600px')
      ),
      size = 12,
      style = "max-height: 600px"
    )
  )
}

#' app server
#' @param input input
#' @param output output
#' @param session session
#' @export
#' @import dplyr ggplot2
rb_server <- function(input, output, session) {

  .salaries <- reactive({
    req(input$title)
    .date <- switch(
      input$date,
      'All' = c('2024', '2023', '2022'),
      '2024' = '2024',
      '2023' = '2023',
      '2022' = '2022'
    )

    salaries |>
      filter(
        lubridate::year(date) %in% .date,
        title_general %in% input$title,
        location_country %in% input$location_country,
        location_granular %in% input$location_granular
      )
  })

  # reactive inputs
  .location_granular <- eventReactive(input$location_country, {
    salaries |>
      filter(location_country %in% input$location_country) |>
      mutate(key = location_granular, text = location_granular ) |>
      select(key, text) |>
      distinct()  |>
      arrange(key) |>
      mutate(across(everything(), as.character))
  })

  observe({
        shiny.fluent::updateDropdown.shinyInput(
          session = session,
          inputId = 'location_granular',
          multiSelect = TRUE,
          value = .location_granular() |>  pull(key),
          options =  .location_granular()
        )
  })
  observeEvent(input$deselect_all, {
    shiny.fluent::updateDropdown.shinyInput(
      session = session,
      inputId = 'location_granular',
      multiSelect = TRUE,
      value = NULL,
      options =  .location_granular()
    )
  })
  observeEvent(input$select_all, {
    shiny.fluent::updateDropdown.shinyInput(
      session = session,
      inputId = 'location_granular',
      multiSelect = TRUE,
      value = .location_granular() |>  pull(key),
      options =  .location_granular()
    )
  })

  #plot
  output$salary_stats_text <- gt::render_gt({
    if (nrow(.salaries()) > 0) {
      gt_salary_stats(.salaries())
    } else {
      tibble('No matching salary data' = '') |> gt::gt() |>
        gt::tab_options(
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden")
    }
  })
  output$plot <- plotly::renderPlotly({
    plot_salary(.salaries(), title = 'Total Compensation (Base + Bonus)')
  })

  output$table_career_progression <- gt::render_gt({
    gt_career_progression(.salaries())
  })
  output$plot_experience <- plotly::renderPlotly({

    # suppress warnings for app session
    storeWarn<- getOption("warn")
    options(warn = -1)

    if (nrow(.salaries()) > 0) {
      plot_experience(.salaries())
    } else {
      p <- ggplot() + theme_minimal(); plotly::ggplotly(p, height = 250)
    }
  })

  # render the table + other components
  output$table_raw <- DT::renderDataTable(
    server = TRUE, expr = {
      items_list <- if(nrow(.salaries()) > 0){
        selected_cols <- .salaries() |>
          arrange(desc(date)) |>
          select(
            `Location` = location_granular,
            `Job title` = title_general,
            `Job details` = title_detail,
            `Field` = biotech_sub_industry,
            `Company/org` = company_or_institution_name,
            `Salary (base)` = salary_base,
            `Bonus` = bonus,
            `Bonus %` = bonus_pct,
            `Experience (yr)` = years_of_experience,
            `Highest education` = highest_achieved_formal_education,

            `Date` = date
          ) |>
          DT::datatable(
            rownames = FALSE,
            options = list('dom' = 'ftipr')) |>
          DT::formatCurrency('Salary (base)') |>
          DT::formatPercentage('Bonus %') |>
          DT::formatDate('Date', method = 'toLocaleDateString')
        selected_cols
      } else {
        tibble() |>  DT::datatable()
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
