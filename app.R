# Colorado COVID case surveillance (CDC Socrata) — Shiny
# Expects cached RDS files under ./cache produced by your Quarto analysis:
#   co_month.rds, co_by_age.rds, co_county.rds, co_county_month_YYYY.rds, [optional] severe.rds

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(purrr)
  library(readr)
})

# Show real errors during development
options(shiny.sanitize.errors = FALSE)

# -------------------------------
# Data loading from cache
# -------------------------------
cache_read <- function(path) {
  if (!file.exists(path)) stop(sprintf("Missing required cache file: %s", path))
  readRDS(path)
}

# Required cached frames (coerce numerics immediately)
co_month <- cache_read("cache/co_month.rds") |>
  dplyr::mutate(case_month = as.Date(case_month),
                n = suppressWarnings(as.numeric(n)))

co_by_age <- cache_read("cache/co_by_age.rds") |>
  dplyr::mutate(case_month = as.Date(case_month),
                n = suppressWarnings(as.numeric(n)))

co_county <- cache_read("cache/co_county.rds") |>
  dplyr::mutate(n = suppressWarnings(as.numeric(n)),
                county_fips_code = sprintf("%05s", county_fips_code))

# County x month: bind yearly slices present in cache
yr_files <- list.files("cache", pattern = "^co_county_month_\\d{4}\\.rds$", full.names = TRUE)
if (length(yr_files) == 0L) {
  warning("No county-month cache files found (co_county_month_YYYY.rds). County Explorer will be limited.")
  co_county_month <- tibble::tibble(
    case_month = as.Date(character()),
    county_fips_code = character(),
    n = numeric(),
    res_county = character()
  )
} else {
  co_county_month <- map_dfr(yr_files, readRDS) |>
    mutate(
      case_month = as.Date(case_month),
      county_fips_code = sprintf("%05s", county_fips_code),
      n = suppressWarnings(as.numeric(n))
    ) |>
    # add county names from totals (lighter than returning from API)
    left_join(co_county |> select(county_fips_code, res_county) |> distinct(),
              by = "county_fips_code") |>
    arrange(case_month, county_fips_code)
}

# Optional severity cache (if you saved one during Quarto)
severe_path <- "cache/severe.rds"
severe <- if (file.exists(severe_path)) {
  readRDS(severe_path) |>
    mutate(case_month = as.Date(case_month))
} else {
  NULL
}

date_min <- min(co_month$case_month, na.rm = TRUE)
date_max <- max(co_month$case_month, na.rm = TRUE)
age_opts <- sort(unique(na.omit(co_by_age$age_group)))

# -------------------------------
# Theming
# -------------------------------
app_theme <- bs_theme(
  version     = 5,
  bootswatch  = "flatly",
  primary     = "#0d6efd",
  base_font   = font_google("Inter", local = TRUE),
  heading_font= font_google("Inter", local = TRUE)
)

# -------------------------------
# UI
# -------------------------------
ui <- page_navbar(
  title  = "Colorado COVID (CDC) — API Summaries",
  theme  = app_theme,
  collapsible = TRUE,

  nav_panel(
    "Overview",
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 8),
      card(
        full_screen = TRUE,
        card_header("Filters"),
        sliderInput("date_range", "Month range",
                    min = date_min, max = date_max,
                    value = c(date_min, date_max), timeFormat = "%Y-%m",
                    step = 31, width = "100%"),
        selectizeInput("age_sel", "Age groups (for distribution):",
                       choices = age_opts, selected = age_opts,
                       multiple = TRUE, options = list(plugins = list("remove_button")))
      ),
      card(
        full_screen = TRUE,
        card_header("Monthly cases (Colorado)"),
        plotOutput("plot_cases", height = 280),
        hr(),
        if (!is.null(severe)) {
          tagList(
            card_header("Severity proxies: hospitalization & death ratios"),
            plotOutput("plot_severe", height = 280)
          )
        } else {
          card_body(
            p(em("Hospitalization/death ratios not cached — run the Quarto analysis to generate ",
                 code("cache/severe.rds"), " or uncomment API helpers to fetch on the fly."))
          )
        }
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Age distribution over time"),
      plotOutput("plot_age_area", height = 320)
    )
  ),

  nav_panel(
    "County Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        dateInput("month_pick", "Select month",
                  value = date_max, min = date_min, max = date_max),
        numericInput("top_n", "Top N counties (by cases in selected month)", 10, min = 5, max = 64, step = 1),
        checkboxInput("show_table_allmonths", "Show full monthly table for selected county", FALSE),
        uiOutput("county_picker_ui"),
        hr(),
        downloadButton("dl_county_month_csv", "Download county-month CSV")
      ),
      card(
        full_screen = TRUE,
        card_header("Top counties (selected month)"),
        plotOutput("plot_top_counties", height = 340)
      ),
      card(
        full_screen = TRUE,
        card_header("County-month table"),
        DTOutput("tbl_county_month")
      )
    )
  ),

  nav_panel(
    "Tables",
    card(
      full_screen = TRUE,
      card_header("Monthly totals (Colorado)"),
      DTOutput("tbl_co_month"),
      downloadButton("dl_co_month_csv", "Download CSV")
    ),
    card(
      full_screen = TRUE,
      card_header("Age distribution by month"),
      DTOutput("tbl_by_age"),
      downloadButton("dl_by_age_csv", "Download CSV")
    )
  ),

  nav_panel(
    "About",
    card(
      card_body(
        h4("About this dashboard"),
        p("This Shiny app reads aggregated Colorado COVID-19 summaries from cached RDS files under ",
          code("cache/"), ". The caches are produced by the Quarto analysis that queries CDC's Socrata API ",
          "with server-side aggregation (SoQL)."),
        tags$ul(
          tags$li(code("cache/co_month.rds"), " — monthly totals for Colorado"),
          tags$li(code("cache/co_by_age.rds"), " — monthly counts by age group"),
          tags$li(code("cache/co_county.rds"), " — county totals"),
          tags$li(code("cache/co_county_month_YYYY.rds"), " — county×month slices"),
          tags$li(code("cache/severe.rds"), " — optional hospitalization/death ratios by month")
        ),
        p("To refresh data, re-run the Quarto analysis (or uncomment the API helpers and run this app with a CDC token set as ",
          code("SOCRATA_APP_TOKEN_CDC"), ")."),
        p("Author: Ikaia Leleiwi • GitHub repo: https://github.com/ileleiwi/colorado_covid_cases")
      )
    )
  )
)

# -------------------------------
# Helpers + Server
# -------------------------------
# robust label helper (works on older/newer 'scales')
label_si_safe <- function() {
  if ("label_number_si" %in% getNamespaceExports("scales")) {
    scales::label_number_si()
  } else {
    scales::comma
  }
}

server <- function(input, output, session) {

  # ---- reactive filters ----
  co_month_f <- reactive({
    req(input$date_range)
    co_month |>
      dplyr::filter(case_month >= input$date_range[1],
                    case_month <= input$date_range[2]) |>
      dplyr::mutate(n = suppressWarnings(as.numeric(n)))
  })

  by_age_f <- reactive({
    req(input$date_range)
    co_by_age |>
      dplyr::filter(case_month >= input$date_range[1],
                    case_month <= input$date_range[2],
                    age_group %in% input$age_sel) |>
      dplyr::mutate(n = suppressWarnings(as.numeric(n)))
  })

  selected_month_df <- reactive({
    req(input$month_pick)
    co_county_month |>
      dplyr::filter(case_month == as.Date(input$month_pick)) |>
      dplyr::arrange(dplyr::desc(n))
  })

  observe({
    updateSelectizeInput(session, "county_pick",
      choices = unique(co_county$res_county), server = TRUE)
  })

  output$county_picker_ui <- renderUI({
    selectizeInput("county_pick", "Focus county (optional):",
                   choices = unique(co_county$res_county), selected = NULL,
                   multiple = FALSE, options = list(placeholder = "All counties"))
  })

  # ---- plots ----
  output$plot_cases <- renderPlot({
    df <- req(co_month_f())
    validate(need(nrow(df) > 0, "No data for the selected range."))

    ggplot(df, aes(x = case_month, y = n)) +
      geom_line(size = 0.8) +
      scale_y_continuous(labels = label_si_safe()) +
      labs(x = NULL, y = "Monthly cases") +
      theme_minimal(base_size = 13)
  })

  output$plot_severe <- renderPlot({
    req(severe)
    df <- severe |>
      dplyr::filter(case_month >= input$date_range[1],
                    case_month <= input$date_range[2]) |>
      tidyr::pivot_longer(dplyr::all_of(c("hosp_rate","death_rate")),
                          names_to = "metric", values_to = "rate")

    ggplot(df, aes(case_month, rate, color = metric)) +
      geom_line(size = 0.8) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_color_manual(NULL, values = c("#2c7fb8", "#d95f0e"),
                         labels = c("Hospitalization ratio", "Death ratio")) +
      labs(x = NULL, y = "Rate") +
      theme_minimal(base_size = 13)
  })

  output$plot_age_area <- renderPlot({
    df <- req(by_age_f()) |>
      dplyr::group_by(case_month) |>
      dplyr::mutate(pct = n / sum(n)) |>
      dplyr::ungroup()

    ggplot(df, aes(case_month, pct, fill = age_group)) +
      geom_area(alpha = 0.9) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Share of monthly cases", fill = "Age group") +
      theme_minimal(base_size = 13)
  })

  output$plot_top_counties <- renderPlot({
    df <- req(selected_month_df()) |>
      dplyr::slice_head(n = input$top_n) |>
      dplyr::mutate(res_county = forcats::fct_reorder(res_county, n))

    ggplot(df, aes(res_county, n)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = label_si_safe()) +
      labs(x = NULL, y = "Cases (selected month)") +
      theme_minimal(base_size = 13)
  })

  # ---- tables & downloads ----
  output$tbl_county_month <- renderDT({
    df <- selected_month_df()
    if (!isTruthy(input$county_pick) || is.na(input$county_pick)) {
      datatable(df, rownames = FALSE, options = list(pageLength = 15))
    } else {
      cm <- co_county_month |>
        dplyr::filter(res_county == input$county_pick) |>
        dplyr::arrange(case_month)
      if (isTRUE(input$show_table_allmonths)) {
        datatable(cm, rownames = FALSE, options = list(pageLength = 15))
      } else {
        datatable(df, rownames = FALSE, options = list(pageLength = 15))
      }
    }
  })

  output$tbl_co_month <- renderDT({
    datatable(co_month_f(), rownames = FALSE, options = list(pageLength = 12))
  })
  output$tbl_by_age <- renderDT({
    datatable(by_age_f(), rownames = FALSE, options = list(pageLength = 12))
  })

  output$dl_co_month_csv <- downloadHandler(
    filename = function() sprintf("co_month_%s_%s.csv", input$date_range[1], input$date_range[2]),
    content  = function(file) write_csv(co_month_f(), file)
  )
  output$dl_by_age_csv <- downloadHandler(
    filename = function() "co_by_age_filtered.csv",
    content  = function(file) write_csv(by_age_f(), file)
  )
  output$dl_county_month_csv <- downloadHandler(
    filename = function() {
      m <- format(as.Date(input$month_pick), "%Y-%m")
      sprintf("co_county_month_%s.csv", m)
    },
    content  = function(file) write_csv(selected_month_df(), file)
  )
}

# -------------------------------
# Run
# -------------------------------
shinyApp(ui, server)
