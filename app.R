# Colorado COVID case surveillance (CDC Socrata) — Shiny
# Cached inputs under ./cache:
#   co_month.rds, co_by_age.rds, co_county.rds, co_county_month_YYYY.rds, severe.rds

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
  library(sf)
  library(tigris)
  library(leaflet)
})

options(shiny.sanitize.errors = FALSE)
options(tigris_use_cache = TRUE)

# -------------------------------
# Data loading from cache
# -------------------------------
cache_read <- function(path) {
  if (!file.exists(path)) stop(sprintf("Missing required cache file: %s", path))
  readRDS(path)
}

co_month <- cache_read("cache/co_month.rds") |>
  mutate(case_month = as.Date(case_month),
         n = suppressWarnings(as.numeric(n)))

co_by_age <- cache_read("cache/co_by_age.rds") |>
  mutate(case_month = as.Date(case_month),
         n = suppressWarnings(as.numeric(n)))

co_county <- cache_read("cache/co_county.rds") |>
  mutate(n = suppressWarnings(as.numeric(n)),
         county_fips_code = sprintf("%05s", county_fips_code))

# County × month
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
    left_join(co_county |> select(county_fips_code, res_county) |> distinct(),
              by = "county_fips_code") |>
    arrange(case_month, county_fips_code)
}

# Severe metrics — ensure object always exists
severe <- local({
  p <- "cache/severe.rds"
  if (file.exists(p)) {
    tryCatch({
      readRDS(p) |> mutate(case_month = as.Date(case_month))
    }, error = function(e) NULL)
  } else NULL
})

date_min <- min(co_month$case_month, na.rm = TRUE)
date_max <- max(co_month$case_month, na.rm = TRUE)
age_opts <- sort(unique(na.omit(co_by_age$age_group)))

# -------------------------------
# County geometry (sf) for Leaflet
# -------------------------------
co_shapes <- counties(state = "CO", year = 2023, class = "sf") |>
  st_transform(4326) |>
  transmute(
    county_fips_code = GEOID,
    county_name = NAME,
    geometry
  ) |>
  st_make_valid()

# Simple numeric pretty-printer for labels
fmt_int <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")

# Palette domain helper robust to all-NA or all-equal
pal_domain <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1])) return(c(0, 1))        # all NA
  if (diff(rng) == 0) {
    hi <- if (rng[2] == 0) 1 else rng[2] * 1.05  # expand flat domain
    return(c(min(0, rng[1]), hi))
  }
  rng
}

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
  navbar_options = navbar_options(collapsible = TRUE),

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
        card_header("Monthly cases (Colorado) — Interactive map"),
        leafletOutput("map_monthly_cases", height = 360),
        hr(),
        if (is.data.frame(severe) && nrow(severe) > 0) {
          tagList(
            card_header("Severity proxies: hospitalization & death ratios"),
            plotOutput("plot_severe", height = 280)
          )
        } else {
          card_body(
            p(em("Hospitalization/death ratios not found in ",
                 code("cache/severe.rds"),
                 ". Re-run the Quarto analysis to generate it."))
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
        card_header("Top counties (selected month) — Interactive map"),
        leafletOutput("map_top_counties", height = 360)
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
          tags$li(code("cache/severe.rds"), " — hospitalization/death ratios by month")
        ),
        p("Maps use ", code("tigris"), " county geometries cached locally; interactive tiles via ", code("leaflet"), "."),
        p("Author: Ikaia Leleiwi • GitHub repo: https://github.com/ileleiwi/colorado_covid_cases")
      )
    )
  )
)

# -------------------------------
# Helpers + Server
# -------------------------------
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
      filter(case_month >= input$date_range[1],
             case_month <= input$date_range[2]) |>
      mutate(n = suppressWarnings(as.numeric(n)))
  })

  by_age_f <- reactive({
    req(input$date_range)
    co_by_age |>
      filter(case_month >= input$date_range[1],
             case_month <= input$date_range[2],
             age_group %in% input$age_sel) |>
      mutate(n = suppressWarnings(as.numeric(n)))
  })

  selected_month_df <- reactive({
    req(input$month_pick)
    df <- co_county_month |>
      filter(case_month == as.Date(input$month_pick)) |>
      arrange(desc(n))
    validate(need(nrow(df) > 0, "No county data for the selected month."))
    df
  })

  # County picker
  observe({
    updateSelectizeInput(session, "county_pick",
      choices = unique(co_county$res_county), server = TRUE)
  })
  output$county_picker_ui <- renderUI({
    selectizeInput("county_pick", "Focus county (optional):",
                   choices = unique(co_county$res_county), selected = NULL,
                   multiple = FALSE, options = list(placeholder = "All counties"))
  })

  # ---- Map: Monthly cases (last month in selected range) ----
  map_month_df <- reactive({
    rng <- req(input$date_range)
    map_month <- max(rng, na.rm = TRUE)
    df <- co_county_month |>
      filter(case_month == as.Date(map_month)) |>
      group_by(county_fips_code) |>
      summarise(n = sum(as.numeric(n), na.rm = TRUE), .groups = "drop")
    left_join(co_shapes, df, by = "county_fips_code") |>
      mutate(n = coalesce(n, 0)) |>
      st_make_valid()
  })

  output$map_monthly_cases <- renderLeaflet({
    sf_df <- req(map_month_df())
    validate(need(nrow(sf_df) > 0, "No data for selected period."))
    dom <- pal_domain(sf_df$n)
    pal <- colorNumeric("viridis", domain = dom, na.color = "#f0f0f0")

    leaflet(sf_df, options = leafletOptions(minZoom = 5, maxZoom = 12)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(n),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 0.6,
        opacity = 1,
        label = ~htmltools::HTML(sprintf("<b>%s County</b><br/>Cases: %s", county_name, fmt_int(n))),
        highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = pal, values = sf_df$n,
                title = paste0("Cases (", format(max(req(input$date_range)), "%Y-%m"), ")"))
  })

  # ---- Map: Top counties (selected month) ----
  map_top_df <- reactive({
    df <- selected_month_df()
    left_join(co_shapes, df, by = "county_fips_code") |>
      mutate(n = coalesce(n, 0)) |>
      st_make_valid()
  })

  output$map_top_counties <- renderLeaflet({
    sf_df <- req(map_top_df())
    validate(need(nrow(sf_df) > 0, "No county data for selected month."))
    dom <- pal_domain(sf_df$n)
    pal <- colorNumeric("viridis", domain = dom, na.color = "#f0f0f0")

    top_ids <- sf_df |>
      st_drop_geometry() |>
      arrange(desc(n)) |>
      slice_head(n = input$top_n) |>
      pull(county_fips_code)

    leaflet(sf_df, options = leafletOptions(minZoom = 5, maxZoom = 12)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(n),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 0.6,
        opacity = 1,
        label = ~htmltools::HTML(sprintf("<b>%s County</b><br/>Cases: %s", county_name, fmt_int(n))),
        highlightOptions = highlightOptions(weight = 2, color = "#333333", bringToFront = TRUE)
      ) |>
      addPolygons(
        data = sf_df[sf_df$county_fips_code %in% top_ids, ],
        fill = FALSE, color = "#ff6a00", weight = 3, opacity = 1
      ) |>
      addLegend("bottomright", pal = pal, values = sf_df$n,
                title = paste0("Cases (", format(as.Date(input$month_pick), "%Y-%m"), ")"))
  })

  # ---- severity plot ----
  output$plot_severe <- renderPlot({
    req(is.data.frame(severe), nrow(severe) > 0, input$date_range)
    df <- severe |>
      filter(case_month >= input$date_range[1],
             case_month <= input$date_range[2]) |>
      pivot_longer(c("hosp_rate","death_rate"),
                   names_to = "metric", values_to = "rate")

    ggplot(df, aes(case_month, rate, color = metric)) +
      geom_line(size = 0.8) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      scale_color_manual(NULL, values = c("#2c7fb8", "#d95f0e"),
                         labels = c("Hospitalization ratio", "Death ratio")) +
      labs(x = NULL, y = "Rate") +
      theme_minimal(base_size = 13)
  })

  # ---- age area plot ----
  output$plot_age_area <- renderPlot({
    df <- req(by_age_f()) |>
      group_by(case_month) |>
      mutate(pct = n / sum(n)) |>
      ungroup()

    ggplot(df, aes(case_month, pct, fill = age_group)) +
      geom_area(alpha = 0.9) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Share of monthly cases", fill = "Age group") +
      theme_minimal(base_size = 13)
  })

  # ---- tables & downloads ----
  output$tbl_county_month <- renderDT({
    df <- selected_month_df()
    if (!isTruthy(input$county_pick) || is.na(input$county_pick)) {
      datatable(df, rownames = FALSE, options = list(pageLength = 15))
    } else {
      cm <- co_county_month |>
        filter(res_county == input$county_pick) |>
        arrange(case_month)
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
