library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(readr)
library(DT)

# ── Data ───────────────────────────────────────────────────────────────────────
# Place wood_central_heaters.csv and wood_room_heaters.csv in the same
# directory as app.R, or adjust the paths below.

central_raw <- read_csv(
  "wood_central_heaters.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "latin1")
) |>
  mutate(
    database                                  = "Central Heater",
    `Heat Output Upper (BTUs)`                = as.numeric(`Heat Output Maximum (BTUs)`),
    `Heat Output Lower (BTUs)`                = NA_real_,
    `Emission Rate (grams/hr)`                = NA_real_,
    `Emission Rate Annual Average (lb/mmBTU)` = as.numeric(`Emission Rate Annual Average (lb/mmBTU)`)
  ) |>
  rename(`Heat Output Range (BTUs)` = `Heat Output Maximum (BTUs)`)

room_raw <- read_csv(
  "wood_room_heaters.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "latin1")
) |>
  mutate(
    database                                  = "Room Heater",
    `Emission Rate Annual Average (lb/mmBTU)` = NA_real_
  )

# Shared columns for the merged table
shared_cols <- c(
  "database", "Manufacturer", "Model",
  "Type", "Subtype", "Fuel Type",
  "Firebox Volume",
  "Emission Rate (grams/hr)",
  "Emission Rate Annual Average (lb/mmBTU)",
  "Heat Output Upper (BTUs)", "Heat Output Lower (BTUs)",
  "Heat Output Range (BTUs)",
  "Efficiency", "CO",
  "Test Method", "NSPS Compliance 2020"
)

add_missing <- function(df, cols) {
  for (col in setdiff(cols, names(df))) df[[col]] <- NA
  df[cols]
}

all_heaters <- bind_rows(
  add_missing(central_raw, shared_cols),
  add_missing(room_raw,    shared_cols)
) |>
  mutate(
    across(c(Manufacturer, Model, Type, Subtype,
             `Fuel Type`, `NSPS Compliance 2020`, `Test Method`), trimws),
    across(c(Efficiency, CO, `Firebox Volume`,
             `Heat Output Upper (BTUs)`), as.numeric)
  )

# Pre-compute choice lists
all_manufacturers <- sort(unique(na.omit(all_heaters$Manufacturer)))
all_types         <- sort(unique(na.omit(all_heaters$Type)))
all_subtypes      <- sort(unique(na.omit(all_heaters$Subtype)))
all_fuels         <- sort(unique(na.omit(all_heaters$`Fuel Type`)))

eff_range <- range(all_heaters$Efficiency,               na.rm = TRUE)
co_range  <- range(all_heaters$CO,                       na.rm = TRUE)
btu_range <- range(all_heaters$`Heat Output Upper (BTUs)`, na.rm = TRUE)

# ── Display columns ────────────────────────────────────────────────────────────
display_cols <- c(
  "Manufacturer", "Model", "database", "Type", "Subtype",
  "Fuel Type", "Emission Rate (grams/hr)",
  "Emission Rate Annual Average (lb/mmBTU)",
  "Heat Output Upper (BTUs)", "Efficiency", "CO",
  "NSPS Compliance 2020"
)
display_labels <- c(
  "Manufacturer", "Model", "Database", "Type", "Subtype",
  "Fuel Type", "Emission Rate (g/hr)",
  "Emission Rate (lb/mmBTU)",
  "Max Heat Output (BTU)", "Efficiency (%)", "CO",
  "Step 2 Compliant"
)

# ── Theme ──────────────────────────────────────────────────────────────────────
epa_theme <- bs_theme(
  version      = 5,
  bg           = "#f5f7fa",
  fg           = "#1a1a2e",
  primary      = "#00693e",
  secondary    = "#205493",
  success      = "#2e8540",
  info         = "#02bfe7",
  warning      = "#fdb81e",
  danger       = "#e31c3d",
  base_font    = font_google("Source Sans 3"),
  heading_font = font_google("Merriweather"),
  "navbar-bg"           = "#00693e",
  "sidebar-bg"          = "#ffffff",
  "sidebar-border-color" = "#d6d7d9",
  "card-border-color"   = "#d6d7d9",
  "card-cap-bg"         = "#eef2ee"
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = tags$span(
    tags$img(
      src    = "https://www.epa.gov/sites/default/files/2013-06/epa_seal_verysmall_trim.gif",
      height = "36px",
      style  = "margin-right:10px; vertical-align:middle;"
    ),
    tags$span(
      "EPA-Certified Wood Heater Database",
      style = "vertical-align:middle; font-family:'Merriweather',serif;
               font-size:1.1rem; font-weight:700; color:#ffffff;"
    )
  ),
  theme        = epa_theme,
  window_title = "EPA Wood Heater Database",

  tags$head(tags$style(HTML("
    .navbar { background-color: #00693e !important;
              border-bottom: 3px solid #fdb81e; }
    .bslib-sidebar-layout > .sidebar {
      border-right: 1px solid #d6d7d9;
      background: #ffffff;
    }
    .sidebar-title {
      color: #205493; font-weight: 700; font-size: 0.82rem;
      text-transform: uppercase; letter-spacing: .06em;
    }
    .filter-header {
      background: #205493; color: #fff; padding: 3px 8px;
      border-radius: 3px; font-size: 0.75rem; font-weight: 700;
      text-transform: uppercase; letter-spacing: .06em;
      margin: 10px 0 5px;
    }
    #gate_msg {
      background: #e1f3f8; border: 1px solid #02bfe7;
      border-left: 5px solid #02bfe7; padding: 14px 18px;
      border-radius: 4px; color: #1a1a2e; font-size: 0.95rem;
    }
    .detail-label {
      font-weight: 700; color: #205493; min-width: 210px;
      display: inline-block;
    }
    .detail-section { border-left: 4px solid #00693e;
                      padding-left: 10px; margin-bottom: 14px; }
    .detail-section-title {
      font-size: 0.75rem; text-transform: uppercase;
      letter-spacing: .08em; color: #777; margin-bottom: 5px;
    }
    table.dataTable thead th {
      background-color: #205493 !important;
      color: #fff !important; font-size: 0.81rem;
    }
    table.dataTable tbody tr:hover {
      background-color: #e8f5e0 !important; cursor: pointer;
    }
    .dataTables_info, .dataTables_length,
    .dataTables_filter { font-size: 0.81rem; }
    .epa-footer {
      font-size: 0.73rem; color: #666;
      border-top: 1px solid #d6d7d9; padding-top: 8px; margin-top: 6px;
    }
  "))),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 285,
    open  = "open",

    div(class = "sidebar-title", bs_icon("search"), " Search Filters"),
    tags$p("Select at least one filter to display results.",
           style = "font-size:0.76rem; color:#666; margin-bottom:2px;"),

    div(class = "filter-header", "Database"),
    checkboxGroupInput(
      "db_filter", NULL,
      choices  = c("Room Heater", "Central Heater"),
      selected = character(0)
    ),

    div(class = "filter-header", "Manufacturer"),
    selectizeInput(
      "mfr_filter", NULL,
      choices  = c("All" = "", all_manufacturers),
      selected = "",
      multiple = FALSE,
      options  = list(placeholder = "Search manufacturers…")
    ),

    div(class = "filter-header", "Appliance Type"),
    selectizeInput(
      "type_filter", NULL,
      choices  = c("All" = "", all_types),
      selected = "",
      multiple = TRUE,
      options  = list(placeholder = "Wood Stove, Pellet Stove…")
    ),

    div(class = "filter-header", "Subtype"),
    selectizeInput(
      "subtype_filter", NULL,
      choices  = c("All" = "", all_subtypes),
      selected = "",
      multiple = TRUE,
      options  = list(placeholder = "Catalytic, Non-Catalytic…")
    ),

    div(class = "filter-header", "Fuel Type"),
    checkboxGroupInput(
      "fuel_filter", NULL,
      choices  = all_fuels,
      selected = character(0)
    ),

    div(class = "filter-header", "Step 2 Compliant"),
    radioButtons(
      "nsps_filter", NULL,
      choices  = c("Any" = "any", "Yes" = "Yes", "No" = "No"),
      selected = "any",
      inline   = TRUE
    ),

    hr(style = "margin:8px 0;"),
    div(class = "filter-header", "Performance"),

    sliderInput(
      "eff_filter", "Efficiency (%)",
      min   = floor(eff_range[1]),
      max   = ceiling(eff_range[2]),
      value = c(floor(eff_range[1]), ceiling(eff_range[2])),
      step  = 1
    ),
    sliderInput(
      "co_filter", "Carbon Monoxide (g/hr)",
      min   = 0,
      max   = ceiling(co_range[2] * 10) / 10,
      value = c(0, ceiling(co_range[2] * 10) / 10),
      step  = 0.1
    ),
    sliderInput(
      "btu_filter", "Max Heat Output (BTU)",
      min   = 0,
      max   = ceiling(btu_range[2] / 1000) * 1000,
      value = c(0, ceiling(btu_range[2] / 1000) * 1000),
      step  = 1000,
      sep   = ","
    ),

    hr(style = "margin:8px 0;"),
    actionButton(
      "reset_btn", "Reset All Filters",
      icon  = icon("rotate-left"),
      class = "btn-outline-secondary btn-sm w-100"
    ),

    div(
      class = "epa-footer mt-2",
      "Data from the U.S. EPA. For official information see ",
      tags$a("epa.gov/burnwise",
             href = "https://www.epa.gov/burnwise", target = "_blank"), "."
    )
  ),

  # ── Main panel ───────────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title    = "Records Shown",
      value    = textOutput("vb_shown"),
      showcase = bs_icon("table"),
      theme    = "primary"
    ),
    value_box(
      title    = "Manufacturers",
      value    = textOutput("vb_mfrs"),
      showcase = bs_icon("building"),
      theme    = value_box_theme(bg = "#205493", fg = "#ffffff")
    ),
    value_box(
      title    = "Step 2 Compliant",
      value    = textOutput("vb_step2"),
      showcase = bs_icon("patch-check"),
      theme    = "success"
    )
  ),

  uiOutput("results_ui"),
  uiOutput("detail_modal_ui")
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Any filter active? ──────────────────────────────────────────────────────
  any_filter_active <- reactive({
    length(input$db_filter) > 0 ||
    nzchar(input$mfr_filter) ||
    length(input$type_filter) > 0 ||
    length(input$subtype_filter) > 0 ||
    length(input$fuel_filter) > 0 ||
    input$nsps_filter != "any" ||
    !is.null(input$eff_filter) && (
      input$eff_filter[1] > floor(eff_range[1]) ||
      input$eff_filter[2] < ceiling(eff_range[2])
    ) ||
    !is.null(input$co_filter) && (
      input$co_filter[1] > 0 ||
      input$co_filter[2] < ceiling(co_range[2] * 10) / 10
    ) ||
    !is.null(input$btu_filter) && (
      input$btu_filter[1] > 0 ||
      input$btu_filter[2] < ceiling(btu_range[2] / 1000) * 1000
    )
  })

  # ── Filtered data ───────────────────────────────────────────────────────────
  filtered <- reactive({
    df <- all_heaters

    if (length(input$db_filter) > 0)
      df <- df |> filter(database %in% input$db_filter)
    if (nzchar(input$mfr_filter))
      df <- df |> filter(Manufacturer == input$mfr_filter)
    if (length(input$type_filter) > 0)
      df <- df |> filter(Type %in% input$type_filter)
    if (length(input$subtype_filter) > 0)
      df <- df |> filter(Subtype %in% input$subtype_filter)
    if (length(input$fuel_filter) > 0)
      df <- df |> filter(`Fuel Type` %in% input$fuel_filter)
    if (input$nsps_filter != "any")
      df <- df |> filter(`NSPS Compliance 2020` == input$nsps_filter)

    req(input$eff_filter, input$co_filter, input$btu_filter)

    df <- df |>
      filter(is.na(Efficiency) |
             (Efficiency >= input$eff_filter[1] &
              Efficiency <= input$eff_filter[2])) |>
      filter(is.na(CO) |
             (CO >= input$co_filter[1] & CO <= input$co_filter[2])) |>
      filter(is.na(`Heat Output Upper (BTUs)`) |
             (`Heat Output Upper (BTUs)` >= input$btu_filter[1] &
              `Heat Output Upper (BTUs)` <= input$btu_filter[2]))
    df
  })

  # ── Value boxes ─────────────────────────────────────────────────────────────
  output$vb_shown <- renderText({
    if (!any_filter_active()) return("—")
    scales::comma(nrow(filtered()))
  })
  output$vb_mfrs <- renderText({
    if (!any_filter_active()) return("—")
    length(unique(na.omit(filtered()$Manufacturer)))
  })
  output$vb_step2 <- renderText({
    if (!any_filter_active()) return("—")
    n <- sum(filtered()$`NSPS Compliance 2020` == "Yes", na.rm = TRUE)
    paste0(n, " / ", nrow(filtered()))
  })

  # ── Results UI ───────────────────────────────────────────────────────────────
  output$results_ui <- renderUI({
    if (!any_filter_active()) {
      return(div(
        id = "gate_msg",
        bs_icon("info-circle-fill"), " ",
        tags$strong("Select at least one filter"),
        " in the sidebar to display results.",
        tags$br(), tags$br(),
        tags$em("Start with the Database checkboxes to show Room Heaters,
                 Central Heaters, or both, then refine with manufacturer,
                 type, fuel, compliance, and performance filters.")
      ))
    }

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span(bs_icon("table"), " Search Results"),
        downloadButton("dl_csv", "Download CSV",
                       icon  = icon("download"),
                       class = "btn-sm btn-outline-primary")
      ),
      DTOutput("heater_table"),
      card_footer(
        class = "text-muted",
        style = "font-size:0.78rem;",
        icon("hand-pointer"), " Click any row to view the full record. ",
        textOutput("footer_n", inline = TRUE), " record(s) found."
      ),
      full_screen = TRUE
    )
  })

  output$footer_n <- renderText(scales::comma(nrow(filtered())))

  # ── DT ───────────────────────────────────────────────────────────────────────
  output$heater_table <- renderDT({
    req(any_filter_active())
    df_show <- filtered()[, intersect(display_cols, names(filtered()))]
    names(df_show) <- display_labels[match(names(df_show), display_cols)]

    datatable(
      df_show,
      selection  = "single",
      rownames   = FALSE,
      options    = list(
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","All")),
        scrollX    = TRUE,
        dom        = "lfrtip",
        order      = list(list(0, "asc")),
        columnDefs = list(
          list(className = "dt-center",
               targets = c(6, 7, 8, 9, 10, 11))
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatRound(
        columns = intersect(
          c("Emission Rate (g/hr)", "Emission Rate (lb/mmBTU)", "Efficiency (%)", "CO"),
          display_labels
        ),
        digits = 2
      ) |>
      formatCurrency(
        columns  = "Max Heat Output (BTU)",
        currency = "", digits = 0, mark = ","
      ) |>
      formatStyle(
        "Step 2 Compliant",
        color      = styleEqual(c("Yes","No"), c("#2e8540","#e31c3d")),
        fontWeight = "bold"
      )
  }, server = TRUE)

  # ── Detail modal ─────────────────────────────────────────────────────────────
  fmt_val <- function(x) {
    if (length(x) == 0 || is.na(x) || x == "")
      return(tags$em("—", style = "color:#bbb;"))
    if (is.numeric(x)) return(formatC(x, format = "fg", digits = 4))
    as.character(x)
  }

  detail_row <- function(label, value) {
    tags$tr(
      tags$td(tags$span(class = "detail-label", label)),
      tags$td(fmt_val(value))
    )
  }

  observeEvent(input$heater_table_rows_selected, {
    row <- filtered()[input$heater_table_rows_selected, ]
    req(nrow(row) > 0)

    showModal(modalDialog(
      title = tags$span(
        bs_icon("fire"), " ",
        tags$strong(row$Model),
        tags$small(paste0(" — ", row$Manufacturer),
                   style = "color:#555; font-weight:400; margin-left:6px;")
      ),
      size      = "lg",
      easyClose = TRUE,
      footer    = modalButton("Close"),

      tags$div(class = "detail-section",
        tags$div(class = "detail-section-title", "Identification"),
        tags$table(class = "table table-sm table-borderless",
          detail_row("Database",       row$database),
          detail_row("Manufacturer",   row$Manufacturer),
          detail_row("Model",          row$Model),
          detail_row("Type",           row$Type),
          detail_row("Subtype",        row$Subtype),
          detail_row("Fuel Type",      row$`Fuel Type`),
          detail_row("Firebox Volume (cu ft)", row$`Firebox Volume`),
          detail_row("Test Method",    row$`Test Method`)
        )
      ),

      tags$div(class = "detail-section",
        tags$div(class = "detail-section-title", "Performance Data"),
        tags$table(class = "table table-sm table-borderless",
          detail_row("Emission Rate (g/hr)",     row$`Emission Rate (grams/hr)`),
          detail_row("Emission Rate (lb/mmBTU)", row$`Emission Rate Annual Average (lb/mmBTU)`),
          detail_row("Heat Output Upper (BTU)",  row$`Heat Output Upper (BTUs)`),
          detail_row("Heat Output Lower (BTU)",  row$`Heat Output Lower (BTUs)`),
          detail_row("Heat Output Range (BTU)",  row$`Heat Output Range (BTUs)`),
          detail_row("Efficiency (%)",            row$Efficiency),
          detail_row("Carbon Monoxide (CO)",      row$CO)
        )
      ),

      tags$div(class = "detail-section",
        tags$div(class = "detail-section-title", "Compliance"),
        tags$table(class = "table table-sm table-borderless",
          detail_row("NSPS Step 2 Compliant (2020)", row$`NSPS Compliance 2020`)
        )
      )
    ))
  })

  # ── Download ─────────────────────────────────────────────────────────────────
  output$dl_csv <- downloadHandler(
    filename = function() paste0("epa_wood_heaters_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(filtered(), file)
  )

  # ── Reset ────────────────────────────────────────────────────────────────────
  observeEvent(input$reset_btn, {
    updateCheckboxGroupInput(session, "db_filter",      selected = character(0))
    updateSelectizeInput(    session, "mfr_filter",     selected = "")
    updateSelectizeInput(    session, "type_filter",    selected = character(0))
    updateSelectizeInput(    session, "subtype_filter", selected = character(0))
    updateCheckboxGroupInput(session, "fuel_filter",    selected = character(0))
    updateRadioButtons(      session, "nsps_filter",    selected = "any")
    updateSliderInput(session, "eff_filter",
                      value = c(floor(eff_range[1]), ceiling(eff_range[2])))
    updateSliderInput(session, "co_filter",
                      value = c(0, ceiling(co_range[2] * 10) / 10))
    updateSliderInput(session, "btu_filter",
                      value = c(0, ceiling(btu_range[2] / 1000) * 1000))
  })
}

shinyApp(ui, server)
