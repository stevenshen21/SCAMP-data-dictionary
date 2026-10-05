library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(DT)

# -------------------------------------------------------------------------
# SCAMP Data Dictionary - prototype v0.1
#
# The Excel workbook remains the source of truth.
#
# By default the app reads:
#   data/SCAMP_data_dictionary.xlsx
#
# You can point the app to a different copy of the workbook by setting:
#   Sys.setenv(SCAMP_DICT_PATH = "X:/path/to/SCAMP_data_dictionary.xlsx")
#
# before running the app. This is useful if the maintained dictionary is
# stored on a shared/network drive.
# -------------------------------------------------------------------------

dictionary_path <- file.path("data", "SCAMP_data_dictionary.xlsx")

read_dictionary <- function(path) {
  read_excel(path, sheet = "Survey") |>
    mutate(
      Instance = as.character(Instance),
      across(everything(), ~ ifelse(is.na(.x), "", as.character(.x)))
    )
}

read_readme <- function(path) {
  read_excel(path, sheet = "README") |>
    mutate(
      Instance = as.character(Instance),
      across(everything(), ~ ifelse(is.na(.x), "", as.character(.x)))
    )
}

read_references <- function(path) {
  read_excel(path, sheet = "References") |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
}

instance_label <- function(instance, readme) {
  hit <- readme |> filter(Instance == instance)
  if (nrow(hit) == 0) return(paste("Instance", instance))
  paste0(
    "Instance ", instance, " — ",
    hit$Timepoint[1],
    if (nzchar(hit$Time[1])) paste0(" (", hit$Time[1], ")") else ""
  )
}

empty_to_na <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", NA_character_, x)
}

# Modern Bootstrap UI. page_navbar() gives a multi-page app structure.
ui <- page_navbar(
  title = "SCAMP Data Dictionary",
  theme = bs_theme(version = 5),
  fillable = FALSE,

  nav_panel(
    "Variable Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,

        textInput(
          "search",
          "Search",
          placeholder = "Variable name, question, coding, topic..."
        ),

        selectInput(
          "instance",
          "Instance",
          choices = "All",
          selected = "All"
        ),

        selectInput(
          "cohort",
          "Cohort",
          choices = "All",
          selected = "All"
        ),

        selectInput(
          "top_level",
          "Top level",
          choices = "All",
          selected = "All"
        ),

        selectInput(
          "level1",
          "Level 1 topic",
          choices = "All",
          selected = "All"
        ),

        selectInput(
          "level2",
          "Level 2 topic",
          choices = "All",
          selected = "All"
        ),

        selectInput(
          "datatype",
          "Data type",
          choices = "All",
          selected = "All"
        ),

        actionButton("reset_filters", "Reset filters", width = "100%"),
        br(), br(),
        downloadButton("download_results", "Download filtered CSV", width = "100%")
      ),

      card(
        full_screen = TRUE,
        card_header(
          div(
            style = "display:flex;justify-content:space-between;align-items:center;",
            tags$strong("Variables"),
            textOutput("result_count", inline = TRUE)
          )
        ),
        DTOutput("dictionary_table")
      )
    ),

    br(),

    card(
      card_header(tags$strong("Selected variable")),
      uiOutput("variable_detail")
    )
  ),

  nav_panel(
    "Collections",
    card(
      card_header(tags$strong("SCAMP data collections")),
      DTOutput("collections_table")
    )
  ),

  nav_panel(
    "References",
    card(
      card_header(tags$strong("Questionnaires and scales")),
      DTOutput("references_table")
    )
  ),

  nav_panel(
    "About",
    card(
      card_body(
        h3("About this prototype"),
        p(
          "This app reads the SCAMP Excel data dictionary directly. ",
          "The Excel workbook remains the maintained source of truth."
        ),
        p(
          "When the workbook file is replaced or updated at the configured path, ",
          "the app periodically checks for changes and reloads it."
        ),
        p(
          tags$strong("Configured dictionary path: "),
          textOutput("dictionary_path_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Poll the workbook every 5 seconds. If its modification time changes,
  # the sheet is re-read and dependent outputs refresh.
  survey_data <- reactiveFileReader(
    intervalMillis = 5000,
    session = session,
    filePath = dictionary_path,
    readFunc = read_dictionary
  )

  readme_data <- reactiveFileReader(
    intervalMillis = 5000,
    session = session,
    filePath = dictionary_path,
    readFunc = read_readme
  )

  reference_data <- reactiveFileReader(
    intervalMillis = 5000,
    session = session,
    filePath = dictionary_path,
    readFunc = read_references
  )

  # Refresh filter choices whenever the workbook changes.
  observe({
    x <- survey_data()
    meta <- readme_data()

    instances <- sort(unique(empty_to_na(x$Instance)))
    instances <- instances[!is.na(instances)]

    instance_choices <- c(
      "All" = "All",
      setNames(
        instances,
        vapply(instances, instance_label, character(1), readme = meta)
      )
    )

    updateSelectInput(session, "instance", choices = instance_choices)
    updateSelectInput(
      session, "cohort",
      choices = c("All", sort(unique(na.omit(empty_to_na(x$Cohort)))))
    )
    updateSelectInput(
      session, "top_level",
      choices = c("All", sort(unique(na.omit(empty_to_na(x$`Top Level`)))))
    )
    updateSelectInput(
      session, "level1",
      choices = c("All", sort(unique(na.omit(empty_to_na(x$`Level 1 topic`)))))
    )
    updateSelectInput(
      session, "level2",
      choices = c("All", sort(unique(na.omit(empty_to_na(x$`Level 2 topic`)))))
    )
    updateSelectInput(
      session, "datatype",
      choices = c("All", sort(unique(na.omit(empty_to_na(x$`Data type`)))))
    )
  })

  observeEvent(input$reset_filters, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "instance", selected = "All")
    updateSelectInput(session, "cohort", selected = "All")
    updateSelectInput(session, "top_level", selected = "All")
    updateSelectInput(session, "level1", selected = "All")
    updateSelectInput(session, "level2", selected = "All")
    updateSelectInput(session, "datatype", selected = "All")
  })

  filtered_data <- reactive({
    x <- survey_data()

    if (!is.null(input$instance) && input$instance != "All") {
      x <- x |> filter(Instance == input$instance)
    }
    if (!is.null(input$cohort) && input$cohort != "All") {
      x <- x |> filter(Cohort == input$cohort)
    }
    if (!is.null(input$top_level) && input$top_level != "All") {
      x <- x |> filter(`Top Level` == input$top_level)
    }
    if (!is.null(input$level1) && input$level1 != "All") {
      x <- x |> filter(`Level 1 topic` == input$level1)
    }
    if (!is.null(input$level2) && input$level2 != "All") {
      x <- x |> filter(`Level 2 topic` == input$level2)
    }
    if (!is.null(input$datatype) && input$datatype != "All") {
      x <- x |> filter(`Data type` == input$datatype)
    }

    q <- trimws(input$search %||% "")
    if (nzchar(q)) {
      searchable <- apply(
        x[, c(
          "Column name", "Description", "Coding", "Notes", "Skip logic",
          "Cohort", "Top Level", "Level 1 topic", "Level 2 topic"
        ), drop = FALSE],
        1,
        paste,
        collapse = " | "
      )

      x <- x[grepl(q, searchable, ignore.case = TRUE, fixed = TRUE), , drop = FALSE]
    }

    x
  })

  output$result_count <- renderText({
    paste(format(nrow(filtered_data()), big.mark = ","), "matching rows")
  })

  output$dictionary_table <- renderDT({
    x <- filtered_data() |>
      select(
        `Column name`,
        Instance,
        `Data type`,
        Description,
        Coding,
        Cohort,
        `Level 1 topic`,
        `Level 2 topic`
      )

    datatable(
      x,
      rownames = FALSE,
      selection = "single",
      filter = "none",
      escape = TRUE,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = "120px", targets = 0),
          list(width = "90px", targets = 1),
          list(width = "100px", targets = 2),
          list(width = "300px", targets = 3),
          list(width = "300px", targets = 4)
        )
      )
    )
  }, server = TRUE)
  
  output$variable_detail <- renderUI({
    selected <- input$dictionary_table_rows_selected

    if (length(selected) != 1) {
      return(
        div(
          class = "text-muted",
          "Click one row in the table above to inspect the full metadata and cross-wave matches."
        )
      )
    }

    current <- filtered_data()
    if (selected > nrow(current)) return(NULL)

    row <- current[selected, , drop = FALSE]
    all_survey <- survey_data()
    meta <- readme_data()

    # First-pass cross-wave matching: exact question/description text.
    # Later we can replace this with a curated Concept ID.
    related <- all_survey |>
      filter(
        nzchar(Description),
        Description == row$Description[[1]]
      ) |>
      arrange(as.numeric(Instance))

    detail_line <- function(label, value) {
      if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return(NULL)
      tags$div(
        style = "margin-bottom:10px;",
        tags$strong(paste0(label, ": ")),
        tags$span(value)
      )
    }

    related_ui <- NULL
    if (nrow(related) > 1) {
      related_rows <- lapply(seq_len(nrow(related)), function(i) {
        inst <- related$Instance[i]
        tags$tr(
          tags$td(instance_label(inst, meta)),
          tags$td(tags$code(related$`Column name`[i])),
          tags$td(related$`Data type`[i])
        )
      })

      related_ui <- tagList(
        hr(),
        h5("Same question/description in other instances"),
        tags$table(
          class = "table table-sm table-striped",
          tags$thead(
            tags$tr(
              tags$th("Collection"),
              tags$th("Column name"),
              tags$th("Data type")
            )
          ),
          tags$tbody(related_rows)
        )
      )
    }

    coding_box <- function(value) {
      
      if (is.null(value) ||
          is.na(value) ||
          !nzchar(trimws(value))) {
        
        return(
          div(
            class = "alert alert-light",
            em("No coding information recorded.")
          )
        )
      }
      
      div(
        class = "alert alert-secondary",
        tags$strong("Variable coding"),
        tags$br(),
        tags$br(),
        tags$div(
          style = "white-space: pre-wrap;",
          value
        )
      )
    }
    
    tagList(
      h4(tags$code(row$`Column name`[[1]])),
      h5(row$Description[[1]]),
      hr(),
      detail_line("Instance", instance_label(row$Instance[[1]], meta)),
      detail_line("Cohort", row$Cohort[[1]]),
      detail_line("Top level", row$`Top Level`[[1]]),
      detail_line("Level 1 topic", row$`Level 1 topic`[[1]]),
      detail_line("Level 2 topic", row$`Level 2 topic`[[1]]),
      detail_line("Data type", row$`Data type`[[1]]),
      coding_box(row$Coding[[1]]),
      detail_line("Skip logic", row$`Skip logic`[[1]]),
      detail_line("Notes", row$Notes[[1]]),
      detail_line("Master data table", row$`Master data table name`[[1]]),
      detail_line("Spatial", row$Spatial[[1]]),
      related_ui
    )
  })

  output$collections_table <- renderDT({
    datatable(
      readme_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        scrollX = TRUE
      )
    )
  })

  output$references_table <- renderDT({
    datatable(
      reference_data(),
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("SCAMP_dictionary_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE, na = "")
    }
  )

  output$dictionary_path_text <- renderText({
    normalizePath(dictionary_path, winslash = "/", mustWork = FALSE)
  })
}

shinyApp(ui, server)
