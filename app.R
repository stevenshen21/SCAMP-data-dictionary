library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(DT)

dictionary_path <- file.path("data", "SCAMP_data_dictionary.xlsx")

read_sheet <- function(path, sheet, has_instance = TRUE) {
  x <- read_excel(path, sheet = sheet)
  if (has_instance) {x$Instance <- as.character(x$Instance)
  }
  x |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
}

read_dictionary <- function(path) {
  read_sheet(path, "Survey")
}

read_biomarkers <- function(path) {
  read_sheet(path, "Biomarker and measurements")
}

read_readme <- function(path) {
  read_sheet(path, "README")
}

read_references <- function(path) {
  read_sheet(path, "References", has_instance = FALSE)
}

##Helper function##
instance_label <- function(instance, readme) {hit <- readme |> filter(Instance == instance)
  if (nrow(hit) == 0) return(paste("Instance", instance))
  paste0("Instance ", instance, " — ", hit$Timepoint[1], 
         if (nzchar(hit$Time[1])) paste0(" (", hit$Time[1], ")") else "")
}

empty_to_na <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", NA_character_, x)
}

detail_line <- function(label, value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return(NULL)
  }

  tags$div(style = "margin-bottom:10px;", tags$strong(paste0(label, ": ")), tags$span(value))
}

coding_box <- function(value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return(div(class = "alert alert-light", em("No coding information recorded.")))
  }
  
  div(class = "alert alert-secondary", tags$strong("Variable coding"), tags$br(), tags$br(),
      tags$div(style = "white-space: pre-wrap;", value))
}


update_filter_choices <- function(session, prefix, x, meta) {
  instances <- sort(unique(empty_to_na(x$Instance)))
  instances <- instances[!is.na(instances)]
  instance_choices <- c("All" = "All", setNames(instances, vapply(instances, instance_label, character(1), readme = meta)))
  updateSelectInput(session, paste0(prefix, "instance"), choices = instance_choices)
  fields <- c(cohort = "Cohort", top_level = "Top Level", level1 = "Level 1 topic", level2 = "Level 2 topic", datatype = "Data type")
  
  for (id in names(fields)) {
    values <- sort(unique(na.omit(empty_to_na(x[[fields[[id]]]]))))
    updateSelectInput(session, paste0(prefix, id), choices = c("All", values))
  }
}

reset_filters <- function(session, prefix = "") {
  updateTextInput(session, paste0(prefix, "search"), value = "")
  
  for (id in c("instance", "cohort", "top_level", "level1", "level2", "datatype")) {
    updateSelectInput(session, paste0(prefix, id), selected = "All")
  }
}

filter_dataset <- function(x, instance, cohort, top_level, level1, level2, datatype, search, search_columns) {
  
  if (!is.null(instance) && instance != "All")
    x <- x |> filter(Instance == instance)
  
  if (!is.null(cohort) && cohort != "All")
    x <- x |> filter(Cohort == cohort)
  
  if (!is.null(top_level) && top_level != "All")
    x <- x |> filter(`Top Level` == top_level)
  
  if (!is.null(level1) && level1 != "All")
    x <- x |> filter(`Level 1 topic` == level1)
  
  if (!is.null(level2) && level2 != "All")
    x <- x |> filter(`Level 2 topic` == level2)
  
  if (!is.null(datatype) && datatype != "All")
    x <- x |> filter(`Data type` == datatype)
  
  q <- trimws(search %||% "")
  
  if (nzchar(q)) {
    searchable <- apply(x[, search_columns, drop = FALSE], 1, paste, collapse = " | ")
    
    x <- x[grepl(q, searchable, ignore.case = TRUE, fixed = TRUE),, drop = FALSE]
  }
  
  x
}



# Reusable sidebar for Survey/Biomarker tabs
dictionary_sidebar <- function(prefix = "", biomarker = FALSE) {
  
  placeholder <- if (biomarker) {
    "Variable name, measurement, biomarker, topic..."
  } else {
    "Variable name, question, coding, topic..."
  }
  
  sidebar(width = 330, textInput(paste0(prefix, "search"), "Search", placeholder = placeholder),
    selectInput(paste0(prefix, "instance"), "Instance", choices = "All", selected = "All"),
    selectInput(paste0(prefix, "cohort"), "Cohort", choices = "All", selected = "All"),
    selectInput(paste0(prefix, "top_level"), "Top level", choices = "All", selected = "All"),
    selectInput(paste0(prefix, "level1"), "Level 1 topic", choices = "All", selected = "All"),
    selectInput(paste0(prefix, "level2"), "Level 2 topic", choices = "All", selected = "All"),
    selectInput(paste0(prefix, "datatype"), "Data type", choices = "All", selected = "All"),
    actionButton(paste0(prefix, "reset_filters"), "Reset filters", width = "100%"),
    br(),br(),
    downloadButton(if (biomarker) "download_biomarkers" else "download_results", "Download filtered CSV", width = "100%"))
}


# =========================================================================
# USER INTERFACE
# =========================================================================

ui <- page_navbar(
  title = "SCAMP Data Dictionary",
  theme = bs_theme(version = 5),
  fillable = FALSE,
  
  # -----------------------------------------------------------------------
  # Survey
  # -----------------------------------------------------------------------
  
  nav_panel("Survey", layout_sidebar( sidebar = dictionary_sidebar(),
      card(full_screen = TRUE, card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
            tags$strong("Variables"), textOutput("result_count", inline = TRUE))),
        DTOutput("dictionary_table"))), br(),
    card(card_header(tags$strong("Selected variable")),uiOutput("variable_detail"))),
  
  # -----------------------------------------------------------------------
  # Biomarkers and measurements
  # -----------------------------------------------------------------------
  
  nav_panel("Biomarker and measurements",
    layout_sidebar(sidebar = dictionary_sidebar(prefix = "bio_", biomarker = TRUE),
      card(full_screen = TRUE,
        card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
            tags$strong("Biomarkers and measurements"),
            textOutput("bio_result_count", inline = TRUE))),
        DTOutput("biomarker_table"))),
    br(),
    card(card_header(tags$strong("Selected biomarker or measurement")), uiOutput("biomarker_detail"))),
  
  # -----------------------------------------------------------------------
  # Collections
  # -----------------------------------------------------------------------
  
  nav_panel("Collections", card(card_header(tags$strong("SCAMP data collections")),
      DTOutput("collections_table")
    )),
  
  # -----------------------------------------------------------------------
  # References
  # -----------------------------------------------------------------------
  
  nav_panel("References", card(card_header(tags$strong("Questionnaires and scales")),
      DTOutput("references_table")
    )))


# =========================================================================
# SERVER
# =========================================================================

server <- function(input, output, session) {
  survey_data <- reactiveFileReader(5000, session, dictionary_path, read_dictionary)
  biomarker_data <- reactiveFileReader(5000, session, dictionary_path, read_biomarkers)
  readme_data <- reactiveFileReader(5000, session, dictionary_path, read_readme)
  reference_data <- reactiveFileReader(5000, session, dictionary_path, read_references)
  
  observe({
    update_filter_choices(session, prefix = "", x = survey_data(), meta = readme_data())
  })
  
  observeEvent(input$reset_filters, {
    reset_filters(session)
  })
  
  
  filtered_data <- reactive({
    filter_dataset(x = survey_data(), instance = input$instance, cohort = input$cohort,
      top_level = input$top_level, level1 = input$level1, level2 = input$level2,
      datatype = input$datatype, search = input$search,
      search_columns = c("Column name","Description", "Coding", "Notes", "Skip logic", "Cohort",
        "Top Level", "Level 1 topic", "Level 2 topic"))
  })
  
  output$result_count <- renderText({
    paste(format(nrow(filtered_data()), big.mark = ","), "matching rows")
  })
  
  output$dictionary_table <- renderDT({
    x <- filtered_data() |>
      select(`Column name`, Instance, `Data type`, Description, Coding, Cohort, `Level 1 topic`, `Level 2 topic`)
    datatable(x, rownames = FALSE,
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
    meta <- readme_data()
    
    related <- survey_data() |>
      filter(
        nzchar(Description),
        Description == row$Description[[1]]
      ) |>
      arrange(as.numeric(Instance))
    
    related_ui <- NULL
    
    if (nrow(related) > 1) {
      
      related_rows <- lapply(seq_len(nrow(related)), function(i) {
        tags$tr(
          tags$td(instance_label(related$Instance[i], meta)),
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
    
    tagList(
      h4(tags$code(row$`Column name`[[1]])),
      h5(row$Description[[1]]),
      hr(),
      
      detail_line(
        "Instance",
        instance_label(row$Instance[[1]], meta)
      ),
      
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
  
  
  # =======================================================================
  # BIOMARKERS AND MEASUREMENTS
  # =======================================================================
  
  observe({
    update_filter_choices(
      session,
      prefix = "bio_",
      x = biomarker_data(),
      meta = readme_data()
    )
  })
  
  
  observeEvent(input$bio_reset_filters, {
    reset_filters(session, prefix = "bio_")
  })
  
  
  filtered_biomarkers <- reactive({
    
    filter_dataset(
      x = biomarker_data(),
      instance = input$bio_instance,
      cohort = input$bio_cohort,
      top_level = input$bio_top_level,
      level1 = input$bio_level1,
      level2 = input$bio_level2,
      datatype = input$bio_datatype,
      search = input$bio_search,
      
      search_columns = c(
        "Column name",
        "Description",
        "Notes",
        "Cohort",
        "Top Level",
        "Level 1 topic",
        "Level 2 topic"
      )
    )
  })
  
  
  output$bio_result_count <- renderText({
    paste(
      format(nrow(filtered_biomarkers()), big.mark = ","),
      "matching rows"
    )
  })
  
  
  output$biomarker_table <- renderDT({
    
    x <- filtered_biomarkers() |>
      select(
        `Column name`,
        Instance,
        `Data type`,
        Description,
        Notes,
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
          list(width = "130px", targets = 0),
          list(width = "90px", targets = 1),
          list(width = "100px", targets = 2),
          list(width = "300px", targets = 3),
          list(width = "220px", targets = 4)
        )
      )
    )
  }, server = TRUE)
  
  
  output$biomarker_detail <- renderUI({
    
    selected <- input$biomarker_table_rows_selected
    
    if (length(selected) != 1) {
      return(
        div(
          class = "text-muted",
          "Click one row in the table above to inspect the full biomarker or measurement metadata."
        )
      )
    }
    
    current <- filtered_biomarkers()
    if (selected > nrow(current)) return(NULL)
    
    row <- current[selected, , drop = FALSE]
    meta <- readme_data()
    
    tagList(
      h4(tags$code(row$`Column name`[[1]])),
      h5(row$Description[[1]]),
      hr(),
      
      detail_line(
        "Instance",
        instance_label(row$Instance[[1]], meta)
      ),
      
      detail_line("Cohort", row$Cohort[[1]]),
      detail_line("Top level", row$`Top Level`[[1]]),
      detail_line("Level 1 topic", row$`Level 1 topic`[[1]]),
      detail_line("Level 2 topic", row$`Level 2 topic`[[1]]),
      detail_line("Data type", row$`Data type`[[1]]),
      detail_line("Notes", row$Notes[[1]])
    )
  })
  
  
  # =======================================================================
  # COLLECTIONS / REFERENCES
  # =======================================================================
  
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
  
  
  # =======================================================================
  # DOWNLOADS
  # =======================================================================
  
  output$download_results <- downloadHandler(
    
    filename = function() {
      paste0(
        "SCAMP_dictionary_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  
  
  output$download_biomarkers <- downloadHandler(
    
    filename = function() {
      paste0(
        "SCAMP_biomarkers_measurements_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_biomarkers(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  
  
}


shinyApp(ui, server)