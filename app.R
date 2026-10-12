library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(DT)

dictionary_path <- file.path("data", "SCAMP_data_dictionary.xlsx")

read_sheet <- function(path, sheet, add_id = FALSE) {
  x <- read_excel(path, sheet = sheet)
  if ("Instance" %in% names(x)) x$Instance <- as.character(x$Instance)
  x <- x |> mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
  if (add_id) {
    for (field in c("Column name", "Instance", "Data type", "Description", "Notes",
                    "Cohort", "Top Level", "Level 1 topic", "Level 2 topic")) {
      if (!field %in% names(x)) x[[field]] <- rep("", nrow(x))
    }
    x$.__row_id <- seq_len(nrow(x))
  }
  x
}

instance_label <- function(instance, readme) {
  hit <- readme[readme$Instance == instance, , drop = FALSE]
  if (!nrow(hit)) return(paste("Instance", instance))
  paste0("Instance ", instance, " — ", hit$Timepoint[1],
         if (nzchar(hit$Time[1])) paste0(" (", hit$Time[1], ")") else "")
}

detail_line <- function(label, value) {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return(NULL)
  tags$div(style = "margin-bottom:10px;", tags$strong(paste0(label, ": ")), tags$span(value))
}

coding_box <- function(value) {
  if (!nzchar(trimws(value))) return(div(class = "alert alert-light", em("No coding information recorded.")))
  div(class = "alert alert-secondary", tags$strong("Variable coding"), tags$br(), tags$br(),
      tags$div(style = "white-space:pre-wrap;", value))
}

related_instances <- function(data, description, meta) {
  related <- data |>
    filter(nzchar(Description), Description == description) |>
    arrange(suppressWarnings(as.numeric(Instance)))
  if (n_distinct(related$Instance) <= 1) return(NULL)
  rows <- lapply(seq_len(nrow(related)), function(i) {
    tags$tr(tags$td(instance_label(related$Instance[i], meta)),
            tags$td(tags$code(related$`Column name`[i])), tags$td(related$`Data type`[i]))
  })
  tagList(hr(), h5("Same question/description in other instances"),
          tags$table(class = "table table-sm table-striped",
                     tags$thead(tags$tr(tags$th("Collection"), tags$th("Column name"), tags$th("Data type"))),
                     tags$tbody(rows)))
}

update_filter_choices <- function(session, prefix, x, meta) {
  instances <- sort(unique(x$Instance[nzchar(x$Instance)]))
  choices <- c("All" = "All", setNames(instances,
                                       vapply(instances, instance_label, character(1), readme = meta)))
  updateSelectInput(session, paste0(prefix, "instance"), choices = choices)
  fields <- c(cohort = "Cohort", top_level = "Top Level", level1 = "Level 1 topic",
              level2 = "Level 2 topic", datatype = "Data type")
  for (id in names(fields)) {
    values <- sort(unique(x[[fields[[id]]]][nzchar(x[[fields[[id]]]])]))
    updateSelectInput(session, paste0(prefix, id), choices = c("All", values))
  }
}

reset_filters <- function(session, prefix = "") {
  updateTextInput(session, paste0(prefix, "search"), value = "")
  for (id in c("instance", "cohort", "top_level", "level1", "level2", "datatype"))
    updateSelectInput(session, paste0(prefix, id), selected = "All")
}

filter_dataset <- function(x, instance, cohort, top_level, level1, level2, datatype,
                           search, search_columns) {
  inputs <- list(Instance = instance, Cohort = cohort, `Top Level` = top_level,
                 `Level 1 topic` = level1, `Level 2 topic` = level2, `Data type` = datatype)
  for (nm in names(inputs)) {
    val <- inputs[[nm]]
    if (!is.null(val) && val != "All") x <- x[x[[nm]] == val, , drop = FALSE]
  }
  q <- trimws(if (is.null(search)) "" else search)
  if (nzchar(q) && nrow(x)) {
    searchable <- apply(x[, search_columns, drop = FALSE], 1, paste, collapse = " | ")
    x <- x[grepl(q, searchable, fixed = TRUE, ignore.case = TRUE), , drop = FALSE]
  }
  x
}

# Each checkbox carries a stable key for its source sheet and original Excel row.
# JavaScript sends each tick/untick to Shiny; redraws restore checked states.
selection_table <- function(x, source, table_id, columns) {
  keys <- paste0(source, ":", x$.__row_id)
  checkboxes <- sprintf(
    '<input type="checkbox" class="scamp-check" data-key="%s" aria-label="Select variable">', keys
  )
  shown <- data.frame(Select = checkboxes, x[, columns, drop = FALSE], check.names = FALSE)
  js <- JS(sprintf("\n    var tableId = '%s';\n    window.scampChecked = window.scampChecked || {};\n    window.scampChecked[tableId] = window.scampChecked[tableId] || {};\n    function refreshChecks() {\n      table.$('input.scamp-check').each(function() {\n        this.checked = !!window.scampChecked[tableId][this.dataset.key];\n      });\n    }\n    table.on('draw.dt', refreshChecks);\n    table.on('change', 'input.scamp-check', function(e) {\n      e.stopPropagation();\n      window.scampChecked[tableId][this.dataset.key] = this.checked;\n      Shiny.setInputValue(tableId + '_toggle',\n        {key: this.dataset.key, checked: this.checked, nonce: Math.random()},\n        {priority: 'event'});\n    });\n    table.on('click', 'input.scamp-check', function(e) { e.stopPropagation(); });\n    Shiny.addCustomMessageHandler('scamp_sync_%s', function(keys) {\n      window.scampChecked[tableId] = {};\n      keys.forEach(function(k) { window.scampChecked[tableId][k] = true; });\n      refreshChecks();\n    });\n    refreshChecks();\n  ", table_id, table_id))
  datatable(shown, rownames = FALSE, selection = "single", escape = seq.int(2, ncol(shown)),
            callback = js,
            options = list(pageLength = 20, lengthMenu = c(10, 20, 50, 100),
                           scrollX = TRUE, autoWidth = TRUE,
                           columnDefs = list(list(orderable = FALSE, searchable = FALSE,
                                                  width = "65px", targets = 0))))
}

# Each tab has its own controls, but all use the same selection basket.
filter_sidebar <- function(prefix = "", placeholder = "Variable name, description, topic...") {
  sidebar(width = 330,
          textInput(paste0(prefix, "search"), "Search", placeholder = placeholder),
          selectInput(paste0(prefix, "instance"), "Instance", choices = "All"),
          selectInput(paste0(prefix, "cohort"), "Cohort", choices = "All"),
          selectInput(paste0(prefix, "top_level"), "Top level", choices = "All"),
          selectInput(paste0(prefix, "level1"), "Level 1 topic", choices = "All"),
          selectInput(paste0(prefix, "level2"), "Level 2 topic", choices = "All"),
          selectInput(paste0(prefix, "datatype"), "Data type", choices = "All"),
          br(),
          textOutput(paste0(prefix, "selected_count")),
          downloadButton(paste0(prefix, "download_selected"),
                         "Download selected variables", width = "100%"),
          br(), br(),
          actionButton(paste0(prefix, "clear_selected"),
                       "Clear selections", width = "100%"),
          br(), br(),
          actionButton(paste0(prefix, "reset_filters"),
                       "Reset filters", width = "100%"))
}

variable_tab <- function(title, prefix, table_id, count_id, detail_id, detail_title) {
  nav_panel(title,
            layout_sidebar(sidebar = filter_sidebar(prefix),
                           card(full_screen = TRUE,
                                card_header(tags$strong(title), textOutput(count_id, inline = TRUE)),
                                DTOutput(table_id))),
            br(), card(card_header(tags$strong(detail_title)), uiOutput(detail_id)))
}

ui <- page_navbar(
  title = "SCAMP Data Dictionary", theme = bs_theme(version = 5), fillable = FALSE,
  variable_tab("Survey", "", "dictionary_table", "result_count", "variable_detail", "Selected variable"),
  variable_tab("Biomarker and measurements", "bio_", "biomarker_table", "bio_result_count",
               "biomarker_detail", "Selected biomarker or measurement"),
  variable_tab("Environmental exposures", "env_", "env_table", "env_result_count",
               "env_detail", "Selected environmental exposure"),
  nav_panel("Collections", card(card_header("SCAMP data collections"), DTOutput("collections_table"))),
  nav_panel("References", card(card_header("Questionnaires and scales"), DTOutput("references_table")))
)

server <- function(input, output, session) {
  survey_data <- reactiveFileReader(5000, session, dictionary_path,
                                    function(p) read_sheet(p, "Survey", TRUE))
  biomarker_data <- reactiveFileReader(5000, session, dictionary_path,
                                       function(p) read_sheet(p, "Biomarker and measurements", TRUE))
  env_data <- reactiveFileReader(5000, session, dictionary_path,
                                 function(p) read_sheet(p, "Environmental exposure", TRUE))
  readme_data <- reactiveFileReader(5000, session, dictionary_path,
                                    function(p) read_sheet(p, "README"))
  reference_data <- reactiveFileReader(5000, session, dictionary_path,
                                       function(p) read_sheet(p, "References"))
  
  # One persistent selection basket for all three datasets.
  selected_keys <- reactiveVal(character())
  for (prefix in c("", "bio_", "env_")) local({
    p <- prefix
    output[[paste0(p, "selected_count")]] <- renderText({
      paste(length(selected_keys()), "variables selected across all tabs")
    })
    observeEvent(input[[paste0(p, "clear_selected")]], {
      selected_keys(character())
    })
  })
  
  sources <- list(
    list(source = "Survey", prefix = "", table = "dictionary_table", count = "result_count",
         detail = "variable_detail", data = survey_data,
         columns = c("Column name", "Instance", "Data type", "Description", "Coding",
                     "Cohort", "Level 1 topic", "Level 2 topic"),
         search = c("Column name", "Description", "Coding", "Notes", "Skip logic",
                    "Cohort", "Top Level", "Level 1 topic", "Level 2 topic")),
    list(source = "Biomarker", prefix = "bio_", table = "biomarker_table", count = "bio_result_count",
         detail = "biomarker_detail", data = biomarker_data,
         columns = c("Column name", "Instance", "Data type", "Description", "Notes",
                     "Cohort", "Level 1 topic", "Level 2 topic"),
         search = c("Column name", "Description", "Notes", "Cohort", "Top Level",
                    "Level 1 topic", "Level 2 topic")),
    list(source = "Environmental exposure", prefix = "env_", table = "env_table", count = "env_result_count",
         detail = "env_detail", data = env_data,
         columns = c("Column name", "Instance", "Data type", "Description", "Notes",
                     "Cohort", "Level 1 topic", "Level 2 topic"),
         search = c("Column name", "Description", "Notes", "Cohort", "Top Level",
                    "Level 1 topic", "Level 2 topic"))
  )
  
  # local() captures a distinct configuration for each iteration.
  for (config in sources) local({
    cfg <- config
    p <- cfg$prefix
    id <- cfg$table
    
    observe({ update_filter_choices(session, p, cfg$data(), readme_data()) })
    observeEvent(input[[paste0(p, "reset_filters")]], { reset_filters(session, p) })
    
    filtered <- reactive({
      filter_dataset(cfg$data(), input[[paste0(p, "instance")]],
                     input[[paste0(p, "cohort")]], input[[paste0(p, "top_level")]],
                     input[[paste0(p, "level1")]], input[[paste0(p, "level2")]],
                     input[[paste0(p, "datatype")]], input[[paste0(p, "search")]], cfg$search)
    })
    
    output[[cfg$count]] <- renderText(paste(format(nrow(filtered()), big.mark = ","), "matching rows"))
    output[[id]] <- renderDT(selection_table(filtered(), cfg$source, id, cfg$columns), server = TRUE)
    
    observeEvent(input[[paste0(id, "_toggle")]], {
      msg <- input[[paste0(id, "_toggle")]]
      keys <- selected_keys()
      if (isTRUE(msg$checked)) keys <- union(keys, msg$key)
      else keys <- setdiff(keys, msg$key)
      selected_keys(keys)
    })
    
    observeEvent(selected_keys(), {
      session$sendCustomMessage(paste0("scamp_sync_", id), selected_keys())
    })
    
    output[[cfg$detail]] <- renderUI({
      chosen <- input[[paste0(id, "_rows_selected")]]
      current <- filtered()
      if (length(chosen) != 1 || chosen > nrow(current)) {
        return(div(class = "text-muted", "Click a row to inspect its metadata; tick the checkbox to add it to your shortlist."))
      }
      row <- current[chosen, , drop = FALSE]
      meta <- readme_data()
      extra <- if (cfg$source == "Survey") {
        tagList(coding_box(row$Coding[[1]]),
                detail_line("Skip logic", row$`Skip logic`[[1]]),
                detail_line("Master data table", row$`Master data table name`[[1]]),
                detail_line("Spatial", row$Spatial[[1]]))
      } else NULL
      tagList(h4(tags$code(row$`Column name`[[1]])), h5(row$Description[[1]]), hr(),
              detail_line("Instance", instance_label(row$Instance[[1]], meta)),
              detail_line("Cohort", row$Cohort[[1]]),
              detail_line("Top level", row$`Top Level`[[1]]),
              detail_line("Level 1 topic", row$`Level 1 topic`[[1]]),
              detail_line("Level 2 topic", row$`Level 2 topic`[[1]]),
              detail_line("Data type", row$`Data type`[[1]]),
              extra, detail_line("Notes", row$Notes[[1]]),
              related_instances(cfg$data(), row$Description[[1]], meta))
    })
  })
  
  output$collections_table <- renderDT(datatable(readme_data(), rownames = FALSE,
                                                 options = list(paging = FALSE, searching = FALSE, scrollX = TRUE)))
  output$references_table <- renderDT(datatable(reference_data(), rownames = FALSE,
                                                options = list(pageLength = 20, scrollX = TRUE)))
  
  for (prefix in c("", "bio_", "env_")) local({
    p <- prefix
    output[[paste0(p, "download_selected")]] <- downloadHandler(
      filename = function() paste0("SCAMP_selected_variables_", Sys.Date(), ".csv"),
      content = function(file) {
        keys <- selected_keys()
        if (!length(keys)) {
          write.csv(data.frame(Message = "No variables selected"),
                    file, row.names = FALSE)
          return()
        }
        parts <- lapply(sources, function(cfg) {
          x <- cfg$data()
          matched <- paste0(cfg$source, ":", x$.__row_id) %in% keys
          result <- x[matched, setdiff(names(x), ".__row_id"), drop = FALSE]
          if (!nrow(result)) return(NULL)
          data.frame(Source = cfg$source, result, check.names = FALSE)
        })
        write.csv(dplyr::bind_rows(parts), file, row.names = FALSE, na = "")
      }
    )
  })
}

shinyApp(ui, server)
