# Only presentation strings go through this catalogue. Workbook data, input
# values, tab IDs and model keys keep their original spelling in both languages.
em_catalog_path <- file.path(if (exists("app_dir", inherits = FALSE)) app_dir else ".",
  "www", "i18n", "zh-CN.json")
em_catalog_json <- paste(readLines(em_catalog_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
em_catalog <- unlist(jsonlite::fromJSON(em_catalog_json), use.names = TRUE)

em_language <- function() {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain) || is.null(domain$userData$em_language)) return("en")
  if (identical(domain$userData$em_language(), "zh-CN")) "zh-CN" else "en"
}

em_tr <- function(text, language = em_language()) {
  if (language == "en" || !length(text)) return(text)
  keys <- trimws(gsub("[[:space:]]+", " ", text))
  translated <- unname(em_catalog[keys])
  found <- !is.na(translated)
  text[found] <- translated[found]
  text
}

# Both labels are present so switching languages does not rebind Shiny inputs.
em_label <- function(text) {
  zh <- em_tr(text, "zh-CN")
  if (identical(text, zh)) return(text)
  shiny::span(class = "em-label",
    shiny::span(lang = "en", text), shiny::span(lang = "zh-CN", zh))
}

# Walk application-authored UI after Shiny has assigned each tab its data-value.
# Raw HTML (including option markup), dependencies, code and citations are opaque.
em_ui <- function(x) {
  if (inherits(x, "shiny.tag")) {
    if (x$name %in% c("head", "script", "style", "pre", "code", "option", "iframe") ||
        identical(x$attribs$class, "em-label") ||
        identical(x$attribs$id, "em-language-control")) return(x)
    if (x$name == "select") {
      x$attribs[["data-em-choices"]] <- "true"
      return(x)
    }
    for (attr in c("placeholder", "title", "aria-label")) {
      label <- x$attribs[[attr]]
      if (is.character(label) && length(label) == 1L && !identical(label, em_tr(label, "zh-CN"))) {
        x$attribs[[paste0("data-em-", attr)]] <- label
      }
    }
    x$children <- lapply(x$children, em_ui)
  } else if (is.character(x) && length(x) == 1L) {
    # Shiny also represents plain checkbox captions as HTML-escaped text.
    if (!inherits(x, "html") || !grepl("[<&]", x)) return(em_label(as.character(x)))
  } else if (is.list(x) && !inherits(x, "html_dependency")) {
    x[] <- lapply(x, em_ui)
  }
  x
}

em_language_control <- function() {
  shiny::tags$div(id = "em-language-control",
    shiny::tags$label("Language / \u8bed\u8a00", `for` = "ui_language"),
    shiny::tags$select(id = "ui_language", class = "form-control",
      shiny::tags$option(value = "en", lang = "en", "English"),
      shiny::tags$option(value = "zh-CN", lang = "zh-CN", "\u7b80\u4f53\u4e2d\u6587")))
}

em_language_head <- function() {
  shiny::tagList(
    shiny::tags$link(rel = "stylesheet", href = "i18n/language.css"),
    shiny::tags$script(type = "application/json", id = "em-catalog",
      htmltools::HTML(em_catalog_json)),
    shiny::tags$script(src = "i18n/language.js"))
}

# Display built-in filter choices in either language without rebinding the input.
# Dynamic event, country, cause and worksheet names come directly from the data.
em_known_strata <- c("Total", "Male", "Female", "Both sexes", "All ages")

em_select_filter <- function(inputId, label, choices, ..., translate = em_known_strata) {
  htmltools::tagAppendAttributes(
    shiny::selectInput(inputId, em_label(label), choices, ...),
    `data-em-select` = paste(translate, collapse = "|"))
}

em_stratum <- function(value) {
  ifelse(value %in% em_known_strata, em_tr(value), value)
}

em_check_filter <- function(inputId, label, choices, ...) {
  choice_label <- function(value) {
    if (endsWith(inputId, "-selected_models") || value %in% em_known_strata) em_label(value) else value
  }
  shiny::checkboxGroupInput(inputId, if (!is.null(label)) em_label(label),
    choiceNames = lapply(choices, choice_label), choiceValues = choices, ...)
}

em_pdf <- function(filename, width, height, ...) {
  if (em_language() == "en") return(grDevices::pdf(filename, width, height, ...))
  if (capabilities("cairo")) return(grDevices::cairo_pdf(filename, width, height, ...))
  grDevices::pdf(filename, width, height, family = "GB1", ...)
}

em_context <- function(rv) {
  source <- rv$display_source
  if (em_language() == "en" || is.null(source)) return(rv$data_description)
  prefix <- if (source$type == "example") {
    sprintf(em_tr("Built-in example: %s"), em_tr(source$name))
  } else {
    sprintf(em_tr("Uploaded file: %s"), source$name)
  }
  paste(c(prefix, rv$selected_country, rv$selected_cause), collapse = " | ")
}

em_validation_text <- function(text) {
  if (em_language() == "en") return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  bullet <- "\u2022 "
  prefix <- "Mortality data is missing required column(s): "
  translate_line <- function(line) {
    if (startsWith(line, prefix)) {
      paste0(em_tr("Mortality data is missing required column(s):"), substring(line, nchar(prefix) + 1L))
    } else em_tr(line)
  }
  content <- ifelse(startsWith(lines, bullet), substring(lines, 3L), lines)
  paste0(ifelse(startsWith(lines, bullet), bullet, ""),
    vapply(content, translate_line, character(1)), collapse = "\n")
}

# DataTables accepts display headers separately from data/column identifiers.
em_display_data <- function(data) {
  for (key in intersect(c("SEX", "AGE_GROUP"), names(data))) {
    data[[key]] <- em_stratum(as.character(data[[key]]))
  }
  if ("WM_IDENTIFIER" %in% names(data)) {
    value <- as.character(data$WM_IDENTIFIER)
    known <- value %in% c("Week", "Month")
    value[known] <- em_tr(value[known])
    data$WM_IDENTIFIER <- value
  }
  if ("Model" %in% names(data) && any(c("EXCESS_DEATHS", "TOTAL_EXCESS") %in% names(data))) {
    data$Model <- em_tr(as.character(data$Model))
  }
  data
}

em_datatable <- function(data, ..., options = list(), colnames = NULL) {
  if (is.null(colnames)) colnames <- names(data)
  if (em_language() == "zh-CN") {
    data <- em_display_data(data)
    options$language <- list(
      search = em_tr("Search:"), lengthMenu = em_tr("Show _MENU_ entries"),
      info = em_tr("Showing _START_ to _END_ of _TOTAL_ entries"),
      infoEmpty = em_tr("No entries"), infoFiltered = em_tr("(filtered from _MAX_ entries)"),
      zeroRecords = em_tr("No matching records"), emptyTable = em_tr("No data available."),
      processing = em_tr("Loading..."), paginate = list(
        first = em_tr("First"), last = em_tr("Last"), `next` = em_tr("Next"), previous = em_tr("Previous")))
    colnames <- em_tr(colnames)
    if (length(options$buttons)) options$buttons <- lapply(options$buttons, function(button) {
      if (is.list(button) && !is.null(button$text)) button$text <- em_tr(button$text)
      button
    })
  }
  DT::datatable(data, ..., colnames = colnames, options = options)
}

em_reactable <- function(data, ..., columns = list()) {
  if (em_language() == "en") return(reactable::reactable(data, ..., columns = columns))
  data <- em_display_data(data)
  for (key in names(data)) {
    def <- columns[[key]]
    if (is.null(def)) def <- reactable::colDef()
    def$name <- em_tr(if (is.null(def$name)) key else def$name)
    columns[[key]] <- def
  }
  language <- reactable::reactableLang(
    sortLabel = em_tr("Sort {name}"), filterPlaceholder = em_tr("Filter"),
    filterLabel = em_tr("Filter {name}"), searchPlaceholder = em_tr("Search"),
    searchLabel = em_tr("Search"), noData = em_tr("No matching records"),
    pageNext = em_tr("Next"), pagePrevious = em_tr("Previous"),
    pageInfo = em_tr("{rowStart}–{rowEnd} of {rows} rows"),
    pageSizeOptions = em_tr("Show {rows}"), pageNextLabel = em_tr("Next"),
    pagePreviousLabel = em_tr("Previous"), pageNumberLabel = em_tr("Page {page}"),
    pageJumpLabel = em_tr("Go to page"), pageSizeOptionsLabel = em_tr("Rows per page"))
  reactable::reactable(data, ..., columns = columns, language = language)
}
