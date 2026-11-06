language_env <- app_module_env("interface_language.R")

testthat::test_that("translated tabs and choices retain their Shiny values", {
  page <- shiny::navbarPage("Test",
    shiny::tabPanel("Model", value = "tab3",
      shiny::selectInput("example", "Open a data set", c("Built-in example data" = "2"))))
  html <- as.character(htmltools::renderTags(language_env$em_ui(page))$html)
  testthat::expect_match(html, 'data-value="tab3"', fixed = TRUE)
  testthat::expect_match(html, 'lang="zh-CN">\u6a21\u578b</span>', fixed = TRUE)
  testthat::expect_match(html, '<option value="2"', fixed = TRUE)
  testthat::expect_match(html, 'data-em-choices="true"', fixed = TRUE)
  switcher <- as.character(language_env$em_language_control())
  testthat::expect_match(switcher, 'value="zh-CN"', fixed = TRUE)
  testthat::expect_match(switcher, 'value="en"', fixed = TRUE)
})

testthat::test_that("language is per session and leaves user data alone", {
  en <- shiny::MockShinySession$new()
  zh <- shiny::MockShinySession$new()
  on.exit({ en$close(); zh$close() }, add = TRUE)
  bind_language <- function(session) {
    session$userData$em_language <- shiny::reactive(session$input$ui_language)
  }
  bind_language(en)
  bind_language(zh)
  en$setInputs(ui_language = "en")
  zh$setInputs(ui_language = "zh-CN")

  words <- function(session) shiny::withReactiveDomain(session, shiny::isolate({
    c(language_env$em_tr("Historical Average"), language_env$em_stratum("Model"))
  }))
  testthat::expect_identical(words(zh), c("\u5386\u53f2\u5e73\u5747\u6cd5", "Model"))
  testthat::expect_identical(words(en), c("Historical Average", "Model"))
  zh$setInputs(ui_language = "en")
  testthat::expect_identical(words(zh), words(en))
})
