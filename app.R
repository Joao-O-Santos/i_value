library(shiny)
source("i-value.R")

ui <- fluidPage(
  titlePanel("Statistic region checker"),
  p("Classify an observed statistic and calculate its null smallness probability (i-value)."),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Statistic", choices = c("F" = "F", "t" = "t", "χ²" = "chisq"),
                  selected = "F"),
      numericInput("statistic", "Observed statistic", value = 0.5,
                   min = -Inf, step = 0.01),
      uiOutput("df_inputs"),
      numericInput("alpha_si", "SI threshold (α)", value = 0.05,
                   min = 0.001, max = 0.999, step = 0.01),
      actionButton("check", "Check", class = "btn-primary")
    ),
    mainPanel(
      h3("Result"),
      uiOutput("result")
    )
  )
)

server <- function(input, output, session) {
  output$df_inputs <- renderUI({
    if (identical(input$type, "F")) {
      tagList(
        numericInput("df1", "Numerator degrees of freedom", value = 1,
                     min = 0.001, step = 1),
        numericInput("df2", "Denominator degrees of freedom", value = 98,
                     min = 0.001, step = 1)
      )
    } else {
      numericInput("df1", "Degrees of freedom", value = 98,
                   min = 0.001, step = 1)
    }
  })

  result <- reactiveVal(NULL)
  observeEvent(input$check, {
    result(tryCatch({
      check_statistic(
        statistic = input$statistic,
        type = input$type,
        df1 = input$df1,
        df2 = if (identical(input$type, "F")) input$df2 else NULL,
        alpha_si = input$alpha_si
      )
    }, error = function(e) e))
  })

  # Avoid presenting a stale answer as though it described edited inputs.
  observeEvent(list(input$type, input$statistic, input$df1, input$df2,
                    input$alpha_si), {
    result(NULL)
  }, ignoreInit = TRUE)

  output$result <- renderUI({
    answer <- result()
    if (is.null(answer)) return(p("Enter the test details and click Check."))
    if (inherits(answer, "error")) return(tags$div(class = "text-danger",
                                                    strong("Input error: "),
                                                    answer$message))

    label <- switch(answer$classification,
      report_full = "Report in full (at or above the reference)",
      below_reference = "Below reference; use the compact reporting rule",
      significantly_insignificant = "Significantly insignificant (SI region)"
    )
    tagList(
      p(strong("Classification: "), label),
      p(strong("Statistic: "), format(answer$statistic, digits = 6)),
      p(strong("Reference: "), format(answer$reference, digits = 6)),
      p(strong("i-value: "), format.pval(answer$i_value, digits = 4, eps = 1e-4)),
      p(strong("Degrees of freedom: "), if (answer$type == "F") {
        paste(answer$df1, answer$df2, sep = ", ")
      } else answer$df1),
      p(em("The i-value is a lower-tail/smallness probability under the null; it is not the probability that the null is true."))
    )
  })
}

shinyApp(ui = ui, server = server)
