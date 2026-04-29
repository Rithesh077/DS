library(shiny)

`%||%` <- function(left, right) {
  if (is.null(left)) {
    right
  } else {
    left
  }
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      dir.exists(file.path(current, ".git")) ||
        file.exists(file.path(current, "README.md"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the project root.")
    }

    current <- parent
  }
}

project_root <- find_project_root()

project_path <- function(...) {
  file.path(project_root, ...)
}

ensure_output_dirs <- function() {
  dir.create(project_path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(project_path("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
}

format_output <- function(lines) {
  if (length(lines) == 0) {
    return("Script completed without console output.")
  }

  paste(lines, collapse = "\n")
}

run_project_script <- function(relative_path) {
  script_path <- project_path(relative_path)

  if (!file.exists(script_path)) {
    stop("Missing script: ", relative_path)
  }

  ensure_output_dirs()

  previous_wd <- getwd()
  on.exit(setwd(previous_wd), add = TRUE)
  setwd(project_root)

  script_env <- new.env(parent = globalenv())
  capture.output(source(script_path, local = script_env), type = "output")
}

ui <- fluidPage(
  titlePanel("Statistical Applications & Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Run Statistical Projects"),
      actionButton("run_retail_forecast", "Retail Demand Forecasting"),
      br(), br(),
      actionButton("run_sentiment_analysis", "Sentiment Analysis"),
      br(), br(),
      actionButton("run_sales_analysis", "Sales Data Analysis"),
      br(), br(),
      actionButton("run_eda_iris", "EDA on Iris Dataset"),
      br(), br(),
      actionButton("run_probability_simulation", "Probability Distribution Simulation"),
      br(), br(),
      actionButton("run_estimation_theory", "Theory of Estimation"),
      br(), br(),
      actionButton("run_hypothesis_testing", "Hypothesis Testing"),
      br(), br(),
      actionButton("run_regression_regularization", "Regression & Regularization")
    ),
    mainPanel(
      h4("Project Output"),
      verbatimTextOutput("output_text"),
      uiOutput("output_note"),
      imageOutput("output_image", width = "100%")
    )
  )
)

server <- function(input, output, session) {
  active_result <- reactiveVal(list(
    text = "Choose a project from the sidebar.",
    note = NULL,
    image = NULL,
    alt = NULL
  ))

  show_message <- function(text, note = NULL) {
    active_result(list(text = text, note = note, image = NULL, alt = NULL))
  }

  show_project <- function(relative_path, image = NULL, alt = NULL, note = NULL) {
    text <- tryCatch(
      format_output(run_project_script(relative_path)),
      error = function(error) {
        paste("Error:", conditionMessage(error))
      }
    )

    active_result(list(text = text, note = note, image = image, alt = alt))
  }

  output$output_text <- renderText({
    active_result()$text
  })

  output$output_note <- renderUI({
    note <- active_result()$note

    if (is.null(note)) {
      return(NULL)
    }

    tags$p(note)
  })

  output$output_image <- renderImage(
    {
      image_file <- active_result()$image

      if (is.null(image_file)) {
        return(NULL)
      }

      image_path <- project_path("outputs", "figures", image_file)
      validate(need(file.exists(image_path), paste("No plot found at", image_path)))

      list(
        src = normalizePath(image_path, winslash = "/"),
        contentType = "image/png",
        alt = active_result()$alt %||% image_file
      )
    },
    deleteFile = FALSE
  )

  observeEvent(input$run_retail_forecast, {
    show_project(
      "R/01_retail_demand_forecasting.R",
      image = "retail_forecast.png",
      alt = "Retail demand forecast plot"
    )
  })

  observeEvent(input$run_sentiment_analysis, {
    show_project(
      "R/02_sentiment_analysis_reviews.R",
      note = "Sentiment analysis saves tabular results under outputs/tables."
    )
  })

  observeEvent(input$run_sales_analysis, {
    show_project(
      "R/03_sales_dataset_analysis.R",
      note = "Sales analysis currently produces console summaries."
    )
  })

  observeEvent(input$run_eda_iris, {
    show_message(
      paste(capture.output(summary(iris)), collapse = "\n"),
      note = "The original EDA script uses console prompts, so the dashboard shows the Iris summary without launching the interactive prompt flow."
    )
  })

  observeEvent(input$run_probability_simulation, {
    show_project(
      "R/05_probability_distributions_simulation.R",
      note = "Probability simulation plots are drawn by the R graphics device."
    )
  })

  observeEvent(input$run_estimation_theory, {
    show_message(
      "Theory of Estimation requires console inputs for population and sample sizes.",
      note = "Run R/06_theory_of_estimation.R manually when you want the prompted workflow."
    )
  })

  observeEvent(input$run_hypothesis_testing, {
    show_project(
      "R/07_hypothesis_testing_reviews.R",
      image = "reviews_helpfulness_boxplot.png",
      alt = "Ratings by review helpfulness boxplot"
    )
  })

  observeEvent(input$run_regression_regularization, {
    show_project(
      "R/08_regression_and_regularization.R",
      image = "regression_regularization_coefficients.png",
      alt = "Ridge and lasso coefficient paths",
      note = "Regression and regularization outputs are saved under outputs/figures and outputs/tables."
    )
  })
}

shinyApp(ui = ui, server = server)
