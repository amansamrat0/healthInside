# =============================================================================
# Heart Attack Risk Analysis and Prediction System
# Framingham Heart Study - Shiny Application
# =============================================================================

# --- Load Required Libraries ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(corrplot)

# --- Load and Prepare Dataset ---
# Read framingham.csv from the same directory as this app
raw_data <- read.csv("framingham.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))

# Convert all columns to appropriate types for analysis
data <- raw_data

# --- Build Logistic Regression Model (complete cases only) ---
model_data <- data %>%
  dplyr::select(age, sysBP, diaBP, BMI, glucose, diabetes, currentSmoker, TenYearCHD) %>%
  na.omit()

logistic_model <- glm(
  TenYearCHD ~ age + sysBP + diaBP + BMI + glucose + diabetes + currentSmoker,
  data = model_data,
  family = "binomial"
)

# --- Prepare K-Means Clustering Data ---
cluster_vars <- c("age", "sysBP", "diaBP", "BMI", "glucose", "diabetes", "currentSmoker")
cluster_data <- model_data[, cluster_vars]
cluster_scaled <- scale(cluster_data)

set.seed(123)
kmeans_result <- kmeans(cluster_scaled, centers = 3, nstart = 25)

# Assign risk labels based on average CHD rate in each cluster
cluster_summary <- data.frame(
  cluster = 1:3,
  chd_rate = tapply(model_data$TenYearCHD, kmeans_result$cluster, mean)
)
cluster_summary <- cluster_summary[order(cluster_summary$chd_rate), ]
risk_labels <- c("Low Risk Group", "Medium Risk Group", "High Risk Group")
cluster_label_map <- setNames(risk_labels, cluster_summary$cluster)
cluster_names <- cluster_label_map[as.character(kmeans_result$cluster)]

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  # --- Simple White Background Styling ---
  tags$head(
    tags$style(HTML("
      body {
        background-color: #ffffff;
        font-family: Arial, sans-serif;
      }
      .navbar-default {
        background-color: #f5f5f5;
        border-color: #dddddd;
      }
      .navbar-default .navbar-brand {
        color: #333333;
        font-weight: bold;
      }
      .well {
        background-color: #fafafa;
        border: 1px solid #e0e0e0;
      }
      h2, h3, h4 {
        color: #333333;
      }
      .metric-box {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 10px;
        text-align: center;
      }
      .metric-value {
        font-size: 24px;
        font-weight: bold;
        color: #2c3e50;
      }
      .metric-label {
        font-size: 14px;
        color: #666666;
      }
      .interpretation-box {
        background-color: #eef6ff;
        border-left: 4px solid #3498db;
        padding: 12px;
        margin-top: 10px;
      }
      .risk-low { color: #27ae60; font-weight: bold; }
      .risk-medium { color: #f39c12; font-weight: bold; }
      .risk-high { color: #e74c3c; font-weight: bold; }
    "))
  ),

  # --- Application Title ---
  titlePanel("Heart Attack Risk Analysis and Prediction System"),

  # --- Navigation Menu with Four Pages ---
  navbarPage(
    id = "main_nav",
    title = NULL,

    # -------------------------------------------------------------------------
    # A. DASHBOARD PAGE
    # -------------------------------------------------------------------------
    tabPanel(
      "Dashboard",
      br(),
      h3("Dataset Overview"),
      fluidRow(
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("total_records")),
                      div(class = "metric-label", "Total Records"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("total_columns")),
                      div(class = "metric-label", "Total Columns"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("missing_count")),
                      div(class = "metric-label", "Missing Values"))),
        column(3, div(class = "metric-box",
                      div(class = "metric-value", textOutput("complete_records")),
                      div(class = "metric-label", "Complete Records (Model)")))
      ),
      br(),
      h4("Dataset Preview (First 10 Rows)"),
      DTOutput("data_preview"),
      br(),
      h4("Basic Dataset Summary"),
      verbatimTextOutput("data_summary")
    ),

    # -------------------------------------------------------------------------
    # B. DATA VISUALIZATION PAGE
    # -------------------------------------------------------------------------
    tabPanel(
      "Data Visualization",
      br(),
      h3("Exploratory Data Analysis"),
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Graph Settings"),
            selectInput(
              "selected_column",
              "Select Column:",
              choices = names(data),
              selected = "age"
            ),
            selectInput(
              "graph_type",
              "Select Graph Type:",
              choices = c(
                "Histogram" = "histogram",
                "Box Plot" = "boxplot",
                "Bar Chart" = "barchart",
                "Scatter Plot" = "scatter",
                "Density Plot" = "density"
              ),
              selected = "histogram"
            ),
            actionButton("generate_graph", "Generate Graph", class = "btn-primary")
          )
        ),
        column(
          8,
          plotOutput("main_plot", height = "400px"),
          br(),
          h4("Descriptive Statistics"),
          tableOutput("desc_stats"),
          div(class = "interpretation-box", uiOutput("variable_interpretation"))
        )
      ),
      hr(),
      h3("Correlation Analysis"),
      p("Explore relationships between all numerical variables in the dataset."),
      actionButton("show_correlation", "Show Correlation Heatmap", class = "btn-info"),
      br(), br(),
      plotOutput("correlation_plot", height = "600px"),
      br(),
      verbatimTextOutput("correlation_insights")
    ),

    # -------------------------------------------------------------------------
    # C. RISK PREDICTION PAGE
    # -------------------------------------------------------------------------
    tabPanel(
      "Risk Prediction",
      br(),

      # --- Heart Disease Prediction Section ---
      h3("Heart Disease Risk Prediction"),
      p("Enter patient details below to predict 10-year coronary heart disease (CHD) risk using logistic regression."),
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Patient Information"),
            numericInput("input_age", "Age (years):", value = 50, min = 20, max = 90),
            numericInput("input_sysbp", "Systolic Blood Pressure (mmHg):", value = 120, min = 80, max = 250),
            numericInput("input_diabp", "Diastolic Blood Pressure (mmHg):", value = 80, min = 50, max = 150),
            numericInput("input_bmi", "BMI:", value = 25, min = 15, max = 50, step = 0.1),
            numericInput("input_glucose", "Glucose (mg/dL):", value = 85, min = 40, max = 400),
            selectInput("input_diabetes", "Diabetes:", choices = c("No" = 0, "Yes" = 1)),
            selectInput("input_smoker", "Current Smoker:", choices = c("No" = 0, "Yes" = 1)),
            br(),
            actionButton("predict_risk", "Predict Risk", class = "btn-danger btn-lg", width = "100%")
          )
        ),
        column(
          8,
          wellPanel(
            h4("Prediction Results"),
            uiOutput("prediction_result"),
            br(),
            h4("Model Information"),
            p("Model: Logistic Regression"),
            p("Formula: TenYearCHD ~ age + sysBP + diaBP + BMI + glucose + diabetes + currentSmoker"),
            verbatimTextOutput("model_summary_brief")
          )
        )
      ),

      hr(),

      # --- Risk Factor Analysis Section ---
      h3("Top Risk Factor Analysis"),
      p("Risk factors ranked by importance based on logistic regression coefficients (absolute value)."),
      tableOutput("risk_factors_table"),
      plotOutput("risk_factors_plot", height = "350px"),

      hr(),

      # --- Clustering Analysis Section ---
      h3("K-Means Clustering Analysis"),
      p("Patients grouped into Low, Medium, and High risk clusters based on key health indicators."),
      fluidRow(
        column(6, plotOutput("cluster_plot", height = "450px")),
        column(6,
               h4("Patients in Each Cluster"),
               tableOutput("cluster_counts"),
               div(class = "interpretation-box", uiOutput("cluster_interpretation"))
        )
      )
    ),

    # -------------------------------------------------------------------------
    # D. PROJECT CONCLUSION PAGE
    # -------------------------------------------------------------------------
    tabPanel(
      "Project Conclusion",
      br(),
      h3("Project Conclusion and Key Insights"),
      wellPanel(
        h4("Summary of Findings"),
        uiOutput("conclusion_insights")
      ),
      wellPanel(
        h4("Recommendations"),
        tags$ul(
          tags$li("Regular health check-ups help monitor blood pressure and glucose levels."),
          tags$li("Smoking cessation programs can significantly reduce cardiovascular risk."),
          tags$li("Maintaining a healthy BMI through diet and exercise is recommended."),
          tags$li("Diabetes management is essential for preventing heart disease."),
          tags$li("Early identification of at-risk individuals enables timely intervention.")
        )
      ),
      wellPanel(
        h4("About This Project"),
        p("This application analyzes the Framingham Heart Study dataset to explore heart disease
          risk factors, visualize health data patterns, predict 10-year CHD risk using logistic
          regression, and identify patient groups through K-Means clustering. The project
          demonstrates practical application of statistical learning in healthcare analytics.")
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # --- Helper: Get numeric columns from dataset ---
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # --- Helper: Classify risk category from probability ---
  get_risk_category <- function(probability) {
    pct <- probability * 100
    if (pct <= 30) {
      list(category = "Low Risk", class = "risk-low")
    } else if (pct <= 60) {
      list(category = "Medium Risk", class = "risk-medium")
    } else {
      list(category = "High Risk", class = "risk-high")
    }
  }

  # --- Helper: Generate automatic variable interpretation ---
  generate_interpretation <- function(col_name, values) {
    if (!is.numeric(values)) {
      freq_table <- table(values, useNA = "no")
      top_level <- names(freq_table)[which.max(freq_table)]
      return(paste(
        "The variable", col_name, "is categorical.",
        "The most frequent category is", top_level, ".",
        "Review the bar chart to compare category frequencies."
      ))
    }

    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)

    spread_text <- if (sd_val < (0.1 * mean_val)) {
      "The values are relatively consistent with low variation."
    } else if (sd_val > (0.3 * mean_val)) {
      "The values show high variation across patients."
    } else {
      "The values show moderate variation across patients."
    }

    range_text <- paste0(
      "Values range from ", round(min_val, 2), " to ", round(max_val, 2),
      " with a mean of ", round(mean_val, 2), "."
    )

    col_context <- switch(
      col_name,
      age = "Higher age is generally associated with increased cardiovascular risk.",
      sysBP = "Systolic blood pressure is a key indicator of hypertension and heart disease risk.",
      diaBP = "Diastolic blood pressure reflects vascular resistance and cardiovascular health.",
      BMI = "BMI indicates body weight status; higher values may increase heart disease risk.",
      glucose = "Blood glucose levels are important for assessing diabetes-related heart risk.",
      diabetes = "Diabetes status is a significant predictor of coronary heart disease.",
      currentSmoker = "Smoking status strongly influences long-term cardiovascular outcomes.",
      TenYearCHD = "This is the target variable indicating 10-year CHD risk (0 = No, 1 = Yes).",
      paste("This variable provides useful insight into patient health characteristics.")
    )

    paste(range_text, spread_text, col_context)
  }

  # ===========================================================================
  # DASHBOARD OUTPUTS
  # ===========================================================================

  output$total_records <- renderText(nrow(data))
  output$total_columns <- renderText(ncol(data))
  output$missing_count <- renderText(sum(is.na(data)))
  output$complete_records <- renderText(nrow(model_data))

  output$data_preview <- renderDT({
    datatable(
      head(data, 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$data_summary <- renderPrint({
    summary(data)
  })

  # ===========================================================================
  # DATA VISUALIZATION OUTPUTS
  # ===========================================================================

  viz_data <- eventReactive(input$generate_graph, {
    col_name <- input$selected_column
    values <- data[[col_name]]
    list(col_name = col_name, values = values)
  }, ignoreNULL = FALSE)

  output$main_plot <- renderPlot({
    req(input$generate_graph)

    viz <- viz_data()
    col_name <- viz$col_name
    values <- viz$values
    plot_df <- data.frame(value = values)
    plot_df <- plot_df[!is.na(plot_df$value), , drop = FALSE]

    if (nrow(plot_df) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid data available for the selected column.")
      return(invisible(NULL))
    }

    p <- switch(
      input$graph_type,
      histogram = {
        if (!is.numeric(plot_df$value)) {
          ggplot(plot_df, aes(x = factor(value))) +
            geom_bar(fill = "#3498db", color = "white") +
            labs(title = paste("Bar Chart of", col_name), x = col_name, y = "Count") +
            theme_minimal(base_size = 14)
        } else {
          ggplot(plot_df, aes(x = value)) +
            geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.85) +
            labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
            theme_minimal(base_size = 14)
        }
      },
      boxplot = {
        ggplot(plot_df, aes(y = if (is.numeric(plot_df$value)) value else factor(value))) +
          geom_boxplot(fill = "#2ecc71", alpha = 0.7) +
          labs(title = paste("Box Plot of", col_name), y = col_name) +
          theme_minimal(base_size = 14)
      },
      barchart = {
        if (is.numeric(plot_df$value) && length(unique(plot_df$value)) > 15) {
          ggplot(plot_df, aes(x = value)) +
            geom_histogram(bins = 20, fill = "#9b59b6", color = "white") +
            labs(title = paste("Bar Chart (Binned) of", col_name), x = col_name, y = "Count") +
            theme_minimal(base_size = 14)
        } else {
          count_df <- as.data.frame(table(plot_df$value))
          names(count_df) <- c("category", "count")
          ggplot(count_df, aes(x = category, y = count)) +
            geom_bar(stat = "identity", fill = "#9b59b6", color = "white") +
            labs(title = paste("Bar Chart of", col_name), x = col_name, y = "Count") +
            theme_minimal(base_size = 14)
        }
      },
      scatter = {
        plot_df$index <- seq_len(nrow(plot_df))
        ggplot(plot_df, aes(x = index, y = if (is.numeric(value)) value else as.numeric(factor(value)))) +
          geom_point(color = "#e67e22", alpha = 0.6) +
          labs(
            title = paste("Scatter Plot of", col_name),
            x = "Observation Index",
            y = col_name
          ) +
          theme_minimal(base_size = 14)
      },
      density = {
        if (!is.numeric(plot_df$value)) {
          ggplot(plot_df, aes(x = factor(value))) +
            geom_bar(fill = "#1abc9c", color = "white") +
            labs(title = paste("Distribution of", col_name), x = col_name, y = "Count") +
            theme_minimal(base_size = 14)
        } else {
          ggplot(plot_df, aes(x = value)) +
            geom_density(fill = "#1abc9c", alpha = 0.5, color = "#16a085") +
            labs(title = paste("Density Plot of", col_name), x = col_name, y = "Density") +
            theme_minimal(base_size = 14)
        }
      }
    )

    print(p)
  })

  output$desc_stats <- renderTable({
    req(input$generate_graph)

    viz <- viz_data()
    values <- viz$values[!is.na(viz$values)]

    if (!is.numeric(values)) {
      data.frame(
        Statistic = c("Unique Values", "Most Frequent", "Count of Most Frequent"),
        Value = c(
          length(unique(values)),
          names(sort(table(values), decreasing = TRUE))[1],
          max(table(values))
        )
      )
    } else {
      data.frame(
        Statistic = c("Mean", "Median", "Minimum", "Maximum", "Standard Deviation"),
        Value = round(c(
          mean(values),
          median(values),
          min(values),
          max(values),
          sd(values)
        ), 3)
      )
    }
  }, striped = TRUE, bordered = TRUE)

  output$variable_interpretation <- renderUI({
    req(input$generate_graph)

    viz <- viz_data()
    interpretation <- generate_interpretation(viz$col_name, viz$values)
    HTML(paste("<strong>Interpretation:</strong>", interpretation))
  })

  # --- Correlation Heatmap ---
  cor_data <- eventReactive(input$show_correlation, {
    num_data <- data[, numeric_cols, drop = FALSE]
    num_data <- na.omit(num_data)
    cor(num_data, use = "complete.obs")
  }, ignoreNULL = FALSE)

  output$correlation_plot <- renderPlot({
    req(input$show_correlation)

    cor_matrix <- cor_data()
    corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      tl.cex = 0.8,
      tl.col = "black",
      addCoef.col = "black",
      number.cex = 0.6,
      title = "Correlation Heatmap - Numerical Variables",
      mar = c(0, 0, 2, 0)
    )
  })

  output$correlation_insights <- renderPrint({
    req(input$show_correlation)

    cor_matrix <- cor_data()
    cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
    cor_df <- as.data.frame(as.table(cor_matrix))
    names(cor_df) <- c("Variable1", "Variable2", "Correlation")
    cor_df <- cor_df[!is.na(cor_df$Correlation), ]
    cor_df <- cor_df[cor_df$Variable1 != cor_df$Variable2, ]
    cor_df$AbsCorrelation <- abs(cor_df$Correlation)
    high_cor <- cor_df[cor_df$AbsCorrelation >= 0.5, ]
    high_cor <- high_cor[order(-high_cor$AbsCorrelation), ]

    cat("Highly Correlated Variable Pairs (|r| >= 0.5):\n")
    cat("================================================\n\n")

    if (nrow(high_cor) == 0) {
      cat("No variable pairs found with correlation >= 0.5.\n")
    } else {
      for (i in seq_len(nrow(high_cor))) {
        cat(sprintf(
          "%s vs %s: r = %.3f\n",
          high_cor$Variable1[i],
          high_cor$Variable2[i],
          high_cor$Correlation[i]
        ))
      }
    }
  })

  # ===========================================================================
  # RISK PREDICTION OUTPUTS
  # ===========================================================================

  prediction_result <- eventReactive(input$predict_risk, {
    new_patient <- data.frame(
      age = input$input_age,
      sysBP = input$input_sysbp,
      diaBP = input$input_diabp,
      BMI = input$input_bmi,
      glucose = input$input_glucose,
      diabetes = as.numeric(input$input_diabetes),
      currentSmoker = as.numeric(input$input_smoker)
    )

    probability <- predict(logistic_model, newdata = new_patient, type = "response")
    risk_info <- get_risk_category(probability)

    list(probability = probability, risk_info = risk_info)
  }, ignoreNULL = FALSE)

  output$prediction_result <- renderUI({
    req(input$predict_risk)

    result <- prediction_result()
    pct <- round(result$probability * 100, 2)
    risk_class <- result$risk_info$class
    risk_category <- result$risk_info$category

    tagList(
      h4("Predicted 10-Year CHD Risk"),
      p(paste0("Probability: ", pct, "%"), style = "font-size: 20px;"),
      p(
        paste("Risk Category:", risk_category),
        class = risk_class,
        style = "font-size: 18px;"
      ),
      if (pct <= 30) {
        p("The patient shows a low probability of developing coronary heart disease within 10 years.")
      } else if (pct <= 60) {
        p("The patient shows a moderate probability of developing coronary heart disease. Regular monitoring is advised.")
      } else {
        p("The patient shows a high probability of developing coronary heart disease. Medical consultation is strongly recommended.")
      }
    )
  })

  output$model_summary_brief <- renderPrint({
    cat("Model trained on", nrow(model_data), "complete records.\n")
    cat("AIC:", round(AIC(logistic_model), 2), "\n")
  })

  # --- Risk Factor Analysis Table and Plot ---
  risk_factors_df <- reactive({
    coefs <- coef(logistic_model)[-1]
    abs_coefs <- abs(coefs)

    bp_importance <- max(abs_coefs["sysBP"], abs_coefs["diaBP"], na.rm = TRUE)
    bp_direction <- ifelse(
      abs_coefs["sysBP"] >= abs_coefs["diaBP"],
      coefs["sysBP"],
      coefs["diaBP"]
    )

    factors <- data.frame(
      Risk_Factor = c(
        "Age",
        "Blood Pressure",
        "Glucose",
        "Diabetes",
        "Smoking Status"
      ),
      Coefficient = c(
        coefs["age"],
        bp_direction,
        coefs["glucose"],
        coefs["diabetes"],
        coefs["currentSmoker"]
      ),
      Importance = c(
        abs_coefs["age"],
        bp_importance,
        abs_coefs["glucose"],
        abs_coefs["diabetes"],
        abs_coefs["currentSmoker"]
      ),
      Direction = c(
        ifelse(coefs["age"] > 0, "Increases Risk", "Decreases Risk"),
        ifelse(bp_direction > 0, "Increases Risk", "Decreases Risk"),
        ifelse(coefs["glucose"] > 0, "Increases Risk", "Decreases Risk"),
        ifelse(coefs["diabetes"] > 0, "Increases Risk", "Decreases Risk"),
        ifelse(coefs["currentSmoker"] > 0, "Increases Risk", "Decreases Risk")
      ),
      stringsAsFactors = FALSE
    )

    factors <- factors[order(-factors$Importance), ]
    factors$Rank <- seq_len(nrow(factors))
    factors[, c("Coefficient", "Importance")] <- round(factors[, c("Coefficient", "Importance")], 4)
    factors
  })

  output$risk_factors_table <- renderTable({
    risk_factors_df()[, c("Rank", "Risk_Factor", "Coefficient", "Importance", "Direction")]
  }, striped = TRUE, bordered = TRUE)

  output$risk_factors_plot <- renderPlot({
    factors <- risk_factors_df()
    factors$Risk_Factor <- factor(factors$Risk_Factor, levels = rev(factors$Risk_Factor))

    ggplot(factors, aes(x = Risk_Factor, y = Importance, fill = Risk_Factor)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      scale_fill_brewer(palette = "Blues") +
      labs(
        title = "Top Risk Factors by Importance",
        x = "Risk Factor",
        y = "Absolute Coefficient Value"
      ) +
      theme_minimal(base_size = 14)
  })

  # --- Clustering Analysis Outputs ---
  cluster_plot_data <- reactive({
    pca_result <- prcomp(cluster_scaled, center = FALSE, scale. = FALSE)
    data.frame(
      PC1 = pca_result$x[, 1],
      PC2 = pca_result$x[, 2],
      Cluster = factor(cluster_names, levels = risk_labels)
    )
  })

  output$cluster_plot <- renderPlot({
    plot_df <- cluster_plot_data()

    ggplot(plot_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.5, size = 2) +
      scale_color_manual(values = c("#27ae60", "#f39c12", "#e74c3c")) +
      labs(
        title = "K-Means Clustering of Patients",
        subtitle = "Clusters labeled by heart disease risk level",
        x = "Principal Component 1",
        y = "Principal Component 2",
        color = "Risk Group"
      ) +
      theme_minimal(base_size = 14)
  })

  output$cluster_counts <- renderTable({
    counts <- as.data.frame(table(cluster_names))
    names(counts) <- c("Risk Group", "Number of Patients")
    counts <- counts[order(match(counts$`Risk Group`, risk_labels)), ]
    counts
  }, striped = TRUE, bordered = TRUE)

  output$cluster_interpretation <- renderUI({
    counts <- table(cluster_names)
    HTML(paste0(
      "<strong>Cluster Summary:</strong> K-Means clustering (k = 3) grouped patients into ",
      "Low (", counts["Low Risk Group"], "), Medium (", counts["Medium Risk Group"], "), ",
      "and High (", counts["High Risk Group"], ") risk groups based on age, blood pressure, ",
      "BMI, glucose, diabetes, and smoking status."
    ))
  })

  # ===========================================================================
  # CONCLUSION PAGE OUTPUTS
  # ===========================================================================

  output$conclusion_insights <- renderUI({
    factors <- risk_factors_df()
    top_factor <- factors$Risk_Factor[1]
    coefs <- coef(logistic_model)

    age_effect <- ifelse(coefs["age"] > 0,
                         "Higher age increases heart disease risk.",
                         "Age shows an inverse relationship with predicted risk in this model.")
    bp_effect <- ifelse(max(coefs["sysBP"], coefs["diaBP"], na.rm = TRUE) > 0,
                        "High blood pressure is a major risk factor.",
                        "Blood pressure shows protective association in this sample.")
    diabetes_effect <- ifelse(coefs["diabetes"] > 0,
                              "Diabetes significantly affects prediction.",
                              "Diabetes status shows limited positive association in this model.")
    smoking_effect <- ifelse(coefs["currentSmoker"] > 0,
                             "Smokers have higher risk than non-smokers.",
                             "Smoking status did not show a strong positive effect in this model.")

    chd_rate <- round(mean(model_data$TenYearCHD, na.rm = TRUE) * 100, 1)
    smoker_rate <- round(mean(model_data$TenYearCHD[model_data$currentSmoker == 1], na.rm = TRUE) * 100, 1)
    nonsmoker_rate <- round(mean(model_data$TenYearCHD[model_data$currentSmoker == 0], na.rm = TRUE) * 100, 1)

    tagList(
      tags$ul(
        tags$li(age_effect),
        tags$li(bp_effect),
        tags$li(diabetes_effect),
        tags$li(smoking_effect),
        tags$li("Lifestyle modifications can reduce future cardiovascular risk."),
        tags$li(paste0("Overall 10-year CHD rate in the dataset: ", chd_rate, "%.")),
        tags$li(paste0("CHD rate among smokers: ", smoker_rate, "% vs non-smokers: ", nonsmoker_rate, "%.")),
        tags$li(paste0("Most influential risk factor in the model: ", top_factor, "."))
      )
    )
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================
shinyApp(ui = ui, server = server)
