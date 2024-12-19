# Load necessary libraries
library(shiny)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(randomForest)
library(reshape2)
library(GGally)
library(survival)
library(randomForestSRC)
library(gbm)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Breast Cancer Survival Analysis and Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Breast Cancer CSV File", accept = ".csv"),
      checkboxInput("cap_outliers", "Cap Outliers", value = TRUE),
      actionButton("analyze", "Run Analysis"),
      hr(),
      h4("Plots:"),
      selectInput("plot_type", "Choose Plot Type:",
                  choices = c("Histograms", "Boxplots", "Correlation Heatmap", "Pair Plot")),
      hr(),
      h4("Categorical Breakdown:"),
      selectInput("cat_feature", "Select Categorical Feature for Barplot:",
                  choices = NULL) # Will be dynamically updated
    ),
    
    mainPanel(
      h3("Data Preview"),
      tableOutput("data_preview"),
      h3("Selected Plot"),
      plotOutput("plot_output"),
      h3("Categorical Barplot"),
      plotOutput("barplot_output")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)  # Reactive dataset container
  
  # Load and preprocess the data
  observeEvent(input$datafile, {
    req(input$datafile)
    data <- read_csv(input$datafile$datapath)
    colnames(data) <- make.names(colnames(data), unique = TRUE)
    dataset(data)
    
    # Update categorical feature choices dynamically
    cat_columns <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
    updateSelectInput(session, "cat_feature", choices = cat_columns)
  })
  
  # Run data preprocessing and analysis
  observeEvent(input$analyze, {
    req(dataset())
    data <- dataset()
    
    # Handle outliers if selected
    if (input$cap_outliers) {
      continuous_columns <- c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive", "Survival.Months")
      cap_outliers <- function(x) {
        qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
        caps <- c(qnt[1] - 1.5 * IQR(x), qnt[2] + 1.5 * IQR(x))
        x[x < caps[1]] <- caps[1]
        x[x > caps[2]] <- caps[2]
        return(x)
      }
      data[continuous_columns] <- lapply(data[continuous_columns], cap_outliers)
    }
    
    dataset(data)
  })
  
  # Render data preview
  output$data_preview <- renderTable({
    req(dataset())
    head(dataset())
  })
  
  # Render selected plot
  output$plot_output <- renderPlot({
    req(dataset())
    data <- dataset()
    
    if (input$plot_type == "Histograms") {
      par(mfrow = c(2, 2))
      hist(data$Age, col = "purple", main = "Age", xlab = "Age")
      hist(data$Tumor.Size, col = "blue", main = "Tumor Size", xlab = "Tumor Size")
      hist(data$Regional.Node.Examined, col = "green", main = "Regional Node Examined", xlab = "Nodes")
      hist(data$Reginol.Node.Positive, col = "red", main = "Regional Node Positive", xlab = "Nodes")
    }
    else if (input$plot_type == "Boxplots") {
      continuous_columns <- c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive", "Survival.Months")
      boxplots <- lapply(continuous_columns, function(col) {
        ggplot(data, aes_string(y = col)) +
          geom_boxplot() +
          ggtitle(paste("Boxplot of", col)) +
          theme_minimal()
      })
      grid.arrange(grobs = boxplots, ncol = 2)
    }
    else if (input$plot_type == "Correlation Heatmap") {
      numeric_data <- data[sapply(data, is.numeric)]
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)
    }
    else if (input$plot_type == "Pair Plot") {
      numerical_columns <- c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive", "Survival.Months")
      selected_data <- data[, numerical_columns]
      ggpairs(data = selected_data)
    }
  })
  
  # Render categorical barplot
  output$barplot_output <- renderPlot({
    req(dataset(), input$cat_feature)
    data <- dataset()
    cat_feature <- input$cat_feature
    barplot(
      table(data[[cat_feature]], data$Status),
      beside = TRUE,
      col = rainbow(length(unique(data[[cat_feature]]))),
      legend = TRUE,
      main = paste("Survival Status by", cat_feature),
      xlab = cat_feature,
      ylab = "Count"
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
