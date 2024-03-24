# Install and load required packages
if (!require("shiny")) {
  install.packages("shiny")
}
if (!require("caret")) {
  install.packages("caret")
}
if (!require("rpart")) {
  install.packages("rpart")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("e1071")) {
  install.packages("e1071")
}
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
}
if (!require("viridis")) {
  install.packages("viridis")
}
if (!require("randomForest")) {
  install.packages("randomForest")
}

library(shiny)
library(caret)
library(rpart)
library(ggplot2)
library(dplyr)
library(e1071)
library(rpart.plot)
library(viridis)
library(randomForest)

# Define UI
ui <- fluidPage(
  titlePanel("Data Analysis App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Plot Type:", choices = c("Scatter Plot", "Histogram", "Bar Plot")),
      selectInput("xvar", "X Variable:", choices = NULL, selected = NULL),
      selectInput("yvar", "Y Variable (for Scatter Plot):", choices = NULL, selected = NULL, multiple = FALSE),
      actionButton("update", "Update EDA Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preprocessing", verbatimTextOutput("preprocessSummary")),
        tabPanel("EDA", plotOutput("edaPlot")),
        tabPanel("Data Split", verbatimTextOutput("splitInfo")),
        tabPanel("Decision Tree", plotOutput("treePlot"), verbatimTextOutput("treeMetrics")),
        tabPanel("Naive Bayes", verbatimTextOutput("naiveMetrics")),
        tabPanel("Random Forest", verbatimTextOutput("randomForestMetrics")), # New tab for Random Forest
        tabPanel("Model Comparison", plotOutput("comparisonPlot")) # New tab for Model Comparison
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load and preprocess the dataset
  data <- read.csv("Student_data.csv")
  
  data$Zone_EN <- as.factor(data$Zone_EN)
  data$Gender_EN <- as.factor(data$Gender_EN)
  data$NationalityCategory_EN <- as.factor(data$NationalityCategory_EN)
  
  # Update choices for selectInput based on the dataset
  observe({
    updateSelectInput(session, "xvar", choices = names(data))
    updateSelectInput(session, "yvar", choices = names(data))
  })
  
  # Data Preprocessing Summary
  output$preprocessSummary <- renderPrint({
    cat("Data Preprocessing Summary:\n\n")
    
    # Check for missing values
    missingVals <- sum(is.na(data))
    cat("1. Checking for Missing Values:\n")
    if (missingVals > 0) {
      cat("There are", missingVals, "missing values in the dataset.\n\n")
    } else {
      cat("No missing values found in the dataset.\n\n")
    }
    
    # Check for duplicates
    duplicates <- sum(duplicated(data))
    cat("2. Checking for Duplicates:\n")
    if (duplicates > 0) {
      cat("There are", duplicates, "duplicate rows in the dataset.\n\n")
    } else {
      cat("No duplicate rows found in the dataset.\n\n")
    }
    
    # Structure of the data
    cat("3. Structure of the Data:\n")
    str(data)
  })
  
  # EDA Plot
  plotData <- reactive({
    req(input$xvar)
    req(input$plotType)
    
    plot <- ggplot(data, aes_string(x = input$xvar))
    
    if (input$plotType == "Scatter Plot") {
      req(input$yvar)
      plot <- plot + geom_point(aes_string(y = input$yvar, color = input$yvar)) +
        scale_color_viridis_d()
    } else if (input$plotType == "Histogram") {
      plot <- plot + geom_histogram(bins = 30, fill = "blue", color = "white")
    } else if (input$plotType == "Bar Plot") {
      plot <- plot + geom_bar(fill = "orange")
    }
    
    plot + theme_minimal() + labs(x = input$xvar, y = ifelse(input$plotType == "Scatter Plot", input$yvar, "Count"))
  })
  
  output$edaPlot <- renderPlot({
    input$update
    plotData()
  })
  
  # Data Split
  dataSplit <- reactive({
    set.seed(300)
    ind <- createDataPartition(data$NationalityCategory_EN, p = 0.70, list = FALSE)
    list(train = data[ind,], test = data[-ind,])
  })
  
  output$splitInfo <- renderPrint({
    split <- dataSplit()
    cat("Training data dimensions:", dim(split$train), "\n")
    cat("Testing data dimensions:", dim(split$test))
  })
  
  # Decision Tree Model
  treeModel <- reactive({
    split <- dataSplit()
    set.seed(500)
    rpart(NationalityCategory_EN ~ ., data = split$train)
  })
  
  output$treePlot <- renderPlot({
    rpart.plot(treeModel())
  })
  
  output$treeMetrics <- renderPrint({
    split <- dataSplit()
    tree_pred <- predict(treeModel(), newdata = split$test, type = "class")
    confusionMatrix(tree_pred, split$test$NationalityCategory_EN)
  })
  
  # Naive Bayes Model
  naiveModel <- reactive({
    split <- dataSplit()
    set.seed(42)
    naiveBayes(NationalityCategory_EN ~ ., data = split$train)
  })
  
  output$naiveMetrics <- renderPrint({
    split <- dataSplit()
    naive_pred <- predict(naiveModel(), newdata = split$test, type = "class")
    confusionMatrix(naive_pred, split$test$NationalityCategory_EN)
  })
  
  # Random Forest Model
  randomForestModel <- reactive({
    split <- dataSplit()
    set.seed(123) # for reproducibility
    randomForest(NationalityCategory_EN ~ ., data = split$train, ntree = 100) # You can change ntree if needed
  })
  
  output$randomForestMetrics <- renderPrint({
    split <- dataSplit()
    rf_pred <- predict(randomForestModel(), newdata = split$test)
    confusionMatrix(rf_pred, split$test$NationalityCategory_EN)
  })
  
  # Model Comparison
  output$comparisonPlot <- renderPlot({
    split <- dataSplit()
    
    # Decision Tree Accuracy
    tree_pred <- predict(treeModel(), newdata = split$test, type = "class")
    cm_tree <- confusionMatrix(tree_pred, split$test$NationalityCategory_EN)
    accuracy_tree <- cm_tree$overall["Accuracy"]
    
    # Naive Bayes Accuracy
    naive_pred <- predict(naiveModel(), newdata = split$test, type = "class")
    cm_naive <- confusionMatrix(naive_pred, split$test$NationalityCategory_EN)
    accuracy_naive <- cm_naive$overall["Accuracy"]
    
    # Random Forest Accuracy
    rf_pred <- predict(randomForestModel(), newdata = split$test)
    cm_rf <- confusionMatrix(rf_pred, split$test$NationalityCategory_EN)
    accuracy_rf <- cm_rf$overall["Accuracy"]
    
    # Data frame for plotting
    comparison_data <- data.frame(
      Model = c("Decision Tree", "Naive Bayes", "Random Forest"),
      Accuracy = c(accuracy_tree, accuracy_naive, accuracy_rf)
    )
    
    ggplot(comparison_data, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Comparison of Model Accuracies", x = "Model", y = "Accuracy") +
      scale_fill_viridis_d()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
