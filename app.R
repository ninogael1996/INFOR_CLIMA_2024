# Cargar las librerías necesarias para el código.

library(shiny)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(zoo)
library(stats)
library(forecast)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("UNIVERSIDAD CENTRAL DEL ECUADOR "),
  
  div("Carrera de Ingeniería Ambiental", style = "font-style: italic; color: #666; font-size: 25px; font-weight: bold;"),
  
  div("Modelo de Predicción Estadístico para Temperatura con el algoritmo Random Forest en lenguaje R",
      style = "font-style: italic; color: #666; font-size: 18px; font-weight: bold;"),
  
  div("Naim Aguilar - Jhonatan Cruz - Javier Quishpe -  Susana Arciniegas Ortega", style = "font-style: italic; color: #666; font-size: 18px; font-weight: bold;"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      fileInput("data_file", "Seleccionar archivo CSV:"),
      actionButton("transform_button", "1. Transformar a Data Frame"),
      actionButton("model_button", "2. Modelar con Random Forest"),
      actionButton("predict_button", "3. Predecir")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Serie de Tiempo", plotOutput("timeseries_plot")),
        tabPanel("Tendencia", plotOutput("trend_plot")),
        tabPanel("Estacionalidad", plotOutput("seasonal_plot")),
        tabPanel("Aleatoriedad", plotOutput("random_plot")),
        tabPanel("Predicciones", verbatimTextOutput("predictions")),
        tabPanel("Gráfica de Predicciones vs Originales", plotOutput("temp_vs_pred_plot")),
        tabPanel("Índices de Confiabilidad", verbatimTextOutput("reliability_indices"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive value for transformed data frame
  transformed_data <- reactiveVal(NULL)
  
  # Reactive value for trained model
  trained_model <- reactiveVal(NULL)
  
  # Transform uploaded file to data frame
  observeEvent(input$transform_button, {
    req(input$data_file)
    df <- read.csv(input$data_file$datapath)
    transformed_data(df)
  })
  
  # Train model with Random Forest
  observeEvent(input$model_button, {
    req(transformed_data())
    # Perform modeling with Random Forest
    model <- randomForest(Temperatura ~ ., data = transformed_data(), ntree = 100)
    trained_model(model)
  })
  
  # Make predictions
  output$predictions <- renderPrint({
    req(input$predict_button, trained_model(), transformed_data())
    # Predict using the trained model
    predictions <- predict(trained_model(), newdata = transformed_data())
    # Combine original data with predictions
    result <- data.frame(Fecha = transformed_data()$Fecha, Original = transformed_data()$Temperatura, Prediccion = predictions)
    result
  })
  
  # Plot time series
  output$timeseries_plot <- renderPlot({
    req(transformed_data())
    ts_data <- ts(transformed_data()$Temperatura, frequency = 12, start = c(1975, 1))
    plot(ts_data, main = "Serie de Tiempo", xlab = "Años", ylab = "Temperatura (°C)")
  })
  
  # Plot trend
  output$trend_plot <- renderPlot({
    req(transformed_data())
    ts_data <- ts(transformed_data()$Temperatura, frequency = 12, start = c(1975, 1))
    decomposed <- decompose(ts_data)
    plot(decomposed$trend, main = "Tendencia", xlab = "Años", ylab = "Temperatura (°C)")
  })
  
  # Plot seasonal component
  output$seasonal_plot <- renderPlot({
    req(transformed_data())
    ts_data <- ts(transformed_data()$Temperatura, frequency = 12, start = c(1975, 1))
    decomposed <- decompose(ts_data)
    plot(decomposed$seasonal, main = "Estacionalidad", xlab = "Años")
  })
  
  # Plot random component
  output$random_plot <- renderPlot({
    req(transformed_data())
    ts_data <- ts(transformed_data()$Temperatura, frequency = 12, start = c(1975, 1))
    decomposed <- decompose(ts_data)
    plot(decomposed$random, main = "Aleatoriedad", xlab = "Años")
  })
  
  # Plot original and predicted values
  output$temp_vs_pred_plot <- renderPlot({
    req(input$predict_button, trained_model(), transformed_data())
    predictions <- predict(trained_model(), newdata = transformed_data())
    df <- data.frame(Fecha = as.Date(transformed_data()$Fecha, format = "%d/%m/%Y"), Original = transformed_data()$Temperatura, Prediccion = predictions)
    ggplot(df, aes(x = Fecha)) +
      geom_line(aes(y = Original, color = "Original")) +
      geom_line(aes(y = Prediccion, color = "Prediccion")) +
      labs(title = "Valores Originales vs. Predicciones de Temperatura", x = "Fecha", y = "Temperatura (°C)") +
      theme_minimal() +
      scale_color_manual(values = c("Original" = "blue", "Prediccion" = "red"))
  })
  
  # Calculate reliability indices
  output$reliability_indices <- renderPrint({
    req(input$predict_button, trained_model(), transformed_data())
    predictions <- predict(trained_model(), newdata = transformed_data())
    original <- transformed_data()$Temperatura
    rmse <- sqrt(mean((predictions - original)^2))
    mae <- mean(abs(predictions - original))
    mape <- mean(abs((original - predictions) / original)) * 100
    mse <- mean((predictions - original)^2)
    correlation <- cor(original, predictions)
    nash_sutcliffe <- 1 - sum((original - predictions)^2) / sum((original - mean(original))^2)
    mef <- 1 - sum((original - predictions)^2) / sum((abs(original - mean(original)) + abs(predictions - mean(original)))^2)
    bias <- mean(predictions - original)
    reliability_df <- data.frame(RMSE = rmse, MAE = mae, MAPE = mape, MSE = mse, Correlacion = correlation, Nash_Sutcliffe = nash_sutcliffe, MEF = mef, Bias = bias)
    reliability_df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
