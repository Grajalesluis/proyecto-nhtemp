# ============================================================
# App Shiny: Pronóstico de temperatura media anual - New Haven (nhtemp)
# Ejecutar con: shiny::runApp()   (desde la carpeta del proyecto)
# ============================================================

library(shiny)
library(forecast)
library(ggplot2)
library(tseries)

data(nhtemp)

ui <- fluidPage(
  titlePanel("Pronóstico de temperatura media anual - New Haven (nhtemp)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("h", "Años a pronosticar:", min = 1, max = 20, value = 5),
      selectInput("nivel", "Nivel de confianza (%):",
                  choices = c(80, 90, 95), selected = 95),
      radioButtons("modo", "Modelo:",
                   choices = c("auto.arima" = "auto", "Manual" = "manual")),
      conditionalPanel(
        "input.modo == 'manual'",
        numericInput("p", "p (AR)", value = 0, min = 0, max = 5),
        numericInput("d", "d (diferencias)", value = 1, min = 0, max = 2),
        numericInput("q", "q (MA)", value = 1, min = 0, max = 5)
      ),
      hr(),
      sliderInput("pct", "% de datos para entrenamiento (pestaña Desempeño):",
                  min = 50, max = 90, value = 80, step = 5),
      hr(),
      downloadButton("descarga", "Descargar pronóstico (CSV)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Serie",
                 br(), plotOutput("serie"),
                 br(), verbatimTextOutput("resumen_serie")),
        tabPanel("Modelo",
                 br(), verbatimTextOutput("resumen_modelo")),
        tabPanel("Diagnóstico",
                 br(), verbatimTextOutput("pruebas"),
                 plotOutput("resid", height = "450px")),
        tabPanel("Pronóstico",
                 br(), plotOutput("fc"), br(), tableOutput("tabla")),
        tabPanel("Desempeño",
                 br(), verbatimTextOutput("mape_txt"),
                 plotOutput("plot_test"), br(), tableOutput("metricas"))
      )
    )
  )
)

server <- function(input, output) {

  # Función que ajusta el modelo según lo elegido por el usuario
  ajustar <- function(x) {
    if (input$modo == "auto") {
      auto.arima(x)
    } else {
      tryCatch(
        Arima(x, order = c(input$p, input$d, input$q)),
        error = function(e) NULL
      )
    }
  }

  modelo <- reactive({
    m <- ajustar(nhtemp)
    validate(need(!is.null(m),
                  "El modelo no pudo estimarse con ese orden (p,d,q). Prueba con otros valores."))
    m
  })

  pron <- reactive({
    forecast(modelo(), h = input$h, level = as.numeric(input$nivel))
  })

  # --- Serie ---
  output$serie <- renderPlot({
    autoplot(nhtemp, colour = "darkgreen", size = 1) +
      labs(title = "Temperatura media anual en New Haven",
           x = "Año", y = "Temperatura (°F)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  output$resumen_serie <- renderPrint({
    cat("Observaciones:", length(nhtemp), "\n")
    cat("Periodo:", start(nhtemp)[1], "-", end(nhtemp)[1], "\n")
    print(summary(nhtemp))
  })

  # --- Modelo ---
  output$resumen_modelo <- renderPrint({
    m <- modelo()
    print(m)
    cat("\nCriterios de información:\n")
    cat("AIC :", round(m$aic, 3), "\n")
    cat("AICc:", round(m$aicc, 3), "\n")
    cat("BIC :", round(m$bic, 3), "\n\n")
    print(accuracy(m))
  })

  # --- Diagnóstico ---
  output$pruebas <- renderPrint({
    m <- modelo()
    res <- residuals(m)
    ord <- arimaorder(m)
    cat("Prueba de Dickey-Fuller sobre la serie (H0: no estacionaria)\n")
    print(adf.test(nhtemp))
    cat("\nLjung-Box sobre los residuales (H0: no hay autocorrelación)\n")
    print(Box.test(res, lag = 15, type = "Ljung-Box", fitdf = ord[1] + ord[3]))
    cat("\nShapiro-Wilk sobre los residuales (H0: normalidad)\n")
    print(shapiro.test(res))
  })

  output$resid <- renderPlot({
    checkresiduals(modelo())
  })

  # --- Pronóstico ---
  output$fc <- renderPlot({
    autoplot(pron()) +
      labs(title = paste("Pronóstico a", input$h, "años"),
           x = "Año", y = "Temperatura (°F)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  tabla_fc <- reactive({
    f <- pron()
    data.frame(
      Año = as.integer(time(f$mean)),
      Pronóstico = round(as.numeric(f$mean), 2),
      Inferior = round(as.numeric(f$lower[, 1]), 2),
      Superior = round(as.numeric(f$upper[, 1]), 2)
    )
  })

  output$tabla <- renderTable(tabla_fc(), digits = 2)

  output$descarga <- downloadHandler(
    filename = function() "pronostico_nhtemp.csv",
    content = function(file) write.csv(tabla_fc(), file, row.names = FALSE)
  )

  # --- Desempeño (partición entrenamiento / prueba) ---
  particion <- reactive({
    n <- length(nhtemp)
    n_train <- round(input$pct / 100 * n)
    validate(need(n - n_train >= 2, "Deja al menos 2 datos para la prueba."))
    train <- window(nhtemp, end = time(nhtemp)[n_train])
    test  <- window(nhtemp, start = time(nhtemp)[n_train + 1])
    m <- ajustar(train)
    validate(need(!is.null(m), "El modelo no pudo estimarse con ese orden."))
    list(train = train, test = test, fc = forecast(m, h = length(test)))
  })

  output$mape_txt <- renderPrint({
    p <- particion()
    mape <- mean(abs((as.numeric(p$test) - as.numeric(p$fc$mean)) /
                       as.numeric(p$test))) * 100
    cat("Datos de entrenamiento:", length(p$train), "\n")
    cat("Datos de prueba:", length(p$test), "\n")
    cat(sprintf("MAPE en prueba: %.2f%%\n", mape))
  })

  output$plot_test <- renderPlot({
    p <- particion()
    autoplot(p$fc) +
      autolayer(p$test, series = "Prueba real") +
      labs(title = "Evaluación out-of-sample", x = "Año", y = "°F", colour = "") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })

  output$metricas <- renderTable({
    p <- particion()
    as.data.frame(round(accuracy(p$fc, p$test), 3))
  }, rownames = TRUE)
}

shinyApp(ui, server)
