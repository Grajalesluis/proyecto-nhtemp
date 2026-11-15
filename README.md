# Pronóstico de temperatura media anual – New Haven (nhtemp)

Proyecto de series de tiempo en R: análisis exploratorio, modelación ARIMA,
diagnóstico de residuales, evaluación del pronóstico y una app Shiny.

## Datos
Serie `nhtemp` (incluida en R): temperatura media anual en °F en New Haven,
Connecticut, de 1912 a 1971 (60 observaciones). No requiere archivos externos.

## Contenido
- `analisis_nhtemp.R`: análisis completo
  - Exploración y prueba de Levene (homogeneidad de varianzas)
  - Estacionariedad (Dickey-Fuller), ACF y PACF
  - Selección del modelo por AIC/BIC y comparación con `auto.arima()`
  - Diagnóstico de residuales (Ljung-Box, Shapiro-Wilk, Lilliefors, Jarque-Bera)
  - Pronóstico con intervalos de confianza
  - Desempeño: trayectoria ajustada, partición 80/20 y validación cruzada expansiva
- `app.R`: aplicación Shiny para explorar la serie, el modelo, el diagnóstico y los pronósticos.

## Cómo ejecutar
1. Instalar R y los paquetes:
   ```r
   install.packages(c("forecast", "tseries", "nortest", "car", "ggplot2", "gridExtra", "shiny"))
   ```
2. Análisis: abrir `analisis_nhtemp.R` y ejecutarlo línea por línea.
3. App: desde la carpeta del proyecto, ejecutar en R:
   ```r
   shiny::runApp()
   ```

## Resultados principales
- Modelo elegido: _(completar, por ejemplo ARIMA(0,1,1))_
- MAPE en prueba (80/20): _(completar)_
- MAPE medio en validación cruzada: _(completar)_

![App](img/app.png)
