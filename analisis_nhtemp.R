# ============================================================
# Proyecto: Pronóstico de la temperatura media anual en New Haven
# Datos: nhtemp (incluido en R) - 1912 a 1971, n = 60, frecuencia anual
# Serie sin estacionalidad -> se usa un modelo ARIMA(p,d,q)
#
# Los gráficos se guardan automáticamente en la carpeta img/
# Ejecutar con la carpeta del proyecto como directorio de trabajo
# (comprobar con getwd()).
# ============================================================

# 1. PAQUETES ---------------------------------------------------
pkgs <- c("forecast", "tseries", "nortest", "car", "ggplot2", "gridExtra")
nuevos <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(nuevos)) install.packages(nuevos)
invisible(lapply(pkgs, library, character.only = TRUE))

# Carpeta para guardar los gráficos
dir.create("img", showWarnings = FALSE)

# Función auxiliar: muestra el gráfico y lo guarda como PNG
guardar <- function(grafico, archivo, ancho = 8, alto = 5) {
  print(grafico)
  ggsave(file.path("img", archivo), plot = grafico,
         width = ancho, height = alto, dpi = 150)
}

# 2. DATOS Y EXPLORACIÓN ----------------------------------------
data(nhtemp)
class(nhtemp)
start(nhtemp); end(nhtemp); frequency(nhtemp)
paste("Número de observaciones:", length(nhtemp))
summary(nhtemp)

g_serie <- autoplot(nhtemp, colour = "darkgreen", size = 1) +
  labs(title = "Temperatura media anual en New Haven",
       x = "Año", y = "Temperatura (°F)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
guardar(g_serie, "01_serie.png")

# 3. HOMOGENEIDAD DE VARIANZAS (LEVENE) -------------------------
# H0: varianzas iguales entre ventanas  vs  H1: alguna es distinta
# Se dividen los 60 años en 6 ventanas de 10 años
ventanas <- cut(seq_along(nhtemp), breaks = 6)
pl <- leveneTest(as.numeric(nhtemp) ~ ventanas)
print(pl)
if (pl$`Pr(>F)`[1] < 0.05) {
  print("Se rechaza H0: varianzas diferentes (considerar Box-Cox)")
} else {
  print("No se rechaza H0: varianzas homogéneas (no se requiere Box-Cox)")
}

# Boxplot por ventanas
df_box <- data.frame(Ventana = ventanas, Temp = as.numeric(nhtemp))
g_box <- ggplot(df_box, aes(x = Ventana, y = Temp)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot por ventanas de 10 años", x = "Ventana", y = "°F") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
guardar(g_box, "02_boxplot_ventanas.png")

# 4. ESTACIONARIEDAD (DICKEY-FULLER) ----------------------------
# H0: no estacionaria  vs  Ha: estacionaria
adf <- adf.test(nhtemp)
print(adf)
if (adf$p.value < 0.05) {
  print("La serie es estacionaria")
} else {
  print("La serie no es estacionaria")
}
ndiffs(nhtemp)   # número de diferencias sugerido
cat("Frecuencia de la serie:", frequency(nhtemp),
    "-> serie anual, sin componente estacional (no aplica nsdiffs)\n")
# 5. ACF Y PACF -------------------------------------------------
png("img/03_acf_pacf_original.png", width = 1000, height = 700, res = 120)
ggtsdisplay(nhtemp, main = "Serie original: serie, ACF y PACF")
dev.off()
ggtsdisplay(nhtemp, main = "Serie original: serie, ACF y PACF")

dtemp <- diff(nhtemp)
summary(dtemp)
adf.test(dtemp)

png("img/04_acf_pacf_diferenciada.png", width = 1000, height = 700, res = 120)
ggtsdisplay(dtemp, main = "Serie diferenciada: serie, ACF y PACF")
dev.off()
ggtsdisplay(dtemp, main = "Serie diferenciada: serie, ACF y PACF")

# 6. SELECCIÓN DEL MODELO (AIC / BIC) ---------------------------
d <- ndiffs(nhtemp)      # se puede cambiar a mano (por ejemplo d <- 1)

buscar_arima <- function(x, pmax = 3, qmax = 3, d = 1) {
  filas <- list()
  for (p in 0:pmax) {
    for (q in 0:qmax) {
      fit <- tryCatch(Arima(x, order = c(p, d, q)), error = function(e) NULL)
      if (!is.null(fit)) {
        filas[[length(filas) + 1]] <- data.frame(
          p = p, d = d, q = q,
          AIC = fit$aic, AICc = fit$aicc, BIC = fit$bic
        )
      }
    }
  }
  tabla <- do.call(rbind, filas)
  tabla[order(tabla$BIC), ]       # ordenado por BIC (cambiar por AIC si se prefiere)
}

tabla_modelos <- buscar_arima(nhtemp, pmax = 3, qmax = 3, d = d)
print(head(tabla_modelos, 10))
write.csv(tabla_modelos, "tabla_modelos.csv", row.names = FALSE)

# Comparación con auto.arima
mod_auto <- auto.arima(nhtemp)
print(mod_auto)

# Modelo elegido: por defecto el de menor BIC. Se puede fijar a mano:
# orden <- c(0, 1, 1)
orden <- as.numeric(tabla_modelos[1, c("p", "d", "q")])
cat("Modelo elegido: ARIMA(", paste(orden, collapse = ","), ")\n")

# 7. ESTIMACIÓN -------------------------------------------------
mod <- Arima(nhtemp, order = orden)
print(summary(mod))

# Significancia de los coeficientes (z = coef / error estándar)
coefs <- coef(mod)
ses   <- sqrt(diag(vcov(mod)))
print(data.frame(coef = coefs, se = ses, z = coefs / ses,
                 p.valor = 2 * (1 - pnorm(abs(coefs / ses)))))

# 8. DIAGNÓSTICO DE RESIDUALES ----------------------------------
res <- residuals(mod)

# Panel: residuales, ACF y histograma + prueba de Ljung-Box
png("img/05_residuales.png", width = 1000, height = 800, res = 120)
checkresiduals(mod)
dev.off()
checkresiduals(mod)

# Ljung-Box para varios rezagos (H0: no hay autocorrelación)
lb_pvals <- sapply(1:20, function(k) {
  Box.test(res, lag = k, type = "Ljung-Box",
           fitdf = min(sum(orden[c(1, 3)]), k - 1))$p.value
})
grafico_lb <- function() {
  plot(1:20, lb_pvals, ylim = c(0, 1), pch = 19,
       xlab = "Rezago", ylab = "p-valor", main = "Ljung-Box por rezago")
  abline(h = 0.05, col = "red", lty = 2)
}
png("img/06_ljung_box.png", width = 900, height = 600, res = 120)
grafico_lb()
dev.off()
grafico_lb()

# Normalidad de los residuales
shapiro.test(res)
lillie.test(res)                 # Kolmogorov-Smirnov con corrección de Lilliefors
jarque.bera.test(res)            # Jarque-Bera (paquete tseries)

grafico_qq <- function() {
  qqnorm(res, main = "Q-Q plot de residuales")
  qqline(res, col = "red")
}
png("img/07_qqplot.png", width = 900, height = 600, res = 120)
grafico_qq()
dev.off()
grafico_qq()

# Homocedasticidad de los residuales (Levene por ventanas)
leveneTest(as.numeric(res) ~ cut(seq_along(res), breaks = 6))

# 9. PRONÓSTICO -------------------------------------------------
h <- 5   # años a pronosticar
fc <- forecast(mod, h = h, level = c(80, 95))
print(fc)
write.csv(as.data.frame(fc), "pronostico_nhtemp.csv")

g_fc <- autoplot(fc) +
  labs(title = paste0("Pronóstico de temperatura a ", h, " años - ARIMA(",
                      paste(orden, collapse = ","), ")"),
       x = "Año", y = "Temperatura (°F)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
guardar(g_fc, "08_pronostico.png")

# 10. DESEMPEÑO -------------------------------------------------
# 10.1 Trayectoria ajustada vs serie original
g_ajuste <- autoplot(nhtemp, series = "Serie original") +
  autolayer(fitted(mod), series = "Ajustado") +
  labs(title = "Serie vs. trayectoria ajustada", x = "Año", y = "°F", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
guardar(g_ajuste, "09_ajuste.png")

# Métricas dentro de muestra (ME, RMSE, MAE, MPE, MAPE, MASE)
print(accuracy(mod))

# 10.2 Partición entrenamiento / prueba (80% - 20%)
n <- length(nhtemp)
n_train <- round(0.80 * n)
train <- window(nhtemp, end = time(nhtemp)[n_train])
test  <- window(nhtemp, start = time(nhtemp)[n_train + 1])

mod_train <- Arima(train, order = orden)
fc_test   <- forecast(mod_train, h = length(test))

mape_test <- mean(abs((as.numeric(test) - as.numeric(fc_test$mean)) / as.numeric(test))) * 100
cat(sprintf("MAPE (prueba): %.2f%%\n", mape_test))
print(accuracy(fc_test, test))

g_test <- autoplot(fc_test) +
  autolayer(test, series = "Prueba real") +
  labs(title = "Evaluación out-of-sample",
       subtitle = sprintf("MAPE (prueba): %.2f%%", mape_test),
       x = "Año", y = "°F", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
guardar(g_test, "10_prueba_out_of_sample.png")

# 10.3 Validación cruzada con ventana expansiva (5 folds)
k_folds   <- 5
min_train <- floor(n * 0.50)
step      <- floor((n - min_train) / k_folds)
x <- as.numeric(nhtemp)
mapes <- numeric(k_folds)

for (i in 1:k_folds) {
  train_end <- min_train + (i - 1) * step
  test_end  <- if (i == k_folds) n else min_train + i * step
  tr <- x[1:train_end]
  te <- x[(train_end + 1):test_end]
  fit_i <- Arima(tr, order = orden)
  pr_i  <- forecast(fit_i, h = length(te))$mean
  mapes[i] <- mean(abs((te - as.numeric(pr_i)) / te)) * 100
}

print(data.frame(Fold = 1:k_folds, MAPE = round(mapes, 2)))
cat(sprintf("MAPE medio: %.2f%%  |  Desv. est.: %.2f%%\n", mean(mapes), sd(mapes)))

g_cv <- ggplot(data.frame(Fold = 1:k_folds, MAPE = mapes),
               aes(x = factor(Fold), y = MAPE)) +
  geom_col(fill = "lightblue", color = "white") +
  geom_hline(yintercept = mean(mapes), color = "firebrick", linetype = "dashed") +
  labs(title = "MAPE por fold (validación cruzada expansiva)",
       x = "Fold", y = "MAPE (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
guardar(g_cv, "11_validacion_cruzada.png")

cat("\nListo. Gráficos guardados en la carpeta img/\n")