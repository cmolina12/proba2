library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)   
library(car)      
library(zoo)      

cafe <- read_excel("Tarea3.xlsx", 
                   sheet = "Cafe") |>
  mutate(
    Fecha = as.yearmon(Fecha, "%Y-%m"),  # 1980-1, 1980-2, ...
    Mes   = as.integer(Mes),
    Genero = factor(Genero),
    Sector = factor(Sector)
  ) |>
  arrange(Fecha)

reg <- lm(ingresos ~. - Fecha, data = cafe)
summary(reg)

# =======================  A)  =======================
# --- 1) Multicolinealidad ---
vif(reg)

# Los VIF de todas las variables están por debajo de 10.
# No hay relación lineal fuerte entre los regresores.

# --- 2) HETEROCEDASTICIDAD ---
bptest(reg)

# El estadístico de Breusch–Pagan arroja un p-value mayor a 0.05.
# Esto indica que no se rechaza la hipótesis nula de varianza constante en los errores.

# --- 3) AUTOCORRELACIÓN ---
dwtest(reg)

# El estadístico Durbin–Watson es inferior a 2 y su p-value < 0.05.
# Se rechaza la hipótesis nula de independencia de los errores, indicando autocorrelación positiva.

# --- 4) ERROR DE ESPECIFICACIÓN ---
resettest(reg, power = 2:3, type = "fitted")

# La prueba RESET de Ramsey arroja un p-value superior a 0.05.
# No se rechaza la hipótesis nula de correcta especificación del modelo.

# --- Gráficos para el diagnóstico ---
res <- resid(reg)
cafe$e <- res
cafe <- dplyr::arrange(cafe, Fecha)
cafe$e_lag <- dplyr::lag(cafe$e, 1)

ggplot(cafe, aes(Fecha, e)) + geom_line() + geom_point() +
  labs(title = "Residuos vs tiempo", x = "Fecha", y = "e_t")

ggplot(dplyr::filter(cafe, !is.na(e_lag)), aes(e_lag, e)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "e_t vs e_{t-1}", x = "e_{t-1}", y = "e_t")

# =======================  B)  =======================
res  <- residuals(reg)
res1 <- dplyr::lag(res) 
n <- length(res)

coef <- cor(res[2:n], res1[2:n], use = "complete.obs")
coef

# Se detecta autocorrelación de orden 1 con rho_hat alto.
# Solución propuesta: Transformación de Cochrane–Orcutt.

ynew <- c()
P_new <- c()
F_new <- c()
E_new <- c()
M_new <- c()

for (i in 2:nrow(cafe)) {
  ynew[i-1] <- cafe$ingresos[i]      - coef * cafe$ingresos[i-1]
  P_new[i-1] <- cafe$Precipitacion[i] - coef * cafe$Precipitacion[i-1]
  F_new[i-1] <- cafe$Followers[i]     - coef * cafe$Followers[i-1]
  E_new[i-1] <- cafe$Estado[i]        - coef * cafe$Estado[i-1]
  M_new[i-1] <- cafe$EstadoMaq[i]     - coef * cafe$EstadoMaq[i-1]
}

datosTransformados <- data.frame(
  ynew, P_new, F_new, E_new, M_new,
  Genero = cafe$Genero[-1],
  Sector = cafe$Sector[-1]
)

head(datosTransformados)

cor(datosTransformados$ynew[-1], dplyr::lag(datosTransformados$ynew)[-1], use = "complete.obs")

cat("Autocorrelación inicial:", round(coef, 3), "\n")
cat("Transformación Cochrane–Orcutt aplicada con éxito.")

# =======================  C)  =======================
# Estimación del modelo corregido
datosTransformados <- data.frame(
  ynew, P_new, F_new, E_new, M_new,
  Genero = cafe$Genero[-1],
  Sector = cafe$Sector[-1],
  Mes = cafe$Mes[-1]
)

reg_corregido <- lm(ynew ~ P_new + F_new + E_new + M_new + Genero + Sector + Mes,
                    data = datosTransformados)
summary(reg_corregido)

dwtest(reg_corregido)

# --- 1) Multicolinealidad ---
vif(reg_corregido)

# --- 2) HETEROCEDASTICIDAD ---
bptest(reg_corregido)

# --- 3) AUTOCORRELACIÓN ---
dwtest(reg_corregido)

# --- 4) ERROR DE ESPECIFICACIÓN ---
resettest(reg_corregido, power = 2:3, type = "fitted")

# No hay relacion lineal fuerte entre los regresores.
# No se rechaza ninguna de las hipotesis nulas, por ende no hay problemas.

# Comparación entre el modelo original y el modelo corregido

comparativo <- data.frame(
  Modelo = c("Original (OLS)", "Corregido (Cochrane–Orcutt)"),
  R2_Ajustado = c(summary(reg)$adj.r.squared,
                  summary(reg_corregido)$adj.r.squared),
  DW = c(as.numeric(dwtest(reg)$statistic),
         as.numeric(dwtest(reg_corregido)$statistic))
)
comparativo

# El R2 ajustado aumentó de 0.6840 a 0.7699, mostrando un mejor ajuste del modelo.
# El estadístico Durbin–Watson pasó de 0.44 a 2.04, indicando que la autocorrelación fue eliminada.
# Los coeficientes mantienen sus signos y la mayoría su significancia,
# lo que demuestra estabilidad del modelo tras la corrección.

# En conclusión, el modelo corregido resuelve el problema detectado en (a),
# cumple con todos los supuestos del modelo lineal clásico
# y produce estimaciones más eficientes y confiables.