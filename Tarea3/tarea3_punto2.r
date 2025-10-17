library(readxl)
library(car)
library(lmtest)
library(sandwich)

# a)

# Importar datos de la hoja Innovación

datos_startups <- read_excel("Tarea3.xlsx", sheet="Innovación")

# Verificar importacion correcta de datos

head(datos_startups)

modelo_startups <- lm(tasa ~ Inversion + Calidad + Experiencia + HorasEntrenamiento + Networking, 
                      data = datos_startups)

summary(modelo_startups)


# 1) Analisis de multicolinealidad

cor(datos_startups[, c("Inversion", "Calidad", "Experiencia", "HorasEntrenamiento", "Networking")])

vif(modelo_startups)

# 2) Analisis de heterocedasticidad

par(mfrow=c(2,3))
plot(modelo_startups$fitted.values, residuals(modelo_startups)^2,
     main="Residuos² vs Valores ajustados")

plot(datos_startups$Inversion, residuals(modelo_startups)^2,
     main="Residuos² vs Inversión")

plot(datos_startups$Calidad, residuals(modelo_startups)^2,
     main="Residuos² vs Calidad")

plot(datos_startups$Experiencia, residuals(modelo_startups)^2,
     main="Residuos² vs Experiencia")

plot(datos_startups$HorasEntrenamiento, residuals(modelo_startups)^2,
     main="Residuos² vs Horas Entrenamiento")

plot(datos_startups$Networking, residuals(modelo_startups)^2,
     main="Residuos² vs Networking")

bptest(modelo_startups)

bptest(modelo_startups, ~ Inversion, data = datos_startups)
bptest(modelo_startups, ~ HorasEntrenamiento, data = datos_startups)
bptest(modelo_startups, ~ Calidad, data = datos_startups)
bptest(modelo_startups, ~ Experiencia, data = datos_startups)
bptest(modelo_startups, ~ Networking, data = datos_startups)




# 3) Analisis de especificación del modelo

resettest(modelo_startups)

# 4) Análisis de autocorrelación

dwtest(modelo_startups)

# b) Correcion de heterocedasticidad

# Modelo original
modelo_startups <- lm(tasa ~ Inversion + Calidad + Experiencia + HorasEntrenamiento + Networking, data = datos_startups)

# 1. Modelo auxiliar de los residuos al cuadrado
modelo_aux <- lm(log(residuals(modelo_startups)^2) ~ Inversion + Calidad + Experiencia + HorasEntrenamiento + Networking, data = datos_startups)

# 2. Estimar varianzas y pesos
sigma2_est <- exp(fitted(modelo_aux))
weights <- 1 / sigma2_est

# 3. Reestimar el modelo ponderado
modelo_wls <- lm(tasa ~ Inversion + Calidad + Experiencia + HorasEntrenamiento + Networking,
                 data = datos_startups, weights = weights)

# 4. Resultados
summary(modelo_wls)

plot(modelo_startups$fitted.values, residuals(modelo_startups)^2,
     main = "Residuos² vs valores ajustados (OLS)",
     xlab = "Valores ajustados", ylab = "Residuos²")
abline(h = 0, col = "red")

plot(modelo_wls$fitted.values, residuals(modelo_wls)^2,
     main = "Residuos² vs valores ajustados (WLS)",
     xlab = "Valores ajustados", ylab = "Residuos²")
abline(h = 0, col = "red")

bptest(modelo_wls)


