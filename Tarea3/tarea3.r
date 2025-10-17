library(car)
library(readxl)

# 1)

# Importar datos
datos_vivienda <- read_excel("Tarea3.xlsx", sheet="Viviendas")

# Verificar importacion
head(datos_vivienda)

# Convertir a factor por si acaso
datos_vivienda$Zona <- as.factor(datos_vivienda$Zona)

# Revisar los niveles
levels(datos_vivienda$Zona)

# Establecer "Residencial Media Densidad" como la base

datos_vivienda$Zona <- relevel(datos_vivienda$Zona, ref = "Residencial Media Densidad")

# Verificar que ahora la base apareza de primero
levels(datos_vivienda$Zona)

# a) Estimar el modelo propuesto

modelo_vivienda <- lm(precio ~ Zona + area + habitaciones + baños + areaGaraje, data = datos_vivienda)

summary(modelo_vivienda)

# b) Identificar problema de multicolinealidad

# Hacemos prueba VIF para ver si hay multicolinealidad en el modelo o no

vif(modelo_vivienda)

cor(datos_vivienda$area, datos_vivienda$areaGaraje)

plot(datos_vivienda$area, datos_vivienda$areaGaraje,
     xlab = "Área total", ylab = "Área de garaje",
     main = "Correlación alta entre área y área de garaje")
abline(lm(areaGaraje ~ area, data = datos_vivienda), col = "red")

# Aplicamos regresion Ridge para corregir el problema de autocolinealidad

library(glmnet)

X <- model.matrix(precio ~ Zona + area + habitaciones + baños + areaGaraje, data = datos_vivienda)[, -1]
y <- datos_vivienda$precio

set.seed(123)
cv_ridge <- cv.glmnet(x = X, y = y, alpha = 0, standardize = TRUE)

# λ óptimo
cv_ridge$lambda.min
cv_ridge$lambda.1se

# Ajuste final con el λ más parsimonioso
ridge_fit <- glmnet(x = X, y = y, alpha = 0, lambda = cv_ridge$lambda.1se, standardize = TRUE)

coef(ridge_fit)

# Predicciones dentro de la muestra (in-sample)
y_hat_ridge <- predict(ridge_fit, newx = X)

# Calcular RSS (suma de residuos al cuadrado)
rss <- sum((y - y_hat_ridge)^2)

# Calcular TSS (suma total de cuadrados)
tss <- sum((y - mean(y))^2)

# Calcular R² clásico
r2_ridge <- 1 - (rss / tss)

# Número de observaciones y predictores
n <- nrow(X)
p <- ncol(X)  # número de variables explicativas

# Calcular R² ajustado
r2_adj_ridge <- 1 - (1 - r2_ridge) * ((n - 1) / (n - p - 1))

# Mostrar resultados
r2_ridge
r2_adj_ridge


# Eliminar una variable dependiente para probar cual metodo es preferible

modelo_vivienda_recortado <- lm(precio ~ Zona + area + habitaciones + baños, data = datos_vivienda)
summary(modelo_vivienda_recortado)

