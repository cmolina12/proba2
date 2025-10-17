# Paquetes 
library(ggplot2)
library(readxl)

# Cargue su archivo Excel 
datos <- read_excel("Tarea3.xlsx", sheet = "Crecimiento")

head(datos)

# 2. Ajuste los modelos (debe escribir cada fórmula teniendo en cuenta los modelos que planteó en el literal anterior)

datos$h1 <- pmax(0, datos$dias-10)
datos$h2 <- pmax(0, datos$dias-20)

fit1 <-  lm(altura ~ dias + h1 + h2, data = datos) # modelo del analista junior 1

# PISTA: para este primer modelo debe crear nuevas variables que permitan asimilar los cambios de fase. Luego inclúyalas en la fórmula de fit1.

fit2 <-  lm(altura ~ dias, data = datos) # modelo del analista junior 2
fit3 <-  lm(altura ~ dias + I(dias^2)+I(dias^3), data = datos) # modelo del analista junior 3

# 3. Grilla de valores de "dias"
dias_grid <- seq(min(datos$dias), max(datos$dias), length.out = 200)

grid <- data.frame(dias = dias_grid)

# IMPORTANTE: debe incluir también las variables adicionales de fit1 en el dataframe "grid"
grid$h1 <- pmax(0, grid$dias-10)
grid$h2 <- pmax(0, grid$dias -20)

# 4. Predicciones
grid$fit1 <- predict(fit1, newdata = grid)
grid$fit2 <- predict(fit2, newdata = grid)
grid$fit3 <- predict(fit3, newdata = grid)

# 5. Gráfico comparativo
ggplot(datos, aes(x = dias, y = altura)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_line(data = grid, aes(y = fit1, color = "fit1"), size = 1) +
  geom_line(data = grid, aes(y = fit2, color = "fit2"), size = 1) +
  geom_line(data = grid, aes(y = fit3, color = "fit3"), size = 1) +
  geom_vline(xintercept = c(10, 20), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("fit1" = "blue",
                                "fit2" = "red",
                                "fit3" = "purple")) +
  labs(title = "Comparación de modelos",
       x = "Días", y = "Altura (cm)", color = "Modelo") +
  theme_minimal()

ggplot(datos, aes(x = dias, y = altura)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_line(data = grid, aes(y = fit1, color = "fit1"), size = 1) +
  #geom_line(data = grid, aes(y = fit2, color = "fit2"), size = 1) +
  #geom_line(data = grid, aes(y = fit3, color = "fit3"), size = 1) +
  geom_vline(xintercept = c(10, 20), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("fit1" = "blue",
                                "fit2" = "red",
                                "fit3" = "purple")) +
  labs(title = "Comparación de modelos",
       x = "Días", y = "Altura (cm)", color = "Modelo") +
  theme_minimal()

ggplot(datos, aes(x = dias, y = altura)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  #geom_line(data = grid, aes(y = fit1, color = "fit1"), size = 1) +
  geom_line(data = grid, aes(y = fit2, color = "fit2"), size = 1) +
  #geom_line(data = grid, aes(y = fit3, color = "fit3"), size = 1) +
  geom_vline(xintercept = c(10, 20), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("fit1" = "blue",
                                "fit2" = "red",
                                "fit3" = "purple")) +
  labs(title = "Comparación de modelos",
       x = "Días", y = "Altura (cm)", color = "Modelo") +
  theme_minimal()

ggplot(datos, aes(x = dias, y = altura)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  #geom_line(data = grid, aes(y = fit1, color = "fit1"), size = 1) +
  #geom_line(data = grid, aes(y = fit2, color = "fit2"), size = 1) +
  geom_line(data = grid, aes(y = fit3, color = "fit3"), size = 1) +
  geom_vline(xintercept = c(10, 20), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("fit1" = "blue",
                                "fit2" = "red",
                                "fit3" = "purple")) +
  labs(title = "Comparación de modelos",
       x = "Días", y = "Altura (cm)", color = "Modelo") +
  theme_minimal()


# Probar problemas de especifiacion

library(lmtest)

resettest(fit1)
resettest(fit2)
resettest(fit3)

plot(fit1$fitted.values, fit1$residuals)
abline(h=0, col="red")

plot(fit2$fitted.values, fit2$residuals)
abline(h=0, col="red")

plot(fit3$fitted.values, fit3$residuals)
abline(h=0, col="red")

# Comparacion de modelos

# Bondad de ajuste
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared

# Criterios de información
AIC(fit1, fit2, fit3)
BIC(fit1, fit2, fit3)