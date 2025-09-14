
# a) (0.3/5) Con los datos anexos, realice un modelo de regresión lineal y concluya sobre el efecto marginal de cada una de las variables (edad y GPA) mediante una prueba adecuada.

  # 0) Importar datos de salario.txt
  
    datos <- read.table('salario.txt', header=TRUE)
    
    head(datos)
  
  # 1) Ajustar modelo multiple
    modelo_multiple <- lm(salario ~ GPA + Edad, data = datos)
  
  # 2) Resumen de modelos (coeficientes, t-test para significancia individual, R^2, F-test para significancia global)
    summary(modelo_multiple)
    
    modelo_multiple
    
    # 2.1) Hacer tabla ANOVA
    
      # Modelo reducido (Solo intercepto, porque queremos probar la hipotesis que todas las variables son insignificantes, osea 0)
      modelo_multiple_reducido <- lm(salario ~ 1, data = datos)
      
      # Hacer tabla ANOVA
      
      anova(modelo_multiple_reducido, modelo_multiple)
  
  # 3) Intervalos de confianza
    IC <- confint(modelo_multiple, level=0.95)
    IC
  
# b) (0.3/5) Realice un boxplot del GPA con respecto al género y con respecto al salario del recién egresado, para saber si pueden existir relaciones entre estas variables. ¿Cree que es importante incluir el género en el análisis?
  
  
  
  # GPA por genero
  
  boxplot(GPA ~ genero, data = datos, main = "Distribucion de GPA por género", xlab="Género", ylab="GPA")
  
  # Salario por genero
  
  boxplot(salario ~ genero, data = datos, main = "Distribucion de salario por género", xlab="Género", ylab="Salario")
