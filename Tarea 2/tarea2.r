library(knitr) # Tablas markdown

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
  
  # 0) Crear vector de genero
    
  genero <- c(rep('Hombre',86), rep('Mujer',89))
  
  datos$genero <- factor(genero, levels=c('Hombre','Mujer'))
  
  # 1) GPA por genero
  
  boxplot(GPA ~ genero, data = datos, main = "Distribucion de GPA por género", xlab="Género", ylab="GPA")
  
  # 2) Salario por genero
  
  boxplot(salario ~ genero, data = datos, main = "Distribucion de salario por género", xlab="Género", ylab="Salario")

# c) (0.4/5) En un modelo que incluye el género, estime el efecto de GPA y el efecto del género. 
  
  # 0) Realizar la regresion lineal multiple con genero, edad y GPA
  
  modelo_genero <- lm(salario ~ genero + Edad + GPA, data = datos)
  
  # 1) Resumen de modelos (coeficientes, t-test para significancia individual, R^2, F-test para significancia global)
  
  summary(modelo_genero)
  
  # 2) Intervalo de confianza para Edad, GPA y generoMujer
  confint(modelo_genero, level=0.95)
  
  # 3) Prueba conjunta
    
    # 3.1) Prueba conjunta con modelos anidados
    
      modelo_genero_restringido <- lm(salario ~ Edad, data = datos)
    
      anova(modelo_genero_restringido, modelo_genero)
    
    # 3.2) Prueba conjunta con restricciones lineales
      
      library(car)
    
      linearHypothesis(modelo_genero, c("GPA = 0","generoMujer = 0"))
      
# d) (0.3/5) Además del consultor político, otro experto cultural explica que el fenómeno en estos municipios es que a las mujeres no se les valora el esfuerzo académico, mientras que a los hombres si se les reconoce en términos de salario. Usando un modelo adecuado, prueba si la afirmación del experto es correcta o no.
      
  # Modelo con interaccion
      
      modelo_interaccion = lm(salario ~ Edad + GPA*genero, data = datos)
      
      summary(modelo_interaccion)
      
      # Prueba conjunta: (i) beta_GPA = 0 y (ii) beta_GPA + beta_GPA:generoMujer = 0
      linearHypothesis(modelo_interaccion, c("GPA = 0", "GPA + GPA:generoMujer = 0"))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      