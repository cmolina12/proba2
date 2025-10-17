library(knitr) # Tablas markdown
# Problema 1
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
      
        linearHypothesis(modelo_genero, c("GPA = 0","generoMujer = 0"), test = "F")
        
  # d) (0.3/5) Además del consultor político, otro experto cultural explica que el fenómeno en estos municipios es que a las mujeres no se les valora el esfuerzo académico, mientras que a los hombres si se les reconoce en términos de salario. Usando un modelo adecuado, prueba si la afirmación del experto es correcta o no.
        
    # Modelo con interaccion
        
        modelo_interaccion = lm(salario ~ Edad + GPA*genero, data = datos)
        
        summary(modelo_interaccion)
        
        # Prueba para mujeres: beta_GPA + beta_GPA:generoMujer = 0
        linearHypothesis(modelo_interaccion, c("GPA + GPA:generoMujer = 0"), test = "F")
        
        # Prueba para hombres: beta_GPA <= 0
        
        n = nobs(modelo_interaccion)
        p = length(coef(modelo_interaccion)) 
          
        c = c(0,0,1,0,0)
        
        estimador <- t(c)%*%coef(modelo_interaccion)
        varianza <- t(c)%*%vcov(modelo_interaccion)%*%c
        t_critico <- qt(0.95, n-p) # Prueba de cola derecha
        intervalo_1 <- estimador - t_critico * sqrt(varianza)
        intervalo_2 <- estimador + t_critico * sqrt(varianza)
        intervalo_1
        intervalo_2
        
# Problema 2
        
  # b) Presente la ecuación general y estime un modelo de regresión lineal con intercepto e interprete cada uno de los coeficientes estimados. Recuerde elegir una base específica. ¿Existe alguna relación entre los coeficientes estimados y las estadísticas descriptivas? Si es así, indique cuál es la relación.
        
    library(readxl)
        
    # 0) Leer la hoja 'Restaurantes' del Excel
    
      datos_restaurantes <- read_excel("DatosTarea2.xlsx", sheet="Restaurantes")
      
      head(datos_restaurantes)
      
      # Convertir ciudad a un factor
      
      datos_restaurantes$Programa <- factor(datos_restaurantes$Programa)
      
      # Confirmar niveles
      
      levels(datos_restaurantes$Programa)
      
      # Definir 'Bogotá' como nuestra base
      
      datos_restaurantes$Programa <- relevel(datos_restaurantes$Programa, ref = "Bogotá")
      
  # 1) Ajustar el modelo de regresion
      
      modelo_restaurantes <- lm(Calificación ~ Programa, data = datos_restaurantes)
      
      summary(modelo_restaurantes)
        
      # 1.1) Intervalo de confianza
      
      confint(modelo_restaurantes, level = 0.95)
      
  # c) Presente la ecuación general y estime un modelo de regresión lineal sin intercepto e interprete cada uno de los coeficientes estimados. ¿Existe alguna relación entre los coeficientes estimados y las estadísticas descriptivas? Si es así, indique cuál es la relación.
      
      # 0) Ya datos_restaurantes se le aplico el factor para las variables categoricas, en este caso solo tenemos que implementar el modelo sin intercepto.
      
      modelo_sin_intercepto = lm(Calificación ~ 0 + Programa, data = datos_restaurantes)
      
      # 1) Modelo de regresion lineal sin intercepto
      
      summary(modelo_sin_intercepto)
      
      # 2) Intervalo de confianza
      
      confint(modelo_sin_intercepto, level = 0.95)
      
# Problema 3
      
  # a) Estime un modelo de regresión lineal utilizando todas las variables proporcionadas por la agencia de viajes. Analice la significancia global del modelo y la significancia individual de cada una de las variables explicativas incluidas.
      
      # 0) Importar datos del Excel 'DatosTarea2.xlsx' y la hoja 'EventPlus'
      
        # Importar libreria para leer el Excel
        library(readxl)
        
        # Leer el Excel y la hoja EventPlus
        datos_eventos = read_excel('DatosTarea2.xlsx', sheet="EventPlus ")
        
        # Confirmar importacion correcta de datos
        head(datos_eventos)
      
        # Variables categoricas
        
          # Cliente frecuente
        
          datos_eventos$`Cliente frecuente` <- factor(datos_eventos$`Cliente frecuente`)
          
          datos_eventos$`Cliente frecuente` <- relevel(datos_eventos$`Cliente frecuente`, ref="No")
        
          levels(datos_eventos$`Cliente frecuente`)
          
          # Ubicacion del evento
          
          datos_eventos$`Ubicación del evento` <- factor(datos_eventos$`Ubicación del evento`)
      
          datos_eventos$`Ubicación del evento` <- relevel(datos_eventos$`Ubicación del evento`, ref="Nacional")
          
          levels(datos_eventos$`Ubicación del evento`)
          
          # Tipo de recinto
          
          datos_eventos$`Tipo de Recinto` <- factor(datos_eventos$`Tipo de Recinto`)
          
          datos_eventos$`Tipo de Recinto` <- relevel(datos_eventos$`Tipo de Recinto`, ref = "Hotel")
          
          levels(datos_eventos$`Tipo de Recinto`)
          
          # Catering
          
          datos_eventos$`Incluye catering` <- factor(datos_eventos$`Incluye catering`)
          
          datos_eventos$`Incluye catering` <- relevel(datos_eventos$`Incluye catering`, ref = "No")
          
          levels(datos_eventos$`Incluye catering`)
          
          # Epoca del año
          
          datos_eventos$`Época del año` <- factor(datos_eventos$`Época del año`)
          
          datos_eventos$`Época del año` <- relevel(datos_eventos$`Época del año`, ref = "Regular")
          
          levels(datos_eventos$`Época del año`)
      
      # 1) Significancia individual y global
          
      modelo_eventplus <- lm(`Satisfacción Global` ~ ., data = datos_eventos)
      
      summary(modelo_eventplus)
      
      modelo_reducido <- lm(`Satisfacción Global` ~ 1, data = datos_eventos)

  # b) Con base en los resultados obtenidos en el punto anterior, determine estadísticamente si es necesario excluir de manera simultánea todas las variables que resultaron no significativas en el modelo.
      
      modelo_eventplus_reducido <- lm(`Satisfacción Global` ~ `Duración del evento`+`Contratiempos reportados`+`Número de actividades` +`Incluye catering`+`Tiempo de Respuesta` +`Calificación del ponente principal`, data = datos_eventos)
      
      anova(modelo_eventplus_reducido, modelo_eventplus)
      
  # c) 
      
      # Modelo 1 - La Satisfacción Global explicada por la frecuencia del cliente, si el plan incluye catering, y el tiempo de respuesta.
      
      modelo_1 <- lm(`Satisfacción Global` ~ `Cliente frecuente` + `Incluye catering` + `Tiempo de Respuesta`, data = datos_eventos)
      
      
      # Modelo 2 - La Satisfacción Global explicada por los contratiempos reportados, el número de actividades, la duración del evento y la calificación del ponente principal.
      
      modelo_2 <- lm(`Satisfacción Global` ~ `Contratiempos reportados` + `Número de actividades` + `Duración del evento` + `Calificación del ponente principal`, data = datos_eventos)
      
      # Funcion para sacar R2 ajustado manualmente
      
      n = nrow(datos_eventos)
      
      get_R2ajustado <- function(modelo) {
        
        RSS <- sum(residuals(modelo)^2)
        TSS <- sum( (datos_eventos$`Satisfacción Global`-mean(datos_eventos$`Satisfacción Global`))^2 )
        k <- length(coef(modelo))-1
        R2ajustado <- 1- (RSS/(n-k-1))/(TSS/(n-1))
        return(R2ajustado)
        
        
      } 
      
      R2_modelo_1 <- get_R2ajustado(modelo_1)
      R2_modelo_2 <- get_R2ajustado(modelo_2)
      
      R2_modelo_1
      R2_modelo_2
      
  # d)
      
      # Calculo con linear hypothesis
      
      linearHypothesis(modelo_eventplus_reducido, c(
        "`Duración del evento` = `Número de actividades`"),
        test = "F"
      )
      
      # Calculo manual como combinacion lineal
      
        # Coeficientes
      
        coeficientes <- coef(modelo_eventplus_reducido)
        names(coeficientes)
        
        # Vector de coeficientes
        c <- c(
          0,
          1,
          0,
          -1,
          0
          ,0
          ,0
        )
        
        # simbolo
        valor_combinacion <- 0
        
        # (X(t)X)-1
        
        X <- model.matrix(modelo_eventplus_reducido)
        head(X)
        XtXinv <- solve(t(X)%*%X)
        
        # MSE
        res <- residuals(modelo_eventplus_reducido)
        RSS <- sum(res^2)
        
        n <- nobs(modelo_eventplus_reducido)
        p <- length(coef(modelo_eventplus_reducido))
        
        MSE = RSS/(n-p)
        
        # Numerador
        numerador <- as.numeric(  t(c)%*%coeficientes-valor_combinacion            )
        
        #Denominador
        denominador <- as.numeric ( sqrt( t(c)%*%(MSE*XtXinv  ) %*% c      )            )
        
        # ep
        
        ep = numerador / denominador
        
        # t critico de desigualdad (dos colas)
        
        t_crit <- qt(0.975, n-p)
        
        ep
        t_crit
        
  # e) Haciendo uso del modelo obtenido en el literal (b), determine si la media del nivel de satisfacción global de los participantes supera los 7.5 puntos bajo las siguientes condiciones específicas:
        
        coeficientes <- coef(modelo_eventplus_reducido)
        names(coeficientes)
        
        c <- c(
          1,
          3,
          2,
          8,
          0,
          6,
          7.8
        )
        
        estimador = t(c) %*% coeficientes
        
        estimador
        
        varianza = t(c)%*%vcov(modelo_eventplus_reducido)%*%c
        
        t_critico = qt(0.95, n-p)
        
        IC_1 = estimador - t_critico * sqrt(varianza)
        IC_2 = estimador + t_critico * sqrt(varianza)
        
        IC_1
        IC_2
        
        estadistico_prueba <- (estimador - 7.5)/sqrt(varianza)
        
        estadistico_prueba
        t_critico
        
  # f) 
        c = c(
          1,
          4,
          3,
          6,
          1,
          5,
          8.2
          
        )
        
        estimador <- t(c)%*%coeficientes
        
        RSS <- sum(residuals(modelo_eventplus_reducido)^2)
        
        MSE <- RSS/(n-p)
        
        
        varianza <- t(c)%*%(vcov(modelo_eventplus_reducido))%*%c
        
        varianza_mse <- varianza + MSE
        
        t_critico <- qt(0.975, n-p)
        
        IP_1 <- estimador - t_critico * sqrt(varianza_mse)
        IP_2 <- estimador + t_critico * sqrt(varianza_mse)
        
        IP_1
        IP_2
        
        
        
        
        
        
        
      