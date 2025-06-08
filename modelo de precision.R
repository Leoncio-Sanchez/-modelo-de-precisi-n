# Paso 1: Carga la librería
library(nnet)

# Paso 2: Normaliza las columnas predictoras
diabetes$pregnancies <- scale(diabetes$pregnancies)
diabetes$glucose <- scale(diabetes$glucose)
diabetes$bloodpressure <- scale(diabetes$bloodpressure)
diabetes$skinthickness <- scale(diabetes$skinthickness)
diabetes$insulin <- scale(diabetes$insulin)
diabetes$bmi <- scale(diabetes$bmi)
diabetes$diabetespedigreefunction <- scale(diabetes$diabetespedigreefunction)
diabetes$age <- scale(diabetes$age)

# Paso 3: Crear el modelo
modelo <- nnet(outcome ~ pregnancies + glucose + bloodpressure + skinthickness +
                 insulin + bmi + diabetespedigreefunction + age,
               data = diabetes,
               size = 9,
               maxit = 500,
               decay = 0.01,
               linout = FALSE)

# Paso 4: Predicciones (como probabilidades)
probabilidades <- predict(modelo, diabetes, type = "raw")

# Paso 5: Clasificación binaria (0 si < 0.5, 1 si >= 0.5)
predicciones_clasificadas <- ifelse(probabilidades >= 0.5, 1, 0)

# Paso 6: Tabla de confusión
tabla <- table(Predicho = predicciones_clasificadas, Real = diabetes$outcome)
print(tabla)

# Paso 7: Precisión del modelo
Precision <- sum(diag(tabla)) / sum(tabla)
cat("Precisión del modelo:", round(Precision * 100, 2), "%\n")
