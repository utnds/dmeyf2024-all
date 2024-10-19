# Limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("yaml")
require("caret") # Para la estratificación

# Parametros experimento
PARAM <- list()
PARAM$experimento <- 3610

# Parametros rpart
PARAM$rpart <- data.table( 
  "cp" = -1,
  "minsplit" = 25,
  "minbucket" = 10,
  "maxdepth" = 4
)

# Parametros del arbol
PARAM$feature_fraction <- 0.5
PARAM$num_trees_max <- 512

# Defino en qué iteraciones se graba
grabar <- c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)  # Grabo estos árboles

#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# Cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# Cargo los datos
dataset <- fread(miAmbiente$dataset_pequeno)

# Creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(carpeta_experimento, showWarnings = FALSE)
setwd(carpeta_experimento)

# Estratificación del dataset 202107 para generar dtrain y dapply
set.seed(123)  # Establezco semilla para reproducibilidad
dtrain_index <- createDataPartition(dataset[foto_mes == 202107]$clase_ternaria, 
                                    p = 0.5, list = FALSE)  # 50% para entrenamiento
dtrain <- dataset[foto_mes == 202107][dtrain_index]
dapply <- dataset[foto_mes == 202107][-dtrain_index]

# Arreglo clase_ternaria por algún distraído
dapply[, clase_ternaria := NA]

# Elimino lo que ya no utilizo
rm(dataset)
gc()

# Establezco los campos que puedo usar para la predicción
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))

# Genero las salidas
for (icorrida in seq(nrow(PARAM$rpart))) {
  
  cat("Corrida ", icorrida, " ; \n")
  
  # Acumulo la probabilidad del ensemble
  dapply[, prob_acumulada := 0]
  
  # Parámetros para rpart
  param_rpart <- PARAM$rpart[icorrida]
  
  set.seed(miAmbiente$semilla_primigenia)
  
  for (arbolito in seq(PARAM$num_trees_max)) {
    qty_campos_a_utilizar <- as.integer(length(campos_buenos) * PARAM$feature_fraction)
    campos_random <- sample(campos_buenos, qty_campos_a_utilizar)
    campos_random <- paste(campos_random, collapse = " + ")
    formulita <- paste0("clase_ternaria ~ ", campos_random)
    
    # Genero el árbol de decisión
    modelo <- rpart(formulita, data = dtrain, xval = 0, control = param_rpart)
    
    # Aplico el modelo a los datos que no tienen clase
    prediccion <- predict(modelo, dapply, type = "prob")
    dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]
    
    if (arbolito %in% grabar) {
      # Genero la entrega
      umbral_corte <- (1 / 40) * arbolito
      entrega <- as.data.table(list(
        "numero_de_cliente" = dapply[, numero_de_cliente],
        "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
      ))
      
      nom_arch_kaggle <- paste0(
        "KA", PARAM$experimento, "_",
        icorrida, "_",
        sprintf("%.3d", arbolito), ".csv"
      )
      
      # Calculo la ganancia (aquí deberías definir cómo se calcula la ganancia)
      ganancia <- sum(entrega$Predicted) * 20000  # Este es un ejemplo
      cat("Ganancia: ", ganancia, "\t", arbolito, "\n")
    }
  
  }
}
