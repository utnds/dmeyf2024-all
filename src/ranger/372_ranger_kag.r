# Ranger  una librería que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")
require("ranger")
require("randomForest") # solo se usa para imputar nulos

# Definir el archivo de parámetros
param_file <- "/home/sebastiancendra/buckets/b1/exp/HT3740/HT3740.txt"

# Leer y filtrar los primeros 20 parámetros por ganancia
param_data <- fread(param_file)
param_data <- param_data[order(-ganancia)][1:20]

# Establezco el Working Directory
setwd("~/buckets/b1/")

# Creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA3720/")
dir.create(carpeta_experimento, showWarnings = FALSE)
setwd(carpeta_experimento)

# Cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# Cargo los datos
dataset <- fread(miAmbiente$dataset_pequeno)

# Asigno valores negativos para las columnas con nulos
if ("Master_Finiciomora" %in% colnames(dataset)) {
  dataset[is.na(Master_Finiciomora), Master_Finiciomora := -999]
}
if ("Visa_Finiciomora" %in% colnames(dataset)) {
  dataset[is.na(Visa_Finiciomora), Visa_Finiciomora := -999]
}

# Defino los conjuntos de entrenamiento y aplicación
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# Imputo los nulos en dtrain y ranger necesita la clase de tipo factor
dtrain <- na.roughfix(dtrain)
dtrain[, clase_ternaria := as.factor(clase_ternaria)]
setorder(dtrain, clase_ternaria)

# Imputo nulos en dapply y elimino la columna de clase
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)

# Iterar sobre los 20 primeros parámetros y hacer submits a Kaggle
for (i in 1:nrow(param_data)) {
  # Extraer hiperparámetros para esta iteración
  num_trees <- param_data[i, num_trees]
  mtry <- param_data[i, mtry]
  min_node_size <- param_data[i, min_node_size]
  max_depth <- param_data[i, max_depth]
  
  # Generar el modelo de Random Forest con los hiperparámetros del archivo
  modelo <- ranger(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    probability = TRUE,
    num.trees = num_trees,
    mtry = mtry,
    min.node.size = min_node_size,
    max.depth = max_depth
  )
  
  # Aplicar el modelo a dapply
  prediccion <- predict(modelo, dapply)
  
  # Generar la entrega para Kaggle
  entrega <- as.data.table(list(
    "numero_de_cliente" = dapply[, numero_de_cliente],
    "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
  ))
  
  # Crear el nombre del archivo de Kaggle con el número de iteración
  nom_arch_kaggle <- paste0("KA3720_", sprintf("%03d", i), ".csv")
  
  # Guardar la entrega
  fwrite(entrega, file = nom_arch_kaggle, sep = ",")
  
  # Preparar y ejecutar el comando para subir a Kaggle
  comentario <- paste0(
    "'num.trees=", num_trees, 
    " mtry=", mtry, 
    " min.node.size=", min_node_size, 
    " max.depth=", max_depth, "'"
  )
  
  comando <- paste0(
    "~/install/proc_kaggle_submit.sh TRUE ", 
    miAmbiente$modalidad, " ", 
    nom_arch_kaggle, " ", 
    comentario
  )
  
  ganancia <- system(comando, intern = TRUE)
  
  # Registrar la ganancia y el archivo generado
  cat(paste0(ganancia, "\t", nom_arch_kaggle, "\n"), file = "tb_ganancias.txt", append = TRUE)
}
