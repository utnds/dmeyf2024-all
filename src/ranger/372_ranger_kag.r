# Librerias necesarias
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

# Convertir los valores necesarios a numéricos (por si fueron leídos como texto)
param_data[, `:=`(
  num.trees = as.numeric(num.trees),
  max.depth = as.numeric(max.depth),
  min.node.size = as.numeric(min.node.size),
  mtry = as.numeric(mtry)
)]

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

# Ranger necesita la clase de tipo factor
dtrain[, clase_ternaria := as.factor(clase_ternaria)]

# Convertir las columnas de dtrain a numéricas o factores
# Se excluye la columna 'clase_ternaria' ya que es un factor
cols_to_convert <- setdiff(colnames(dtrain), "clase_ternaria")
dtrain[, (cols_to_convert) := lapply(.SD, function(x) {
  if (is.character(x)) {
    as.factor(x)  # Convertir a factor si es texto
  } else {
    as.numeric(x)  # Convertir a numérico si es otro tipo
  }
}), .SDcols = cols_to_convert]

# Imputo los nulos en dtrain
dtrain <- na.roughfix(dtrain)

# Imputo nulos en dapply y elimino la columna de clase
dapply[, clase_ternaria := NULL]
cols_to_convert_apply <- colnames(dapply)
dapply[, (cols_to_convert_apply) := lapply(.SD, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    as.numeric(x)
  }
}), .SDcols = cols_to_convert_apply]

# Imputo los nulos en dapply
dapply <- na.roughfix(dapply)

# Función para entrenar y hacer la subida a Kaggle
train_and_submit <- function(param_row, iter_num, PARAM) {
  # Reemplazo los hiperparámetros de Random Forest con los valores del archivo
  PARAM$ranger <- list(
    "num.trees" = as.numeric(param_row$num.trees),
    "mtry" = as.numeric(param_row$mtry),
    "min.node.size" = as.numeric(param_row$min.node.size),
    "max.depth" = as.numeric(param_row$max.depth)
  )
  
  # Genero el modelo de Random Forest con los hiperparámetros del archivo
  modelo <- ranger(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    probability = TRUE,
    num.trees = PARAM$ranger$num.trees,
    mtry = PARAM$ranger$mtry,
    min.node.size = PARAM$ranger$min.node.size,
    max.depth = PARAM$ranger$max.depth
  )
  
  # Aplicar el modelo a dapply
  prediccion <- predict(modelo, dapply)
  
  # Generar la entrega para Kaggle
  entrega <- as.data.table(list(
    "numero_de_cliente" = dapply[, numero_de_cliente],
    "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
  ))
  
  # Crear el nombre del archivo de Kaggle con el número de iteración
  nom_arch_kaggle <- paste0("KA3720_", sprintf("%03d", iter_num), ".csv")
  
  # Guardar la entrega
  fwrite(entrega, file = nom_arch_kaggle, sep = ",")
  
  # Preparar y ejecutar el comando para subir a Kaggle
  comentario <- paste0(
    "'num.trees=", PARAM$ranger$num.trees, 
    " mtry=", PARAM$ranger$mtry, 
    " min.node.size=", PARAM$ranger$min.node.size, 
    " max.depth=", PARAM$ranger$max.depth, "'"
  )
  
  comando <- paste0(
    "~/install/proc_kaggle_submit.sh TRUE ", 
    miAmbiente$modalidad, " ", 
    nom_arch_kaggle, " ", 
    comentario
  )
  
  # Ejecutar el comando para subir la predicción y capturar la ganancia
  ganancia <- system(comando, intern = TRUE)
  
  # Registrar la ganancia y el archivo generado
  cat(paste0(ganancia, "\t", nom_arch_kaggle, "\n"), file = "tb_ganancias.txt", append = TRUE)
}

# Iterar sobre los 20 primeros parámetros y hacer submits a Kaggle
for (i in 1:nrow(param_data)) {
  # Inicializar los parámetros antes de cada iteración
  PARAM <- list()
  PARAM$experimento <- 3720
  PARAM$ranger <- list()  # Se actualizará en la función
  
  train_and_submit(param_data[i], i, PARAM)
}
