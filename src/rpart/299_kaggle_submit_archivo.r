# Script prueba de submit Kaggle 

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")

PARAM <- list()

PARAM$modalidad <- "conceptual"

# Leer archivo de parámetros
param_file <- "/home/sebastiancendra/buckets/b1/exp/HT2810/kag2.txt"
param_data <- fread(param_file)

# Función para incrementar el contador y leer/escribir YAML
getandincrement <- function(nom_archivo) {
  contador <- read_yaml(nom_archivo)
  valor <- contador$contador
  contador$contador <- contador$contador + 1L
  write_yaml(contador, file=nom_archivo)
  return(valor)
}

# Función para generar el modelo y realizar el submit
generarmodelo <- function(pmodalidad, param) {
  dataset <- fread(paste0("~/datasets/", pmodalidad, "_dataset_pequeno.csv"))
  dtrain <- dataset[foto_mes == 202107]
  dapply <- dataset[foto_mes == 202109]
  
  modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    xval = 0,
    control = param
  )
  
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )
  
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  contador <- getandincrement("contador.yml")
  archivo_submit <- paste0("K100_", pmodalidad, "_", sprintf("%.3d", contador), ".csv")
  
  fwrite(dapply[, list(numero_de_cliente, Predicted)], file=archivo_submit, sep=",")
  
  comentario <- paste0("'",
                       "cp=", param$cp,
                       " minsplit=", param$minsplit,
                       " minbucket=", param$minbucket,
                       " maxdepth=", param$maxdepth,
                       "'")
  
  comando <- paste0("~/install/proc_kaggle_submit.sh ",
                    "TRUE ",
                    pmodalidad, " ",
                    archivo_submit, " ",
                    comentario)
  
  ganancia <- system(comando, intern=TRUE)
  cat(paste0(ganancia, "\t", archivo_submit, "\n"), file="tb_ganancias.txt", append=TRUE)
  
  return(as.numeric(ganancia))
}

# Main
dir.create("~/buckets/b1/exp/KA2000", showWarnings=FALSE)
setwd("~/buckets/b1/exp/KA2000")

if (!file.exists("contador.yml")) {
  contador <- list("contador" = 1L)
  write_yaml(contador, file="contador.yml")
}

# Iterar sobre cada fila de los parámetros y ejecutar el modelo
for (i in 1:nrow(param_data)) {
  PARAM$rpart <- list(
    "cp" = param_data$cp[i],
    "minsplit" = param_data$minsplit[i],
    "minbucket" = param_data$minbucket[i],
    "maxdepth" = param_data$maxdepth[i]
  )
  
  # Ejecutar el modelo con los parámetros actuales
  generarmodelo(PARAM$modalidad, PARAM$rpart)
}

