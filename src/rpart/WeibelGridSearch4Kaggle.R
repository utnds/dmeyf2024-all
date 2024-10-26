# cargo las librerías que necesito
require("data.table")
require("rpart")
require("yaml")

# Defino la modalidad
PARAM <- list()
PARAM$modalidad <- "vivencial"  # "conceptual"

# Defino las combinaciones de hiperparámetros
param_combinations <- list(
  list(cp = 0.01, minsplit = 100, minbucket = 10, maxdepth = 6),
  list(cp = 0.02, minsplit = 150, minbucket = 15, maxdepth = 8),
  list(cp = 0.03, minsplit = 200, minbucket = 20, maxdepth = 10),
  list(cp = 0.04, minsplit = 250, minbucket = 25, maxdepth = 12),
  list(cp = 0.05, minsplit = 300, minbucket = 30, maxdepth = 14),
  list(cp = 0.06, minsplit = 350, minbucket = 35, maxdepth = 16)
)

#------------------------------------------------------------------------------

getandincrement <- function(nom_archivo) {
  contador <- read_yaml(nom_archivo)
  valor <- contador$contador
  contador$contador <- contador$contador + 1L
  write_yaml(contador, file = nom_archivo)
  return(valor)
}
#------------------------------------------------------------------------------

generarmodelo <- function(pmodalidad, param) {
  # cargo el dataset pequeño
  dataset <- fread(paste0("~/datasets/", pmodalidad, "_dataset_pequeno.csv"))
  
  dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
  dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo
  
  # genero el modelo
  modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    xval = 0,
    control = param
  )
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(object = modelo, newdata = dapply, type = "prob")
  
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  # archivo de salida
  contador <- getandincrement("contador.yml")
  archivo_submit <- paste0("K100_", pmodalidad, "_", sprintf("%.3d", contador), ".csv")
  
  fwrite(dapply[, list(numero_de_cliente, Predicted)], file = archivo_submit, sep = ",")
  
  # preparo todo para el submit
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
  
  ganancia <- system(comando, intern = TRUE)
  cat(paste0(ganancia, "\t", archivo_submit, "\n"),
      file = "tb_ganancias.txt",
      append = TRUE)
  
  return(as.numeric(ganancia))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# aquí empieza el programa

# creo la carpeta donde voy a trabajar
dir.create("~/buckets/b1/exp/KA2000", showWarnings = FALSE)
setwd("~/buckets/b1/exp/KA2000")

# creo el contador
if (!file.exists("contador.yml")) {
  contador <- list("contador" = 1L)
  write_yaml(contador, file = "contador.yml")
}

# Genero modelos y submits a Kaggle para cada combinación de hiperparámetros
ganancias_totales <- numeric(length(param_combinations))
for (i in seq_along(param_combinations)) {
  ganancias_totales[i] <- generarmodelo(PARAM$modalidad, param_combinations[[i]])
}

# Almaceno las ganancias en un archivo para referencia
write.table(ganancias_totales, file = "ganancias_totales.txt", row.names = FALSE, col.names = FALSE)

