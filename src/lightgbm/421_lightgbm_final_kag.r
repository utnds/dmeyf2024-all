# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("yaml")
require("rlist")

# defino los parametros de la corrida, en una lista, la variable global PARAM
PARAM <- list()
PARAM$experimento <- "KA4210"

PARAM$input$training <- c(202107) # meses donde se entrena el modelo
PARAM$input$future <- c(202109)   # meses donde se aplica el modelo

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos
loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(substitute(reg), ext)
  
  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0("fecha\t", paste(list.names(reg), collapse = "\t"), "\n")
    cat(linea, file = archivo)
  }
  
  # la fecha y hora
  linea <- paste0(format(Sys.time(), "%Y%m%d %H%M%S"), "\t", gsub(", ", "\t", toString(reg)), "\n")
  
  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)
  
  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------

# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# cargo los datos
dataset <- fread(miAmbiente$dataset_pequeno, stringsAsFactors = TRUE)

# paso la clase a binaria
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

# creo las carpetas donde van los resultados
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#--------------------------------------
# Leo los parametros del archivo HT4220.txt
param_file <- "/home/arjonacristianemmd/buckets/b1/exp/HT4220/HT4220.txt"
parametros <- fread(param_file)
parametros <- parametros[order(-ganancia)][1:20] #ordeno por ganancia y filtro los primeros 20 elementos

# Itero sobre los primeros 20 registros
for (i in 1:20) {
  # Asigno los parametros de cada iteración
  PARAM$finalmodel$learning_rate <- parametros$learning_rate[i]
  PARAM$finalmodel$feature_fraction <- parametros$feature_fraction[i]
  PARAM$finalmodel$min_data_in_leaf <- parametros$min_data_in_leaf[i]
  PARAM$finalmodel$num_leaves <- parametros$num_leaves[i]
  PARAM$finalmodel$max_bin <- parametros$max_bin[i]
  
  # Genero el modelo
  modelo <- lgb.train(
    data = dtrain,
    param = list(
      objective = "binary",
      max_bin = PARAM$finalmodel$max_bin,
      learning_rate = PARAM$finalmodel$learning_rate,
      num_iterations = PARAM$finalmodel$num_iterations,
      num_leaves = PARAM$finalmodel$num_leaves,
      min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
      feature_fraction = PARAM$finalmodel$feature_fraction,
      seed = miAmbiente$semilla_primigenia
    )
  )
  
  # Aplicacion del modelo a los datos de future
  dapply <- dataset[foto_mes == PARAM$input$future]
  prediccion <- predict(modelo, data.matrix(dapply[, campos_buenos, with = FALSE]))
  
  # Genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  # Grabo las predicciones
  fwrite(tb_entrega, file = "prediccion.txt", sep = "\t")
  setorder(tb_entrega, -prob)
  
  # Genero los archivos con los "envios" mejores
  cortes <- seq(1500, 2500, by = 100)
  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    nom_arch_kaggle <- paste0(PARAM$experimento, "_", envios, ".csv")
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)], file = nom_arch_kaggle, sep = ",")
    
    # Subo a Kaggle
    comentario <- paste0(
      "'envios=", envios,
      " num_iterations=", PARAM$finalmodel$num_iterations,
      " learning_rate=", PARAM$finalmodel$learning_rate,
      " num_leaves=", PARAM$finalmodel$num_leaves,
      " min_data_in_leaf=", PARAM$finalmodel$min_data_in_leaf,
      " feature_fraction=", PARAM$finalmodel$feature_fraction, "'"
    )
    
    comando <- paste0("~/install/proc_kaggle_submit.sh ",
                      "TRUE ", miAmbiente$modalidad, " ", nom_arch_kaggle, " ", comentario)
    
    ganancia <- system(comando, intern = TRUE)
    
    # Logueo la ganancia
    linea <- c(list("ganancia" = ganancia), PARAM$finalmodel)
    loguear(linea, arch = "tb_ganancias.txt")
  }
}

cat("\n\nSe han realizado los submits a Kaggle\n")