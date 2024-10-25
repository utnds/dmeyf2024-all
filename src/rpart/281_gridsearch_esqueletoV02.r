# esqueleto de grid search con Montecarlo Cross Validation
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

PARAM <- list()
PARAM$semilla_primigenia <- 100103 # Cambia esto por tu semilla
PARAM$qsemillas <- 20
PARAM$training_pct <- 70L  # entre  1L y 99L 

# elegir SU dataset comentando/ descomentando
PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
# PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

# Función de partición estratificada
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  bloque <- unlist(mapply(function(x, y) rep(y, x), division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

# Función para estimar ganancia
ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  particionar(dataset, division = c(training_pct, 100L - training_pct), agrupa = "clase_ternaria", seed = semilla)
  
  modelo <- rpart("clase_ternaria ~ .", data = dataset[fold == 1], xval = 0, control = param_basicos)
  
  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")
  
  ganancia_test <- dataset[fold == 2, sum(ifelse(prediccion[, "BAJA+2"] > 0.025, ifelse(clase_ternaria == "BAJA+2", 117000, -3000), 0))]
  ganancia_test_normalizada <- ganancia_test / ((100 - PARAM$training_pct) / 100)
  
  return(c(list("semilla" = semilla), param_basicos, list("ganancia_test" = ganancia_test_normalizada)))
}

# Función para Montecarlo
ArbolesMontecarlo <- function(semillas, param_basicos) {
  salida <- mcmapply(ArbolEstimarGanancia, semillas, MoreArgs = list(PARAM$training_pct, param_basicos), SIMPLIFY = FALSE, mc.cores = detectCores())
  return(salida)
}

# Preparación de datos
setwd("~/buckets/b1/")
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia)
PARAM$semillas <- sample(primos, PARAM$qsemillas)

dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[clase_ternaria != ""]

# Creación del directorio para resultados
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/HT2810/")

# Data.table para resultados
tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)

# Iteración por todos los hiperparámetros
for (vcp in seq(0.001, 0.1, by = 0.005)) { # Más valores pequeños para cp
  for (vmax_depth in seq(4, 20, by = 2)) { # Max depth más amplio
    for (vmin_split in seq(10, nrow(dataset), by = 50)) { # Min split dinámico
      for (vmin_bucket in c(1, 5, 10, 25, round(nrow(dataset) / 1000))) { # Min bucket flexible
        param_basicos <- list(
          "cp" = vcp,
          "maxdepth" = vmax_depth,
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket
        )
        
        ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        tb_grid_search_detalle <- rbindlist(list(tb_grid_search_detalle, rbindlist(ganancias)))
      }
    }
    
    fwrite(tb_grid_search_detalle, file = "gridsearch_detalleV02.txt", sep = "\t")
  }
}

# Resumen y ordenamiento
tb_grid_search <- tb_grid_search_detalle[, list("ganancia_mean" = mean(ganancia_test), "qty" = .N), list(cp, maxdepth, minsplit, minbucket)]
setorder(tb_grid_search, -ganancia_mean)
tb_grid_search[, id := .I]
fwrite(tb_grid_search, file = "gridsearchV02.txt", sep = "\t")

