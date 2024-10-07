# esqueleto de grid search con Montecarlo Cross Validation
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

PARAM <- list()
PARAM$semilla_primigenia <- 524287
PARAM$qsemillas <- 20
PARAM$training_pct <- 70L
PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}

ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  particionar(dataset, division = c(training_pct, 100L - training_pct), agrupa = "clase_ternaria", seed = semilla)

  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1],
                  xval = 0,
                  control = param_basicos)

  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")

  ganancia_test <- dataset[fold == 2,
                           sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
                                      ifelse(clase_ternaria == "BAJA+2", 117000, -3000), 0))]

  ganancia_test_normalizada <- ganancia_test / ((100 - PARAM$training_pct) / 100)

  return(c(list("semilla" = semilla), param_basicos, list("ganancia_test" = ganancia_test_normalizada)))
}

ArbolesMontecarlo <- function(semillas, param_basicos) {
  salida <- mcmapply(ArbolEstimarGanancia, semillas, MoreArgs = list(PARAM$training_pct, param_basicos), SIMPLIFY = FALSE, mc.cores = detectCores())
  return(salida)
}

setwd("~/buckets/b1/")
dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[clase_ternaria != ""]
data_size <- nrow(dataset)

# Definir límites según el tamaño de la base de datos para evitar sobreajuste
minbucket_limit <- floor(0.05 * data_size)  # Ejemplo: mínimo 5% del dataset por hoja
minsplit_min <- 2 * minbucket_limit         # minsplit debe ser el doble de minbucket

primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia)
PARAM$semillas <- sample(primos, PARAM$qsemillas)

dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/HT2810/")

tb_grid_search_detalle <- data.table(semilla = integer(), cp = numeric(), maxdepth = integer(), minsplit = integer(), minbucket = integer(), ganancia_test = numeric())

for (vmin_split in seq(minsplit_min, minsplit_min + 20, by=10)) {
  for (vmax_depth in seq(4, 12, by=2)) {
    for (v_cp in seq(-0.9, -0.1, by=0.2)) {
      for (v_minbucket in seq(3, minbucket_limit, by=2)) {
        if (v_minbucket < (vmin_split / 2)) {  # Condicional para asegurar consistencia
          
          param_basicos <- list(
            "cp" = v_cp,
            "maxdepth" = vmax_depth,
            "minsplit" = vmin_split,
            "minbucket" = v_minbucket
          )
          
          ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          
          tb_grid_search_detalle <- rbindlist(list(tb_grid_search_detalle, rbindlist(ganancias)))
        }
      }
    }
  }  
  fwrite(tb_grid_search_detalle, file = "gridsearch_detalle.txt", sep = "\t")
}

tb_grid_search <- tb_grid_search_detalle[,
  list("ganancia_mean" = mean(ganancia_test), "qty" = .N), 
  list(cp, maxdepth, minsplit, minbucket)]

setorder(tb_grid_search, -ganancia_mean)
tb_grid_search[, id := .I]
fwrite(tb_grid_search, file = "gridsearch.txt", sep = "\t")
