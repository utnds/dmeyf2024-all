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
PARAM$semilla_primigenia <- 122777
PARAM$qsemillas <- 20
PARAM$training_pct <- 70L
PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  bloque <- unlist(mapply(function(x, y) { rep(y, x) }, division, seq(from = start, length.out = length(division))))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  particionar(dataset, division = c(training_pct, 100L - training_pct), agrupa = "clase_ternaria", seed = semilla)
  modelo <- rpart("clase_ternaria ~ .", data = dataset[fold == 1], xval = 0, control = param_basicos)
  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")
  ganancia_test <- dataset[fold == 2, sum(ifelse(prediccion[, "BAJA+2"] > 0.025, ifelse(clase_ternaria == "BAJA+2", 117000, -3000), 0))]
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

minbucket_limit <- floor(0.05 * data_size)
minsplit_min <- 2 * minbucket_limit

primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia)
PARAM$semillas <- sample(primos, PARAM$qsemillas)

dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/HT2810/")

tb_grid_search_detalle <- data.table(semilla = integer(), cp = numeric(), maxdepth = integer(), minsplit = integer(), minbucket = integer(), ganancia_test = numeric())

# Primera pasada (búsqueda general)
for (vmin_split in c(320,480,640,800)) {
  for (vmax_depth in seq(6, 8, by=1)) {
    for (v_cp in seq(-1, -0.6, by=0.2)) {
      for (v_minbucket in c(32,48,64,80)) {
        if (v_minbucket < (vmin_split / 2)) {
          param_basicos <- list("cp" = v_cp, "maxdepth" = vmax_depth, "minsplit" = vmin_split, "minbucket" = v_minbucket)
          ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          tb_grid_search_detalle <- rbindlist(list(tb_grid_search_detalle, rbindlist(ganancias)))
        }
      }
    }
  }
  fwrite(tb_grid_search_detalle, file = "gridsearch_detalle.txt", sep = "\t")
}


# Resumen de la primera pasada
tb_grid_search <- tb_grid_search_detalle[, list("ganancia_mean" = mean(ganancia_test), "qty" = .N), list(cp, maxdepth, minsplit, minbucket)]
setorder(tb_grid_search, -ganancia_mean)

# Exportación del resumen final
fwrite(tb_grid_search, file = "gridsearch_final.txt", sep = "\t")


#=========================================================================

# # Selección de los mejores valores de cada hiperparámetro
# best_params <- tb_grid_search[1]
# opt_vmin_split <- best_params$minsplit
# opt_vmax_depth <- best_params$maxdepth
# opt_v_cp <- best_params$cp
# opt_v_minbucket <- best_params$minbucket
# 
# # Definir rangos acotados para la segunda pasada
# vmin_split_range <- seq(opt_vmin_split - 9, opt_vmin_split + 9, by=2)
# vmax_depth_range <- seq(max(4, opt_vmax_depth - 2), opt_vmax_depth + 2, by=1)
# v_cp_range <- seq(opt_v_cp - 0.1, opt_v_cp + 0.1, by=0.05)
# v_minbucket_range <- seq(max(3, opt_v_minbucket - 4), minbucket_limit, by=1)
# 
# # Segunda pasada (búsqueda más detallada)
# tb_grid_search_detalle_2 <- data.table(semilla = integer(), cp = numeric(), maxdepth = integer(), minsplit = integer(), minbucket = integer(), ganancia_test = numeric())
# 
# for (vmin_split in vmin_split_range) {
#   for (vmax_depth in vmax_depth_range) {
#     for (v_cp in v_cp_range) {
#       for (v_minbucket in v_minbucket_range) {
#         if (v_minbucket < (vmin_split / 2)) {
#           param_basicos <- list("cp" = v_cp, "maxdepth" = vmax_depth, "minsplit" = vmin_split, "minbucket" = v_minbucket)
#           ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
#           tb_grid_search_detalle_2 <- rbindlist(list(tb_grid_search_detalle_2, rbindlist(ganancias)))
#         }
#       }
#     }
#   }
#   fwrite(tb_grid_search_detalle_2, file = "gridsearch_detalle_2.txt", sep = "\t")
# }

#=========================================================================

# # Resumen final
# tb_grid_search_final <- tb_grid_search_detalle_2[, list("ganancia_mean" = mean(ganancia_test), "qty" = .N), list(cp, maxdepth, minsplit, minbucket)]
# setorder(tb_grid_search_final, -ganancia_mean)
# tb_grid_search_final[, id := .I]
# 
# # Exportación del resumen final
# fwrite(tb_grid_search_final, file = "gridsearch_final.txt", sep = "\t")
