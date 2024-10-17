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
# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 100003
PARAM$qsemillas <- 20

PARAM$training_pct <- 70L  # entre  1L y 99L 

# elegir SU dataset comentando/ descomentando
PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
# PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset,
    division = c(training_pct, 100L -training_pct), 
    agrupa = "clase_ternaria",
    seed = semilla # aqui se usa SU semilla
  )

  # genero el modelo
  # predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )

  return( 
    c( list("semilla" = semilla),
      param_basicos,
      list( "ganancia_test" = ganancia_test_normalizada )
     )
  )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {

  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  salida <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(PARAM$training_pct, param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = detectCores()
  )

  return(salida)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos


# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, PARAM$qsemillas )


# cargo los datos
dataset <- fread(PARAM$dataset_nom)
# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/HT2810/" )


# genero la data.table donde van los resultados detallados del Grid Search
# un registro para cada combinacion de < semilla, parametros >
tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
  for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {
    # notar como se agrega

    # vminsplit  minima cantidad de registros en un nodo para hacer el split
    param_basicos <- list(
      "cp" = -0.5, # complejidad minima
      "maxdepth" = vmax_depth, # profundidad máxima del arbol
      "minsplit" = vmin_split, # tamaño minimo de nodo para hacer split
      "minbucket" = 5 # minima cantidad de registros en una hoja
    )

    # Un solo llamado, con la semilla 17
    ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

    # agrego a la tabla
    tb_grid_search_detalle <- rbindlist( 
      list( tb_grid_search_detalle,
            rbindlist(ganancias) )
    )

  }

  # grabo cada vez TODA la tabla en el loop mas externo
  fwrite( tb_grid_search_detalle,
          file = "gridsearch_detalle.txt",
          sep = "\t" )
}

#----------------------------

# genero y grabo el resumen
tb_grid_search <- tb_grid_search_detalle[,
  list( "ganancia_mean" = mean(ganancia_test),
    "qty" = .N ),
  list( cp, maxdepth, minsplit, minbucket )
]

# ordeno descendente por ganancia
setorder( tb_grid_search, -ganancia_mean )

# genero un id a la tabla
tb_grid_search[, id := .I ]

fwrite( tb_grid_search,
  file = "gridsearch.txt",
  sep = "\t"
)

#---------------------------------------------------------------------------



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

# Primera pasada (bÃºsqueda general)
for (vmin_split in c(10, 20, 40, 80, 160, 320, 640)) {
  for (vmax_depth in seq(4, 16, by=2)) {
    for (v_cp in seq(-1, 0, by=0.2)) {
      for (v_minbucket in c(2,4,8,16,32,64)) {
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


# SelecciÃ³n de los mejores valores de cada hiperparÃ¡metro
best_params <- tb_grid_search[1]
opt_vmin_split <- best_params$minsplit
opt_vmax_depth <- best_params$maxdepth
opt_v_cp <- best_params$cp
opt_v_minbucket <- best_params$minbucket

# Definir rangos acotados para la segunda pasada
vmin_split_range <- seq(opt_vmin_split - 9, opt_vmin_split + 9, by=2)
vmax_depth_range <- seq(max(4, opt_vmax_depth - 2), opt_vmax_depth + 2, by=1)
v_cp_range <- seq(opt_v_cp - 0.1, opt_v_cp + 0.1, by=0.05)
v_minbucket_range <- seq(max(3, opt_v_minbucket - 4), minbucket_limit, by=1)

# Segunda pasada (bÃºsqueda mÃ¡s detallada)
tb_grid_search_detalle_2 <- data.table(semilla = integer(), cp = numeric(), maxdepth = integer(), minsplit = integer(), minbucket = integer(), ganancia_test = numeric())

for (vmin_split in vmin_split_range) {
  for (vmax_depth in vmax_depth_range) {
    for (v_cp in v_cp_range) {
      for (v_minbucket in v_minbucket_range) {
        if (v_minbucket < (vmin_split / 2)) {
          param_basicos <- list("cp" = v_cp, "maxdepth" = vmax_depth, "minsplit" = vmin_split, "minbucket" = v_minbucket)
          ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          tb_grid_search_detalle_2 <- rbindlist(list(tb_grid_search_detalle_2, rbindlist(ganancias)))
        }
      }
    }
  }
  fwrite(tb_grid_search_detalle_2, file = "gridsearch_detalle_2.txt", sep = "\t")
}

# Resumen final
tb_grid_search_final <- tb_grid_search_detalle_2[, list("ganancia_mean" = mean(ganancia_test), "qty" = .N), list(cp, maxdepth, minsplit, minbucket)]
setorder(tb_grid_search_final, -ganancia_mean)
tb_grid_search_final[, id := .I]

# ExportaciÃ³n del resumen final
fwrite(tb_grid_search_final, file = "gridsearch_final.txt", sep = "\t")

