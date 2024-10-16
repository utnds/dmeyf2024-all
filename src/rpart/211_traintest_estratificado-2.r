rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

PARAM <- list()
PARAM$semilla <- 597361 #524287
PARAM$training_pct <- 70L  # entre  1L y 99L 

PARAM$rpart <- list (
  "cp" = -0.1, # complejidad minima
  "minsplit" = 631, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 250, #260, # minima cantidad de regs en una hoja
  "maxdepth" = 4 # profundidad máxima del arbol
)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa

# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30


particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}

#particionar <- function(
#    data, division, agrupa = "",
#    campo = "fold", start = 1, seed = NA) {
#  if (!is.na(seed)) set.seed(seed)
#  data[, (campo) := {
#    n <- .N
#    tamanos <- ceiling(n * (division / 100))
#    bloque <- unlist(mapply(function(x, y) rep(y, x), tamanos, seq(from = start, length.out = length(division))))
#    sample(rep(bloque, length.out = n))[1:n]
#  }, by = agrupa]
#}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# Establezco el Working Directory
setwd("~/buckets/b1/")

# cargo los datos,  alternar comentario segun corresponda
#dataset <- fread("~/datasets/vivencial_dataset_pequeno.csv")
dataset <- fread("~/datasets/conceptual_dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset1 <- dataset[clase_ternaria == ""]
dataset2 <- dataset[clase_ternaria != ""]

# particiono estratificadamente el dataset 70%, 30%
particionar(dataset2,
  division = c(PARAM$training_pct, 100L -PARAM$training_pct), 
  agrupa = "clase_ternaria",
  seed = PARAM$semilla # aqui se usa SU semilla
)



# genero el modelo
# quiero predecir clase_ternaria a partir del resto
# fold==1  es training,  el 70% de los datos
modelo <- rpart("clase_ternaria ~ .",
  data = dataset2[fold == 1],
  xval = 0,
  control = PARAM$rpart # aqui van los parametros
)

dataset1 = dataset1[, fold := 2]
prueba <-dataset1
prueba2 <-prueba[, c("clase_ternaria", "fold")]


# aplico el modelo a los datos de testing
prediccion <- predict(modelo, # el modelo que genere recien
  dataset1, # fold==2  es testing, el 30% de los datos
  type = "prob"
) # type= "prob"  es que devuelva la probabilidad

# prediccion es una matriz con TRES columnas,
#  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego una columna que es la de las ganancias
dataset1[, ganancia := ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]

# para testing agrego la probabilidad
dataset1[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]

# calculo la ganancia en testing  qu es fold==2
ganancia_test <- dataset1[fold == 2 & prob_baja2 > 0.025, sum(ganancia)]

# escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )

estimulos <- dataset1[fold == 2 & prob_baja2 > 0.025, .N]
aciertos <- dataset1[fold == 2 & prob_baja2 > 0.025 & clase_ternaria == "BAJA+2", .N]


cat("Testing total: ", dataset1[fold == 2, .N], "\n")
cat("Testing BAJA+2: ", dataset1[fold == 2 & clase_ternaria == "BAJA+2", .N], "\n")

cat("Estimulos: ", estimulos, "\n")
cat("Aciertos (BAJA+2): ", aciertos, "\n")

cat("Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n")

# Exportación del resumen final
fwrite(dataset1, file = "traintest.csv", sep = "\t")
