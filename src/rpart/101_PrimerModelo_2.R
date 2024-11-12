# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot
# Correr en Google Cloud con RStudio

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1") # Establezco el Working Directory

# cargo el dataset pequeno vivencial del disco local
dataset <- fread("/home/sebastiancendra/buckets/b1/exp/HT2810/kag.txt")

dataset <- dataset[, .(cp,	maxdepth,	minsplit,	minbucket,	ganancia_mean)]
# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
    formula = "ganancia_mean ~ .",
    data = dataset, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.3, # esto significa no limitar la complejidad de los splits
    minsplit = 0, # minima cantidad de registros para que se haga el split
    minbucket = 2, # tamaño minimo de una hoja
    maxdepth = 3  # profundidad maxima del arbol
)


# grafico el arbol
par(mar = c(1, 1, 1, 1))  # Ajusta los márgenes a un valor más pequeño
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


