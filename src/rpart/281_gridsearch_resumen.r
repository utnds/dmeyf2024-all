# esqueleto de grid search con Montecarlo Cross Validation
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

tb_grid_search_detalle <- fread("/home/sebastiancendra/buckets/b1/exp/HT2810/gridsearch_detalle.txt")

#----------------------------
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/HT2810/" )

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

# Filtrar registros con cp >= -1
tb_grid_search_filtered <- tb_grid_search[cp >= -1]

# Identificar combinaciones que cumplen la condición
valid_combinations <- unique(tb_grid_search_filtered[, .(maxdepth, minsplit, minbucket)])

# Filtrar nuevamente la tabla original para incluir solo las combinaciones válidas
tb_grid_search_final <- tb_grid_search[maxdepth %in% valid_combinations$maxdepth &
                                         minsplit %in% valid_combinations$minsplit &
                                         minbucket %in% valid_combinations$minbucket &
                                         cp >= -1]

tb_grid_search_unique <- tb_grid_search_final[,
                                              .SD[1], # Tomar el primer registro por grupo
                                              by = .(maxdepth, minsplit, minbucket)
]

# Guardar la tabla resultante
fwrite(tb_grid_search_unique, file = "gridsearch_unique.txt", sep = "\t")
