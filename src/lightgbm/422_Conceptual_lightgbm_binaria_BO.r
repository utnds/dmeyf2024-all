# Cargar bibliotecas necesarias
library(lightgbm)
library(mlrMBO)

# Configuración de parámetros
PARAM <- list(
  hyperparametertuning = list(
    iteraciones = 50,  # número de iteraciones
    xval_folds = 5    # número de pliegues para validación cruzada
  ),
  trainingstrategy = list(
    undersampling = 0.5  # ajustar según sea necesario
  )
)

# Suponiendo que 'dataset' ya está definido en tu entorno y tiene las columnas necesarias
# y que 'campos_buenos' y 'clase01' también están definidos.

# Crear conjunto de entrenamiento
dataset <- dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2"))
]

# Preparar datos para lightgbm
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01]
)

# Definir la función de optimización
funcion_optimizar <- EstimarGanancia_lightgbm

# Configurar MLR
configureMlr(show.learner.output = FALSE)

# Definición del espacio de búsqueda
hs <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeIntegerParam("max_depth", lower = 1L, upper = 10L),
  makeIntegerParam("num_leaves", lower = 31L, upper = 128L),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 100L),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1),
  makeNumericParam("bagging_fraction", lower = 0.1, upper = 1),
  makeIntegerParam("bagging_freq", lower = 1L, upper = 10L),
  makeNumericParam("lambda_l1", lower = 0, upper = 10),
  makeNumericParam("lambda_l2", lower = 0, upper = 10)
)

# Control para la optimización bayesiana
ctrl <- makeMBOControl(save.on.disk.at.time = 1200, save.file.path = "kbayesiana")
ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)

# Definición del modelo de regresión
surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = TRUE))

# Ejecución de la optimización
if (!file.exists("kbayesiana")) {
  run <- mbo(
    makeSingleObjectiveFunction(
      fn = funcion_optimizar,
      minimize = FALSE,
      noisy = TRUE,
      par.set = hs,
      has.simple.signature = FALSE
    ),
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue("kbayesiana")
}

# Finalización y modelado
# La función EstimarGanancia_lightgbm debe estar definida y lista para usarse
