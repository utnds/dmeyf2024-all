import lightgbm as lgb
import numpy as np
import pandas as pd
import optuna
import yaml
import os

# Función para calcular la ganancia
def fganancia_logistic_lightgbm(y_true, y_pred, weights):
    # Calcula las ganancias
    ganancia_pos = 117000
    ganancia_neg = -3000
    vgan = np.where(weights == 1.0000002, ganancia_pos, 
                    np.where(weights == 1.0000001, ganancia_neg, 
                             ganancia_neg / 1.0))  # Ajusta según el undersampling

    # Ordenar las probabilidades y calcular la ganancia
    sorted_indices = np.argsort(-y_pred)
    ganancia = np.sum(vgan[sorted_indices[:5000]])  # solo los 5000 mejores

    return 'ganancia', ganancia, True  # El nombre debe ser el mismo que se usa en lgb

# Función objetivo para la optimización
def objective(trial):
    learning_rate = trial.suggest_float("learning_rate", 0.01, 0.3)
    num_leaves = trial.suggest_int("num_leaves", 8, 1024)
    feature_fraction = trial.suggest_float("feature_fraction", 0.1, 1.0)
    min_data_in_leaf = trial.suggest_int("min_data_in_leaf", 1, 8000)
    envios = trial.suggest_int("envios", 5000, 15000)

    # Crear dataset
    dtrain = lgb.Dataset(data=X_train, label=y_train, weight=weights_train)

    # Entrenamiento con validación cruzada
    cv_results = lgb.cv(
        params={
            'objective': 'binary',
            'metric': 'custom',  # Este es un parámetro que indica el uso de la métrica personalizada
            'learning_rate': learning_rate,
            'num_leaves': num_leaves,
            'feature_fraction': feature_fraction,
            'min_data_in_leaf': min_data_in_leaf,
            'verbosity': -1,
            'max_bin': 31,
            'num_iterations': 9999
        },
        train_set=dtrain,
        num_boost_round=9999,
        nfold=5,
        stratified=True,
        #early_stopping_rounds=50,
        #feval=lambda y_true, y_pred: fganancia_logistic_lightgbm(y_true.get_label(), y_pred, weights_train)
    )

    ganancia_normalizada = np.mean(cv_results['ganancia'])  # Cambiar 'custom' a 'ganancia'
    return ganancia_normalizada

# Cargar el dataset
dataset = pd.read_csv("~/datasets/conceptual_dataset_pequeno.csv") 

# Transformar la clase a binaria
dataset['clase01'] = np.where(dataset['clase_ternaria'] == 'CONTINUA', 0, 1)

# Undersampling
np.random.seed(42)  # Para reproducibilidad
dataset['azar'] = np.random.rand(len(dataset))
undersampling_rate = 1.0  # Cambia esto según tu estrategia
dataset['training'] = np.where(
    (dataset['azar'] <= undersampling_rate) | 
    (dataset['clase_ternaria'].isin(['BAJA+1', 'BAJA+2'])), 1, 0)

# Definir campos de interés
campos_buenos = dataset.columns.difference(['clase_ternaria', 'clase01', 'azar', 'training'])

# Dividir en entrenamiento
X_train = dataset[dataset['training'] == 1][campos_buenos].values
y_train = dataset[dataset['training'] == 1]['clase01'].values
weights_train = np.where(dataset['clase_ternaria'] == 'BAJA+2', 1.0000002, 
                          np.where(dataset['clase_ternaria'] == 'BAJA+1', 1.0000001, 1.0))

# Optimización bayesiana
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=150)

# Guardar los resultados
if not os.path.exists('./exp/'):
    os.makedirs('./exp/')
with open('./exp/optimizacion_resultados.txt', 'w') as f:
    for trial in study.trials:
        f.write(f'Trial {trial.number}: {trial.value}\n')
