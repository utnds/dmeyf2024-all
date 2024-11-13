import optuna
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import lightgbm as lgb
from datetime import datetime

# Cargar y filtrar el dataset
dataset = pd.read_csv("~/datasets/conceptual_dataset_pequeno.csv")
dataset = dataset[dataset['foto_mes'] == 202107]

# Mapear clases a números enteros
class_mapping = {"CONTINUA": 0, "BAJA+1": 1, "BAJA+2": 2}
dataset['clase_ternaria'] = dataset['clase_ternaria'].map(class_mapping)

# Función de partición estratificada
def particionar(dataset, training_pct=70, seed=524281):
    # Dividir el dataset en características y etiquetas
    X = dataset.drop(columns=['clase_ternaria'])
    y = dataset['clase_ternaria']
    
    # Realizar la partición estratificada
    x_train, x_test, y_train, y_test = train_test_split(
        X, y,
        stratify=y,
        test_size=(100 - training_pct) / 100,
        random_state=seed
    )
    return x_train, x_test, y_train, y_test

# Función para calcular la ganancia
def calcular_ganancia(y_true, y_pred):
    ganancia = sum([117000 if (p[2] > 0.025 and t == 2) else -3000 for p, t in zip(y_pred, y_true)])
    return ganancia

# Variables para almacenar el mejor resultado
best_params = None
best_ganancia = float('-inf')

# Función objetivo para Optuna usando ganancia
def objective(trial):
    global best_params, best_ganancia  # Acceder a las variables globales
    
    # Configuración de hiperparámetros para LightGBM
    param = {
        'objective': 'multiclass',
        'metric': 'multi_logloss',
        'verbosity': -1,
        'boosting_type': 'gbdt',
        'num_class': 3,
        
        # Nuevos hiperparámetros a probar
        'num_iterations': trial.suggest_int('num_iterations', 0, 1000),
        'learning_rate': trial.suggest_loguniform('learning_rate', 1e-3, 0.1),
        'feature_fraction': trial.suggest_uniform('feature_fraction', 0.5, 1.0),
        'min_data_in_leaf': trial.suggest_int('min_data_in_leaf', 1, 100),
        'num_leaves': trial.suggest_int('num_leaves', 20, 4000),
        'max_bin': trial.suggest_int('max_bin', 5, 255),
        'bagging_fraction': trial.suggest_uniform('bagging_fraction', 0.5, 1.0),
        'bagging_freq': trial.suggest_int('bagging_freq', 1, 10),
        'lambda_l1': trial.suggest_uniform('lambda_l1', 0, 1000),
        'lambda_l2': trial.suggest_uniform('lambda_l2', 0, 1000),
        'min_split_gain': trial.suggest_uniform('min_split_gain', 0, 20),
        'random_state': 42,
    }
    
    # Partición de datos y conversión a formato LightGBM
    x_train, x_test, y_train, y_test = particionar(dataset, training_pct=80, seed=524281)
    d_train = lgb.Dataset(x_train, label=y_train)
    d_test = lgb.Dataset(x_test, label=y_test)
    
    # Entrenamiento del modelo con LightGBM
    clf = lgb.train(param, d_train, valid_sets=[d_test])
    
    # Predicción sobre el conjunto de prueba y cálculo de ganancia
    y_pred = clf.predict(x_test)
    ganancia = calcular_ganancia(y_test, y_pred)
    
    # Actualizar el mejor resultado
    if ganancia > best_ganancia:
        best_ganancia = ganancia
        best_params = param

    return ganancia

# Configurar y ejecutar Optuna
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=200)

# Mostrar el mejor resultado al finalizar
print("Mejor Ganancia:", best_ganancia)
print("Mejores Hiperparámetros:", best_params)