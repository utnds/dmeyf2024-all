import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
import lightgbm as lgb
import optuna
from datetime import datetime

# Importando el dataset
dataset = pd.read_csv("~/datasets/conceptual_dataset_pequeno.csv")

# Filtrando por la columna 'foto_mes'
dataset = dataset[dataset['foto_mes'] == 202107]

# Definiendo características y etiqueta
X = dataset.drop(columns=['clase_ternaria'])
y = dataset['clase_ternaria']

# Codificando la columna 'clase_ternaria'
label_encoder = LabelEncoder()
y = label_encoder.fit_transform(y)

# Dividiendo el dataset en el conjunto de entrenamiento y el conjunto de prueba (estratificado)
x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.2, stratify=y, random_state=42)

# Escalado de características
sc = StandardScaler()
x_train = sc.fit_transform(x_train)
x_test = sc.transform(x_test)

# Función para calcular la ganancia
def calcular_ganancia(y_true, y_pred, threshold=0.025):
    # Obtener probabilidades de la clase BAJA+2 (indice 2)
    prob_baja2 = y_pred[:, 1]  # Suponiendo que el orden de clases es CONTINUA, BAJA+1, BAJA+2
    ganancia = np.sum(np.where(
        (prob_baja2 > threshold) & (y_true == 1), 117000,  # Ganancia por BAJA+2
        np.where(prob_baja2 > threshold, -3000, 0)  # Penalización por falsa predicción
    ))
    ganancia_normalizada = ganancia / 0.2  # Ajuste por proporción de entrenamiento
    return ganancia_normalizada

# Función objetivo para Optuna usando ganancia
def objective(trial):
    
    d_train = lgb.Dataset(x_train, label=y_train)

    param = {
        'objective': 'multiclass',
        'metric': 'multi_logloss',
        'verbosity': -1,
        'boosting_type': 'gbdt',
        'num_class': 3,  # Tres clases: CONTINUA, BAJA+1, BAJA+2
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
        #'random_state': 42,
    }
    
    
    clf = lgb.train(param, d_train, valid_sets=[d_train])
    
    # Predicción sobre el conjunto de prueba y obtener las probabilidades
    y_pred = clf.predict(x_test)
    ganancia = calcular_ganancia(y_test, y_pred)
    
    return ganancia

# Configurar y ejecutar Optuna
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=100)