import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
import lightgbm as lgb
import optuna
from sklearn.metrics import accuracy_score
from datetime import datetime

# Importando el dataset
dataset = pd.read_csv("~/datasets/conceptual_dataset_pequeno.csv")

# Filtrando por la columna 'foto_mes'
dataset = dataset[dataset['foto_mes'] == 202107]

# Verifica que el dataset no esté vacío
if dataset.shape[0] == 0:
    raise ValueError("El dataset está vacío después de aplicar el filtro por 'foto_mes'.")

# Definiendo características y etiqueta
X = dataset.drop(columns=['clase_ternaria'])
y = dataset['clase_ternaria']

# Codificando la columna 'clase_ternaria'
label_encoder = LabelEncoder()
y = label_encoder.fit_transform(y)

# Dividiendo el dataset en el conjunto de entrenamiento y el conjunto de prueba
x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Escalado de características
sc = StandardScaler()
x_train = sc.fit_transform(x_train)
x_test = sc.transform(x_test)

# Lista para almacenar todos los hiperparámetros y sus resultados
all_params = []

# LightGBM
def objective(trial):
    param = {
        'objective': 'binary',
        'metric': 'binary_logloss',
        'verbosity': -1,
        'boosting_type': 'gbdt',
        'num_iterations': trial.suggest_int('num_iterations', 100, 1000),
        'learning_rate': trial.suggest_loguniform('learning_rate', 1e-3, 0.1),
        'feature_fraction': trial.suggest_uniform('feature_fraction', 0.5, 1.0),
        'min_data_in_leaf': trial.suggest_int('min_data_in_leaf', 1, 100),
        'num_leaves': trial.suggest_int('num_leaves', 20, 150),
        'max_bin': trial.suggest_int('max_bin', 5, 255),
        'bagging_fraction': trial.suggest_uniform('bagging_fraction', 0.5, 1.0),
        'bagging_freq': trial.suggest_int('bagging_freq', 1, 10),
        'lambda_l1': trial.suggest_uniform('lambda_l1', 0, 10),
        'lambda_l2': trial.suggest_uniform('lambda_l2', 0, 10),
        'min_split_gain': trial.suggest_uniform('min_split_gain', 0, 1),
        'random_state': 42,
    }
    
    d_train = lgb.Dataset(x_train, label=y_train)
    clf = lgb.train(param, d_train, valid_sets=[d_train])
    
    y_pred = clf.predict(x_train)
    pred_labels = np.where(y_pred >= 0.5, 1, 0)  # Convertir a etiquetas binarias
    accuracy = accuracy_score(y_train, pred_labels)
    
    # Guardar todos los hiperparámetros y su rendimiento en el diccionario
    current_time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    param['accuracy'] = accuracy
    param['fecha_hora'] = current_time
    all_params.append(param)  # Agregar el diccionario a la lista
    
    return accuracy

# Configurar y ejecutar Optuna
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=100)

# Convertir la lista de diccionarios a un DataFrame y guardar en un archivo CSV
all_params_df = pd.DataFrame(all_params)
all_params_df.to_csv('todos_los_hiperparametros.csv', index=False)

print("All hyperparameters saved.")
