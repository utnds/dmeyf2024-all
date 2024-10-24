import lightgbm as lgb
import optuna
import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedKFold

# Cargar dataset
df = pd.read_csv("~/datasets/conceptual_dataset_pequeno.csv", low_memory=False)  # Cargar el dataset

semilla_prim = 524287

# Columnas a usar en el modelo
campos_buenos = [col for col in df.columns if col not in ["clase_ternaria", "azar", "training"]]

# Undersampling de la clase "CONTINUA"
np.random.seed(semilla_prim)  # Semilla de ejemplo
df['azar'] = np.random.rand(len(df))
df['training'] = 0
undersampling = 1.0  # Definir tasa de undersampling
df.loc[(df['foto_mes'].isin([202107])) & 
       ((df['azar'] <= undersampling) | 
        (df['clase_ternaria'].isin(['BAJA+1', 'BAJA+2']))), 'training'] = 1

# Separar datos de entrenamiento
dtrain = df[df['training'] == 1]
X = dtrain[campos_buenos]
y = dtrain['clase_ternaria'].apply(lambda x: 1 if x in ['BAJA+1', 'BAJA+2'] else 0)  # Convertir a binario

# Convertir DataFrame a Dataset de LightGBM
train_set = lgb.Dataset(X, label=y)

# Configuración de hiperparámetros a optimizar
def objective(trial):
    params = {
        'objective': 'binary',
        'metric': 'binary_logloss',
        'boosting_type': 'gbdt',
        'learning_rate': trial.suggest_float('learning_rate', 0.01, 0.3),
        'num_leaves': trial.suggest_int('num_leaves', 8, 1024),
        'feature_fraction': trial.suggest_float('feature_fraction', 0.1, 1.0),
        'min_data_in_leaf': trial.suggest_int('min_data_in_leaf', 1, 8000),
        'envios': trial.suggest_int('envios', 5000, 15000),
        'verbosity': -1,
        'seed': semilla_prim,
    }

    # Cross-validation
    skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=202107)
    cv_results = lgb.cv(
        params,
        train_set,  # Pasamos el objeto 'train_set' en vez de un DataFrame
        num_boost_round=1000,
        nfold=5,
        stratified=True,
        #n_iter_no_change=10,
        seed=semilla_prim
    )

    # Verificar las claves devueltas por cv_results
    print("Claves devueltas por lgb.cv():", cv_results.keys())

    # Acceder al valor correcto
    best_iteration = np.argmin(cv_results['binary_logloss-mean']) if 'binary_logloss-mean' in cv_results else np.argmin(cv_results['binary_logloss'])
    return -cv_results['binary_logloss-mean'][best_iteration] if 'binary_logloss-mean' in cv_results else -cv_results['binary_logloss'][best_iteration]

# Optimización bayesiana con Optuna
study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=150)

# Resultados
print(f"Best trial: {study.best_trial.params}")