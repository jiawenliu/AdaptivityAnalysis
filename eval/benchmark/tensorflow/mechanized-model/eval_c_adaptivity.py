
import os

import tensorflow as tf
import tensorflow_hub as hub

import gdown
import numpy as np
from sklearn.metrics import classification_report
import matplotlib.pyplot as plt
import seaborn as sns

import sys
sys.path.append("..")

from mechanism.mechanized_sequential import MechanizedSequential
from mechanism.mechanized_sequential import Mechanism
import math

gdown.download(
    url='https://drive.google.com/uc?id=1Ag0jd21oRwJhVFIBohmX_ogeojVtapLy',
    output='bard.zip',
    quiet=True
)

module_path = "text_module"
embedding_layer = hub.KerasLayer(module_path, trainable=False)

embedding_layer(['বাস', 'বসবাস', 'ট্রেন', 'যাত্রী', 'ট্রাক']) 
# Convert to Tensorflow Dataset 

dir_names = ['economy', 'sports', 'entertainment', 'state', 'international']

file_paths = []
labels = []

for i, dir in enumerate(dir_names):
  file_names = ["/".join([dir, name]) for name in os.listdir(dir)]
  file_paths += file_names
  labels += [i] * len(os.listdir(dir))
  
np.random.seed(42)
permutation = np.random.permutation(len(file_paths))

file_paths = np.array(file_paths)[permutation]
labels = np.array(labels)[permutation]
train_frac = 0.8
train_size = int(len(file_paths) * train_frac)
# plot training vs validation distribution


def load_file(path, label):
    return tf.io.read_file(path), label

def make_datasets(step):
  train_size = int(len(file_paths) * train_frac)
  batch_size = train_size/step

  train_files = file_paths[:train_size]
  train_labels = labels[:train_size]
  train_ds = tf.data.Dataset.from_tensor_slices((train_files, train_labels))
  train_ds = train_ds.map(load_file).shuffle(5000)
  train_ds = train_ds.batch(batch_size).prefetch(tf.data.AUTOTUNE)

  test_files = file_paths[train_size:]
  test_labels = labels[train_size:]
  test_ds = tf.data.Dataset.from_tensor_slices((test_files, test_labels))
  test_ds = test_ds.map(load_file)
  test_ds = test_ds.batch(batch_size).prefetch(tf.data.AUTOTUNE)


  return train_ds, test_ds

# Model Training and Evaluation
## Model



def compile_and_fit_model(model, train_data, eager = False):
    tf.keras.backend.clear_session()
    tf.random.set_seed(42)
    np.random.seed(42)
    
    model.compile(loss=tf.losses.SparseCategoricalCrossentropy(from_logits=True),
        optimizer="adam", metrics=['accuracy'],
        run_eagerly = eager)  

    history = model.fit(train_data, 
                    epochs = 3)
    return history

def eval_model(step, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    ''' Compile and fit the empirical model as baseline'''
    model = tf.keras.Sequential([
        tf.keras.layers.Input(shape=[], dtype=tf.string),
        embedding_layer,
        tf.keras.layers.Dense(64, activation="relu"),
        tf.keras.layers.Dense(16, activation="relu"),
        tf.keras.layers.Dense(5),
    ])
    train_data, validation_data = make_datasets(step)


    if mechanism:
        model.choose_mech(mechanism)
        model.set_mechanism_para(sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history = compile_and_fit_model(model, train_data, eager = True)
    else:
        history = compile_and_fit_model(model, train_data, eager = False)


    ''' Validate the result'''
    model.choose_mech(None)
    empirical_y_pred = model.predict(validation_data)
    empirical_y_pred = np.argmax(empirical_y_pred, axis=1)

    return history, empirical_y_pred


def generalization_error(y_true, predict):
    error = tf.keras.metrics.RootMeanSquaredError()
    error.update_state(y_true, predict)
    print(error.result().numpy())
    return error.result().numpy()


def eval_multiple_rounds(rounds, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    history_list, predict_list, generalization_error_list = [], [], []
    for r in rounds:
        history, predict = eval_model(step = r, mechanism = mechanism, sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history_list.append(history)
        predict_list.append(predict)
        generalization_error_list.append(generalization_error(np.array(labels[train_size:]), predict)) 

    return history_list, predict_list, generalization_error_list




'''
Plot the Comparison of the Three Model
'''
def plot_error(rounds, generalization_error, mechanism):
    plt.plot(rounds, generalization_error, label = mechanism)
    plt.xlabel("Queries")
    plt.ylabel("RMSE (Generalization Error) for adaptive queries")
    plt.legend()
    plt.grid()

max_rounds = 5

stepped_rounds = range(math.floor(max_rounds/2), max_rounds, 2)

''' eval the empirical model as baseline'''


baseline_history_list, baseline_predict_list, baseline_generalization_error_list = eval_multiple_rounds(stepped_rounds)
print(baseline_generalization_error_list)

# baseline_generalization_error_list = [41.741657, 34.947556, 29.856426, 26.357683, 24.293772]

''' Compile and fit the gaussian model'''
gaussian_history_list, gaussian_predict_list, gaussian_generalization_error_list = eval_multiple_rounds(stepped_rounds, mechanism = Mechanism.MechanismType.GAUSSIAN, sigma = 0.03)
print(gaussian_generalization_error_list)

# gaussian_generalization_error_list = [65.59238, 53.909763, 41.66362, 32.519978, 26.558846]

''' Compile and fit the threshold out model'''
threshold_history_list, threshold_predict_list, threshold_generalization_error_list = eval_multiple_rounds(stepped_rounds, mechanism = Mechanism.MechanismType.THRESHOLD, sigma = 0.1, hold_frac = 0.4, threshold = 0.5)
print(threshold_generalization_error_list)

# threshold_generalization_error_list = [15.81994, 19.699022, 23.081583, 25.196918, 26.020424]

""" plot the generalization error """
plt.figure()
plot_error(stepped_rounds, baseline_generalization_error_list, "Baseline")
plot_error(stepped_rounds, gaussian_generalization_error_list, "Gaussian - AdaptFun")
plot_error(stepped_rounds, threshold_generalization_error_list, "Threshold")
plt.show()


