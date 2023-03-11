import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import math

keras = tf.keras
WINDOW_SIZE = 30
TRAIN_SIZE = 1000

def plot_series(time, series, format="-", start=0, end=None, label=None):
    plt.plot(time[start:end], series[start:end], format, label=label)
    plt.xlabel("Time")
    plt.ylabel("Value")
    if label:
        plt.legend(fontsize=14)
    plt.grid(True)


def trend(time, slope=0):
    return slope * time
  
  
def seasonal_pattern(season_time):
    """Just an arbitrary pattern, you can change it if you wish"""
    return np.where(season_time < 0.4,
                    np.cos(season_time * 2 * np.pi),
                    1 / np.exp(3 * season_time))

  
def seasonality(time, period, amplitude=1, phase=0):
    """Repeats the same pattern at each period"""
    season_time = ((time + phase) % period) / period
    return amplitude * seasonal_pattern(season_time)
  
  
def white_noise(time, noise_level=1, seed=None):
    rnd = np.random.RandomState(seed)
    return rnd.randn(len(time)) * noise_level
  

def seq2seq_window_dataset(series, WINDOW_SIZE, batch_size=32,
                           shuffle_buffer=1000):
    series = tf.expand_dims(series, axis=-1)
    ds = tf.data.Dataset.from_tensor_slices(series)
    ds = ds.window(WINDOW_SIZE + 1, shift=1, drop_remainder=True)
    ds = ds.flat_map(lambda w: w.batch(WINDOW_SIZE + 1))
    ds = ds.shuffle(shuffle_buffer)
    ds = ds.map(lambda w: (w[:-1], w[1:]))
    return ds.batch(batch_size).prefetch(1)
  

def model_forecast(model, series, WINDOW_SIZE):
    ds = tf.data.Dataset.from_tensor_slices(series)
    ds = ds.window(WINDOW_SIZE, shift=1, drop_remainder=True)
    ds = ds.flat_map(lambda w: w.batch(WINDOW_SIZE))
    ds = ds.batch(32).prefetch(1)
    forecast = model.predict(ds)
    return forecast


time = np.arange(4 * 365 + 1)
slope = 0.05
baseline = 10
amplitude = 40
series = baseline + trend(time, slope) + seasonality(time, period=365, amplitude=amplitude)

noise_level = 5
noise = white_noise(time, noise_level, seed=42)

series += noise

time_train = time[:TRAIN_SIZE]
x_train = series[:TRAIN_SIZE]
time_valid = time[TRAIN_SIZE:]
x_valid = series[TRAIN_SIZE:]



import sys
sys.path.append("..")

from mechanism.mechanized_sequential import MechanizedSequential
from mechanism.mechanized_sequential import Mechanism

keras.backend.clear_session()
tf.random.set_seed(42)
np.random.seed(42)


# tf.enable_eager_execution()

# mechanized_model = MechanizedSequential([
#   keras.layers.Conv1D(filters = 32, kernel_size = 5,
#                       strides = 1, padding = "causal",
#                       activation = "relu",
#                       input_shape=[None, 1]),
#   keras.layers.LSTM(32, return_sequences = True),
#   keras.layers.LSTM(32, return_sequences=True),
#   keras.layers.Dense(1),
#   keras.layers.Lambda(lambda x: x * 200)
# ])



def compile_and_fit_model(model, eager = False, step = 1):
    train_set = seq2seq_window_dataset(x_train, WINDOW_SIZE,
                                    batch_size = math.floor(TRAIN_SIZE/step))
    
    lr_schedule = keras.callbacks.LearningRateScheduler(
        lambda epoch: 1e-8 * 10**(epoch / 20))
    optimizer = keras.optimizers.SGD(lr=1e-8, momentum=0.9)
    model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = eager)

    history = model.fit(train_set, epochs = 2, callbacks=[lr_schedule])
    return history

def eval_model(model, step, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    ''' Compile and fit the empirical model as baseline'''
    if mechanism:
        model.choose_mech(mechanism)
        model.set_mechanism_para(sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history = compile_and_fit_model(model, True, step)
    else:
        history = compile_and_fit_model(model, False, step)


    ''' Validate the result'''
    model.choose_mech(None)
    rnn_forecast = model_forecast(model, series[:,  np.newaxis], WINDOW_SIZE)
    rnn_forecast = rnn_forecast[TRAIN_SIZE - WINDOW_SIZE:-1, -1, 0]
    return history, rnn_forecast


def generalization_error(predict):
    error = keras.metrics.mean_absolute_error(x_valid, predict).numpy()
    print(error)
    return keras.metrics.mean_absolute_error(x_valid, predict).numpy()


def eval_multiple_rounds(model, rounds, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    history_list, rnn_forecast_list, generalization_error_list = [], [], []
    for r in rounds:
        history, rnn_forecast = eval_model(model, step = r, mechanism = mechanism, sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history_list.append(history)
        rnn_forecast_list.append(rnn_forecast)
        generalization_error_list.append(generalization_error(rnn_forecast)) 

    return history_list, rnn_forecast_list, generalization_error_list




'''
Plot the Comparison of the Three Model
'''
def plot_error(rounds, generalization_error, mechanism):
    plt.plot(rounds, generalization_error, label = mechanism)
    plt.xlabel("Queries")
    plt.ylabel("RMSE (Generalization Error) for adaptive queries")
    plt.legend()
    plt.grid()

max_rounds = 200

stepped_rounds = range(math.floor(max_rounds/2), max_rounds, 20)

''' eval the empirical model as baseline'''
mechanized_model = MechanizedSequential([
  keras.layers.Conv1D(filters = 32, kernel_size = 5,
                      strides = 1, padding = "causal",
                      activation = "relu",
                      input_shape=[None, 1]),
  keras.layers.LSTM(32, return_sequences = True),
  keras.layers.LSTM(32, return_sequences=True),
  keras.layers.Dense(1),
  keras.layers.Lambda(lambda x: x * 200)
])
baselin_history_list, baseline_rnn_forecast_list, baseline_generalization_error_list = eval_multiple_rounds(mechanized_model, stepped_rounds)
print(baseline_generalization_error_list)

# baseline_generalization_error_list = [41.741657, 34.947556, 29.856426, 26.357683, 24.293772]

''' Compile and fit the gaussian model'''
mechanized_model = MechanizedSequential([
  keras.layers.Conv1D(filters = 32, kernel_size = 5,
                      strides = 1, padding = "causal",
                      activation = "relu",
                      input_shape=[None, 1]),
  keras.layers.LSTM(32, return_sequences = True),
  keras.layers.LSTM(32, return_sequences=True),
  keras.layers.Dense(1),
  keras.layers.Lambda(lambda x: x * 200)
])
gaussian_history_list, gaussian_rnn_forecast_list, gaussian_generalization_error_list = eval_multiple_rounds(mechanized_model, stepped_rounds, mechanism = Mechanism.GAUSSIAN, sigma = 0.03)
print(gaussian_generalization_error_list)

# gaussian_generalization_error_list = [65.59238, 53.909763, 41.66362, 32.519978, 26.558846]

''' Compile and fit the threshold out model'''
mechanized_model = MechanizedSequential([
  keras.layers.Conv1D(filters = 32, kernel_size = 5,
                      strides = 1, padding = "causal",
                      activation = "relu",
                      input_shape=[None, 1]),
  keras.layers.LSTM(32, return_sequences = True),
  keras.layers.LSTM(32, return_sequences=True),
  keras.layers.Dense(1),
  keras.layers.Lambda(lambda x: x * 200)
])
threshold_history_list, threshold_rnn_forecast_list, threshold_generalization_error_list = eval_multiple_rounds(mechanized_model, stepped_rounds, mechanism = Mechanism.THRESHOLD, sigma = 0.1, hold_frac = 0.4, threshold = 0.5)
print(threshold_generalization_error_list)

# threshold_generalization_error_list = [15.81994, 19.699022, 23.081583, 25.196918, 26.020424]

""" plot the generalization error """
plt.figure()
plot_error(stepped_rounds, baseline_generalization_error_list, "Baseline")
plot_error(stepped_rounds, gaussian_generalization_error_list, "Gaussian")
plot_error(stepped_rounds, threshold_generalization_error_list, "Threshold")
plt.show()


'''
Evaluating the LSTM Forecasting with Checkpoint
'''

def fit_with_earlystop_and_checkpoint():
    keras.backend.clear_session()
    tf.random.set_seed(42)
    np.random.seed(42)

    WINDOW_SIZE = 30
    train_set = seq2seq_window_dataset(x_train, WINDOW_SIZE,
                                    batch_size=128)

    valid_set = seq2seq_window_dataset(x_valid, WINDOW_SIZE,
                                    batch_size=128)

    mechanized_model = MechanizedSequential([
    keras.layers.Conv1D(filters=32, kernel_size=5,
                        strides=1, padding="causal",
                        activation="relu",
                        input_shape=[None, 1]),
    keras.layers.LSTM(32, return_sequences=True),
    keras.layers.LSTM(32, return_sequences=True),
    keras.layers.Dense(1),
    keras.layers.Lambda(lambda x: x * 200)
    ])


    optimizer = keras.optimizers.SGD(lr=1e-5, momentum=0.9)
    mechanized_model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = True)

    mechanized_model_checkpoint = keras.callbacks.ModelCheckpoint(
        "mechanized_checkpoint.h5", save_best_only=True)
    early_stopping = keras.callbacks.EarlyStopping(patience=50)


    ''' Compile and fit the empirical model as baseline'''

    mechanized_model.fit(train_set, epochs= 2,
            validation_data=valid_set,
            callbacks=[early_stopping, mechanized_model_checkpoint])

    empirical_model = keras.models.load_model("mechanized_checkpoint.h5",
                                            custom_objects={'MechanizedSequential':MechanizedSequential})

    empirical_rnn_forecast = model_forecast(empirical_model, series[:,  np.newaxis], WINDOW_SIZE)
    empirical_rnn_forecast = empirical_rnn_forecast[TRAIN_SIZE - WINDOW_SIZE:-1, -1, 0]



    ''' Compile and fit the Gaussian model as baseline'''
    mechanized_model.choose_mech(Mechanism.GAUSSIAN)
    mechanized_model.set_mechanism_para(0, 0.05)

    optimizer = keras.optimizers.SGD(lr=1e-5, momentum=0.9)
    mechanized_model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = True)

    mechanized_model_checkpoint = keras.callbacks.ModelCheckpoint(
        "mechanized_checkpoint.h5", save_best_only=True)
    early_stopping = keras.callbacks.EarlyStopping(patience=50)
    mechanized_model.fit(train_set, epochs = 2,
            validation_data=valid_set,
            callbacks=[early_stopping, mechanized_model_checkpoint])

    gaussian_model = keras.models.load_model("mechanized_checkpoint.h5",
                                            custom_objects={'MechanizedSequential':MechanizedSequential})

    gaussian_rnn_forecast = model_forecast(gaussian_model, series[:,  np.newaxis], WINDOW_SIZE)
    gaussian_rnn_forecast = gaussian_rnn_forecast[TRAIN_SIZE - WINDOW_SIZE:-1, -1, 0]


    ''' Compile and fit the Thresholdout model as baseline'''
    mechanized_model.choose_mech(Mechanism.THRESHOLD)
    mechanized_model.set_mechanism_para(sigma = 0.1, hold_frac = 0.4, threshold = 0.5)

    optimizer = keras.optimizers.SGD(lr=1e-5, momentum=0.9)
    mechanized_model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = True)

    mechanized_model_checkpoint = keras.callbacks.ModelCheckpoint(
        "mechanized_checkpoint.h5", save_best_only=True)
    early_stopping = keras.callbacks.EarlyStopping(patience=50)
    mechanized_model.fit(train_set, epochs = 2,
            validation_data=valid_set,
            callbacks=[early_stopping, mechanized_model_checkpoint])

    threshold_model = keras.models.load_model("mechanized_checkpoint.h5",
                                            custom_objects={'MechanizedSequential':MechanizedSequential})

    threshold_rnn_forecast = model_forecast(threshold_model, series[:,  np.newaxis], WINDOW_SIZE)
    threshold_rnn_forecast = threshold_rnn_forecast[TRAIN_SIZE - WINDOW_SIZE:-1, -1, 0]



    plt.figure(figsize=(10, 6))
    plot_series(time_valid, x_valid, label = "True")
    plot_series(time_valid, gaussian_rnn_forecast, label = "Gaussian")

    plot_series(time_valid, threshold_rnn_forecast, label = "Threshold")
    plot_series(time_valid, empirical_rnn_forecast, label = "Baseline")
    plt.show()
    keras.metrics.mean_absolute_error(x_valid, gaussian_rnn_forecast).numpy()