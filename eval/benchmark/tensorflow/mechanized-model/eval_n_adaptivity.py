import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import math

keras = tf.keras
WINDOW_SIZE = 30
TRAIN_SIZE = 1000
EPOCH = 5

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

# noise_level = 5
# noise = white_noise(time, noise_level, seed=42)

# series += noise

time_train = time[:TRAIN_SIZE]
x_train = series[:TRAIN_SIZE]
time_valid = time[TRAIN_SIZE:]
x_valid = series[TRAIN_SIZE:]



import sys
sys.path.append("..")

from mechanism.mechanized_sequential import MechanizedSequential
from mechanism.mechanized_sequential import Mechanism


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



def compile_and_fit_model(model, train_set, eager = False):
    keras.backend.clear_session()
    tf.random.set_seed(42)
    np.random.seed(42)
    
    lr_schedule = keras.callbacks.LearningRateScheduler(
        lambda epoch: 1e-8 * 10**(epoch / 20))
    optimizer = keras.optimizers.SGD(lr=1e-8, momentum=0.9)
    model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = eager)

    history = model.fit(train_set, epochs = EPOCH, callbacks=[lr_schedule])
    return history

def eval_model(model, step, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    ''' Compile and fit the empirical model as baseline'''
    model = MechanizedSequential([
    keras.layers.Conv1D(filters = 32, kernel_size = 5,
                        strides = 1, padding = "causal",
                        activation = "relu",
                        input_shape=[None, 1]),
    keras.layers.LSTM(32, return_sequences = True),
    keras.layers.LSTM(32, return_sequences=True),
    keras.layers.Dense(1),
    keras.layers.Lambda(lambda x: x * 200)
    ])    

    train_set = seq2seq_window_dataset(x_train, WINDOW_SIZE,
                                    batch_size = math.floor(TRAIN_SIZE/step))
    
    if mechanism:
        model.choose_mech(mechanism)
        model.set_mechanism_para(sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history = compile_and_fit_model(model, train_set, True)
    else:
        history = compile_and_fit_model(model, train_set, False)


    ''' Validate the result'''
    model.choose_mech(None)
    predict = model_forecast(model, series[:,  np.newaxis], WINDOW_SIZE)
    predict = predict[TRAIN_SIZE - WINDOW_SIZE:-1, -1, 0]
    return history, predict


def generalization_error(predict):
    error = keras.metrics.RootMeanSquaredError()
    error.update_state(x_valid, predict)
    print(error.result().numpy())
    return error.result().numpy()


def eval_multiple_rounds(rounds, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    history_list, predict_list, generalization_error_list = [], [], []
    for r in rounds:
        history, predict = eval_model(step = r, mechanism = mechanism, sigma = sigma, hold_frac = hold_frac, threshold = threshold)
        history_list.append(history)
        predict_list.append(predict)
        generalization_error_list.append(generalization_error(predict)) 

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

max_rounds = 20

stepped_rounds = range(math.floor(max_rounds/2), max_rounds, 2)

''' eval the empirical model as baseline'''
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




baselin_history_list, baseline_predict_list, baseline_generalization_error_list = eval_multiple_rounds(stepped_rounds)
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

