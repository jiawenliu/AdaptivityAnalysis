import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

keras = tf.keras

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
  

def seq2seq_window_dataset(series, window_size, batch_size=32,
                           shuffle_buffer=1000):
    series = tf.expand_dims(series, axis=-1)
    ds = tf.data.Dataset.from_tensor_slices(series)
    ds = ds.window(window_size + 1, shift=1, drop_remainder=True)
    ds = ds.flat_map(lambda w: w.batch(window_size + 1))
    ds = ds.shuffle(shuffle_buffer)
    ds = ds.map(lambda w: (w[:-1], w[1:]))
    return ds.batch(batch_size).prefetch(1)
  

def model_forecast(model, series, window_size):
    ds = tf.data.Dataset.from_tensor_slices(series)
    ds = ds.window(window_size, shift=1, drop_remainder=True)
    ds = ds.flat_map(lambda w: w.batch(window_size))
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

split_time = 1000
time_train = time[:split_time]
x_train = series[:split_time]
time_valid = time[split_time:]
x_valid = series[split_time:]


import sys
sys.path.append("..")

from mechanism.mechanized_sequential import MechanizedSequential
from mechanism.mechanized_sequential import Mechanism

keras.backend.clear_session()
tf.random.set_seed(42)
np.random.seed(42)

window_size = 30
train_set = seq2seq_window_dataset(x_train, window_size,
                                   batch_size = 128)
# tf.enable_eager_execution()

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


def compile_and_fit_model(model, eager):
    lr_schedule = keras.callbacks.LearningRateScheduler(
        lambda epoch: 1e-8 * 10**(epoch / 20))
    optimizer = keras.optimizers.SGD(lr=1e-8, momentum=0.9)
    model.compile(loss=keras.losses.Huber(),
                optimizer=optimizer,
                metrics=["mae"],
                run_eagerly = eager)

    return model.fit(train_set, epochs = 2, callbacks=[lr_schedule])

def eval_model(model, mechanism = None, sigma = 0.1, hold_frac = 0.4, threshold = 0.5):
    ''' Compile and fit the empirical model as baseline'''
    if mechanism:
        model.choose_mech(mechanism)
        model.set_mechanism_para(sigma = sigma, hold_frac = hold_frac, threshold = hold_frac)
    history = compile_and_fit_model(mechanized_model, False)


    ''' Validate the result'''
    rnn_forecast = model_forecast(mechanized_model, series[:,  np.newaxis], window_size)
    rnn_forecast = rnn_forecast[split_time - window_size:-1, -1, 0]
    return history, rnn_forecast

''' eval the empirical model as baseline'''

empirical_history, empirical_rnn_forecast = eval_model(mechanized_model, None)

''' Compile and fit the gaussian model'''

# mechanized_model.choose_mech(Mechanism.GAUSSIAN)
# mechanized_model.set_mechanism_para(sigma = 0.1)
# # lr_schedule = keras.callbacks.LearningRateScheduler(
# #     lambda epoch: 1e-8 * 10**(epoch / 20))
# # optimizer = keras.optimizers.SGD(lr=1e-8, momentum=0.9)
# # mechanized_model.compile(loss=keras.losses.Huber(),
# #               optimizer=optimizer,
# #               metrics=["mae"],
# #               run_eagerly=True)
# # gaussian_history = mechanized_model.fit(train_set, epochs = 2, callbacks=[lr_schedule])

# gaussian_history = compile_and_fit_model(mechanized_model, True)

# gaussian_rnn_forecast = model_forecast(mechanized_model, series[:,  np.newaxis], window_size)
# gaussian_rnn_forecast = gaussian_rnn_forecast[split_time - window_size:-1, -1, 0]
gaussian_history, gaussian_rnn_forecast = eval_model(mechanized_model, Mechanism.GAUSSIAN, sigma = 0.1)

''' Compile and fit the threshold out model'''
# mechanized_model.choose_mech(Mechanism.THRESHOLD)
# mechanized_model.set_mechanism_para(sigma = 0.1, hold_frac = 0.4, threshold = 0.5)

# threshold_history = compile_and_fit_model(mechanized_model, True)

# threshold_rnn_forecast = model_forecast(mechanized_model, series[:,  np.newaxis], window_size)
# threshold_rnn_forecast = threshold_rnn_forecast[split_time - window_size:-1, -1, 0]

threshold_history, threshold_rnn_forecast = eval_model(Mechanism.THRESHOLD, sigma = 0.1, hold_frac = 0.4, threshold = 0.5)

def generalization_error():
    keras.metrics.mean_absolute_error(x_valid, gaussian_rnn_forecast).numpy()

'''
Plot the Comparison of the Three Model
'''
def plot_history():
    plt.figure()
    plt.semilogx(empirical_history.history["lr"], empirical_history.history["loss"], label = "Baseline")
    plt.semilogx(gaussian_history.history["lr"], gaussian_history.history["loss"], label = "Gaussian")
    plt.semilogx(threshold_history.history["lr"], threshold_history.history["loss"], label = "Thresholdout")
    # plt.axis([1e-8, 1e-4, 0, 30])
    plt.legend()
    plt.show()


'''
Evaluating the LSTM Forecasting with Checkpoint
'''

def fit_with_earlystop_and_checkpoint():
    keras.backend.clear_session()
    tf.random.set_seed(42)
    np.random.seed(42)

    window_size = 30
    train_set = seq2seq_window_dataset(x_train, window_size,
                                    batch_size=128)

    valid_set = seq2seq_window_dataset(x_valid, window_size,
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

    empirical_rnn_forecast = model_forecast(empirical_model, series[:,  np.newaxis], window_size)
    empirical_rnn_forecast = empirical_rnn_forecast[split_time - window_size:-1, -1, 0]



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

    gaussian_rnn_forecast = model_forecast(gaussian_model, series[:,  np.newaxis], window_size)
    gaussian_rnn_forecast = gaussian_rnn_forecast[split_time - window_size:-1, -1, 0]


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

    threshold_rnn_forecast = model_forecast(threshold_model, series[:,  np.newaxis], window_size)
    threshold_rnn_forecast = threshold_rnn_forecast[split_time - window_size:-1, -1, 0]



    plt.figure(figsize=(10, 6))
    plot_series(time_valid, x_valid, label = "True")
    plot_series(time_valid, gaussian_rnn_forecast, label = "Gaussian")

    plot_series(time_valid, threshold_rnn_forecast, label = "Threshold")
    plot_series(time_valid, empirical_rnn_forecast, label = "Baseline")
    plt.show()
    keras.metrics.mean_absolute_error(x_valid, gaussian_rnn_forecast).numpy()