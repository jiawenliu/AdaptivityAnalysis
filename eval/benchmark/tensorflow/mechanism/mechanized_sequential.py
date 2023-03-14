import tensorflow as tf
# from cw_funcs import cw_funcs as cw
import numpy as np
from enum import Enum
import tensorflow_probability as tfp



class Mechanism():
   class MechanismType(Enum):
        NONE = 0
        GAUSSIAN = 1
        DATASPLIT = 2
        THRESHOLD = 3

   def __init__(self, mechanism_type = MechanismType.NONE, mu = 0.0, sigma = 0.1, hold_frac = 0.5, threshold = 0.5, beta = None, tau = None, check_for_width = None):

        super().__init__()
        self.mechanism_type = mechanism_type
        '''
        Parameters for the Gaussian mechanism 
        '''
        self.mu = mu
        self.sigma = sigma

        '''
        Parameters for the GnC mechanism 
        '''      
        self.beta = beta
        self.tau = tau
        self.check_for_width = check_for_width


        '''
        Parameters for the Naive Data Splitting mechanism 
        '''
        self.split_size = None


        '''
        Parameters for the Thresholdout mechanism 
        '''
        self.hold_size = None
        self.train_size = None 
        assert 0.0 < hold_frac <= 1.0, "hold_frac should take a value in (0, 1]."
        self.hold_frac = hold_frac
        self.threshold = threshold


class MechanizedSequential(tf.keras.Sequential):


  def __init__(self, mechanism = Mechanism(Mechanism.MechanismType.NONE), *args, **kwargs):
      super(MechanizedSequential, self).__init__(*args, **kwargs)
      self.mechanism = mechanism


      '''
      Parameters for the GnC mechanism 
      '''
      self.mu = 0.0
      self.sigma = 0.03

      '''
      Parameters for the GnC mechanism 
      '''      
      self.beta = None
      self.tau = None
      self.check_for_width = None   


      '''
      Parameters for the Naive Data Splitting mechanism 
      '''
      self.split_size = None


      '''
      Parameters for the Thresholdout mechanism 
      '''
      self.hold_size = None
      self.train_size = None
      self.hold_frac = None
      self.threshold = None
      self.noisy_thresh = self.mechanism.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
         
  def choose_mech(self, mech = Mechanism(Mechanism.MechanismType.NONE)):
     self.mechanism = mech
  
  def set_gaussian_para(self, mu, sigma):
     return
     self.set_mechanism_para(mu, sigma)

  def set_mechanism_para(self, mu = 0.0, sigma = None, hold_frac = 0.5, threshold = 0.5, beta = None, tau = None, check_for_width = None):
      return
      self.mu = mu
      self.sigma = sigma
      self.beta = beta
      self.tau = tau
      self.check_for_width = check_for_width
      assert 0.0 < hold_frac <= 1.0, "hold_frac should take a value in (0, 1]."
      self.hold_frac = hold_frac
      self.threshold = threshold
      self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.sigma)
     
  def compute_metrics_gaussin(self, x, y, y_pred, sample_weight):
      x_noise = tf.random.normal(
            tf.shape(x),
            mean=self.mu,
            stddev=self.sigma,
            dtype=x.dtype,
            seed=None,
            name=None
            )
      
      noised_x = x_noise + x
  
      y_noise = tf.random.normal(
        tf.shape(y),
        mean=self.mechanism.mu,
        stddev=self.mechanism.sigma,
        dtype = y.dtype,
        seed=None,
        name=None
        )
      noised_y = y + y_noise

      self.compiled_metrics.update_state(noised_y, y_pred, sample_weight)

      metrics_results = super(MechanizedSequential, self).compute_metrics(
        noised_x, y, y_pred, sample_weight)
      return metrics_results
  
  def compute_metrics(self, x, y, y_pred, sample_weight):
      if self.mechanism is None:
        self.compiled_metrics.update_state(y, y_pred, sample_weight)
        return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)
      elif self.mechanism.mechanism_type == Mechanism.MechanismType.GAUSSIAN:
         return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)
         # return self.compute_metrics_gaussin(x, y, y_pred, sample_weight)
      else:
         self.compiled_metrics.update_state(y, y_pred, sample_weight)
         return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)
      
  def train_step(self, data):
      # Unpack the data. Its structure depends on your model and
      # on what you pass to `fit()`.
      if self.mechanism is None:
        return super(MechanizedSequential, self).train_step(data)
      elif self.mechanism.mechanism_type == Mechanism.MechanismType.GAUSSIAN:
         print("In Gaussian Mechanism")
         return self.gaussian_train_step(data)
      elif self.mechanism.mechanism_type == Mechanism.MechanismType.DATASPLIT:
         print("In Naive Data Splitting Mechanism")
         return self.data_split_train_step(data)
      elif self.mechanism.mechanism_type == Mechanism.MechanismType.THRESHOLD:
         print("In Threshold out Mechanism")
         return self.thresholdout_train_step(data)
      else:
        return super(MechanizedSequential, self).train_step(data)

      
  
  def gaussian_train_step(self, data):
      # Unpack the data. Its structure depends on your model and
      # on what you pass to `fit()`.
      x, y = data
      with tf.GradientTape() as tape:
         print("Create Gaussian noise when accessing the training data")
         if (x.dtype == tf.string or x.dtype == tf.complex64) :
            print("Cannot create Gaussian noise for non-numeric data")
            x_train = x
         else:
            x_noise = tf.random.normal(
                  tf.shape(x),
                  mean=self.mechanism.mu,
                  stddev=self.mechanism.sigma,
                  dtype=x.dtype,
                  seed=None,
                  name=None
                  )
            x_train = x_noise + x
         y_pred = self(x_train, training=True)  # Forward pass

         
         loss = self.compiled_loss(y, y_pred, regularization_losses=self.losses)

      # Compute gradients
      trainable_vars = self.trainable_variables
      gradients = tape.gradient(loss, trainable_vars)

      # Update weights
      self.optimizer.apply_gradients(zip(gradients, trainable_vars))

      # Update metrics (includes the metric that tracks the loss)
      self.compiled_metrics.update_state(y, y_pred)

      # Return a dict mapping metric names to current value
      return {m.name: m.result() for m in self.metrics}
  
  '''
  Naive Data Splitting Mechanism:
  Either splitting the data in smaller batch-size and train with more steps,
  or doing the same training steps as the one without any mechanism.
  This one is the version that is the same as the train_step without any mechanism.
  '''
  def data_split_train_step(self, data):
      # Unpack the data. Its structure depends on your model and
      # on what you pass to `fit()`.

      x, y = data
      with tf.GradientTape() as tape:
         print("In Naive Data Split")
         y_pred = self(x, training=True)  # Forward pass
         loss = self.compiled_loss(y, y_pred, regularization_losses=self.losses)
      # Compute gradients
      trainable_vars = self.trainable_variables
      gradients = tape.gradient(loss, trainable_vars)

      # Update weights
      self.optimizer.apply_gradients(zip(gradients, trainable_vars))

      # Update metrics (includes the metric that tracks the loss)
      self.compiled_metrics.update_state(y, y_pred)

      # Return a dict mapping metric names to current value
      return {m.name: m.result() for m in self.metrics}
  
  def thresholdout_train_step(self, data):
      # Unpack the data. Its structure depends on your model and
      # on what you pass to `fit()`.
      x, y = data
      for _, l in enumerate(x.shape):
         if l:
            length = l
      if(length > 1):
         hold_size, train_size = int(length  * (self.mechanism.hold_frac)), int(length  * (1.0 - self.mechanism.hold_frac))
         x_train, y_train, x_hold, y_hold = x[hold_size:], y[hold_size:], x[:hold_size], y[:hold_size]
      else:
         hold_size, train_size = length, length
         x_train, y_train, x_hold, y_hold = x, y, x, y
         
      with tf.GradientTape() as tape:

         y_pred_train = self(x_train, training=True)  # Forward pass
         y_pred_hold = self(x_hold, training = True)
         '''
         TODO: Need to consider one of the following model as one shot of query:
          model-1. one step of training, the result of the logistic is a query
          model-2. one step of training, the losse of the trained logistic v.s. the true logistic.
         '''

         '''
         model-1.
         drawback: the query result isn't unform data type, the trained logistic has different size dependents on the databse size.
         '''
         diff = (np.sum(y_pred_train.numpy(), axis = 0) / train_size - np.sum(y_pred_hold.numpy(), axis = 0) / hold_size)
         mean_abs_diff = np.absolute(diff).mean()
         if mean_abs_diff >= self.noisy_thresh + np.random.laplace(0, 4 * self.mechanism.sigma):
            self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
            y_true, y_pred = y_hold, y_pred_hold + tfp.distributions.Laplace(self.mechanism.mu, self.mechanism.sigma).sample(tf.shape(y_pred_hold))
            loss = self.compiled_loss(y_hold, y_pred_hold, regularization_losses=self.losses)
         else:
            y_true, y_pred = y_train, y_pred_train
            loss = self.compiled_loss(y_train, y_pred_train, regularization_losses=self.losses)

         '''
         model-2:
         drawback: the "loss of  the trained logistic v.s. the true logistic" isn't a direct
          query on the data. 
         advantage: the losses in different steps or different size of database always have the same type.
         '''
         loss = self.compiled_loss(y_train, y_pred_train, regularization_losses=self.losses)
         loss_hold = self.compiled_loss(y_hold, y_pred_hold, regularization_losses=self.losses)

         if np.abs(loss - loss_hold) >= self.noisy_thresh + np.random.laplace(0, 4 * self.sigma):
            self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.sigma)

            loss = loss_hold + tfp.distributions.Laplace(self.mu, self.sigma).sample(tf.shape(loss_hold))
            # min(1.0, max(0.0, loss_hold + np.random.laplace(0, self.sigma)))
            y_true, y_pred = y_hold, y_pred_hold
         else:
            y_true, y_pred = y_train, y_pred_train
            loss = self.compiled_loss(y_train, y_pred_train, regularization_losses=self.losses)

      
      
      # Compute gradients
      trainable_vars = self.trainable_variables
      gradients = tape.gradient(loss, trainable_vars)
      # Update weights
      self.optimizer.apply_gradients(zip(gradients, trainable_vars))

      # Update metrics (includes the metric that tracks the loss)
      self.compiled_metrics.update_state(y_true, y_pred)

      # Return a dict mapping metric names to current value
      return {m.name: m.result() for m in self.metrics}