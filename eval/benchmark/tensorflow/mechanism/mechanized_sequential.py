import tensorflow as tf

from enum import Enum

class Mechanism(Enum):
   GAUSSIAN = 1
   DATASPLIT = 2
   THRESHOLD = 3

class MechanizedSequential(tf.keras.Sequential):


  def __init__(self, *args, **kwargs):
    super(MechanizedSequential, self).__init__(*args, **kwargs)
    self.mechanism = None
    self.mu = 0.0
    self.sigma = 0.03
    
     
  def choose_mech(self, mech = None):
     self.mechanism = mech
  
  def set_gaussian_para(self, mu, sigma):
     self.mu = mu
     self.sigma = sigma

  def compute_metrics_gaussin(self, x, y, y_pred, sample_weight):
      noise = tf.random.normal(
        tf.shape(y_pred),
        mean=self.mu,
        stddev=self.sigma,
        dtype=tf.dtypes.float32,
        seed=None,
        name=None
        )
      noised_pred = y_pred + noise
      self.compiled_metrics.update_state(y, noised_pred, sample_weight)
      metrics_results = super(MechanizedSequential, self).compute_metrics(
        x, y, noised_pred, sample_weight)
      return metrics_results
  
  def compute_metrics(self, x, y, y_pred, sample_weight):
      if self.mechanism is None:
        self.compiled_metrics.update_state(y, y_pred, sample_weight)
        return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)
      elif self.mechanism == Mechanism.GAUSSIAN:
         print("In Gaussian Mechanism")
         return self.compute_metrics_gaussin(x, y, y_pred, sample_weight)
      else:
         self.compiled_metrics.update_state(y, y_pred, sample_weight)
         return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)
      
  def train_step(self, data):
      # Unpack the data. Its structure depends on your model and
      # on what you pass to `fit()`.
      x, y = data
      # noise = tf.random.normal(
      #   x.shape,
      #   mean=self.mu,
      #   stddev=self.sigma,
      #   dtype=tf.dtypes.float32,
      #   seed=None,
      #   name=None
      #   )
      # print("In Gaussian Mechanizem, the noised data {}".format(noise))
      with tf.GradientTape() as tape:
         print("In Gaussian Mechanizem")

         x_noise = tf.random.normal(
               tf.shape(x),
               mean=self.mu,
               stddev=self.sigma,
               dtype=x.dtype,
               seed=None,
               name=None
               )
         noised_x = x_noise + x
         y_pred = self(noised_x, training=True)  # Forward pass
         # Compute the loss value
         # (the loss function is configured in `compile()`)
         noise = tf.random.normal(
               tf.shape(y_pred),
               mean=self.mu,
               stddev=self.sigma,
               dtype = y_pred.dtype,
               seed=None,
               name=None
               )
         
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