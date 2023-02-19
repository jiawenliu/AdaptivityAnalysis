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
  
  def choose_mech(self, mech = None):
     self.mechanism = mech

  def compute_metrics_gaussin(self, x, y, y_pred, sample_weight):
      noise = tf.random.normal(
        tf.shape(y_pred),
        mean=0.0,
        stddev=0.03,
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
         return self.compute_metrics_gaussin(x, y, y_pred, sample_weight)
      else:
         self.compiled_metrics.update_state(y, y_pred, sample_weight)
         return super(MechanizedSequential, self).compute_metrics(
        x, y, y_pred, sample_weight)