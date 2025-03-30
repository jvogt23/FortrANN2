import math
import nnfs
import numpy as np
from nnfs.datasets import spiral_data, vertical_data

X, y = spiral_data(samples=100, classes=3)
X_arr = np.asarray(X)
y_arr = np.asarray(y)
np.savetxt("X.csv", X_arr, delimiter=",")
np.savetxt("y.csv", y_arr, delimiter=",")