import numpy as np
import matplotlib.pyplot as plt

def grafica_brownian(delta, time, number_graphics):
    A = [0]
    B = [0]  
    for j in range(number_graphics):
        number_partitions = int(time / delta)
        x = [0]
        y = [0]
        bias = 0
        for number in range(2, number_partitions + 1):
            r = bias + delta ** (1 / 2) * np.random.normal(0, 1)
            x.append(number * delta)
            y.append(r)
            bias = r
        A.append(x)
        B.append(y)
    fig, ax = plt.subplots()
    ax.plot(A[1], B[1], 'g', label = 'Trayectoria 1')
    ax.legend()

            
            
   
