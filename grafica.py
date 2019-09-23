import numpy as np
import matplotlib.pyplot as plt


delta = 0.01
time = 10
number_partitions = int(time / delta)
x = [0]
z = [0]
w = [0]
y = [0]
t = [0]
bias = 0

for number in range(2, number_partitions + 1):
    r = bias + delta ** (1 / 2) * np.random.normal(0, 1)
    x.append(number * delta)
    y.append(r)
    bias = r


for number in range(2, number_partitions + 1):
    r = bias + delta ** (1 / 2) * np.random.normal(0, 1)
    z.append(r)
    bias = r


for number in range(2, number_partitions + 1):
    r = bias + delta ** (1 / 2) * np.random.normal(0, 1)
    w.append(r)
    bias = r
    
for number in range(2, number_partitions + 1):
    r = bias + delta ** (1 / 2) * np.random.normal(0, 1)
    t.append(r)
    bias = r
    

fig, ax = plt.subplots()

ax.plot(x, y, 'g', label = 'Trayectoria 1')
ax.plot(x, z, 'b', label = 'Trayectoria 2') 
ax.plot(x, w, 'black', label = 'Trayectoria 3')
#ax.plot(x, t, 'purple', label = 'Trayectoria 4')
ax.legend()
#plt.show()
#plt.ioff()
#plt.plot(z, y)   
#plt.ion()   # Activa modo interactivo de dibujo


#plt.show()
