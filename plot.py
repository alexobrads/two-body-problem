#!/c/Users/obrads/anaconda3/python

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

fig = plt.figure()
graph, = plt.plot([], [])


results = []
with open("results") as file: 
    lines = file.readlines()
    results.append([line.split() for line in lines][1:])

res = np.asarray(results[0])
x = res[:,1].astype('float64')
y = res[:,2].astype('float64')


def animate(i):
    plt.clf()
    plt.xlim(-3.5, 1.5)
    plt.ylim(-1.5, 1.5)
    plt.scatter(x[i], y[i], s=10, c="black")
    plt.plot(x[:i], y[:i], lw=1, c="black")
    plt.scatter(0, 0, s=20, c="black")
    return graph

ani = animation.FuncAnimation(
    fig, animate, frames=5000, interval=2)

plt.show()