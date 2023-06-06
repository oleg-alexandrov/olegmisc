#!/usr/bin/python

import sys, os, re, random
import matplotlib.pyplot as plt
import numpy as np

# Implement a little game

# To interactively update the plot:
#   Start python. Run:
#   import sys, os; sys.path.append(os.environ['HOME'] + "/bin"); import plot_circles
#   Keep on calling do_plot().

# If the code changed, do:
#  import imp; imp.reload(plot_circles)

do_setup = False
last_index = -10

colors = ['red', 'lime', 'blue', 'magenta', 'cyan']

def onclick(event):
    global do_setup, colors, T, last_index

    #print('%s click: button=%d, x=%d, y=%d, xdata=%f, ydata=%f' %
    #      ('double' if event.dblclick else 'single', event.button,
    #       event.x, event.y, event.xdata, event.ydata))

    x = int(round(event.xdata))
    y = int(round(event.ydata))
    if T[x, y] >= 0:
        last_index = int(T[x, y])
        T[x, y] = int(last_index - 10)

    do_plot()
    
def do_plot():

    global do_setup, colors, T, last_index
    
    if not do_setup:
        T = np.zeros((10,10)) - 100
        for x in range(10):
            for y in range(10):
                if x <= 1 or y <= 1 or x >= 8 or (y >= 8 and x >= 3):
                    T[x, y] = random.randint(0, len(colors) - 1)
        do_setup = True
        

    plt.ion()
    plt.clf()
    plt.show(block=False)

    fig = plt.figure(1)

    ax = fig.add_subplot(111)
    ax.set_aspect('equal', adjustable='box')
    
    for x in range(10):
        for y in range(10):
            
            if T[x, y] >= -10:
                index = int(T[x, y])
                if index < 0:
                    index = int(last_index)

                plt.scatter(x, y, s = 380, color = colors[index])
                
                if T[x, y] < 0:
                    plt.text(x, y, 'x', horizontalalignment='center', verticalalignment='center')
                     
    plt.xlim(-1, 11) 
    plt.ylim(-1, 11) 
    plt.axis('off')
    #plt.axis('on')

    # ax = plt.gca()
    # ax.set_facecolor('black')

    fig.patch.set_facecolor('black')
    #fig.patch.set_facecolor('white')

    #plt.show()

    cid = fig.canvas.mpl_connect('button_press_event', onclick)

def reset():
    global do_setup
    do_setup = False
    do_plot()
