#!/usr/bin/python3
# coding=UTF-8
'''
Created on 12 декабря 2019 г.

построение 3D-surface_plot функций z=f(x,y) в прямоугольной области
'''

import os, sys
import math

import matplotlib        as mpl
import matplotlib.pyplot as plt
import numpy             as np

from mpl_toolkits.mplot3d import Axes3D
from matplotlib.ticker    import LinearLocator

if __name__ == '__main__':
    pass

# ************************************************

mpl.rcParams['figure.figsize'] = [8.0, 6.0]

#mpl.rcParams['figure.dpi'] = 100
#mpl.rcParams['savefig.dpi'] = 400

#print(mpl.rcParams['figure.figsize'])
#print(mpl.rcParams['figure.dpi'])
#print(mpl.rcParams['savefig.dpi'])

# ************************************************

# формирование имени файла данных
arr = sys.argv[:] # параметры командной строки
if len(arr) > 1 :
    flname = "fort."+arr[1]
else:
    flname = "fort.53" # по умолчанию

# ************************************************

# открытие файла данных
fl = open(flname)

# ************************************************
# обработка параметра FLNAME
line = fl.readline()
flnew = line.split()[0] # имя выходного файла рисунка

# ************************************************
# обработка параметров сетки

line = fl.readline()
nex = int(line.split()[1]) # количество точек сетки по x-оси 

line = fl.readline()
ney = int(line.split()[1]) # количество точек сетки по y-оси 

line = fl.readline()
x1 = float(line.split()[1]) # left   граница прямоугольной области 

line = fl.readline()
x2 = float(line.split()[1]) # right  граница прямоугольной области 

line = fl.readline()
y1 = float(line.split()[1]) # bottom граница прямоугольной области 

line = fl.readline()
y2 = float(line.split()[1]) # top    граница прямоугольной области 

# ************************************************

title  =""      # наименование рисунка
chdata = "#***" # идентификатор блока числовых данных

for line in fl:

    line = line.split() # расщепление текущей строки на слова
    istr = line[0]      # первое слово строки - ключевой идентификатор

    if istr[:4] == chdata     : # обнаружена строка-разделитель
        break 
    elif istr == "TITLE"      : # наименование рисунка 
        title = " ".join(line[1:]) 

# ************************************************

z = np.fromfile(fl, count=(nex*ney), sep=" ") # создание одномерного массива значений функции z=f(x,y) в прямоугольной области
z = np.reshape(z,(nex,ney))                   # создание двумерного  массива значений функции z=f(x,y) в прямоугольной области 


# ************************************************

dx2 = (x2-x1)/(2*nex)                # половина шага сетки по x-оси 
x = np.linspace(x1+dx2, x2-dx2, nex) # массив координат на x-оси

dy2 = (y2-y1)/(2*ney)                # половина шага сетки по y-оси 
y = np.linspace(y1+dy2, y2-dy2, ney) # массив координат на y-оси

xx, yy = np.meshgrid(x, y)

# ************************************************

# create a new figure
fig = plt.figure(" ")

# наименование рисунка 
fig.suptitle(title) 

# the dimensions of the new axes
# [left, bottom, width, height] values in 0-1 relative figure coordinates
rect = [0.10, 0.10, 0.8, 0.8]  

# create a new axes
ax = fig.add_axes(rect, projection='3d') 

# Plot the surface
pl = ax.plot_surface(xx, yy, z, rstride=1, cstride=1, color='xkcd:lightblue')


# Customize the z axis.
ax.set_zlim(np.amin(z), 3.0*np.amax(z))
ax.w_zaxis.set_major_locator(LinearLocator(5))



# сохранение файла рисунка
plt.savefig(flnew) 

# переименование текстового файла данных
#os.rename(flname, flnew[:-3]+"txt") 

# визуализация рисунка
#plt.show()

