#!/usr/bin/python3
# coding=UTF-8
'''
Created on 07 декабря 2019 г.
@author: alex

построение 2D-image функций z=f(x,y) в прямоугольной области
'''

import os, sys
import math

import matplotlib        as mpl
import matplotlib.pyplot as plt
import numpy             as np
import numpy.ma          as ma

#from cycler import cycler 

if __name__ == '__main__':
    pass

#sys.exit(11)

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
    flname = "fort.52" # по умолчанию

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

chdata = "#***" # идентификатор блока числовых данных

title  ="" # наименование рисунка
mamm   ="" # флаг учета масок при масштабировании
mamax  ="" # максимальное значение for mask
mamin  ="" #  минимальное значение for mask
scale  ="" # масштабирование уровня значений
levels ="" # количество интервалов между контурами
nodes  ="" # порядок элемента 0/1 
filled ="" # флаг цветовой заливки 


for line in fl:

    line = line.split() # расщепление текущей строки на слова
    istr = line[0]      # первое слово строки - ключевой идентификатор

    if istr[:4] == chdata     : # обнаружена строка-разделитель
        break 
    elif istr == "TITLE"      : # наименование рисунка 
        title  = " ".join(line[1:]) 
    elif istr == "MASK"       : # флаг учета масок при масштабировании 
        mamm   = " ".join(line[1:]) 
    elif istr == "MAMAX"      : # максимальное значение for mask 
        mamax  = " ".join(line[1:]) 
    elif istr == "MAMIN"      : # минимальное значение for mask 
        mamin  = " ".join(line[1:]) 
    elif istr == "SCALE"      : # масштабирование уровня значений  
        scale  = " ".join(line[1:]) 
    elif istr == "LEVELS"     : # количество интервалов между контурами 
        levels = " ".join(line[1:]) 
    elif istr == "NODES"      : # флаг порядок элемента 0/1
        nodes  = " ".join(line[1:]) 
    elif istr == "FILLED"     : # флаг цветовой заливки областей
        filled = " ".join(line[1:]) 


# ************************************************

z = np.fromfile(fl, sep=" ") # создание одномерного массива значений функции z=f(x,y) в прямоугольной области

if z.size != (nex*ney) :
    sys.exit(1)

zmax = np.amax(z)
zmin = np.amin(z)

z = np.reshape(z,(nex,ney))                # создание двумерного  массива значений функции z=f(x,y) в прямоугольной области 

if len(mamm) > 0 : # флаг учета масок при масштабировании
    mamm = int(mamm)
else :
    mamm = 0        # by default

if (len(mamax) > 0) and (len(mamin) > 0) : # заданы максимальное и минимальное значение for mask
    if float(mamax) <= float(mamin) :       
        sys.exit(2)

if len(mamax) > 0 : # задано максимальное значение for mask
    mamax = float(mamax)
    z= ma.masked_greater_equal(z, mamax) # Mask an array where greater than a given value
    if mamm == 1  : # флаг учета масок при масштабировании
        zmax = mamax 

if len(mamin) > 0 : # задано минимальное значение for mask
    mamin = float(mamin)
    z= ma.masked_less_equal(z, mamin) # Mask an array where less than or equal to a given value 
    if mamm == 1  : # флаг учета масок при масштабировании
        zmin = mamin 

if len(scale) > 0 :    # масштабирование уровня значений
    scale = float(scale)
    zmm = zmax - zmin  # размах значений
    if scale > 0.0 :   # растяжение вверх 
        zmax = zmin + zmm * scale 
    elif scale < 0.0 : # растяжение вниз
        zmin = zmax + zmm * scale 

if len(levels) > 0 :   # количество интервалов между контурами
    levels  = int(levels)
else :
    levels = 0         # by default

if len(nodes) > 0 :    # флаг порядок элемента 0/1
    nodes = int(nodes)
else :
    nodes = 0          # by default - флаг порядок элемента 0/1

if len(filled) > 0 :   # флаг цветовой заливки областей
    filled = int(filled)
else :
    filled = 1         # by default - флаг цветовой заливки областей выставлен

# ************************************************

if nodes == 0 :  # флаг порядок элемента 0

    dx2 = (x2-x1)/(2*nex)                # половина шага сетки по x-оси 
    dy2 = (y2-y1)/(2*ney)                # половина шага сетки по y-оси 

    x = np.linspace(x1+dx2, x2-dx2, nex) # массив координат на x-оси
    y = np.linspace(y1+dy2, y2-dy2, ney) # массив координат на y-оси

else: # флаг порядок элемента 1

    x = np.linspace(x1, x2, nex) # массив координат на x-оси
    y = np.linspace(y1, y2, ney) # массив координат на y-оси
    
# ************************************************

# create a new figure
fig = plt.figure(" ")

# наименование рисунка 
fig.suptitle(title) 

# the dimensions of the new axes
# [left, bottom, width, height] values in 0-1 relative figure coordinates
rect = [0.10, 0.10, 0.8, 0.8]  

# create a new axes
ax = fig.add_axes(rect) 

if filled == 1 : # флаг цветовой заливки областей выставлен

    if levels == 0 :
        im = ax.contourf(x, y, z, vmin=zmin, vmax=zmax, cmap=plt.cm.jet)
    else :
        im = ax.contourf(x, y, z, levels, vmin=zmin, vmax=zmax, cmap=plt.cm.jet)

else: # только контурные линии


    if levels == 0 :
        im = ax.contour (x, y, z, vmin=zmin, vmax=zmax, cmap=plt.cm.jet)
    else :
        #zq = (zmax-zmin)/(levels)
        #lvl = np.linspace(zmin-zq, zmax+zq, levels+3)
        #im = ax.contour (x, y, z, levels=lvl, cmap=plt.cm.jet)
        #im = ax.contour (x, y, z, levels=lvl, vmin=zmin, vmax=zmax, cmap=plt.cm.jet)
        im = ax.contour (x, y, z, levels, vmin=zmin, vmax=zmax, cmap=plt.cm.jet)

        ax.clabel(im, inline=1, fontsize=10)

# Add a color bar which maps values to colors.
fig.colorbar(im) # shrink=0.5, aspect=5 - ПАРАМЕТРЫ ДЛЯ УПРАВЛЕНИЯ

xlbl = "надпись x-оси"
ax.set_xlabel(xlbl)                    # надпись x-оси

# сохранение файла рисунка
plt.savefig(flnew) 

# переименование текстового файла данных
os.rename(flname, flnew[:-3]+"txt") 

# визуализация рисунка
#plt.show()

