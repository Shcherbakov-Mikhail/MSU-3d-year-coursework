#!/usr/bin/python3
# coding=UTF-8
'''
Created on 06 марта 2019 г.
@author: alex

построение 1D-plot функций y=f(x)
'''



#os.chdir(r"/home/alex/workspace-python/gfort1")

# sys.path - содержит список путей поиска модулей
# print(sys.path)

# атрибут __file__ - путь к исполняемому файлу (не всегда полный)
# os.path.basename(__file__) - имя файла без пути к нему
# os.path.splitext() - возвращает кортеж из двух элементов: пути с названием файла и расширения к нему
# file_out = os.path.splitext(os.path.basename(__file__))[0] + '.txt'
# print(file_out)

import os, sys
import math

import matplotlib.pyplot as plt
#import matplotlib as mpl
import numpy as np

# from Cython.Includes.Deprecated import stl
from cycler import cycler 

if __name__ == '__main__':
    pass

# ************************************************
# TEST
#arr = sys.argv[:] # параметры командной строки

#print(str(arr[1].isdigit()))

#print(mpl.matplotlib_fname())

#print(plt.rcParams['axes.prop_cycle'])

# ************************************************

# формирование имени файла данных
arr = sys.argv[:] # параметры командной строки
if len(arr) > 1 :
    flname = "fort."+arr[1]
else:
    flname = "fort.50" # по умолчанию

# ************************************************

# открытие файла данных
fl = open(flname)

# чтение всех строк файла в список
lines = fl.readlines()

# закрытие файла данных
fl.close()

# ************************************************

# create a new figure
fig = plt.figure(" ")

# the dimensions of the new axes
# [left, bottom, width, height] values in 0-1 relative figure coordinates
rect = [0.10, 0.10, 0.8, 0.8]  

# create a new axes
ax = fig.add_axes(rect) 

# активация сетки на графике
ax.grid(True)

# ************************************************

# идентификатор блока числовых данных
chdata = "#***"

# обработка первой строки файла
flnew = lines[0].split()[0] # имя выходного файла рисунка

# общее количество строк
nlines = len(lines)

# номер последней строки данных
nlast = nlines - 1 

# счетчик строк
iln = 0

nclr   = "" # флаг цвета
lnmrkr = "" # код линия/маркер 


xlbl = "" # надпись x-оси
ylbl = "" # надпись y-оси

xunit = "" # единицы измерения x-оси
yunit = "" # единицы измерения y-оси

x1 = "" # left   граница диапазона изменения переменной x
x2 = "" # right  граница диапазона изменения переменной x
y1 = "" # bottom граница диапазона изменения переменной y
y2 = "" # top    граница диапазона изменения переменной y

intx = "" # целые значения переменной x

# ************************************************

# обработка заголовка файла
while iln < nlines : 
    iln += 1 # счетчик строк - переход к следующей строке

    line = lines[iln].split() # расщепление текущей строки на слова
    istr = line[0]            # первое слово строки - ключевой идентификатор

    if istr[:4] == chdata     : # обнаружена строка-разделитель
        break 
    elif istr == "NCOLOR"     : # nclr флаг цвета
        nclr = line[1]
    elif istr == "LINEMARKER" : # lnmrkr код линия/маркер
        lnmrkr = line[1]
    elif istr == "TITLE"      : # наименование рисунка 
        fig.suptitle(" ".join(line[1:])) 
    elif istr == "XLABEL"     : # надпись x-оси
        xlbl = " ".join(line[1:])
    elif istr == "YLABEL"     : # надпись y-оси
        ylbl = " ".join(line[1:])
    elif istr == "XUNIT"      : # единицы измерения x-оси
        xunit = " ".join(line[1:])
    elif istr == "YUNIT"      : # единицы измерения y-оси
        yunit = " ".join(line[1:])
    elif istr == "XLEFT"      : # left   граница диапазона изменения переменной x
        x1 = line[1]
    elif istr == "XRIGHT"     : # right  граница диапазона изменения переменной x
        x2 = line[1]
    elif istr == "YBOTTOM"    : # bottom граница диапазона изменения переменной y
        y1 = line[1]
    elif istr == "YTOP"       : # top    граница диапазона изменения переменной y
        y2 = line[1]
    elif istr == "XINTEGER"   : # intx   целые значения переменной x
        intx = line[1]

# ************************************************

# флаг цвета
if len(nclr) > 0 : # выставлен флаг цвета
    nclr = int(nclr)
else :
    nclr = 0 # by default

# код линия/маркер
if len(lnmrkr) > 0 : # задан код линия/маркер
    lnmrkr = int(lnmrkr)
else :
    lnmrkr = 0 # by default

# настройка цвета и стиля линия/маркер графика 

# 'b' - blue
# 'g' - green
# 'r' - red
# 'c' - cyan
# 'm' - magenta
# 'y' - yellow
# 'k' - black
# 'w' - white

if nclr > 0 : # черно-белые линии/маркеры 
    if lnmrkr > 0 : # маркеры
        cclr = cycler(color=['k']) * cycler(marker=['.','*','+','o','s','v','^','x','d','<','>']) * cycler(linestyle=['']) # 
    else : # линии
        cclr = cycler(color=['k']) * cycler(marker=['','.']) * cycler(linestyle=['-', '--','-.', ':' ]) # 
else : # цветные линии/маркеры
    cclr = plt.rcParams['axes.prop_cycle'] # by default
    if lnmrkr > 0 : # маркеры
        cclr = cclr * cycler(marker=['.']) * cycler(linestyle=[''])
    else : # линии
        pass        

ax.set_prop_cycle(cclr) 

# ************************************************

# intx целые значения переменной x
if len(intx) > 0 : # заданы целые значения переменной x
    intx = int(intx)
else :
    intx = 0 # by default

# ************************************************

# номер первой строки-разделитель
iln1  = iln

# label-for-legend для первой кривой на графике
if len(line) > 1 :
    lgnd = " ".join(line[1:])
else :        
    lgnd = ""

if iln == nlast :
    print("*** ВНИМАНИЕ. ОШИБКА В ИСХОДНЫХ ДАННЫХ")
    sys.exit()

# ************************************************

# обработка тела файла для нормализации значений

xlst = [] # пустой список абсцисс
ylst = [] # пустой список ординат

while iln < nlast :
    iln += 1 # счетчик строк - переход к следующей строке

    line = lines[iln].split() # расщепление текущей строки на слова

    if line[0][:4] != chdata :      # строка данных
        xlst.append(float(line[0])) # добавляем в список абсцисс первое число 
        ylst.append(float(line[1])) # добавляем в список ординат второе число

# диапазон изменения переменной x
xmin = min(xlst) #  минимальное значение x 
xmax = max(xlst) # максимальное значение x

if len(x1) > 0 and len(x2) > 0 : # заданы обе границы диапазона изменения переменной x
    x1 = float(x1) # left   граница
    x2 = float(x2) # right  граница
    if x1 < x2 :   # новые  границы заданы корректно
        xmin = x1  # left   граница
        xmax = x2  # right  граница
elif len(x1) > 0 : # задана только  left   граница
    x1 = float(x1) # left   граница
    if x1 < xmax : # новая  left   граница задана корректно
        xmin = x1  # left   граница
elif len(x2) > 0 : # задана только  right   граница
    x2 = float(x2) # right  граница
    if x2 > xmin : # новая  right   граница задана корректно
        xmax = x2  # right   граница

xabs = max(abs(xmin), abs(xmax)) # максимальное абсолютное значение x
if xabs <= 0.0 : xabs = 1.0      # корректировка нулевого значения

if intx == 0 :                   # вещественные значения переменной x (by default)

    xml  = math.log10(xabs)      # десятичный логарифм максимального абсолютного значения x
    ixml = math.floor(xml)       # порядок нормализованного значения x в интервале 1-10 
    ixml = -ixml                 # порядок множителя для нормализации значений x
    xmn  = 10**ixml              # множитель для нормализации значений x
    xmin *= xmn                  # нормализация  минимального значения x 
    xmax *= xmn                  # нормализация максимального значения x
    ixmin = math.floor(xmin)     # левая  целая граница интервала изменения x
    ixmax = math.ceil (xmax)     # правая целая граница интервала изменения x

    if ixmin == ixmax :          # левая и правая граница интервала изменения x совпадают
        ixmin -= 1               # сдвиг  левой границы интервала изменения x влево
        ixmax += 1               # сдвиг правой границы интервала изменения x право

    ax.set_xlim(left=ixmin, right=ixmax) # диапазон изменения переменной x

    if ixml != 0 : # учет нормализации в надписи x-оси
        if ixml == 1 :
            # xlbl += r"$\cdot 10$"
            xlbl = r"$10 \cdot$" + xlbl
        else :
            # xlbl += r"$\cdot 10^{"+str(ixml)+"}$"
            xlbl = r"$10^{"+str(ixml)+"} \cdot$" + xlbl

else : # целые значения переменной x

    ixmin  = int(xmin)                                   # левая  целая граница интервала изменения x
    ixmax  = int(xmax)                                   # правая целая граница интервала изменения x
    ixstep = (ixmax-ixmin)//10+1                         # шаг отметок на x-оси правой целой границы интервала изменения x
    ixmax  = ixmin+((ixmax-ixmin-1)//ixstep+1)*ixstep    # корректировка 
    xmn    = 1                                           # множитель для нормализации значений x

    ax.set_xlim(left=ixmin, right=ixmax)                 # диапазон изменения переменной x
    ax.set_xticks(np.arange(ixmin,ixmax+1, step=ixstep)) # отметки x-оси

if len(xunit) > 0 : xlbl += ", "+xunit # единицы измерения x-оси
ax.set_xlabel(xlbl)                    # надпись x-оси

# диапазон изменения переменной y

ymin = min(ylst) #  минимальное значение y
ymax = max(ylst) # максимальное значение y

if len(y1) > 0 and len(y2) > 0 : # заданы обе границы диапазона изменения переменной y
    y1 = float(y1) # bottom граница
    y2 = float(y2) # top    граница
    if y1 < y2 :   # новые  границы заданы корректно
        ymin = y1  # bottom граница
        ymax = y2  # top    граница
elif len(y1) > 0 : # задана только  bottom граница
    y1 = float(y1) # bottom граница
    if y1 < ymax : # новая  bottom граница задана корректно
        ymin = y1  # bottom граница
elif len(y2) > 0 : # задана только top     граница
    y2 = float(y2) # top    граница
    if y2 > ymin : # новая  top    граница задана корректно
        ymax = y2  # top    граница
     
yabs = max(abs(ymin), abs(ymax)) # максимальное абсолютное значение y
if yabs <= 0.0 : yabs = 1.0      # корректировка нулевого значения

yml  = math.log10(yabs)          # десятичный логарифм максимального абсолютного значения y
iyml = math.floor(yml)           # порядок нормализованного значения y в интервале 1-10 
iyml = -iyml                     # порядок множителя для нормализации значений y
ymn  = 10**iyml                  # множитель для нормализации значений y
ymin *= ymn                      # нормализация  минимального значения y
ymax *= ymn                      # нормализация максимального значения y
iymin = math.floor(ymin)         # нижняя  целая граница интервала изменения y
iymax = math.ceil (ymax)         # верхняя целая граница интервала изменения y

if iymin == iymax :              # нижняя и верхняя граница интервала изменения y совпадают
    iymin -= 1                   # сдвиг  нижней границы интервала изменения y вниз
    iymax += 1                   # сдвиг верхней границы интервала изменения y вверх

#iymax =2.5 # WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

ax.set_ylim(bottom=iymin, top=iymax) # диапазон изменения переменной y

# учет нормализации в надписи y-оси
if iyml != 0 :
    if iyml == 1 :
        # ylbl += r"$\cdot 10$"
        ylbl = r"$10 \cdot$" + ylbl
    else :
        # ylbl += r"$\cdot 10^{"+str(iyml)+"}$"
        ylbl = r"$10^{"+str(iyml)+"} \cdot$" + ylbl

if len(yunit) > 0 : ylbl += ", "+yunit # единицы измерения y-оси
ax.set_ylabel(ylbl)                    # надпись y-оси

# ************************************************

# формирование кривых на графике

xlst = [] # пустой список абсцисс
ylst = [] # пустой список ординат

# обработка тела файла для построения кривых на графике

iln = iln1 # номер первой строки данных
while iln < nlast :
    iln += 1 # счетчик строк - переход к следующей строке

    line = lines[iln].split() # расщепление текущей строки на слова

    if line[0][:4] != chdata : # обнаружена строка данных
        xlst.append(float(line[0])*xmn) # добавляем в x-список нормализованное значение 
        ylst.append(float(line[1])*ymn) # добавляем в y-список нормализованное значение

    if line[0][:4] == chdata or iln == nlast : # строка-разделитель или последняя строка

        if intx == 0 :                   # вещественные значения переменной x (by default)
            x = np.asarray(xlst,'float') # преобразование списка абсцисс в массив
        else :                           # целые значения переменной x
            x = np.asarray(xlst,'int')   # преобразование списка абсцисс в массив

        y = np.asarray(ylst) # преобразование списка абсцисс в массив

        if len(lgnd) > 0 : # есть легенда
            ax.plot(x, y, label=lgnd)
        else :
            ax.plot(x, y)

        # подготовка для новой кривой
        xlst = [] # очистка списка абсцисс
        ylst = [] # очистка списка ординат

        if line[0][:4] == chdata : # строка-разделитель
            if len(lgnd) > 0 :
                if len(line) > 1 : # LABEL-FOR-LEGEND для дополнительной кривой на графике
                    lgnd = " ".join(line[1:])
                else :        
                    lgnd = " "

# ************************************************

# легенда
if len(lgnd) > 0 :
    ax.legend(loc=0) # 0-'best';  ncol=2 - две колонки 

# сохранение файла рисунка
plt.savefig(flnew) 

# переименование текстового файла данных
os.rename(flname, flnew[:-3]+"txt") 

#os.rename(flname, flnew[:-3]+"50")

# визуализация рисунка
#plt.show()

