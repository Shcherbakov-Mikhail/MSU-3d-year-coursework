#!/usr/bin/python3
# coding=UTF-8
'''
Created on 12 ноября 2020 г.
@author: alex

препроцессор для подготовки входной БД и 
запуска процессора для решения прямой задачи

обратная задача для упругой балки

1. формирование БД для FEM:
 - БД жесткостей КЭ
 - БД распределенной нагрузки 
2. запуск процессора для решения прямой задачи

'''

import sys, os
import numpy as np

from numpy.random import default_rng
from scipy.spatial.distance import cdist, euclidean


import random
random.seed(1)

import matplotlib.pyplot as plt

import ann01db as db # имена файлов данных и запись БД для FEM

cherr = "\nВЫПОЛНЕНИЕ модуля ann01fem ПРЕРВАНО"
# log-файл сообщений о работе программы
flpy3 = open(r"pyfem.3", mode='wt') 

# FORTRAN-программа
fprg = r"D:\Eclipse\Workspace\WSFortran\STA2BS\Release\STA2BS"
# fprg = r"~/EclipseWS/FortranWS/STA2BS/Release/STA2BS"

#************************************************
# файл параметров FORTRAN-программы

# параметр балки
ib_fort = 1 # параметр типа граничных условий
            # 1 - шарнир  - шарнир 
            # 2 - заделка - свободный край 
            # 3 - заделка - шарнир
            # 4 - заделка - заделка 

xl_fort = 1.0 # длина балки

# открытие файла параметров FORTRAN-программы
fl = open(db.flfort, mode='wt')

# запись в файл номеров каналов ввода-вывода FORTRAN-программы 
fl.write(db.fldbmt.lstrip("fort.") + " "   # номер канала  ввода БД жесткостей КЭ
        +db.fldbrq.lstrip("fort.") + " "   # номер канала  ввода БД распределенной нагрузки
        +db.fldbpr.lstrip("fort.") + '\n') # номер канала вывода БД обобщенных узловых перемещений

# запись в файл параметр типа граничных условий и длины балки
fl.write("{0:d} {1:E}".format(ib_fort, xl_fort))

# закрытие файла параметров FORTRAN-программы
fl.close()

#************************************************
# log-файл

flpy3.write("Создан файл параметров FORTRAN-программы\n")

# запись в log-файл номеров каналов ввода-вывода FORTRAN-программы
flpy3.write(db.fldbmt.lstrip("fort.") + " - номер канала  ввода БД жесткостей КЭ\n")
flpy3.write(db.fldbrq.lstrip("fort.") + " - номер канала  ввода БД распределенной нагрузки\n")
flpy3.write(db.fldbpr.lstrip("fort.") + " - номер канала вывода БД обобщенных узловых перемещений\n")

# запись в log-файл параметров типа граничных условий и длины балки
flpy3.write("{0:2d} - параметр типа граничных условий\n".format(ib_fort))
flpy3.write("{0:E} - длина балки\n".format(xl_fort))

#************************************************

# параметры БД ANN
ne   = 20         # количество КЭ
# nsdb = 30*db.batch_size # количество образцов  
nsdb = 1000

# запись в log-файл параметров БД
flpy3.write("\nПАРАМЕТРЫ БД жесткостей КЭ и распределенной нагрузки\n")
flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
flpy3.write("размерность output vector - {0:d}\n".format(ne))

#************************************************

# инициализация БД
db.dbxe = np.zeros((nsdb,2))  # БД (координата дефекта, величина дефекта)
db.dbne = np.zeros((nsdb,2))  # БД (номер КЭ, величина дефекта)
db.dbmt = np.zeros((nsdb,ne)) # БД жесткостей КЭ  

#************************************************

# массивы для визуализации распределения дефектов по КЭ
x2 = np.arange(ne)
y2 = np.zeros (ne)

#************************************************
# формирование БД жесткостей КЭ (output ANN)

rng = default_rng() #  Construct a new Generator with the default BitGenerator (PCG64).

for lsdb in range(nsdb): # цикл по образцам

    db.dbxe[lsdb,0] = 1         # координата дефекта 
    db.dbxe[lsdb,1] = 1         # величина дефекта

    db.dbne[lsdb,0] = 1             # номер КЭ (float)
    db.dbne[lsdb,1] = 1             # величина дефекта

    db.dbmt[lsdb,:]  = 1.0                         # жесткость однородной балки
    num = int(db.dbne[lsdb,0])                     # номер КЭ (int)
#     db.dbmt[lsdb,num] += db.dbne[lsdb,1]           # учет дефекта жесткости

    y2[num] += 1 # количество дефектов на КЭ


# запись в log-файл параметров БД
flpy3.write("\nПАРАМЕТРЫ БД дефектов\n")
flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
flpy3.write("размерность output vector - 2\n")

# контрольная сумма дефектов
flpy3.write("\nКонтрольная сумма дефектов - {0:d}\n".format(int(np.sum(y2))))

#************************************************

# визуализация распределения дефектов по длине балки
x1 = db.dbxe[:,0]
y1 = db.dbxe[:,1]

# create a new figure
fig = plt.figure("1")

# the dimensions of the new axes
# [left, bottom, width, height] values in 0-1 relative figure coordinates
rect = [0.10, 0.10, 0.8, 0.8]  

# create a new axes
ax = fig.add_axes(rect) 

# диапазон изменения переменной x
ax.set_xlim(left=0, right=1.0)

# диапазон изменения переменной y
ax.set_ylim(bottom=-0.5, top=0.5)

# рисунок
ax.plot(x1, y1, 'k.')

# сохранение файла рисунка
fldd = "dd"  # имя выходного файла рисунка
plt.savefig(fldd) 

#************************************************

# визуализация распределения дефектов по КЭ
# create a new figure
fig = plt.figure("2")

# create a new axes
ax = fig.add_axes(rect) 

# диапазон изменения переменной x
ax.set_xlim(left=0, right=ne)

# диапазон изменения переменной y
ax.set_ylim(bottom=0, top=np.amax(y2))

# рисунок - распределения дефектов по КЭ
ax.plot(x2+1.0, y2, 'k.')

# сохранение файла рисунка
flde = "de"  # имя выходного файла рисунка
plt.savefig(flde) 

#************************************************

def normalize_vector(vect, norm_val):
    return vect*norm_val/np.sum(vect)

def generate_val_set(ch, amount, bounds, norm_val):
    val_set = []
    l_bound, u_bound = bounds
    for i in range(amount):
        coeff_vect = np.random.uniform(low=l_bound, high=u_bound, size=(1, ch.shape[0]))
        norm_coeff_vect = normalize_vector(coeff_vect, norm_val)
        val_set.append(np.dot(norm_coeff_vect, ch)[0])
    return np.array(val_set)


# БД распределенной нагрузки (output vector-2 ANN)
db.dbrq = np.zeros((nsdb,ne)) # инициализация

u_bound = 1.5
l_bound = 0.5
in_out_ratio = db.val_split_ratio

# формирование БД распределенной нагрузки (output vector-2 ANN)
#формирование БД CONVEX HULL
for lsdb in range(ne*2):            
    #axes (ne)-dimentional rectanle 
        if lsdb < ne:
            db.dbrq[lsdb,lsdb] =  l_bound
        else:
            db.dbrq[lsdb,lsdb-ne*2] =  u_bound 
    
#формирование БД TESTING SAMPLES   
val_data_size = nsdb-ne*2
val_in_size = int(val_data_size * in_out_ratio[0] / sum(list(in_out_ratio))) 
val_out_size = val_data_size - val_in_size
    
# Create a ne-dim square
# for lsdb in range(ne*2, nsdb): 
#     for el in range(ne):
#         db.dbrq[lsdb,el] =  random.uniform(0, u_bound)
         
arr_in = generate_val_set(
    db.dbrq[:ne*2], 
    amount=val_in_size, 
    bounds=(0,1), 
    norm_val=1)
 
arr_out = generate_val_set(
    db.dbrq[:ne*2], 
    amount=val_out_size,
    bounds=(0,1), 
    norm_val=0.3)   
 
db.dbrq[ne*2 : (val_in_size + ne*2)] = arr_in
db.dbrq[(val_in_size + ne*2) :] = arr_out

#************************************************
# запись БД в файлы
db.wdbfem1(nsdb, # количество образцов
           ne)   # количество КЭ
             

#          dbxe, # БД (координата дефекта, величина дефекта)
#          dbne, # БД (номер КЭ, величина дефекта)
#          dbmt, # БД жесткостей КЭ  
#          dbrq) # БД распределенной нагрузки

#************************************************

#sys.exit()

# вызов FORTRAN-программы
ierr = os.system(fprg)
#os.rename("fort.3", "fortfem.3")

#************************************************

# ввод БД обобщенных узловых перемещений dbpr[dim0,dim1]
# dim0 - количество образцов
# dim1 - количество параметров
dbheader = np.loadtxt(db.fldbpr, max_rows=1, dtype=int) # заголовок БД
db.dbpr  = np.loadtxt(db.fldbpr, skiprows=1)            # input vector ANN

if db.dbpr.shape[0] != dbheader[0] : 
    flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
    flpy3.write("количество записей в БД обобщенных узловых перемещений не соответствует\n")
    flpy3.write("количеству записей, указанному в заголовке БД\n")
    flpy3.write("db.dbpr.shape[0]={0:d}\n".format(db.dbpr.shape[0]))
    flpy3.write("     dbheader[0]={0:d}\n".format(dbheader[0]))
    flpy3.write(cherr)
    sys.exit(db.exitcode2)

if db.dbpr.shape[1] != dbheader[1] : 
    flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
    flpy3.write("количество параметров в БД обобщенных узловых перемещений не соответствует\n")
    flpy3.write("количеству параметров, указанному в заголовке БД\n")
    flpy3.write("db.dbpr.shape[1]={0:d}\n".format(db.dbpr.shape[1]))
    flpy3.write("     dbheader[1]={0:d}\n".format(dbheader[1]))
    flpy3.write(cherr)
    sys.exit(db.exitcode2)

# проверка количества записей БД обобщенных узловых перемещений
if db.dbpr.shape[0] != nsdb : 
    flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
    flpy3.write("количество записей в БД обобщенных узловых перемещений не соответствует\n")
    flpy3.write("количеству записей в БД жесткостей КЭ и распределенной нагрузки\n")
    flpy3.write("db.dbpr.shape[0]={0:d}\n".format(db.dbpr.shape[0]))
    flpy3.write("            nsdb={0:d}\n".format(nsdb))
    flpy3.write(cherr)
    sys.exit(db.exitcode2)

# запись в log-файл
flpy3.write("\nВЫПОЛНЕН ВВОД БД ANN обобщенных узловых перемещений\n")
flpy3.write("количество образцов   - {0:d}\n".format(np.shape(db.dbpr)[0]))
flpy3.write("количество параметров - {0:d}\n".format(np.shape(db.dbpr)[1]))
