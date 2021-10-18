#!/usr/bin/python3
# coding=UTF-8
'''
Created on 12 ноября 2020 г.
Модуль Model предназначен для создания модели ANN

обратная задача МДТТ

определение моделей  ANN

'''

import sys, os

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 

from keras import models
from keras import layers

import numpy             as np
import matplotlib.pyplot as plt
import ann01db as db

# коды аварийного заверщения программы
exitcode1 = 1 # ошибка оптимизатора
exitcode2 = 2 # ошибка списка метрик
exitcode3 = 3 # ошибка списка слоев
exitcode4 = 4 # ошибка списка слоев

# log-файл сообщений о работе программы
flpy3 = open(r"pymdl.3", mode='wt') 
cherr = "\nВЫПОЛНЕНИЕ модуля ann01mdl ПРЕРВАНО"

def test_mdl1(lstlrs, optimizer, loss, metrics): # list of layers

    flpy3.write("\nГИПЕРПАРАМЕТРЫ МОДЕЛИ ANN\n\n")

    activations = ("relu", 
                   "sigmoid", 
                   "softmax", 
                   "softplus", 
                   "softsign",
                   "tanh",
                   "selu",
                   "elu",
                   "exponential") 
    
    optimizers = ("sgd",
                  "rmsprop",
                  "adam",
                  "nadam",
                  "adagrad",
                  "adamax") 
    
    metrics_ = ("mse",
               "mae",
               "rmse",
               "mape",
               "msle") 
    
    if optimizer not in optimizers:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("параметр optimizer не является корректным\n")
        flpy3.write("optimizer={0}\n".format(optimizer))
        flpy3.write(cherr)
        sys.exit(exitcode1)
        
    if loss not in metrics_:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("параметр loss не является корректным\n")
        flpy3.write("loss={0}\n".format(loss))
        flpy3.write(cherr)
        sys.exit(exitcode4)
        
    if type(metrics) != list:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("параметр metrics не является списком\n")
        flpy3.write("type(metrics)={0}\n".format(type(metrics)))
        flpy3.write(cherr)
        sys.exit(exitcode2)
        
    for metric in metrics:
        if metric not in metrics_:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("параметр metrics содержит некорректное значение\n")
            flpy3.write(cherr)
            sys.exit(exitcode2)

    if type(lstlrs) != list:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("параметр lstlrs не является списком\n")
        flpy3.write("type(lstlrs)={0}\n".format(type(lstlrs)))
        flpy3.write(cherr)
        sys.exit(exitcode3)

    if len(lstlrs) < 2:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество элементов в списке слоев lstlrs меньше 2\n")
        flpy3.write("len(lstlrs)={0:d}\n".format(len(lstlrs)))
        flpy3.write(cherr)
        sys.exit(exitcode3)

    for i, lrs in enumerate(lstlrs):

        if type(lrs) != list:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("элемент списка слоев lstlrs не является списком\n")
            flpy3.write("type(lstlrs({0:d}))={1}\n".format(i, type(lrs)))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        if len(lrs) != 2:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("количество элементов в списке параметров слоя lrs не равно 2\n")
            flpy3.write("len(lstlrs({0:d}))={1:d}\n".format(i, len(lrs)))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        if type(lrs[0]) != int:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("количество нейронов не является типом int\n")
            flpy3.write("слой {0:d} тип количества нейронов {1}\n".format(i, type(lrs[0])))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        if lrs[0] < 1:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("количество нейронов в слое меньше 1\n")
            flpy3.write("слой {0:d} количество нейронов {1:d}\n".format(i, lrs[0]))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        if type(lrs[1]) != str:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("имя функции активации не является типом str\n")
            flpy3.write("слой {0:d} имя функции активации {1}\n".format(i, type(lrs[1])))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        if lrs[1] not in activations:
            flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
            flpy3.write("имя функции активации отсутствует в кортеже activations\n")
            flpy3.write("слой {0:d} имя функции активации {1}\n".format(i, lrs[1]))
            flpy3.write(cherr)
            sys.exit(exitcode3)

        flpy3.write("слой {0:2d}   нейронов {1:3d}   функция активации - {2}\n".format(i, lrs[0], lrs[1]))


def build_mdl1(dbin,    #  input DB ANN 
              dbout,    # output DB ANN
              lstlrs,
              optimizer,
              loss,
              metrics): 

    test_mdl1(lstlrs, optimizer, loss, metrics)
    
    # создание модели
    model = models.Sequential()

    # первый слой
    model.add (
    layers.Dense(lstlrs[0][0],                # количество нейронов первого слоя 
                activation=lstlrs[0][1],      # функция активации первого слоя
                input_shape=(dbin.shape[1],)) # размерность входного вектора 
    )

    for lrs in lstlrs[1:]:
        # внутренние слои
        model.add (layers.Dense(lrs[0],  # количество нейронов слоя
                   activation=lrs[1])    # функция активации слоя
        )

    # выходной слой
    model.add (
    layers.Dense(dbout.shape[1])  # размерность выходного вектора
    )

    # For a mean squared error regression problem
    model.compile(optimizer=optimizer,
                  loss=loss,
                  metrics=metrics)
    return model

def plot_hst1(history_in, history_out): # объект History 

    metrics = ("loss",
               "mse")

    ttl = "Training "
    vt1 = ", validation inside "
    vt2 = "and validation outside "

    # the dimensions of the new axes
    # [left, bottom, width, height] values in 0-1 relative figure coordinates
    rect = [0.10, 0.10, 0.8, 0.8]  

    dict_keys = history_in.keys()

    nfig = 0 # номер рисунка

    for m in metrics: 

        if m in dict_keys: # "Training"

            nfig += 1 # номер рисунка

            fig = plt.figure(nfig, figsize=(10, 10), dpi=150) # create a new figure
            ax  = fig.add_axes(rect) # create a new axes

            t_in = history_in[m]
            t_out = history_in[m]

            epochs = range(1, len(t_in) + 1)

            ax.plot(epochs, t_out, "bo", label="Training")

            title = ttl 

            v_in = history_in['val_loss']

            ax.plot(epochs, v_in, "g-", label="Inside Validation Set")

            title += vt1 
                
            v_out = history_out['val_loss']

            ax.plot(epochs, v_out, "r-", label="Outside Validation Set")

            title += vt2 

            title += m
            fig.suptitle(title)

            ax.set_xlabel("Epochs")
            ax.set_ylabel(m)
            ax.legend()

            # сохранение файла рисунка
            plt.savefig(m)
            plt.close() 

def nrm1avg(act, pred):
    result = []
    for i in range(len(act)):
        l1 = 0
        for j in range(len(act[i])):
            l1 += abs(pred[i][j] - act[i][j])
        result.append(l1)
    return sum(result) / len(result)

def nrm2avg(act, pred):
    result = []
    for i in range(len(act)):
        l2 = 0
        for j in range(len(act[i])):
            l2 += (pred[i][j] - act[i][j]) ** 2
        result.append(np.sqrt(l2))
    return sum(result) / len(result)

def nrm0avg(act, pred):
    result = []
    for i in range(len(act)):
        l1 = []
        for j in range(len(act[i])):
            l1.append(abs(pred[i][j] - act[i][j]))
        result.append(max(l1))
    return sum(result) / len(result)

def nrm2(act, pred):
    result = 0
    for i in range(len(act)):
        result += (pred[i] - act[i])**2;
    return result**(1/2)

def relative_nrm2avg(act, pred):
    result = []
    for i in range(len(act)):
        relative = 0
        relative += nrm2(act[i], pred[i]) / nrm2(act[i], np.zeros(len(act[i])))
        result.append(relative)
    return sum(result) / len(result)

def nrm1disp(act, pred):
    result = []
    for i in range(len(act)):
        l1 = 0
        for j in range(len(act[i])):
            l1 += abs(pred[i][j] - act[i][j])
        result.append(l1)
    return np.std(result) ** 2

def nrm2disp(act, pred):
    result = []
    for i in range(len(act)):
        l2 = 0
        for j in range(len(act[i])):
            l2 += abs(pred[i][j] - act[i][j]) ** 2
        result.append(np.sqrt(l2))
    return np.std(result) ** 2

def nrm0disp(act, pred):
    result = []
    for i in range(len(act)):
        l1 = []
        for j in range(len(act[i])):
            l1.append(abs(pred[i][j] - act[i][j]))
        result.append(max(l1))
    return np.std(result) ** 2
