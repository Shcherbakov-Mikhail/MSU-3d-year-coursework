#!/usr/bin/python3
# coding=UTF-8
'''
Created on 12 ноября 2020 г.
@author: alex

Модуль Model предназначен длясоздания модели ANN

обратная задача МДТТ

определение моделей  ANN

'''

from keras import models
from keras import layers

import numpy             as np
import matplotlib.pyplot as plt

#import numpy as np

# коды аварийного заверщения программы
exitcode3 = 3 # ошибка списка нейронов слоев

# log-файл сообщений о работе программы
flpy3 = open(r"pymdl.3", mode='wt') 
cherr = "\nВЫПОЛНЕНИЕ модуля ann01mdl ПРЕРВАНО"


def build_mdl1(dbin,    #  input DB ANN 
               dbout,   # output DB ANN
               lstnrl): # list of layer's neurals  

    if len(lstnrl) < 2:
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество элементов в параметре-списке нейронов lstnrl меньше 2\n")
        flpy3.write("len(lstnrl)={0:d}\n".format(len(lstnrl)))
        flpy3.write(cherr)
        sys.exit(exitcode3)
        
    # создание модели
    model = models.Sequential()

    # первый слой
    model.add (
    layers.Dense(lstnrl[0],                   # количество нейронов первого слоя 
                activation='relu',
                input_shape=(dbin.shape[1],)) # размерность входного вектора 
    )

    lstnrl1 = lstnrl[1:] # список нейронов без первого слоя

    for ln in lstnrl1:
    
        # внутренние слои
        model.add (
        layers.Dense(ln,                # количество нейронов слоя
                     activation='relu')
        )

    # выходной слой
    model.add (
    layers.Dense(dbout.shape[1])  # размерность выходного вектора
    )

    # For a mean squared error regression problem
    model.compile(optimizer="rmsprop",
                  loss="mse", 
                  metrics=["mse", "mae"])

    return model

def plot_hst1(history): # объект History 

    metrics = ("loss",
               "mean_absolute_error",
               "mean_squared_error")

    val = "val_"
    ttl = "Training "
    vtl = "and validation "

    # the dimensions of the new axes
    # [left, bottom, width, height] values in 0-1 relative figure coordinates
    rect = [0.10, 0.10, 0.8, 0.8]  

    dict_keys = history.history.keys()

    nfig = 0 # номер рисунка

    for m in metrics: 

        if m in dict_keys: # "Training"

            nfig += 1 # номер рисунка

            fig = plt.figure(nfig)   # create a new figure
            ax  = fig.add_axes(rect) # create a new axes 

            t = history.history[m]

            epochs = range(1, len(t) + 1)

            ax.plot(epochs, t, "bo", label="Training")

            title = ttl 

            vm = val + m 

            if vm in dict_keys: # "Validation"

                v = history.history[vm]

                ax.plot(epochs, v, "b", label="Validation")

                title += vtl 

            title += m
            fig.suptitle(title)

            ax.set_xlabel("Epochs")
            ax.set_ylabel(m)
            ax.legend()

            # сохранение файла рисунка
            plt.savefig(m) 

