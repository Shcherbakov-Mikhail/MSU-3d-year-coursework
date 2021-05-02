#!/usr/bin/python3
# coding=UTF-8
'''
Created on 12 ноября 2020 г.
@author: alex

Модуль Model предназначен для создания модели ANN

обратная задача МДТТ

определение моделей  ANN

'''

import sys, os

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 

from keras import models
from keras import layers
import keras
from keras import backend as K

import numpy             as np
import matplotlib.pyplot as plt
import ann01db as db

seed_value = 1
import tensorflow as tf
tf.random.set_seed(seed_value)

#import numpy as np

callback_history = {} 

# коды аварийного заверщения программы
exitcode3 = 3 # ошибка списка слоев

# log-файл сообщений о работе программы
flpy3 = open(r"pymdl.3", mode='wt') 
cherr = "\nВЫПОЛНЕНИЕ модуля ann01mdl ПРЕРВАНО"

def test_mdl1 (lstlrs): # list of layers

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
               dbout,   # output DB ANN
               lstlrs,
               optimizer): 

    test_mdl1(lstlrs)

    # создание модели
    model = models.Sequential()

    # первый слой
    model.add (
    layers.Dense(lstlrs[0][0],                # количество нейронов первого слоя 
                activation=lstlrs[0][1],      # функция активации первого слоя
                input_shape=(dbin.shape[1],)) # размерность входного вектора 
    )

    #lstlrs1 = lstnrl[1:] # список нейронов без первого слоя

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
                  loss='mse',
                  run_eagerly=True)
    return model

def plot_hst1(history, in_out): # объект History 

    metrics = ("loss",
               "mse")

    ttl = "Training "
    if in_out == "inside":
        vtl = ", validation inside "
        val_label = "Inside Validation Set"
        color = "g"
        name = "_in"
        val = "val_"
    elif in_out == "outside":
        vtl = ", validation outside "
        val_label = "Outside Validation Set"
        color = "r"
        name = "_out"
        val = "val_"
    elif in_out == "both":
        val1 = "val_inside_"
        val2 = "val_outside_"
        vtl = ", validation inside "
        vt2 = "and validation outside "

    # the dimensions of the new axes
    # [left, bottom, width, height] values in 0-1 relative figure coordinates
    rect = [0.10, 0.10, 0.8, 0.8]  

    dict_keys = history.keys()

    nfig = 0 # номер рисунка

    for m in metrics: 

        if m in dict_keys: # "Training"

            nfig += 1 # номер рисунка

            fig = plt.figure(nfig, figsize=(10, 10), dpi=150)   # create a new figure
            ax  = fig.add_axes(rect) # create a new axes

            t = history[m]

            epochs = range(1, len(t) + 1)

            ax.plot(epochs, t, "bo", label="Training")

            title = ttl 

            if in_out == "both":
                vm1 = val1 + m 
                
                if vm1 in dict_keys: # "Validation Set 1"
                    v = history[vm1]
                    ax.plot(epochs, v, "g-", label="Inside Validation Set")

                    title += vtl 
                
                    vm2 = val2 + m 
                
                if vm2 in dict_keys: # "Validation Set 2"
                    v = history[vm2]

                    ax.plot(epochs, v, "r-", label="Outside Validation Set")

                    title += vt2 

                    title += m
                    fig.suptitle(title)

                    ax.set_xlabel("Epochs")
                    ax.set_ylabel(m)
                    ax.legend()

                    # сохранение файла рисунка
                    plt.savefig(m+"1")
                    plt.close()
                    
            else:
                vm = val + m 

                if vm in dict_keys:
                    v = history[vm]

                    ax.plot(epochs, v, c=color, label=val_label)

                    title += vtl 

                title += m
                fig.suptitle(title)

                ax.set_xlabel("Epochs")
                ax.set_ylabel(m)
                ax.legend()

                # сохранение файла рисунка
                plt.savefig(vm+name)
                plt.close() 

def plot_hst2(history_in, history_out): # объект History 

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

            fig = plt.figure(nfig, figsize=(10, 10), dpi=150)   # create a new figure
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
                
class MultipleValidationSets(keras.callbacks.Callback):
    def __init__(self, validation_sets, verbose=0, batch_size=None):
        super(MultipleValidationSets, self).__init__()
        self.validation_sets = validation_sets
        self.batch_size = batch_size
        self.n_batches = 0
        self.loss_per_batch = []
        self.epoch = []
        self.history = {}
        self.verbose = verbose
        self.batch_size = batch_size        

    def on_train_begin(self, logs=None): #один раз в самом начале
        self.epoch = []
        self.history = {}
        keys = list(logs.keys())
        print("Starting training; got log keys: {}".format(keys))
        
    def on_train_end(self, logs=None):
        global callback_history
        callback_history = self.history
        keys = list(logs.keys())
        print("Stop training; got log keys: {}".format(keys))

    def on_epoch_end(self, epoch, logs=None):
        logs = logs or {}
        self.epoch.append(epoch)
        
        for k, v in logs.items():
            self.history.setdefault(k, []).append(v)
        
        for validation_set in self.validation_sets:
            self.loss_per_batch = []
            validation_data, validation_targets, validation_set_name = validation_set
            if validation_data.shape[0] == self.batch_size:
                self.n_batches = 1
            else:
                self.n_batches = validation_data.shape[0] // self.batch_size + 1
            # print("n_batches = ", self.n_batches)
            # print(validation_data.shape[0])
            # print(self.batch_size)
            for batch in range(self.n_batches-1):
                val_prediction = self.model.predict(
                    x=validation_data[batch*self.batch_size : (batch+1)*self.batch_size],
                    verbose=self.verbose)
                val_result = l2avg(
                    validation_targets[batch*self.batch_size : (batch+1)*self.batch_size], 
                    val_prediction).numpy()
                self.loss_per_batch.append(val_result)
                
            val_prediction = self.model.predict(
                x=validation_data[(self.n_batches-1)*self.batch_size :],
                verbose=self.verbose)
            val_result = l2avg(
                validation_targets[(self.n_batches-1)*self.batch_size :], 
                val_prediction).numpy()
            self.loss_per_batch.append(val_result)
            
            # print(sum(self.loss_per_batch)/self.n_batches)
            
            for metric in self.model.metrics_names:
                valuename = validation_set_name + '_' + metric
                self.history.setdefault(valuename, []).append(sum(self.loss_per_batch)/self.n_batches)


def l2avg(act, pred):
#     print('\n')
#     print("Batch: ",db.dbch.shape[0])
    if act.shape[0] is not None:
        result =  tf.reduce_sum(tf.sqrt(tf.reduce_sum(K.square(act-pred), 1, keepdims=True)))/act.shape[0]
    else:
        print('act.shape[0] is not None')
        print(db.dbch.shape[0])
        result =  tf.reduce_sum(tf.sqrt(tf.reduce_sum(K.square(act-pred), 1, keepdims=True)))/db.dbch.shape[0]
#     print(act)
#     print(pred)
    # print("\nLoss calculates")
#     tf.print("Loss: ", result)
#     print('\n')
    return result

def l1avg(act, pred):
    result = []
    for i in range(len(act)):
        l1 = 0
        for j in range(len(act[i])):
            l1 += abs(pred[i][j] - act[i][j])
        result.append(l1)
    return sum(result) / len(result)

# def l2avg(act, pred):
#     result = []
#     for i in range(len(act)):
#         l2 = 0
#         for j in range(len(act[i])):
#             l2 += (pred[i][j] - act[i][j]) ** 2
#         result.append(np.sqrt(l2))
#     # print('')
#     # print(act)
#     # print(pred)
#     # print("MSE calculates: ", sum(result) / len(result))
#     # print('')
#     return sum(result) / len(result)

def l1disp(act, pred):
    result = []
    for i in range(len(act)):
        l1 = 0
        for j in range(len(act[i])):
            l1 += abs(pred[i][j] - act[i][j])
        result.append(l1)
    return np.std(result) ** 2

def l2disp(act, pred):
    result = []
    for i in range(len(act)):
        l2 = 0
        for j in range(len(act[i])):
            l2 += abs(pred[i][j] - act[i][j]) ** 2
        result.append(np.sqrt(l2))
    return np.std(result) ** 2

def linfdisp(act, pred):
    result = []
    for i in range(len(act)):
        l1 = []
        for j in range(len(act[i])):
            l1.append(abs(pred[i][j] - act[i][j]))
        result.append(max(l1))
    return np.std(result) ** 2

def linfavg(act, pred):
    result = []
    for i in range(len(act)):
        l1 = []
        for j in range(len(act[i])):
            l1.append(abs(pred[i][j] - act[i][j]))
        result.append(max(l1))
    return sum(result) / len(result)
