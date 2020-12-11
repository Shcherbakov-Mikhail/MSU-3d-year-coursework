'''
Created on 12 ноября 2020 г.

@author: alex

Модуль ANN предназначен для обучения и тестирования ANN

'''

import sys, os
import numpy as np

import matplotlib.pyplot as plt

if __name__ == '__main__':
    pass


import ann01db as db  # имена файлов данных
import ann01mdl as mdl # определение моделей  ANN

exitcode4 = 4 # number of total batches less than one

cherr = "\nВЫПОЛНЕНИЕ модуля ann01krs ПРЕРВАНО"

# log-файл сообщений о работе программы
flpy3 = open(r"pyann.3", mode='wt') 

#************************************************

# ввод БД ANN
db.rdbann()

nsbd = db.dbpr.shape[0] # количество образцов

#************************************************

# модель ANN

lstnrl = [32, 32, 32] # list of layer's neurals

model = mdl.build_mdl1(db.dbpr, #  input DB ANN (обобщенных узловых перемещений)
                       db.dbxe, # output DB ANN (координата дефекта, величина дефекта)
                       lstnrl)  # list of layer's neurals  

#************************************************

# number of batches
nbatch  = nsbd//db.batch_size     # number of total      batches 
nbtest  = 2                       # number of test       batches
nbval   = 2                       # number of validation batches
nbtrain = nbatch - nbtest - nbval # number of train      batches  

if ( nbtrain < 1 ):
    flpy3.write("*** ОБНАРУЖЕНА ОШИБКА\n")
else:
    flpy3.write("ПАРАМЕТРЫ БД для обучения и тестирования модели\n")

flpy3.write("number of total      batches - nbatch    ={0:d}\n".format(nbatch))
flpy3.write("number of test       batches - nbtest    ={0:d}\n".format(nbtest))
flpy3.write("number of validation batches - nbval     ={0:d}\n".format(nbval))
flpy3.write("number of train      batches - nbtrain   ={0:d}\n".format(nbtrain))

if ( nbtrain < 1 ):
    sys.exit(exitcode4)
else:
    flpy3.write("number of samples in batches - batch_size={0:d}\n".format(db.batch_size))

# number of samples
nstrain = nbtrain * db.batch_size # number of train      samples
nsval   = nbval   * db.batch_size # number of validation samples 
nstest  = nbtest  * db.batch_size # number of test       samples 

#************************************************

# arrays
x_train = db.dbpr[:nstrain,:]
y_train = db.dbxe[:nstrain,:] 

x_val   = db.dbpr[nstrain:nstrain+nsval,:]
y_val   = db.dbxe[nstrain:nstrain+nsval,:]

x_test  = db.dbpr[nstrain+nsval:,:]
y_test  = db.dbxe[nstrain+nsval:,:]

print ("size(x_train)=",str(x_train.shape))
print ("size(x_val  )=",str(x_val.shape))
print ("size(x_test )=",str(x_test.shape))
print (" ")

#************************************************

epochs = 10

# Train the model
history = model.fit(
          x_train, 
          y_train,
          epochs=epochs,
          batch_size=db.batch_size,
          validation_data=(x_val,y_val)
          )

#************************************************

# plot 
mdl.plot_hst1(history)

#score = model.evaluate(x_test, y_test, batch_size=128)
#results = model.evaluate(x_test, y_test)

