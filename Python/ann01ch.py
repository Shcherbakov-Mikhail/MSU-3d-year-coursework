'''
Created on 10 апр. 2021 г.

@author: alex

Модуль CH предназначен для решения задач выпуклого анализа

'''

import sys, os
import numpy as np
import matplotlib.pyplot as plt
import ann01db  as db  # имена файлов данных

if __name__ == '__main__':
    pass

cherr = "\nВЫПОЛНЕНИЕ модуля ann01ch ПРЕРВАНО"

# log-файл сообщений о работе программы
flpy3 = open(r"pych.3", mode='wt') 

# FORTRAN-программа
fprg = r"D:\Eclipse\Workspace\WSFortran\CHL1\Release\CHL1"

#************************************************

# ввод БД ANN
db.rdbann1()  

nsdb = db.dbrq.shape[0] # количество образцов БД

#************************************************

nsch    = 2*db.dbrq.shape[1] # количество образцов БД CONVEX HULL
db.dbch = db.dbrq[:nsch,:] # БД CONVEX HULL 
db.dbts = db.dbrq[nsch:,:] # БД TESTING SAMPLES

# запись в файлы БД CONVEX HULL & TESTING SAMPLES
db.wdbch1()

#************************************************
# файл параметров FORTRAN-программы

# открытие файла параметров FORTRAN-программы
fl = open(db.flfort, mode='wt')

# запись в файл номеров каналов ввода-вывода FORTRAN-программы 
fl.write(db.fldbch.lstrip("fort.") + " "   # номер канала  ввода БД CONVEX HULL
    +db.fldbts.lstrip("fort.") + " "       # номер канала  ввода БД TESTING SAMPLES
    +db.fldbds.lstrip("fort.") + " "       # номер канала вывода БД расстояний
    +db.fldbec.lstrip("fort.") + " "       # номер канала вывода БД коэффициентов разложения
    +db.fldbep.lstrip("fort.") + '\n')     # номер канала вывода БД проекций на CONVEX HULL

# закрытие файла параметров FORTRAN-программы
fl.close()

#************************************************
# log-файл

flpy3.write("Создан файл параметров FORTRAN-программы\n")

# запись в log-файл номеров каналов ввода-вывода FORTRAN-программы
flpy3.write(db.fldbch.lstrip("fort.") + " - номер канала  ввода БД CONVEX HULL\n")
flpy3.write(db.fldbts.lstrip("fort.") + " - номер канала  ввода БД TESTING SAMPLES\n")
flpy3.write(db.fldbds.lstrip("fort.") + " - номер канала вывода БД расстояний\n")
flpy3.write(db.fldbec.lstrip("fort.") + " - номер канала вывода БД коэффициентов разложения\n")
flpy3.write(db.fldbep.lstrip("fort.") + " - номер канала вывода БД проекций на CONVEX HULL\n")

#************************************************

# вызов FORTRAN-программы
ierr = os.system(fprg)

#************************************************

