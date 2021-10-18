'''
Created on 12 ноября 2020 г.
@author: alex

Модуль DataBase предназначен для работы с БД

имена файлов данных
запись и ввод БД для FEM и ANN

'''

import sys, os
import numpy as np

#************************************************
# log-файл сообщений о работе модуля

flpy3 = open(r"pydb.3", mode='wt') 

#************************************************
# формирование имен файлов данных

flfort = r"fort.1"  # файл параметров FORTRAN-программ

dbmt   = 0          # БД жесткостей КЭ
fldbmt = r"fort.31" # файл БД
                    #  input FEM 
                    # output ANN

dbrq   = 0          # БД распределенной нагрузки
fldbrq = r"fort.32" # файл БД
                    #  input FEM 
                    # output ANN

dbxe   = 0          # БД (координата дефекта, величина дефекта)
fldbxe = r"fort.33" # файл БД
                    # output ANN

dbne   = 0          # БД (номер КЭ, величина дефекта)
fldbne = r"fort.34" # файл БД
                    # output ANN

dbpr   = 0          # БД обобщенных узловых перемещений
fldbpr = r"fort.41" # файл БД
                    # output FEM
                    #  input ANN 

dbsd   = 0          # БД параметров НДС КЭ
fldbsd = r"fort.42" # файл БД
                    # output FEM
                    #  input ANN 

dbch   = 0          # БД CONVEX HULL
fldbch = r"fort.51" # файл CONVEX HULL
                    #  input FEM 
                    # output ANN

dbts   = 0          # БД TESTING SAMPLES
fldbts = r"fort.52" # файл TESTING SAMPLES
                    #  input FEM 
                    # output ANN

dbds   = 0          # БД расстояний
fldbds = r"fort.53" # файл расстояний
                    #  input ANN
                    # output FEM

dbec   = 0          # БД коэффициентов разложения
fldbec = r"fort.54" # коэффициентов разложения
                    #  input ANN
                    # output FEM
                    
dbep   = 0          # БД проекций на CONVEX HULL
fldbep = r"fort.55" # проекций на CONVEX HULL
                    #  input ANN
                    # output FEM


#************************************************
# коды аварийного заверщения программы

exitcode1 = 1 # изменение количества параметров БД
exitcode2 = 2 # несоотвествие количество записей в БД 

cherr = "\nВЫПОЛНЕНИЕ модуля ann01db ПРЕРВАНО"

#************************************************

batch_size = 64

#************************************************

# запись БД в файлы для Processor
def wdbfem1(nsdb,  # количество образцов
            ne=0): # количество КЭ

    # проверка параметров БД (координата дефекта, величина дефекта)
    if dbxe.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество записей в БД (координата дефекта, величина дефекта)\n")
        flpy3.write("dbxe.shape[0]={0:d}\n".format(dbxe.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    if dbxe.shape[1] != 2 : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество параметров в БД (координата дефекта, величина дефекта)\n")
        flpy3.write("dbxe.shape[1]={0:d}\n".format(dbxe.shape[1]))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # проверка параметров БД (номер КЭ, величина дефекта)
    if dbne.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество записей в БД (номер КЭ, величина дефекта)\n")
        flpy3.write("dbne.shape[0]={0:d}\n".format(dbne.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    if dbne.shape[1] != 2 : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество параметров в БД (номер КЭ, величина дефекта)\n")
        flpy3.write("dbne.shape[1]={0:d}\n".format(dbne.shape[1]))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # проверка параметров БД жесткостей КЭ
    if dbmt.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество записей в БД жесткостей КЭ\n")
        flpy3.write("dbmt.shape[0]={0:d}\n".format(dbmt.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    if dbmt.shape[1] != ne : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество параметров в БД жесткостей КЭ\n")
        flpy3.write("dbmt.shape[1]={0:d}\n".format(dbmt.shape[1]))
        flpy3.write("           ne={0:d}\n".format(ne))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # проверка параметров БД распределенной нагрузки
    if dbrq.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество записей в БД распределенной нагрузки\n")
        flpy3.write("dbrq.shape[0]={0:d}\n".format(dbrq.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    if dbrq.shape[1] != ne : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("изменено количество параметров в БД распределенной нагрузки\n")
        flpy3.write("dbrq.shape[1]={0:d}\n".format(dbrq.shape[1]))
        flpy3.write("           ne={0:d}\n".format(ne))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # сохранение БД для FEM
    np.savetxt(fldbmt, dbmt, delimiter=" ", fmt="%12E", header=str(dbmt.shape[0])+" "+str(dbmt.shape[1]), comments="") # жесткостей КЭ
    np.savetxt(fldbrq, dbrq, delimiter=" ", fmt="%12E", header=str(dbrq.shape[0])+" "+str(dbrq.shape[1]), comments="") # распределенной нагрузки
    np.savetxt(fldbxe, dbxe, delimiter=" ", fmt="%12E", header=str(dbxe.shape[0])+" "+str(dbxe.shape[1]), comments="") # (координата дефекта, величина дефекта)
    np.savetxt(fldbne, dbne, delimiter=" ", fmt="%12E", header=str(dbne.shape[0])+" "+str(dbne.shape[1]), comments="") # (номер КЭ, величина дефекта)

#************************************************

def rdbann1(): # ввод БД для ANN

    global dbpr # input ANN - БД обобщенных узловых перемещений
    global dbmt # output ANN - БД жесткостей КЭ
    global dbrq # output ANN - БД распределенной нагрузки
    global dbxe # output ANN - БД (координата дефекта, величина дефекта)
    global dbne # output ANN - БД (номер КЭ, величина дефекта)

    # ввод БД обобщенных узловых перемещений
    dbpr = np.loadtxt(fldbpr, skiprows=1) # input ANN

    nsdb = dbpr.shape[0] # количество образцов
    nvpr = dbpr.shape[1] # размерность input vector ANN (количество обобщенных узловых перемещений)

    # запись в log-файл параметров БД
    flpy3.write("ПАРАМЕТРЫ БД обобщенных узловых перемещений\n")
    flpy3.write("количество  samples      - {0:d}\n".format(nsdb))
    flpy3.write("размерность input vector - {0:d}\n".format(nvpr))

    # ввод БД жесткостей КЭ
    dbmt = np.loadtxt(fldbmt, skiprows=1) # output ANN

    nvoutmt =  dbmt.shape[1] # размерность output ANN (количество КЭ)

    # проверка количества записей БД жесткостей КЭ
    if dbmt.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество записей в БД жесткостей КЭ не соответствует\n")
        flpy3.write("количеству записей в БД обобщенных узловых перемещений\n")
        flpy3.write("dbmt.shape[0]={0:d}\n".format(dbmt.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode2)

    # запись в log-файл параметров 
    flpy3.write("\nПАРАМЕТРЫ БД жесткостей КЭ\n")
    flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
    flpy3.write("размерность output vector - {0:d}\n".format(nvoutmt))

    # ввод БД распределенной нагрузки
    dbrq = np.loadtxt(fldbrq, skiprows=1) # output ANN

    nvoutrq =  dbrq.shape[1] # размерность output ANN (количество КЭ)

    # проверка количества записей БД жесткостей КЭ
    if dbrq.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество записей в БД распределенной нагрузки не соответствует\n")
        flpy3.write("количеству записей в БД обобщенных узловых перемещений\n")
        flpy3.write("dbrq.shape[0]={0:d}\n".format(dbrq.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode2)

    # запись в log-файл параметров 
    flpy3.write("\nПАРАМЕТРЫ БД распределенной нагрузки\n")
    flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
    flpy3.write("размерность output vector - {0:d}\n".format(nvoutrq))

    # ввод БД (координата дефекта, величина дефекта)
    dbxe = np.loadtxt(fldbxe, skiprows=1) # output ANN

    nvoutxe =  dbxe.shape[1] # размерность output ANN (координата дефекта, величина дефекта) 

    # проверка количества записей БД (координата дефекта, величина дефекта)
    if dbxe.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество записей в БД (координата дефекта, величина дефекта) не соответствует\n")
        flpy3.write("количеству записей в БД обобщенных узловых перемещений\n")
        flpy3.write("dbxe.shape[0]={0:d}\n".format(dbxe.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode2)

    if nvoutxe != 2 : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество параметров в БД (координата дефекта, величина дефекта) должно быть равно 2\n")
        flpy3.write("dbxe.shape[1]={0:d}\n".format(dbxe.shape[1]))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # запись в log-файл параметров 
    flpy3.write("\nПАРАМЕТРЫ БД (координата дефекта, величина дефекта)\n")
    flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
    flpy3.write("размерность output vector - {0:d}\n".format(nvoutxe))

    # ввод БД (номер КЭ, величина дефекта)
    dbne = np.loadtxt(fldbne, skiprows=1) # output ANN

    nvoutne =  dbxe.shape[1] # размерность output ANN (номер КЭ, величина дефекта) 

    # проверка количества записей БД (номер КЭ, величина дефекта)
    if dbne.shape[0] != nsdb : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество записей в БД (номер КЭ, величина дефекта) не соответствует\n")
        flpy3.write("количеству записей в БД обобщенных узловых перемещений\n")
        flpy3.write("dbne.shape[0]={0:d}\n".format(dbne.shape[0]))
        flpy3.write("         nsdb={0:d}\n".format(nsdb))
        flpy3.write(cherr)
        sys.exit(exitcode2)

    if nvoutne != 2 : 
        flpy3.write("\n*** ОБНАРУЖЕНА ОШИБКА\n")
        flpy3.write("количество параметров в БД (номер КЭ, величина дефекта) должно быть равно 2\n")
        flpy3.write("dbne.shape[1]={0:d}\n".format(dbne.shape[1]))
        flpy3.write(cherr)
        sys.exit(exitcode1)

    # запись в log-файл параметров 
    flpy3.write("\nПАРАМЕТРЫ БД (номер КЭ, величина дефекта)\n")
    flpy3.write("количество  samples       - {0:d}\n".format(nsdb))
    flpy3.write("размерность output vector - {0:d}\n".format(nvoutne))

#************************************************

def wdbch1(): # запись в файлы БД CONVEX HULL & TESTING SAMPLES

    np.savetxt(fldbch, dbch, delimiter=" ", fmt="%12E", header=str(dbch.shape[0])+" "+str(dbch.shape[1]), comments="") # БД CONVEX HULL
    np.savetxt(fldbts, dbts, delimiter=" ", fmt="%12E", header=str(dbts.shape[0])+" "+str(dbts.shape[1]), comments="") # БД TESTING SAMPLES

#************************************************

def rdbch1(): # ввод БД расстояний & коэффициентов разложения

    global dbch #  input ANN - БД Convex Hull
    global dbts #  input ANN - БД Training Samples
    global dbds #  input ANN - БД расстояний
    global dbec #  input ANN - БД коэффициентов разложения
    global dbep #  input ANN - БД проекций на CONVEX HULL

    # ввод БД расстояний
    dbds     = np.loadtxt(fldbds, skiprows=1)            # БД расстояний

    # запись в log-файл параметров БД расстояний
    flpy3.write("ПАРАМЕТРЫ БД расстояний\n")
    flpy3.write("количество  samples      - {0:d}\n".format(dbds.shape[0]))
    flpy3.write("размерность input vector - {0:d}\n".format(dbds.shape[1]))

    dbec     = np.loadtxt(fldbec, skiprows=1)            # ввод БД коэффициентов разложения
    dbep     = np.loadtxt(fldbep, skiprows=1)            # ввод БД проекций на CONVEX HULL

    # запись в log-файл параметров БД коэффициентов разложения
    flpy3.write("ПАРАМЕТРЫ БД коэффициентов разложения\n")
    flpy3.write("количество  samples      - {0:d}\n".format(dbec.shape[0]))
    flpy3.write("размерность input vector - {0:d}\n".format(dbec.shape[1]))
    
    dbch = np.loadtxt(fldbch, skiprows=1)
    dbts = np.loadtxt(fldbts, skiprows=1)