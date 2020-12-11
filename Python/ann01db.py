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

flfort = r"fort.1"  # файл параметров для настройки процессора (FORTRAN-программы)

dbpr   = 0          # БД обобщенных узловых перемещений
fldbpr = r"fort.31" # файл БД
                    # output FEM
                    #  input ANN 

dbmt   = 0          # БД жесткостей КЭ
fldbmt = r"fort.32" # файл БД
                    #  input FEM 
                    # output ANN

dbrq   = 0          # БД распределенной нагрузки
fldbrq = r"fort.33" # файл БД
                    #  input FEM 
                    # output ANN

dbxe   = 0          # БД (координата дефекта, величина дефекта)
fldbxe = r"fort.34" # файл БД
                    # output ANN

dbne   = 0          # БД (номер КЭ, величина дефекта)
fldbne = r"fort.35" # файл БД
                    # output ANN

#************************************************
# коды аварийного заверщения программы

exitcode1 = 1 # изменение количества параметров БД
exitcode2 = 2 # несоотвествие количество записей в БД 

cherr = "\nВЫПОЛНЕНИЕ модуля ann01db ПРЕРВАНО"

#************************************************

batch_size = 32 

#************************************************

# запись БД в файлы для Processor
def wdbfem(ne,    # количество КЭ
           nsdb): # количество образцов  

#           dbxe,  # БД (координата дефекта, величина дефекта)
#           dbne,  # БД (номер КЭ, величина дефекта)
#           dbmt,  # БД жесткостей КЭ  
#           dbrq): # БД распределенной нагрузки

#    global dbxe  # БД (координата дефекта, величина дефекта)
#    global dbne  # БД (номер КЭ, величина дефекта)
#    global dbmt  # БД жесткостей КЭ  
#    global dbrq  # БД распределенной нагрузки


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

    np.savetxt(fldbxe, dbxe, delimiter=" ", fmt="%12E", comments="") # (координата дефекта, величина дефекта)
    np.savetxt(fldbne, dbne, delimiter=" ", fmt="%12E", comments="") # (номер КЭ, величина дефекта)

#************************************************

def rdbann(): # ввод БД для ANN

    global dbpr #  input ANN - БД обобщенных узловых перемещений
    global dbmt # output ANN - БД жесткостей КЭ
    global dbxe # output ANN - БД (координата дефекта, величина дефекта)
    global dbne # output ANN - БД (номер КЭ, величина дефекта)

    # ввод БД обобщенных узловых перемещений
    dbpr = np.loadtxt(fldbpr) # input ANN

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

    # ввод БД (координата дефекта, величина дефекта)
    dbxe = np.loadtxt(fldbxe) # output ANN

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
    dbne = np.loadtxt(fldbne) # output ANN

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

