'''
Created on 12 ноября 2020 г.

@author: alex

Модуль ANN предназначен для обучения и тестирования ANN

'''

import sys, os
import numpy as np
import matplotlib.pyplot as plt
import ann01db  as db  # имена файлов данных
import ann01mdl as mdl # определение моделей  ANN
import keras
from scipy.spatial import ConvexHull

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' # скрыть вызов TensorFlow в консоли
seed_value = 1 # ядро генератора сучайных чисел

import tensorflow as tf 
tf.random.set_seed(seed_value) # фиксируем начальные веса модели

import random
random.seed(seed_value) 

if __name__ == '__main__':
    pass

#************************************************

# ввод БД ANN
db.rdbann1()

# ввод БД CONVEX HULL
db.rdbch1()

n_train = db.dbch.shape[0] # количество образцов train
n_test  = db.dbts.shape[0]  # количество образцов test

eps_1 = 1e-10 # максимальное растояние до оболочки для внутренних точек
eps_2 = 1e-2  # минимальное растояние до оболочки для внешних точек

if eps_2 < eps_1:
    print("Distances bounds ERROR! Check eps_1 and eps_2!")
    sys.exit()

print("Max distance in val_inside: ", eps_1)
print("Min distance in val_outside: ", eps_2)
print("\n")

indices_inside = []
indices_outside = []

# БД расстояний имеет вид (расстояние до оболочки, расстояние до ц.м.)
for i in range(len(db.dbds)):
    if db.dbds[i][0] <= eps_1:
        indices_inside.append(i)
    elif db.dbds[i][0] >= eps_2:
        indices_outside.append(i) 
        
# 2-d plot
# hull = ConvexHull(db.dbch)
# for s in hull.simplices:
#     s = np.append(s, s[0])
#     plt.plot(db.dbrq[:n_train,:][s, 0], db.dbrq[:n_train,:][s, 1], "b-")
# plt.scatter(db.dbrq[:n_train,0], db.dbrq[:n_train,1], c="black")
# plt.scatter(np.array([db.dbts[i] for i in indices_inside])[:, 0],np.array([db.dbts[i] for i in indices_inside])[:, 1], c="green", s = 2, label='In_val_points')
# plt.scatter(np.array([db.dbts[i] for i in indices_outside])[:, 0], np.array([db.dbts[i] for i in indices_outside])[:, 1],c="red", s = 2, label='Out_val_points')
# plt.legend(loc = 'upper right', fontsize = 7, markerscale = 2, markerfirst = False, facecolor = "black", labelcolor = 'white')
# plt.savefig("points.png")   
# plt.show()
# plt.close(1)

# **********************************************

n_val = min(len(indices_inside), len(indices_outside))

x_train = db.dbch
y_train = db.dbpr[:n_train,:]

x_val_inside   = np.array([db.dbts[i] for i in indices_inside])[:n_val]
y_val_inside  = np.array([db.dbpr[n_train + i] for i in indices_inside])[:n_val]
  
x_val_outside  = np.array([db.dbts[i] for i in indices_outside])[:n_val]
y_val_outside  = np.array([db.dbpr[n_train + i] for i in indices_outside])[:n_val]

print("size(dbrq)       = " + str(db.dbrq.shape))
print("size(indices_inside)       = " + str(len(indices_inside)))
print("size(indices_outside)       = " + str(len(indices_outside)))
print("size(x_train)       = " + str(x_train.shape))
print("size(y_train)       = " + str(y_train.shape))
print("size(x_val_inside)  = " + str(x_val_inside.shape))
print("size(x_val_outside) = " + str(x_val_outside.shape))
print("\n")

print("Created test and validation sets")

#************************************************

# модель ANN

# list of layers
lstlrs = [[32,"relu"], 
          [32,"relu"], 
          [32,"relu"]] 

optimizer = "adam"

loss = "mse"

metrics = ["mse", "mae"]

epochs = 100

model_in = mdl.build_mdl1(x_train,
                         y_train,
                         lstlrs,
                         optimizer,
                         loss,
                         metrics)

model_out = mdl.build_mdl1(x_train,
                          y_train,
                          lstlrs,
                          optimizer,
                          loss,
                          metrics)

#Copy initial weights to create a twin-model
model_out.set_weights(model_in.get_weights()) 

#************************************************

# Train the model
history_in = model_in.fit(x_train, 
                          y_train,
                          epochs=epochs,
                          batch_size=db.batch_size,
                          validation_data=(x_val_inside, y_val_inside),
                          verbose = 2,
                          shuffle=False
                          )
 
 
history_out = model_out.fit(x_train, 
                            y_train,
                            epochs=epochs,
                            batch_size=db.batch_size,
                            validation_data=(x_val_outside, y_val_outside),
                            verbose = 2,
                            shuffle=False
                            )


print("Trained the ANN")

#************************************************

# plotting
mdl.plot_hst1(history_in.history, history_out.history)
print("Plotted loss")

#calculating losses
pred_in = model_in.predict(x_val_inside)
pred_out = model_in.predict(x_val_outside)

val_in_loss_l1 = mdl.nrm1avg(y_val_inside, pred_in)
val_out_loss_l1 = mdl.nrm1avg(y_val_outside, pred_out)

val_in_loss_l2 = mdl.nrm2avg(y_val_inside, pred_in)
val_out_loss_l2 = mdl.nrm2avg(y_val_outside, pred_out)

val_in_loss_linf = mdl.nrm0avg(y_val_inside, pred_in)
val_out_loss_linf = mdl.nrm0avg(y_val_outside, pred_out)

val_in_loss_relat_l2 = mdl.relative_nrm2avg(y_val_inside, pred_in)
val_out_loss_relat_l2 = mdl.relative_nrm2avg(y_val_outside, pred_out)

print("\nInside validation l1-loss = " + str(val_in_loss_l1))
print("Outside validation l1-loss  = " + str(val_out_loss_l1))
print("\nInside validation l2-loss = " + str(val_in_loss_l2))
print("Outside validation l2-loss  = " + str(val_out_loss_l2))
print("\nInside validation linf-loss = " + str(val_in_loss_linf))
print("Outside validation linf-loss  = " + str(val_out_loss_linf))
print("\nInside validation relative-l2 loss = " + str(val_in_loss_relat_l2))
print("Outside validation relative-l2 loss  = " + str(val_out_loss_relat_l2))

#******************************************

experiment_num = 0 #corresponds to a database type
subexp_num = 1 # corresponds to a ANN for certain experiment
do_log = 0 #log results or not

#log results
# if do_log:
#     directory_name = r"~/STA2BS_Experiments/Forward/Experiment" + str(experiment_num)
#     
#     if not os.path.exists(directory_name):
#         os.makedirs(directory_name)
#     
#     logfile_name = directory_name + "/Experiment" + str(experiment_num) + ".txt"
#     logfile = open(logfile_name, "a")
# 
#     logfile.write("\n  " + str(subexp_num) + ")\n")
# 
#     logfile.write("\tEpochs: " + str(epochs) + "\n\n")
# 
#     logfile.write("\tANN topology: [")
#     for item1 in lstlrs:
#         logfile.write("[")
#         for item2 in item1:
#             logfile.write("%s," % item2)
#         logfile.write("],")
#     logfile.write("]\n\n")
#     
#     logfile.write("\tOptimizer: " + optimizer + "\n\n")
#     
#     logfile.write("\tUpper bound: " + str(eps_1) + "\n")
#     logfile.write("\tLower bound: " + str(eps_2) + "\n\n")
# 
#     logfile.write("\tsize(x_train)       = " + str(x_train.shape) + "\n")
#     logfile.write("\tsize(x_val_inside)  = " + str(x_val_inside.shape) + "\n")
#     logfile.write("\tsize(x_val_outside) = " + str(x_val_outside.shape) + "\n\n")
# 
#     logfile.write("\tAverage val_inside loss during train: " + str(val_in_avg_loss) + "\n")
#     logfile.write("\tAverage val_outside loss during train: " + str(val_out_avg_loss) + "\n")
#     logfile.write("\tAverage Out / In ratio during train: %.2f" % float(val_out_avg_loss/val_in_avg_loss) + "\n\n")
# 
#     logfile.write("\tval_inside loss after train: " + str(val_in_loss) + "\n")
#     logfile.write("\tval_outside loss after train: " + str(val_out_loss) + "\n")
#     logfile.write("\tOut / In ratio after train: %.2f" % float(val_out_loss/val_in_loss) + "\n\n")
# 
#     logfile.close()
# 
#     loss_image_name = "/Experiment" + str(experiment_num) + "." + str(subexp_num) + "_loss.png"
#     shutil.copy("loss.png", directory_name + loss_image_name)
#     
#     if db.dbch.shape[1] == 2 or db.dbch.shape[1] == 3:
#         points_image_name = "/Experiment" + str(experiment_num) + "." + str(subexp_num) + "_points.png"
#         shutil.copy("points.png", directory_name + points_image_name)
# 
#     print('Logged output into file "Experiment' + str(experiment_num) + '.txt"')
# 
# print("DONE!")