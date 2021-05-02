'''
Created on 12 ноября 2020 г.

@author: alex

Модуль ANN предназначен для обучения и тестирования ANN

'''

import sys, os, shutil
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 
import numpy as np

seed_value = 1
import tensorflow as tf
tf.random.set_seed(seed_value)

import random
random.seed(seed_value)

import matplotlib.pyplot as plt
from _cffi_backend import callback

import ann01db  as db  # имена файлов данных
import ann01mdl as mdl # определение моделей  ANN
from ann01mdl import MultipleValidationSets
from mpl_toolkits import mplot3d
from scipy.spatial import ConvexHull
import keras

if __name__ == '__main__':
    pass

#************************************************

# ввод БД ANN
db.rdbann1()

# ввод БД CONVEX HULL
db.rdbch1()

n_train = db.dbch.shape[0] # количество образцов train
n_test = db.dbrq.shape[0] - n_train # количество образцов test

val_in_size = int(n_test * db.val_split_ratio[0] / sum(list(db.val_split_ratio)))

nsch = n_train
nsdb = db.dbrq.shape[0]

eps_1 = max(db.dbds[:val_in_size,0])
eps_2 = min(db.dbds[val_in_size:,0])

# eps_1 = 1e-10
# eps_2 = 1e-2

# index_bad = [i for i, el in enumerate(db.dbds[:,0]) if el == eps_2][0]
# print(db.dbts[index_bad])
# print(index_bad+2)

print("Max distance in val_inside: ", eps_1)
print("Min distance in val_outside: ", eps_2)
print("\n")

if eps_2 < eps_1:
    print("Distances bounds ERROR! Check eps_1 and eps_2!")
    sys.exit()

print("Initial size of each validation set: ", len(db.dbts)//2)
print("\n")

indices_inside = []
indices_outside = []

for i in range(len(db.dbds)):
    if db.dbds[i][0] <= eps_1:
        indices_inside.append(i)
    elif db.dbds[i][0] >= eps_2:
        indices_outside.append(i) 
        
# bad_points_in = []
# for i in range(len(db.dbds)//2):
#     if db.dbds[i][0] > eps_2:
#         bad_points_in.append(db.dbts[i])
# bad_points_in = np.array(bad_points_in)
#    
# bad_points_out = []
# for i in range(len(db.dbds)//2):
#     if db.dbds[len(db.dbds)//2 + i][0] < eps_1:
#         bad_points_out.append(db.dbts[len(db.dbds)//2 + i])
# bad_points_out = np.array(bad_points_out)
# plt.scatter(bad_points_in[:, 0], bad_points_in[:, 1], c="orange", s = 5, label='Deleted in-points')
# plt.scatter(bad_points_out[:, 0], bad_points_out[:, 1], c="gray", s = 5, label='Deleted out-points')

# hull = ConvexHull(db.dbrq[:nsch,:])
# fig = plt.figure(1, dpi = 200)
 
# 3-d plot
# ax = plt.axes(projection='3d')   
# for s in hull.simplices:
#     s = np.append(s, s[0])
#     ax.plot(db.dbrq[:n_train,:][s, 0], db.dbrq[:n_train,:][s, 1], db.dbrq[:n_train,:][s, 2], "b-")
# for i in ["x", "y", "z"]:
#     eval("ax.set_{:s}label('{:s}')".format(i, i))     
# ax.scatter3D(db.dbrq[:nsch,0], db.dbrq[:nsch,1], db.dbrq[:nsch,2], c="black") 
# ax.scatter3D(np.array([db.dbts[i] for i in indices_inside])[:, 0], np.array([db.dbts[i] for i in indices_inside])[:, 1], np.array([db.dbts[i] for i in indices_inside])[:, 2], c="green", s = 1, label='In_val_points')   
# ax.scatter3D(np.array([db.dbts[i] for i in indices_outside])[:, 0], np.array([db.dbts[i] for i in indices_outside])[:, 1], np.array([db.dbts[i] for i in indices_outside])[:, 2], c="red", s = 1, label='Out_val_points')   
# ax.legend(loc = 'upper right', fontsize = 7, markerscale = 2, markerfirst = False, facecolor = "black", labelcolor = 'white')
# plt.savefig("points.png")   
# # plt.show()
# plt.close(1)
# # sys.exit()

#2-d plot
# for s in hull.simplices:
#     s = np.append(s, s[0])
#     plt.plot(db.dbrq[:n_train,:][s, 0], db.dbrq[:n_train,:][s, 1], "b-")
# plt.scatter(db.dbrq[:nsch,0], db.dbrq[:nsch,1], c="black")
# # plt.scatter(db.dbts[index_bad][0], db.dbts[index_bad][1], c="red", s = 15, label='Wrong')
# plt.scatter(np.array([db.dbts[i] for i in indices_inside])[:, 0],np.array([db.dbts[i] for i in indices_inside])[:, 1], c="green", s = 2, label='In_val_points')
# plt.scatter(np.array([db.dbts[i] for i in indices_outside])[:, 0], np.array([db.dbts[i] for i in indices_outside])[:, 1],c="red", s = 2, label='Out_val_points')
# plt.legend(loc = 'upper right', fontsize = 7, markerscale = 2, markerfirst = False, facecolor = "black", labelcolor = 'white')
# plt.savefig("points.png")   
# # plt.show()
# plt.close(1)
# # sys.exit()


# **********************************************

# arrays
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

epochs = 200

model_in = mdl.build_mdl1(db.dbrq, #  input DB ANN (обобщенных узловых перемещений)
                       db.dbpr, # output DB ANN (координата дефекта, величина дефекта)
                       lstlrs,
                       optimizer)

model_out = mdl.build_mdl1(db.dbrq,
                       db.dbpr,
                       lstlrs,
                       optimizer)

#Copy initial weights to create a twin-model
model_out.set_weights(model_in.get_weights()) 

#************************************************
  
# Set callback              
# clb = MultipleValidationSets(
#     validation_sets = [(x_val_inside, y_val_inside, 'val_inside'), 
#                        (x_val_outside, y_val_outside, 'val_outside')],
#     batch_size = db.batch_size
#     )

# Train the model
history_in = model_in.fit(
           x_train, 
           y_train,
           epochs=epochs,
           batch_size=db.batch_size,
            validation_data=(x_val_inside, y_val_inside),
           verbose = 2,
           shuffle=False
           )
 
 
history_out = model_out.fit(
           x_train, 
           y_train,
           epochs=epochs,
           batch_size=db.batch_size,
           validation_data=(x_val_outside, y_val_outside),
           verbose = 2,
           shuffle=False
           )

# history_clb = model_in.fit(
#            x_train, 
#            y_train,
#            epochs=epochs,
#            batch_size=db.batch_size,
#            callbacks = [clb],
#            verbose = 2,
#            shuffle=False
#            )


print("Trained the ANN")
#************************************************

# plot 
# mdl.plot_hst1(history_in.history, "inside")
# mdl.plot_hst1(history_out.history, "outside")
# print(history1.history)
# print('\n')
# print(history2.history)
# print('\n')
mdl.plot_hst2(history_in.history, history_out.history)

# print('\n\n')
# print(mdl.callback_history)

print("Plotted loss")

#calculate loss
# val_in_avg_loss = sum(mdl.callback_history['val_inside_loss'])/len(mdl.callback_history['val_inside_loss'])
# val_out_avg_loss = sum(mdl.callback_history['val_outside_loss'])/len(mdl.callback_history['val_outside_loss'])
# 
pr_in1 = model_in.predict(x_val_inside)
pr_out1 = model_in.predict(x_val_outside)

pr_in2 = model_out.predict(x_val_inside)
pr_out2 = model_out.predict(x_val_outside)
 
val_in_loss1 = mdl.l2avg(y_val_inside, pr_in1).numpy()
val_out_loss1 = mdl.l2avg(y_val_outside, pr_out1).numpy()

val_in_loss2 = mdl.l2avg(y_val_inside, pr_in2).numpy()
val_out_loss2 = mdl.l2avg(y_val_outside, pr_out2).numpy()

print('')
print(val_in_loss1)
print(val_out_loss1)
print('')
print(val_in_loss2)
print(val_out_loss2)

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