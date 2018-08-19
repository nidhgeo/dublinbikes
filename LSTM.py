# -*- coding: utf-8 -*-
"""

"""


import math
from sklearn.metrics import classification_report, confusion_matrix
from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from sklearn.preprocessing import OneHotEncoder
from sklearn import preprocessing
import numpy as np
import pandas as pd
from keras import optimizers
from array import array
import statistics as stat
from scipy import stats
import random
# All parameter gradients will be clipped to
# a maximum norm of 1.

# load dataset
#dataset = read_csv('C:/MSc Materials/Dissertation/dublin_bikes-master/datawithprevweeks.csv', header=0, index_col=0)
np.random.seed(3482)

datasetTrain = read_csv('C:/MSc Materials/Dissertation/dublin_bikes-master/train_df2-with_prevweeks_avind_for_python.csv', header=0, index_col=0)
datasetTest = read_csv('C:/MSc Materials/Dissertation/dublin_bikes-master/test_df2-with_prevweeks_avind_for_python.csv', header=0, index_col=0)

datasets= []
datasets.append(datasetTrain)
datasets.append(datasetTest)

index=0
#for obj in datasets:
obj=datasetTrain.append(datasetTest)
    
catVariables = obj.select_dtypes(include=[object])
le = preprocessing.LabelEncoder()
# 2/3. FIT AND TRANSFORM
# use df.apply() to apply le.fit_transform to all columns
X_2 = catVariables.apply(le.fit_transform)
X_3=X_2
for i in range(0, 6):
    x=X_3.iloc[:,i]
    x=x.as_matrix() 
    x=x.reshape(-1, 1)
    onehotencoder = OneHotEncoder(categorical_features = [0]) 
    x=onehotencoder.fit_transform(x).toarray() 
    print(x.shape)
    x=x[:,1:] 
    obj=np.c_[obj,x]
obj=pd.DataFrame(obj)    
    


#datasets[0].to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/trainOutput-python.csv")

#datasets[1].to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/testOutput-python.csv")

extValues=np.hstack((obj.values[:,[0,1,6,7,8,16]],obj.values[:,17:326]))

n_test = round((extValues.shape[0])/ 10 * 2.5)

randomNums=np.random.randint(extValues.shape[0], size=n_test)

test = extValues[randomNums, :]
trainIndices = [x for x in range(0,extValues.shape[0]) if x not in randomNums]
train = extValues[trainIndices, :]

test_Original=obj.values[randomNums, :]

#pd.DataFrame(train).to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/trainOutput-python.csv")

#pd.DataFrame(test).to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/testOutput-python.csv")

#newData2 = read_csv('C:/MSc Materials/Dissertation/dublin_bikes-master/updatedcsv_withonehot-prev_bikenum and prev_week_cleanedCluster.csv', header=0)


#train= read_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/trainOutput-python_without_prevweeks.csv", header=0)
#test= read_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/testOutput-python_without_prevweeks.csv", header=0)


#train=pd.DataFrame(train).dropna(0)
#pd.DataFrame(train).isnull().any() 


#test=pd.DataFrame(test).dropna(0)
#test.isnull().any() 

#newData2.dropna()
#n_train_hours = round((newData2.shape[0])/ 10 * 7)

#train = newData2.values[:n_train_hours, :]
#test = newData2.values[n_train_hours:, :]

#train=train.values
#test=test.values


train_X, train_y = np.hstack((train[:, 0:1],train[:, 3:314])), train[:, 2]
test_X, test_y = np.hstack((test[:, 0:1],test[:, 3:314])), test[:, 2]

# reshape input to be 3D [samples, timesteps, features]
train_X = train_X.reshape((train_X.shape[0], 1, train_X.shape[1]))
test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)


model = Sequential()
model.add(LSTM(20, input_shape=(train_X.shape[1], train_X.shape[2])))
model.add(Dense(units=1, activation='relu'))
model.compile(loss='mean_squared_error', optimizer='adam')
# fit network
history = model.fit(train_X, train_y, epochs=50, batch_size=40, validation_data=(test_X, test_y), verbose=2, shuffle=True)
# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()




# make a prediction
yhat = model.predict(test_X)



test.shape
yhat.shape


# calculate RMSE
rmse = math.sqrt(mean_squared_error(test_y, yhat))
print('Test RMSE: %.3f' % rmse)

def rmsle(y, y_pred):
	assert len(y) == len(y_pred)
	terms_to_sum = [(math.log(y_pred[i] + 1) - math.log(y[i] + 1)) ** 2.0 for i,pred in enumerate(y_pred)]
	return (sum(terms_to_sum) * (1.0/len(y))) ** 0.5

rmsle(test_y, yhat)

any(n < 0 for n in yhat)

train_X2, train_y2 = np.hstack((train[:, 0:1],train[:, 3:314])), train[:, 2]
test_X2, test_y2 = np.hstack((test[:, 0:1],test[:, 3:314])), test[:, 2]

min_max_scaler = preprocessing.MinMaxScaler()
X_train_minmax = min_max_scaler.fit_transform(train_X2)
X_test_minmax = min_max_scaler.transform(test_X2)

X_train_minmax = X_train_minmax.reshape((X_train_minmax.shape[0], 1, X_train_minmax.shape[1]))
X_test_minmax = X_test_minmax.reshape((X_test_minmax.shape[0], 1, X_test_minmax.shape[1]))

modelNew = Sequential()
modelNew.add(LSTM(20, input_shape=(X_train_minmax.shape[1], X_train_minmax.shape[2])))
modelNew.add(Dense(units=1, activation='relu'))
modelNew.compile(loss='mean_squared_error', optimizer='adam')
# fit network
history = modelNew.fit(X_train_minmax, train_y, epochs=50, batch_size=40, validation_data=(X_test_minmax, test_y2), verbose=2, shuffle=True)
# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()

# make a prediction
yhatMinMax = modelNew.predict(X_test_minmax)



# calculate RMSE
rmse = math.sqrt(mean_squared_error(test_y2, yhatMinMax))
print('Test RMSE: %.3f' % rmse)
rmsle(test_y2, yhatMinMax)

any(n < 0 for n in yhatMinMax)
dataResults = np.hstack((test_Original, yhatMinMax))



########################### CLASSIFICATION #################################
extValues2.shape

extValues2=np.hstack((obj.values[:,[0,1,7,9,10,11,12,16]],obj.values[:,17:328]))

test2 = extValues2[randomNums, :]
trainIndices = [x for x in range(0,extValues2.shape[0]) if x not in randomNums]
train2 = extValues2[trainIndices, :]

test_Original2=pd.DataFrame(test_Original).dropna(0)


train2=pd.DataFrame(train2).dropna(0)
train2.isnull().any() 
train2=train2.values

test2=pd.DataFrame(test2).dropna(0)
test2.isnull().any() 
test2=test2.values

train2.shape

train_X, train_y = np.hstack((train2[:, 0:1],train2[:, 3:317])), train2[:, 318]
test_X, test_y = np.hstack((test2[:, 0:1],test2[:, 3:317])), test2[:, 318]

np.unique(test_y, return_counts=True) 

# reshape input to be 3D [samples, timesteps, features]
train_X = train_X.reshape((train_X.shape[0], 1, train_X.shape[1]))
test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)


model = Sequential()
model.add(LSTM(20, input_shape=(train_X.shape[1], train_X.shape[2])))
model.add(Dense(units=1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
# fit network
history = model.fit(train_X, train_y, epochs=50, batch_size=40, validation_data=(test_X, test_y), verbose=2, shuffle=True)
# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()


# make a prediction
yhat2 = model.predict(test_X)

stats.describe(yhat2)

for i in range(len(yhat2)):
    if(yhat2[i]<=0.5):
        yhat2[i] = 0
    else:
        yhat2[i] = 1
    test_y=test_y.astype(float)
        
labels = [0, 1]
confusion_matrix(test_y, yhat2, labels)

from sklearn.metrics import matthews_corrcoef
matthews_corrcoef(test_y, yhat2) 

from sklearn.metrics import accuracy_score
accuracy_score(test_y, yhat2) 


dataResults2 = np.hstack((obj.values, yhat2))

pd.DataFrame(dataResults).to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/regOutput-python.csv")

pd.DataFrame(dataResults2).to_csv("C:/MSc Materials/Dissertation/dublin_bikes-master/classOutput-python.csv")