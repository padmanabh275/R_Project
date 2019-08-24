# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from IPython.display import display
pd.options.display.max_columns=None
dataset=pd.read_csv(r'C:\Users\Training088\Desktop\datasetnew.csv')
dataset.info()
dataset.columns
dataset.isnull().sum()
dataset.head()
dataset.describe()
x=dataset.iloc[:,2:-1]
yy=dataset.iloc[:,-1]
## Backward Elimination
import statsmodels.api as sm
cols = list(x.columns)
pmax = 1
while (len(cols)>0):
    p= []
    x_1 = x[cols]
    x_1 = sm.add_constant(x_1)
    model = sm.OLS(yy,x_1).fit()
    p = pd.Series(model.pvalues.values[1:],index = cols)      
    pmax = max(p)
    feature_with_p_max = p.idxmax()
    if(pmax>0.05):
        cols.remove(feature_with_p_max)
    else:
        break
selected_features = cols
print(selected_features)
xx=x[['likes', 'Checkins', 'Returns', 'Category', 'comm24', 'comm48', 'baseTime', 'shares', 'hrs']]
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
xx.iloc[:,:] = sc.fit_transform(xx)
from statsmodels.stats.outliers_influence import variance_inflation_factor

vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(xx.values, i) for i in range(xx.shape[1])]
vif["features"] = xx.columns

vif
from sklearn.cross_validation import train_test_split
x_train, x_test, y_train, y_test = train_test_split(xx, yy, test_size = 0.2, random_state = 0)
from sklearn.linear_model import LinearRegression
regressor=LinearRegression()
regressor.fit(x_train,y_train)
print('coefficient values',regressor.coef_)
print('intercept value',regressor.intercept_)
y_pred=regressor.predict(x_test)
from sklearn import metrics
import sklearn
import numpy as np
print('R2 value:',sklearn.metrics.r2_score(y_test,y_pred))
print('mean_absolute_error:',sklearn.metrics.mean_absolute_error(y_test,y_pred))
print('mean_squared_error:',sklearn.metrics.mean_squared_error(y_test,y_pred))
print('Root Mean Squared Error:', np.sqrt(sklearn.metrics.mean_squared_error(y_test, y_pred)))
from sklearn.tree import DecisionTreeRegressor 

regressor = DecisionTreeRegressor() 
regressor.fit(x_train, y_train) 
y_predd = regressor.predict(x_test) 
print(y_pred)
from sklearn import metrics
import numpy as np
print('R2 value:',sklearn.metrics.r2_score(y_test,y_predd))
print('mean_absolute_error:',metrics.mean_absolute_error(y_test,y_predd))
print('mean_squared_error:',metrics.mean_squared_error(y_test,y_predd))
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(y_test, y_predd)))
from sklearn.ensemble import RandomForestRegressor
regressor = RandomForestRegressor(n_estimators = 100, random_state = 0) 
modell=regressor.fit(x_train, y_train)
y_predr = modell.predict(x_test)
from sklearn import metrics
import sklearn
import numpy as np
print('R2 value:',sklearn.metrics.r2_score(y_test,y_predr))
print('mean_absolute_error:',metrics.mean_absolute_error(y_test,y_predr))
print('mean_squared_error:',metrics.mean_squared_error(y_test,y_predr))
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(y_test, y_predr)))

print('Test Variance score: %.2f' % sklearn.metrics.r2_score(y_test, y_predr))

#Gradient Decent 

from sklearn import ensemble           
            
params = {'n_estimators': 500, 'max_depth': 5, 'min_samples_split': 3,'learning_rate': 0.01, 'loss': 'ls'}
model = ensemble.GradientBoostingRegressor(**params)

model.fit(x_train, y_train)
y_predicted = model.predict(x_test)

print('R2 value:',sklearn.metrics.r2_score(y_test,y_predicted))
print('Mean Absolute Error:', sklearn.metrics.mean_absolute_error(y_test, y_predicted))  
print('Mean Squared Error:', sklearn.metrics.mean_squared_error(y_test, y_predicted))  
print('Root Mean Squared Error:', np.sqrt(sklearn.metrics.mean_squared_error(y_test, y_predicted))) 


import xgboost
params = {'n_estimators': 500, 'max_depth': 5, 'min_samples_split': 3,'learning_rate': 0.01, 'loss': 'ls'}
model = xgboost.XGBRegressor(**params).fit(x_train, y_train)
y_predictedx = model.predict(x_test)
print('R2 value:',sklearn.metrics.r2_score(y_test,y_predictedx))
print('Mean Absolute Error:', sklearn.metrics.mean_absolute_error(y_test, y_predictedx))  
print('Mean Squared Error:', sklearn.metrics.mean_squared_error(y_test, y_predictedx))  
print('Root Mean Squared Error:', np.sqrt(sklearn.metrics.mean_squared_error(y_test, y_predictedx))) 
from sklearn.model_selection import cross_val_predict

fig, ax = plt.subplots()
ax.scatter(y_test, y_predr, edgecolors=(0, 0, 0))
ax.plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], 'k--', lw=4)
ax.set_xlabel('Actual')
ax.set_ylabel('Predicted')
ax.set_title("Actual vs Predicted")
plt.show()
import seaborn as sns
sns.residplot(y_pred, y_test, lowess=True, color="g")
import xgboost
from sklearn.metrics import explained_variance_score

xgb = xgboost.XGBRegressor(n_estimators=500, learning_rate=.1, gamma=0, subsample=0.90, max_depth=3,min_child_weight=1)


xgb.fit(x_train,y_train)


predictions = xgb.predict(x_test)
print(explained_variance_score(predictions,y_test))
print('R2 value:',sklearn.metrics.r2_score(y_test,predictions))
print('Mean Absolute Error:', sklearn.metrics.mean_absolute_error(y_test, predictions))  
print('Mean Squared Error:', sklearn.metrics.mean_squared_error(y_test, predictions))  
print('Root Mean Squared Error:', np.sqrt(sklearn.metrics.mean_squared_error(y_test, predictions))) 
from sklearn.ensemble import ExtraTreesClassifier
# load data
# feature extraction
model = ExtraTreesClassifier()
cc=model.fit(xx, yy)
print(model.feature_importances_)
from sklearn.ensemble import AdaBoostRegressor
clf = AdaBoostRegressor (n_estimators=100, base_estimator=rf,learning_rate=1)
clf.fit(X_train,Y_train)
clf_pred=clf.predict(X_test)
print( "Classification Report :\n ", classification_report(Y_test, clf_pred))

imp=list(zip(x.columns,modell.feature_importances_))
impacting_features=pd.Series(modell.feature_importances_,xx.columns).sort_values( ascending=False)[:20].plot(kind='bar')


from sklearn import svm
from sklearn.model_selection import train_test_split

x_train,x_test,y_train,y_test = train_test_split(xx,yy,test_size=0.25)
clf_svc = svm.SVC(kernel='linear')
clf_svc.fit(x_train,y_train)
y_pred = clf_svc.predict(x_test)

