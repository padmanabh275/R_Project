# -*- coding: utf-8 -*-

"""


@author: CAIA
"""
'''objective:-
"Predict behavior to retain customers. You can analyze all relevant customer data and develop focused customer retention programs." [IBM Sample Data Sets]

Content
Each row represents a customer, each column contains customer’s attributes described on the column Metadata.

The data set includes information about:

Customers who left within the last month – the column is called Churn
Services that each customer has signed up for – phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies
Customer account information – how long they’ve been a customer, contract, payment method, paperless billing, monthly charges, and total charges
Demographic info about customers – gender, age range, and if they have partners and dependents'''


import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
sns.set(style="white")
sns.set(style="whitegrid", color_codes=True)

import os
os.chdir(r'G:\New folder\Classification')
dataset = pd.read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
dataset.describe()
dataset.info()
dataset = dataset.replace(' ', np.nan)
dataset["TotalCharges"] = dataset["TotalCharges"].astype('float64')

dataset.isnull().sum()

def null_impute(df):
    for cols in list(df.columns.values):
        if df[cols].isnull().sum() == 0:
            df[cols] = df[cols]
        elif df[cols].dtypes == 'float64' or df[cols].dtypes == 'int64':
            df[cols] = df[cols].fillna(df[cols].mean())
        else:
            df[cols] = df[cols].fillna(df[cols].mode()[0])

null_impute(dataset)
dataset.isnull().sum()

# Mapping 1 and 0 to Yes and No
dict_y_n = {'Yes' : 1, 'No' : 0}
#dataset.Partner = dataset.Partner.map(dict_y_n)
#dataset.Dependents = dataset.Dependents.map(dict_y_n)
#dataset.PhoneService = dataset.PhoneService.map(dict_y_n)
#dataset.PaperlessBilling = dataset.PaperlessBilling.map(dict_y_n)
dataset.Churn = dataset.Churn.map(dict_y_n)

X = dataset.iloc[:, 1:20]
y = dataset.iloc[:, 20].values

X = pd.get_dummies(X)

#X = pd.get_dummies(X[])
#dummies = pd.get_dummies(X[['gender', 'MultipleLines', 'InternetService', 'OnlineSecurity',
#                            'OnlineBackup', 'DeviceProtection', 'TechSupport',
#                            'StreamingTV', 'StreamingMovies', 'Contract', 'PaymentMethod']])
#
#
#X = pd.concat([dummies, X.drop(['gender', 'MultipleLines', 'InternetService', 'OnlineSecurity',
#                            'OnlineBackup', 'DeviceProtection', 'TechSupport',
#                            'StreamingTV', 'StreamingMovies', 'Contract', 'PaymentMethod'], 1)], 1)

#from sklearn.preprocessing import Imputer
#imputer_mean = Imputer(strategy = 'mean')
#X[['tenure', 'MonthlyCharges', 'TotalCharges']] = imputer_mean.fit_transform(X[['tenure', 'MonthlyCharges', 'TotalCharges']])

# Dv Plot
sns.countplot(x = 'Churn',data = dataset, palette = 'hls')
plt.show()

# Correleation
cor = X.corr()
sns.heatmap(cor)
plt.show()

# Train Test Split
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)


# MOdel logistic regression
from sklearn.linear_model import LogisticRegression
log_regressor = LogisticRegression()
log_regressor.fit(X_train, y_train)

from sklearn.feature_selection import RFE
rfe = RFE(log_regressor, 44)
rfe = rfe.fit(X, y)
print(rfe.support_)
print(rfe.ranking_)


# Predict
y_pred_l = log_regressor.predict(X_test)

# Confusion Matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_pred_l, y_test)
cm

scor = log_regressor.score(X_test, y_test)

from sklearn.metrics import accuracy_score
accuracy_l = accuracy_score(y_test, y_pred_l)
print("Accuracy of Our Model: ", round(accuracy_l * 100, 2), ' %')

from sklearn.metrics import cohen_kappa_score
kappa_l = cohen_kappa_score(y_pred_l, y_test)
kappa_l

from sklearn.metrics import classification_report
rep = classification_report(y_test, y_pred_l)
rep

from sklearn.metrics import roc_auc_score
roc_val = roc_auc_score(y_test, y_pred_l)
roc_val

# Plot
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
logit_roc_auc = roc_auc_score(y_test, y_pred_l)
fpr, tpr, thresholds = roc_curve(y_test, log_regressor.predict_proba(X_test)[:,1])
#plt.figure()
plt.plot(fpr, tpr, label='Logistic Regression (area = %0.2f)' % logit_roc_auc)
plt.plot([0, 1], [0, 1],'--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.show()
'''What is discrimination threshold? : When you have a binary classifier system, what you get as output is 
the probability of an observation to be classified as Class 0 or Class 1. Once, you decide the threshold you classify observations into the two classes.  
Say, you are classifying a tumor as cancerous or non-cancerous. You get the probability of the tumor to be cancerous as 0.75.
 If you have set the threshold of your system as 0.8,
 then you classify the tumor as non-cancerous. If you have set the threshold as 0.7, you would classify the tumor as cancerous.
 The area under the curve gives you an idea of how good your classifier is.'''
## 2nd Model
# Fitting Decision Tree Regression to the dataset
from sklearn.tree import DecisionTreeClassifier
d_classifier = DecisionTreeClassifier(criterion = 'entropy', random_state = 0)
d_classifier.fit(X_train, y_train)

y_pred_d = d_classifier.predict(X_test)

# Confusion Matrix
from sklearn.metrics import confusion_matrix
cm_d = confusion_matrix(y_pred_d, y_test)
cm_d

from sklearn.metrics import accuracy_score
accuracy_d = accuracy_score(y_test, y_pred_d)
print("Accuracy of Our Model: ", round(accuracy_d * 100, 2), ' %')
#it measures how much better the classfier is comparing with guessing with the target distribution.
'''Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
So, in our case, the kappa statistic equals: (0.60 - 0.50)/(1 - 0.50) = 0.20.

As another example, here is a less balanced confusion matrix and the corresponding calculations:

     Cats Dogs
Cats| 22 | 9  |
Dogs| 7  | 13 |
Ground truth: Cats (29), Dogs (22) 
Machine Learning Classifier: Cats (31), Dogs (20) 
Total: (51) 
Observed Accuracy: ((22 + 13) / 51) = 0.69 
Expected Accuracy: ((29 * 31 / 51) + (22 * 20 / 51)) / 51 = 0.51 
Kappa: (0.69 - 0.51) / (1 - 0.51) = 0.37'''
from sklearn.metrics import cohen_kappa_score
kappa_d = cohen_kappa_score(y_pred_d, y_test)
kappa_d

# Feature Importance Only For Classification
d_classifier.fit(X, y)
fs = d_classifier.feature_importances_

ff = np.argsort(fs)[::-1]
names = [list(X.columns.values)[i] for i in ff]
plt.figure(figsize=(10,5))
plt.title("Feature Importance")
plt.bar(range(X.shape[1]), fs[ff])
plt.xticks(range(X.shape[1]), names, rotation=90)
plt.show()

#import graphviz
#from sklearn import tree
#dot_data = tree.export_graphviz(d_classifier, out_file=None) 
#graph = graphviz.Source(dot_data) 
#graph.render("dataset") 
#dot_data = tree.export_graphviz(d_classifier, out_file=None, 
#                         
#                         filled=True, rounded=True,  
#                         special_characters=True)  
#graph = graphviz.Source(dot_data)  
#graph 

# Plot
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
dt_roc_auc = roc_auc_score(y_test, y_pred_d)
fpr, tpr, thresholds = roc_curve(y_test, d_classifier.predict_proba(X_test)[:,1])
#plt.figure()
plt.plot(fpr, tpr, label='Decision Tree Classification (area = %0.2f)' % dt_roc_auc)
plt.plot([0, 1], [0, 1],'--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.show()

## 3rd Model
# Random Forest
from sklearn.ensemble import RandomForestClassifier
classifier_rf = RandomForestClassifier(n_estimators = 300, criterion = 'entropy', random_state = 0)
classifier_rf.fit(X_train, y_train)

y_pred_rf = classifier_rf.predict(X_test)
y_pred_rf

# Confusion Matrix
from sklearn.metrics import confusion_matrix
cm_rf = confusion_matrix(y_pred_rf, y_test)
cm_rf

from sklearn.metrics import accuracy_score
accuracy_rf = accuracy_score(y_test, y_pred_rf)
print("Accuracy of Our Model: ", round(accuracy_rf * 100, 2), ' %')

from sklearn.metrics import cohen_kappa_score
kappa_rf = cohen_kappa_score(y_pred_rf, y_test)
kappa_rf

# Feature Importance Only For Classification
classifier_rf.fit(X, y)
fs_rf = classifier_rf.feature_importances_

ff_rf = np.argsort(fs)[::-1]
names = [list(X.columns.values)[i] for i in ff_rf]
plt.figure(figsize=(10,5))
plt.title("Feature Importance")
plt.bar(range(X.shape[1]), fs_rf[ff_rf])
plt.xticks(range(X.shape[1]), names, rotation=90)
plt.show()

# Plot
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
rf_roc_auc = roc_auc_score(y_test, y_pred_rf)
fpr, tpr, thresholds = roc_curve(y_test, classifier_rf.predict_proba(X_test)[:,1])
#plt.figure()
plt.plot(fpr, tpr, label='Random Forest Classification (area = %0.2f)' % rf_roc_auc)
plt.plot([0, 1], [0, 1],'--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.show()


# 4th Model
# Fitting Naive Bayes to the Training set
from sklearn.naive_bayes import GaussianNB
classifier_nb = GaussianNB()
classifier_nb.fit(X_train, y_train)

y_pred_nb = classifier_nb.predict(X_test)

# Confusion Matrix
from sklearn.metrics import confusion_matrix
cm_nb = confusion_matrix(y_pred_nb, y_test)
cm_nb

from sklearn.metrics import accuracy_score
accuracy_nb = accuracy_score(y_test, y_pred_nb)
print("Accuracy of Our Model: ", round(accuracy_nb * 100, 2), ' %')

from sklearn.metrics import cohen_kappa_score
kappa_nb = cohen_kappa_score(y_pred_nb, y_test)
kappa_nb

# Plot
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
nb_roc_auc = roc_auc_score(y_test, y_pred_nb)
fpr, tpr, thresholds = roc_curve(y_test, classifier_nb.predict_proba(X_test)[:,1])
#plt.figure()
plt.plot(fpr, tpr, label='Naive Bayes Classification (area = %0.2f)' % rf_roc_auc)
plt.plot([0, 1], [0, 1],'--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.legend(loc="lower right")
plt.show()

## Accuracy and Kappa
print("Accuracy of Logistic Regression Model: ", round(accuracy_l * 100, 2), '%', " Kappa Value :", round(kappa_l * 100, 2))
print("Accuracy of Decision Tree Model: ", round(accuracy_d * 100, 2), ' %', " Kappa Value :", round(kappa_d * 100, 2))
print("Accuracy of Random Forest Model: ", round(accuracy_rf * 100, 2), ' %', " Kappa Value :", round(kappa_rf * 100, 2))
print("Accuracy of Naive Bayes Model: ", round(accuracy_nb * 100, 2), ' %', " Kappa Value :", round(kappa_nb * 100, 2))