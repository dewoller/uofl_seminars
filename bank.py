


# Import necessary libraries for data manipulation and analysis
import pandas as pd
import numpy as np

# Import statistical functions from scipy
from scipy.stats import randint

# Import machine learning models and evaluation metrics from scikit-learn
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, confusion_matrix, precision_score, recall_score, ConfusionMatrixDisplay
from sklearn.tree import export_graphviz

# Import visualization libraries
from IPython.display import Image
import graphviz

# Import function for hyperparameter tuning
from sklearn.model_selection import RandomizedSearchCV, train_test_split

from sklearn.preprocessing import LabelEncoder

bank_data = pd.read_csv("bank-marketing.csv", sep=";")
# Convert categorical variables to one-hot encoding
bank_data = pd.get_dummies(bank_data, columns=['job', 'marital', 'education', 'housing', 'loan', 'contact', 'month', 'day_of_week', 'poutcome', 'default'])
# Split the data into features (X) and target (y)
X = bank_data.drop('y', axis=1)
y = bank_data['y']
# Convert the target variable to numeric values
le = LabelEncoder()
y = le.fit_transform(y)
# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

X_test

rf = RandomForestClassifier()
rf.fit(X_train, y_train)

y_pred = rf.predict(X_test)
accuracy = accuracy_score(y_test, y_pred)
print("Accuracy:", accuracy)

for i in range(3):
    tree = rf.estimators_[i]
    dot_data = export_graphviz(tree,
                               feature_names=X_train.columns,  
                               filled=True,  
                               max_depth=2, 
                               impurity=False, 
                               proportion=True)
    graph = graphviz.Source(dot_data)
    display(graph)




param_dist = {'n_estimators': randint(50,500),
              'max_depth': randint(1,20)}
param_dist = {'n_estimators': randint(50,500),
              'max_depth': randint(1,20)}
# Create a random forest classifier
rf = RandomForestClassifier()
# Use random search to find the best hyperparameters
rand_search = RandomizedSearchCV(rf, param_distributions = param_dist , n_iter=5, cv=5)
# Fit the random search object to the data
rand_search.fit(X_train, y_train)


# Create a variable for the best model
best_rf = rand_search.best_estimator_
# Print the best hyperparameters
print('Best hyperparameters:',  rand_search.best_params_)
Best hyperparameters: {'max_depth': 4, 'n_estimators': 234}

# Generate predictions with the best model
y_pred = best_rf.predict(X_test)
# Create the confusion matrix
cm = confusion_matrix(y_test, y_pred)

ConfusionMatrixDisplay(confusion_matrix=cm).plot();


accuracy = accuracy_score(y_test, y_pred)
precision = precision_score(y_test, y_pred)
recall = recall_score(y_test, y_pred)
print("Accuracy:", accuracy)
print("Precision:", precision)
print("Recall:", recall)




