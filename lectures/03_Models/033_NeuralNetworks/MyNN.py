#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import tensorflow as tf
import sklearn


# In[2]:


from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, OneHotEncoder, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import accuracy_score


# In[3]:


import os

# Set the working directory
os.chdir("/content/bank.csv")


# In[4]:


# Load your dataframe 
df = pd.read_csv("bank.csv")


# In[5]:


# Separate predictors and target variable
X = df.drop('deposit', axis=1)
y = df['deposit']


# In[6]:


# Convert the outcome variable to numeric using label encoding
label_encoder = LabelEncoder()
y_encoded = label_encoder.fit_transform(y)


# In[7]:


# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y_encoded, test_size=0.2, random_state=42)


# In[8]:


# Identify numerical and categorical columns
numerical_cols = X_train.select_dtypes(include=['float64', 'int64']).columns
categorical_cols = X_train.select_dtypes(include=['object']).columns


# In[9]:


# Preprocess numerical data (standardization)
numerical_transformer = StandardScaler()

# Preprocess categorical data (one-hot encoding)
categorical_transformer = OneHotEncoder(handle_unknown='ignore')


# In[10]:


# Combine numerical and categorical preprocessors
preprocessor = ColumnTransformer(
    transformers=[
        ('num', numerical_transformer, numerical_cols),
        ('cat', categorical_transformer, categorical_cols)
    ])


# In[11]:


# Preprocess data
X_train_preprocessed = preprocessor.fit_transform(X_train)
X_test_preprocessed = preprocessor.transform(X_test)


# In[12]:


# Define the neural network architecture
model = tf.keras.Sequential([
    tf.keras.Input(shape=(X_train_preprocessed.shape[1],)),
    tf.keras.layers.Dense(128, activation='relu'),
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dense(1, activation='sigmoid')
])


# In[13]:


# See model structure
model.summary()


# In[14]:


# Compile the model
model.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])


# In[15]:


# Train the model
model.fit(X_train_preprocessed, y_train, epochs=100, batch_size=32, verbose=1)


# In[16]:


# Make predictions of the test set
pred_test = model.predict(X_test_preprocessed)
y_pred_test = (pred_test > 0.5).astype(int)


# In[17]:


# Evaluate the model 
test_loss, test_acc = model.evaluate(X_test_preprocessed, y_test, verbose=0)
train_loss, train_acc = model.evaluate(X_train_preprocessed, y_train, verbose=0)
print(f'Test accuracy: {test_acc}')
print(f'Apparent accuracy: {train_acc}')


# In[20]:


# Use L1 and L2 regularization to fight against overfitting
model = tf.keras.Sequential([
    tf.keras.Input(shape=(X_train_preprocessed.shape[1],)),
    tf.keras.layers.Dense(128, activation='relu', kernel_regularizer=tf.keras.regularizers.l1_l2(l1=0.001, l2=0.001)),
    tf.keras.layers.Dense(64, activation='relu', kernel_regularizer=tf.keras.regularizers.l1_l2(l1=0.001, l2=0.001)),
    tf.keras.layers.Dense(1, activation='sigmoid')
])


# In[21]:


# Compile the model
model.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])


# In[22]:


# Train the model
model.fit(X_train_preprocessed, y_train, epochs=100, batch_size=32, verbose=1)


# In[23]:


# Evaluate the model
test_loss, test_acc = model.evaluate(X_test_preprocessed, y_test, verbose=0)
train_loss, train_acc = model.evaluate(X_train_preprocessed, y_train, verbose=0)
print(f'Test accuracy: {test_acc}')
print(f'Apparent accuracy: {train_acc}')

