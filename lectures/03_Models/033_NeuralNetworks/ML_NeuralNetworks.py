import pandas as pd
import numpy as np
import tensorflow as tf
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, OneHotEncoder, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.metrics import accuracy_score

# Load your dataframe 
df = pd.read_csv("../../../Data/bank.csv")

# Separate predictors and target variable
X = df.drop('deposit', axis=1)
y = df['deposit']

# Convert the outcome variable to numeric using label encoding
label_encoder = LabelEncoder()
y_encoded = label_encoder.fit_transform(y)

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y_encoded, test_size=0.2, random_state=42)

# Identify numerical and categorical columns
numerical_cols = X_train.select_dtypes(include=['float64', 'int64']).columns
categorical_cols = X_train.select_dtypes(include=['object']).columns

# Preprocess numerical data (standardization)
numerical_transformer = StandardScaler()

# Preprocess categorical data (one-hot encoding)
categorical_transformer = OneHotEncoder(handle_unknown='ignore')

# Combine numerical and categorical preprocessors
preprocessor = ColumnTransformer(
    transformers=[
        ('num', numerical_transformer, numerical_cols),
        ('cat', categorical_transformer, categorical_cols)
    ])

# Preprocess data
X_train_preprocessed = preprocessor.fit_transform(X_train)
X_test_preprocessed = preprocessor.transform(X_test)

# Define the neural network architecture
model = tf.keras.Sequential([
    tf.keras.layers.Dense(128, activation='relu', input_shape=(X_train_preprocessed.shape[1],)),
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dense(1, activation='sigmoid')
])

# See model structure
model.summary()

# Compile the model
model.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

# Train the model
model.fit(X_train_preprocessed, y_train, epochs=100, batch_size=32, verbose=1)

# Make predictions of the test set
pred_test = model.predict(X_test_preprocessed)
y_pred_test = (pred_test > 0.5).astype(int)

# Evaluate the model 
test_loss, test_acc = model.evaluate(X_test_preprocessed, y_test, verbose=0)
train_loss, train_acc = model.evaluate(X_train_preprocessed, y_train, verbose=0)
print(f'Test accuracy: {test_acc}')
print(f'Apparent accuracy: {train_acc}')

## Discussion
## - What does it mean that we have more accuracy on the training set than on the test set?

# Use L1 and L2 regularization to fight against overfitting
model = tf.keras.Sequential([
    tf.keras.layers.Dense(128, activation='relu', kernel_regularizer=tf.keras.regularizers.l1_l2(l1=0.001, l2=0.001), input_shape=(X_train_preprocessed.shape[1],)),
    tf.keras.layers.Dense(64, activation='relu', kernel_regularizer=tf.keras.regularizers.l1_l2(l1=0.001, l2=0.001)),
    tf.keras.layers.Dense(1, activation='sigmoid')
])
# Compile the model
model.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])
# Train the model
model.fit(X_train_preprocessed, y_train, epochs=100, batch_size=32, verbose=1)

# Evaluate the model
test_loss, test_acc = model.evaluate(X_test_preprocessed, y_test, verbose=0)
train_loss, train_acc = model.evaluate(X_train_preprocessed, y_train, verbose=0)
print(f'Test accuracy: {test_acc}')
print(f'Apparent accuracy: {train_acc}')
