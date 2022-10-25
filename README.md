# Cervical-Cancer-Prediction

# Project Objective:
Predicting the detection of Cervical Cancer in Women based on attributes and when Biopsy is conducted on them.

# Dataset Information:
Dataset contains 858 rows/instances and 36 columns/attributes.
Focus on 25 columns out of which only Biopsy will be our target column

# Key Observations:

Low Recall but High Precision of the models suggest that are very picky. It generally classifies false negatives correctly.The model cannot classify the classes well but whenever it does have been classified, they can be trusted upon.

A low AUC value corresponds to the fact that the target column is imbalance.In this case there very few women with Cervical Cancer based on Biopsy.

The models have high Accuracy (>90%) but we observe that TP is low.So many diagnosed with Cancer are classified are undiagnosed.

Taking all the metrics into account we can conclude that Naive Bayes Model performs the best compared to others.

