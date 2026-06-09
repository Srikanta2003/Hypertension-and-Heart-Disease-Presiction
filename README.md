# Cardiovascular Disease Risk Prediction Using Machine Learning

## Project Overview

This project develops and compares multiple statistical and machine learning classification models for predicting **Hypertension** and **Heart Disease** using demographic, behavioral, and clinical risk factors from a healthcare dataset. The primary objective is to evaluate and compare the predictive performance of different classification algorithms in identifying individuals at elevated cardiovascular risk.

Model performance is assessed using standard evaluation metrics including **Accuracy**, **Precision**, **Recall (Sensitivity)**, **F1-Score**, and **Area Under the ROC Curve (AUC)**.

---

## Models Implemented

* Logistic Regression
* Linear Discriminant Analysis (LDA)
* Naive Bayes
* Support Vector Machine (SVM)
* Random Forest
* Gradient Boosting Machine (GBM)
* AdaBoost
* XGBoost

---

## Features Used

The following predictors were used for model development:

* Gender
* Age
* Marital Status
* Work Type
* Residence Type
* Average Glucose Level
* BMI Category
* Smoking Status

---

## Data Preprocessing

The following preprocessing steps were performed:

* Removal of observations with missing BMI values.
* Conversion of categorical variables into factor variables.
* Creation of BMI categories based on standard BMI thresholds.
* Stratified train-test splitting (80:20).
* Oversampling of minority classes using the ROSE package to address class imbalance.
* Dummy variable encoding for models requiring numerical inputs (e.g., XGBoost).

---

## Exploratory Data Analysis

Exploratory Data Analysis (EDA) was conducted to understand the distribution of variables and their relationship with disease outcomes.

Key analyses included:

* Age-wise prevalence of hypertension and heart disease.
* BMI distribution across disease groups.
* Frequency distribution of categorical variables.
* Boxplots and bar charts for risk factor visualization.
* Joint probability analysis of hypertension and heart disease predictions.

---

## Key Objectives

* Predict hypertension and heart disease risk using multiple classification models.
* Compare traditional statistical methods with modern machine learning algorithms.
* Identify the most influential predictors associated with cardiovascular diseases.
* Evaluate the trade-off between sensitivity and specificity in healthcare screening applications.
* Investigate the relationship between predicted hypertension risk and heart disease risk at the individual level.

---

## Key Findings

* **Age** and **Average Glucose Level** emerged as the most influential predictors across nearly all models.
* Ensemble learning methods such as **XGBoost**, **Gradient Boosting**, and **AdaBoost** demonstrated strong predictive performance.
* High-sensitivity models were particularly effective in identifying individuals at elevated cardiovascular risk.
* Predicted probabilities of hypertension and heart disease exhibited a positive correlation, suggesting shared underlying risk factors.
* Random Forest achieved high overall accuracy but comparatively lower sensitivity.
* XGBoost and Gradient Boosting provided strong discrimination ability through superior AUC performance.

---

## Evaluation Metrics

The models were evaluated using:

* Accuracy
* Precision
* Recall (Sensitivity)
* F1-Score
* ROC Curve
* Area Under the Curve (AUC)

---

## Technologies and Packages Used

### Programming Language

* R

### Major Packages

* caret
* randomForest
* e1071
* gbm
* adabag
* xgboost
* MASS
* pROC
* ROSE
* ggplot2
* dplyr

---

## Repository Structure

```text
├── data/
├── scripts/
│   ├── preprocessing.R
│   ├── hypertension_models.R
│   ├── heart_disease_models.R
│   ├── exploratory_analysis.R
│   └── evaluation.R
├── plots/
├── results/
├── report/
└── README.md
```

---

## Future Work

Potential extensions of this project include:

* Hyperparameter tuning using cross-validation.
* Calibration analysis of predicted probabilities.
* Explainable AI techniques such as SHAP values.
* Multi-label prediction of cardiovascular conditions.
* External validation on independent healthcare datasets.

---

## Author

**Srikanta Saha**


