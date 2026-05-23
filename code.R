rm(list = ls())

library(dplyr)
data = read.csv("D:/predoc/stroke/healthcare-dataset-stroke-data.csv")
head(data)

data = data %>%
  dplyr::select(-id, -stroke)

str(data)
summary(data)


# BMI missing value removed 

data$bmi = as.numeric(ifelse(data$bmi == "N/A", NA, data$bmi))
sum(is.na(data$bmi))
data = data[!is.na(data$bmi), ]


# Factor Conversions

data$gender = as.factor(data$gender)
table(data$gender)
data = data %>% filter(gender != "Other")
data$gender = droplevels(data$gender)

data$ever_married= as.factor(data$ever_married)
data$work_type= as.factor(data$work_type)
data$Residence_type = as.factor(data$Residence_type)
data$smoking_status = as.factor(data$smoking_status)

str(data)

# hypertension and heart_disease as numeric
data$hypertension= as.numeric(data$hypertension)
data$heart_disease = as.numeric(data$heart_disease)
table(data$hypertension)


# BMI Category

data = data %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "<18.5",
      bmi >= 18.5 & bmi < 22.9 ~ "18.5-22.9",
      bmi >= 22.9 & bmi < 24.9~ "23-24.9",
      bmi >= 24.9 ~ ">=25",
      TRUE ~ NA_character_
    )
  )

data$bmi_category = factor(
  data$bmi_category,
  levels = c("<18.5", "18.5-22.9", "23-24.9", ">=25")
)

table(data$bmi_category)
table(data$bmi_category, data$hypertension)
table(data$hypertension, data$work_type)
table(data$work_type)

str(data)

#EDA
library(dplyr)

data %>%
  
  group_by(gender) %>%
  
  summarise(
    Hypertension_Percentage =
      round(mean(hypertension == 1) * 100, 2)
  )


data %>%
  group_by(gender) %>%
  summarise(
    Heart_Disease_Percentage =
      round(mean(heart_disease == 1) * 100, 2)
  )
# Age Group 

data$age_group = cut(
  
  data$age,
  
  breaks = c(
    0, 16, 30, 45, 60, 75, 90
  ),
  
  labels = c(
    "0-16",
    "17-30",
    "31-45",
    "46-60",
    "61-75",
    "76-90"
  ),
  
  include.lowest = TRUE
)


# Age-wise Hypertension Summary


library(dplyr)

age_summary = data %>%
  group_by(age_group) %>%
  summarise(
    Total_Sample = n(),
    Hypertension_Cases = sum(hypertension == 1),
    Hypertension_Percentage = round(
      100 * Hypertension_Cases / Total_Sample,
      2
    )
  )

age_summary

heart_summary = data %>%
  group_by(age_group) %>%
  summarise(
    Total_Sample = n(),
    Heart_Disease_Cases = sum(heart_disease == 1),
    Heart_Disease_Percentage = round(
      100 * Heart_Disease_Cases / Total_Sample,2
    )
  )

print(heart_summary)

# BMI Distribution by Hypertension Status


library(ggplot2)

ggplot(
  data,
  
  aes(
    x = factor(hypertension),
    y = bmi,
    fill = factor(hypertension)
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "BMI vs Hypertension",
    x = "Hypertension",
    y = "BMI",
    fill = "Hypertension"
  ) +
  
  scale_x_discrete(
    labels = c(
      "0" = "No Hypertension",
      "1" = "Hypertension"
    )
  ) +
  
  theme_minimal()


# BMI vs Heart Disease

library(ggplot2)

ggplot(
  data,
  aes(
    x = factor(heart_disease),
    y = bmi,
    fill = factor(heart_disease)
  )
) +
  geom_boxplot() +
  labs(
    title = "BMI vs Heart Disease",
    x = "Heart Disease",
    y = "BMI",
    fill = "Heart Disease"
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "No Heart Disease",
      "1" = "Heart Disease"
    )
  ) +
  theme_minimal()

# Average Glucose Level vs Hypertension

library(ggplot2)

ggplot(
  data,
  
  aes(
    x = factor(hypertension),
    y = avg_glucose_level,
    fill = factor(hypertension)
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Average Glucose Level vs Hypertension",
    x = "Hypertension",
    y = "Average Glucose Level",
    fill = "Hypertension"
  ) +
  
  scale_x_discrete(
    labels = c(
      "0" = "No Hypertension",
      "1" = "Hypertension"
    )
  ) +
  
  theme_minimal()



# Average Glucose Level vs Heart Disease

ggplot(
  data,
  
  aes(
    x = factor(heart_disease),
    y = avg_glucose_level,
    fill = factor(heart_disease)
  )
) +
  
  geom_boxplot() +
  
  labs(
    title = "Average Glucose Level vs Heart Disease",
    x = "Heart Disease",
    y = "Average Glucose Level",
    fill = "Heart Disease"
  ) +
  
  scale_x_discrete(
    labels = c(
      "0" = "No Heart Disease",
      "1" = "Heart Disease"
    )
  ) +
  
  theme_minimal()





# Train/Test Split

library(caret)
set.seed(123)

data$strata = interaction(data$gender,data$hypertension,drop = TRUE)

train_index = createDataPartition(data$strata, p = 0.8,list = FALSE)

train_data = data[train_index, ]
test_data  = data[-train_index, ]

table(train_data$hypertension)
table(test_data$hypertension)

# Remove heart_disease and strata
train_data = train_data %>%
  dplyr::select(-heart_disease, -strata)

test_data = test_data %>%
  dplyr::select(-heart_disease, -strata)


# ROSE Oversampling
library(ROSE)
N = 2*max(table(train_data$hypertension))
balanced_train = ovun.sample(hypertension ~ .,data= train_data,method = "over",N= N)$data
table(balanced_train$hypertension)

for(col in names(balanced_train)) {
  if(is.factor(balanced_train[[col]])) {
    test_data[[col]] = factor(
      test_data[[col]],
      levels = levels(balanced_train[[col]])
    )
  }
}

# -----------------------------
# Logistic Regression
# -----------------------------

glm.fit = glm(hypertension ~gender+age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,data=balanced_train,family=binomial)
summary(glm.fit)

logit_prob=predict(glm.fit, newdata = test_data, type = "response")
logit_pred=ifelse(logit_prob > 0.5, 1, 0)

confusionMatrix(
  factor(logit_pred, levels = c("0", "1")),
  factor(test_data$hypertension, levels = c("0", "1")),
  positive = "1"
)

# ROC Curve
library(pROC)
par(pty = "s")
roc(test_data$hypertension,logit_prob,plot= TRUE,legacy.axes = TRUE,main="ROC Curve - Logistic Regression")


# Random Forest
library(randomForest)
set.seed(123)

balanced_train$hypertension = as.factor(balanced_train$hypertension)
test_data$hypertension= as.factor(test_data$hypertension)

rf.fit=randomForest(hypertension ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,data= balanced_train,ntree= 500,mtry= 3,importance = TRUE)

print(rf.fit)

rf_pred = predict(rf.fit, newdata = test_data, type = "class")

rf_prob = predict(rf.fit, newdata = test_data, type = "prob")[, 2]

confusionMatrix(
  rf_pred,
  test_data$hypertension,
  positive = "1"
)

importance(rf.fit)
varImpPlot(rf.fit, main = "Variable Importance-Random Forest")

par(pty = "s")
roc_obj = roc(response= as.numeric(as.character(test_data$hypertension)),predictor = rf_prob)
plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - Random Forest")
auc(roc_obj)


# SVM
library(e1071)
set.seed(123)

svm_model = svm(hypertension ~gender +age +ever_married +work_type +Residence_type
                +avg_glucose_level +bmi_category +smoking_status,data= balanced_train,
                kernel= "radial",probability= TRUE)

svm_pred = predict(svm_model,newdata = test_data,probability = TRUE)
svm_prob = attr(svm_pred, "probabilities")[, 2]

confusionMatrix(svm_pred,test_data$hypertension,positive = "1")

roc_obj = roc(response= as.numeric(as.character(test_data$hypertension)),predictor= svm_prob)
plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - SVM")
auc(roc_obj)


# Gradient Boosting (GBM)


library(gbm)
set.seed(123)

balanced_train$hypertension = as.numeric(as.character(balanced_train$hypertension))


test_data$hypertension_num = as.numeric(as.character(test_data$hypertension))

gbm_model = gbm(hypertension ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level+bmi_category +smoking_status,
  data= balanced_train,
  distribution = "bernoulli",
  n.trees= 500,
  interaction.depth = 3,
  shrinkage= 0.01,
  cv.folds= 5,
  n.minobsinnode= 10,
  verbose= FALSE
)

best_iter= gbm.perf(gbm_model, method = "cv")

gbm_prob= predict(gbm_model,newdata = test_data,n.trees = best_iter,type="response")

gbm_pred=ifelse(gbm_prob > 0.5, "1", "0")

gbm_pred= factor(gbm_pred,levels = levels(test_data$hypertension))

confusionMatrix(
  gbm_pred,
  test_data$hypertension,
  positive = "1"
)

roc_obj = roc(response= test_data$hypertension_num,predictor = gbm_prob)
plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - Gradient Boosting")
auc(roc_obj)

summary(gbm_model)


# Naive Bayes

library(e1071)


balanced_train$hypertension = as.factor(balanced_train$hypertension)
nb_model = naiveBayes(hypertension ~gender+age+ever_married+work_type+Residence_type+avg_glucose_level+bmi_category +smoking_status,data=balanced_train)

nb_pred = predict(nb_model, newdata = test_data, type = "class")
nb_prob = predict(nb_model, newdata = test_data, type = "raw")[, 2]

confusionMatrix(
  nb_pred,
  test_data$hypertension,
  positive = "1"
)

roc_obj = roc(response  = as.numeric(as.character(test_data$hypertension)),predictor = nb_prob)
plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - Naive Bayes")
auc(roc_obj)



# AdaBoost


library(adabag)
set.seed(123)
balanced_train$hypertension = as.factor(balanced_train$hypertension)
test_data$hypertension= as.factor(test_data$hypertension)

ada_model = boosting(hypertension ~gender+age+ever_married+work_type+Residence_type+avg_glucose_level+bmi_category+smoking_status,
  data= balanced_train,
  boos= TRUE,
  mfinal=100
)

ada_pred= predict(ada_model, newdata = test_data)
ada_class= ada_pred$class
ada_prob= ada_pred$prob[, 2]

confusionMatrix(
  factor(ada_class, levels = c("0", "1")),
  factor(test_data$hypertension, levels = c("0", "1")),
  positive = "1"
)

roc_obj = roc(response= as.numeric(as.character(test_data$hypertension)),predictor = ada_prob)
plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - AdaBoost")
auc(roc_obj)

importanceplot(ada_model)

# Linear Discriminant Analysis (LDA)
library(MASS)
library(caret)
library(pROC)



balanced_train$hypertension = as.factor(balanced_train$hypertension)
test_data$hypertension= as.factor(test_data$hypertension)

if("hypertension_num" %in% names(test_data)) {
  test_data = test_data %>% dplyr::select(-hypertension_num)
}

lda_model = lda(hypertension ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,
  data = balanced_train
)

print(lda_model)

lda_pred=predict(lda_model,newdata = test_data)

lda_class = lda_pred$class

lda_prob=lda_pred$posterior[, "1"]



confusionMatrix(
  factor(lda_class, levels = c("0", "1")),
  factor(test_data$hypertension, levels = c("0", "1")),
  positive = "1")

roc_obj=roc(response= as.numeric(as.character(test_data$hypertension)),predictor = lda_prob)

plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main="ROC Curve - LDA")

auc(roc_obj)
abs(lda_model$scaling)


# XGBoost
library(xgboost)
library(caret)
library(pROC)

set.seed(123)

balanced_train$hypertension = as.numeric(as.character(balanced_train$hypertension))
test_data$hypertension_num = as.numeric(as.character(test_data$hypertension))

dummy_model_xgb= dummyVars(hypertension ~gender +age +ever_married+work_type+Residence_type+avg_glucose_level+bmi_category+smoking_status,data = balanced_train)

train_xgb= predict(dummy_model_xgb,newdata = balanced_train)

test_data_xgb=test_data[, names(test_data) %in% names(balanced_train)]

test_xgb = predict(dummy_model_xgb,newdata = test_data_xgb)

train_xgb=as.matrix(train_xgb)
test_xgb=as.matrix(test_xgb)



dtrain = xgb.DMatrix(data  = train_xgb,label = balanced_train$hypertension)

dtest = xgb.DMatrix(data  = test_xgb,label = test_data$hypertension_num)


xgb_model = xgb.train(
  params = list(
    objective= "binary:logistic",
    eval_metric= "auc",
    eta= 0.01,     
    max_depth= 3,         
    min_child_weight = 10,        
    subsample= 0.8,       
    colsample_bytree = 0.8,       
    lambda= 1,        
    alpha= 0          
  ),
  data= dtrain,
  nrounds= 500,
  watchlist= list(train = dtrain, test = dtest),
  early_stopping_rounds = 50,    # stop if no improvement
  verbose= 0
)

cat("Best iteration:", xgb_model$best_iteration, "\n")
xgb_prob = predict(xgb_model,newdata = dtest)

xgb_pred = ifelse(xgb_prob > 0.5, "1", "0")

xgb_pred = factor(xgb_pred,levels = levels(test_data$hypertension))
confusionMatrix(
  xgb_pred,
  test_data$hypertension,
  positive = "1"
)
importance_matrix = xgb.importance(model = xgb_model)

print(importance_matrix)
xgb.plot.importance(importance_matrix,main = "Variable Importance - XGBoost")

roc_obj = roc(response= test_data$hypertension_num,predictor= xgb_prob)

plot(roc_obj,col= "blue",lwd= 2,legacy.axes = TRUE,main= "ROC Curve - XGBoost")
auc(roc_obj)

importance_matrix = xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)





# HEART DISEASE CLASSIFIER


# Train-Test Split

library(caret)
set.seed(123)

data$strata=interaction(data$gender,data$heart_disease,drop = TRUE)

train_index=createDataPartition(data$strata,p = 0.8,list = FALSE)

train_data=data[train_index, ]
test_data=data[-train_index, ]

# Remove hypertension and strata
train_data = train_data %>%
  dplyr::select(-hypertension, -strata)

test_data = test_data %>%
  dplyr::select(-hypertension, -strata)


# ROSE Oversampling
library(ROSE)
N=2*max(table(train_data$heart_disease))
balanced_train = ovun.sample(heart_disease ~ .,data = train_data,method = "over",N = N)$data
table(balanced_train$heart_disease)

# Match factor levels
for(col in names(balanced_train)) {
  if(is.factor(balanced_train[[col]])) {
    test_data[[col]] = factor(
      test_data[[col]],
      levels = levels(balanced_train[[col]])
    )
  }
}


#logistic regression
glm.fit = glm(heart_disease ~gender+age+ever_married+work_type+Residence_type+avg_glucose_level+bmi_category+smoking_status,
  data = balanced_train,family = binomial)
summary(glm.fit)
heart_prob = predict(glm.fit,newdata = test_data,type = "response")

heart_pred = ifelse(heart_prob>0.5,"1","0")

confusionMatrix(
  factor(heart_pred, levels = c("0", "1")),
  factor(test_data$heart_disease, levels = c("0", "1")),
  positive = "1"
)

library(pROC)
roc_obj = roc(response = as.numeric(as.character(test_data$heart_disease)),predictor = heart_prob)
plot(roc_obj,col = "blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve-Logistic Regression")
auc(roc_obj)


#random forest
library(randomForest)

balanced_train$heart_disease=as.factor(balanced_train$heart_disease)
test_data$heart_disease = as.factor(test_data$heart_disease)

rf.fit=randomForest(heart_disease ~gender+age+ever_married +work_type+Residence_type+avg_glucose_level+bmi_category+smoking_status,
  data=balanced_train,ntree = 500,mtry = 3,importance = TRUE)

rf_pred=predict(rf.fit,newdata=test_data,type="class")

rf_prob = predict(rf.fit,newdata = test_data,type = "prob")[, 2]

confusionMatrix(rf_pred,test_data$heart_disease,positive = "1")

importance(rf.fit)

varImpPlot(rf.fit,main="Variable Importance - Random Forest")

roc_obj = roc(response = as.numeric(as.character(test_data$heart_disease)),predictor = rf_prob)

plot(roc_obj,col="blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve - Random Forest")

auc(roc_obj)


#SVM
library(e1071)

svm_model = svm(heart_disease ~gender +age +ever_married+work_type+Residence_type+avg_glucose_level+bmi_category+smoking_status,
  data = balanced_train,
  kernel = "radial",
  probability = TRUE
)

svm_pred = predict(svm_model,newdata = test_data,probability=TRUE)
svm_prob = attr(svm_pred,"probabilities")[, 2]
confusionMatrix(svm_pred,test_data$heart_disease,positive = "1")

roc_obj = roc(response=as.numeric(as.character(test_data$heart_disease)),predictor=svm_prob)
plot(roc_obj,col = "blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve - SVM")
auc(roc_obj)


#gradient boosting
library(gbm)
set.seed(123)

balanced_train$heart_disease = as.numeric(as.character(balanced_train$heart_disease))
test_data$heart_disease_num = as.numeric(as.character(test_data$heart_disease))

gbm_model = gbm(heart_disease ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,
  data = balanced_train,
  distribution = "bernoulli",
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = FALSE
)

best_iter = gbm.perf(gbm_model,method = "cv")
gbm_prob = predict(gbm_model,newdata=test_data,n.trees=best_iter,type="response")
gbm_pred = ifelse(gbm_prob>0.5,"1","0")
gbm_pred = factor(gbm_pred,levels=levels(test_data$heart_disease))
confusionMatrix(gbm_pred,test_data$heart_disease,positive="1")
library(pROC)

roc_obj = roc(response = test_data$heart_disease_num,predictor = gbm_prob)

plot(roc_obj,col="blue",lwd=2,legacy.axes=TRUE,main="ROC Curve - Gradient Boosting")

auc(roc_obj)

summary(gbm_model)


# naive bayes
library(e1071)


balanced_train$heart_disease=as.factor(balanced_train$heart_disease)

nb_model=naiveBayes(heart_disease ~ gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,
  data = balanced_train)

nb_pred = predict(nb_model,newdata = test_data,type = "class")

nb_prob = predict(nb_model,newdata = test_data,type = "raw")[, 2]

confusionMatrix(nb_pred,test_data$heart_disease,positive = "1")

roc_obj = roc(response = as.numeric(as.character(test_data$heart_disease)),predictor = nb_prob)

plot(roc_obj,col = "blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve - Naive Bayes")

auc(roc_obj)


#ADABOOST
library(adabag)

set.seed(123)

balanced_train$heart_disease=as.factor(balanced_train$heart_disease)

test_data$heart_disease=as.factor(test_data$heart_disease)

ada_model = boosting(heart_disease ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,
  data = balanced_train,
  boos = TRUE,
  mfinal = 100
)

ada_pred=predict(ada_model,newdata = test_data)

ada_class=ada_pred$class

ada_prob=ada_pred$prob[, 2]

confusionMatrix(
  factor(ada_class, levels = c("0", "1")),
  factor(test_data$heart_disease, levels = c("0", "1")),
  positive = "1"
)

roc_obj = roc(response = as.numeric(as.character(test_data$heart_disease)),predictor = ada_prob)

plot(roc_obj,col = "blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve - AdaBoost")

auc(roc_obj)

importanceplot(ada_model)


#lda

library(MASS)


balanced_train$heart_disease = as.factor(balanced_train$heart_disease)

lda_model = lda(heart_disease ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +
    bmi_category +smoking_status,
  data = balanced_train
)

print(lda_model)

lda_pred = predict(lda_model,newdata = test_data)
lda_class = lda_pred$class

lda_prob = lda_pred$posterior[, "1"]

confusionMatrix(
  factor(lda_class, levels = c("0", "1")),
  factor(test_data$heart_disease, levels = c("0", "1")),
  positive = "1"
)

roc_obj = roc(response = as.numeric(as.character(test_data$heart_disease)),predictor = lda_prob)

plot(roc_obj,col = "blue",lwd = 2,legacy.axes = TRUE,main = "ROC Curve - LDA")

auc(roc_obj)

abs(lda_model$scaling)


#XGBOOST


library(xgboost)

set.seed(123)

balanced_train$heart_disease=as.numeric(as.character(balanced_train$heart_disease))

test_data$heart_disease_num=as.numeric(as.character(test_data$heart_disease))

dummy_model_xgb=dummyVars(heart_disease ~gender +age +ever_married +work_type +Residence_type +avg_glucose_level +bmi_category +smoking_status,
  data = balanced_train)

train_xgb=predict(dummy_model_xgb,newdata = balanced_train)

test_data_xgb = test_data[
  ,
  names(test_data) %in% names(balanced_train)
]

test_xgb = predict(
  dummy_model_xgb,
  newdata = test_data_xgb
)

train_xgb = as.matrix(train_xgb)

test_xgb = as.matrix(test_xgb)

dtrain = xgb.DMatrix(
  data = train_xgb,
  label = balanced_train$heart_disease
)

dtest = xgb.DMatrix(
  data = test_xgb,
  label = test_data$heart_disease_num
)

xgb_model = xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.01,
    max_depth = 3,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8,
    lambda = 1,
    alpha = 0
  ),data = dtrain,nrounds = 500,watchlist = list(train = dtrain,test = dtest
  ),early_stopping_rounds = 50,
  verbose = 0
)

cat("Best iteration:",xgb_model$best_iteration,"\n")

xgb_prob = predict(
  xgb_model,
  newdata = dtest
)

xgb_pred = ifelse(xgb_prob > 0.5,"1","0")

xgb_pred = factor(xgb_pred,levels = levels(test_data$heart_disease))

confusionMatrix(
  xgb_pred,
  test_data$heart_disease,
  positive = "1"
)

importance_matrix = xgb.importance(
  model = xgb_model
)

print(importance_matrix)

xgb.plot.importance(
  importance_matrix,
  main = "Variable Importance - XGBoost"
)

roc_obj = roc(
  response = test_data$heart_disease_num,
  predictor = xgb_prob
)

plot(
  roc_obj,
  col = "blue",
  lwd = 2,
  legacy.axes = TRUE,
  main = "ROC Curve - XGBoost"
)

auc(roc_obj)

# ── Shared split ──────────────────────────────────────────────
set.seed(123)
data$strata = interaction(data$gender, data$hypertension, drop = TRUE)
train_index = createDataPartition(data$strata, p = 0.8, list = FALSE)

train_data = data[train_index, ]
test_data  = data[-train_index, ]
data$strata = NULL

# ── Balanced trains (separate, one per outcome) ───────────────

# For hypertension
train_hyp = train_data %>% dplyr::select(-heart_disease)
N_hyp     = 2 * max(table(train_hyp$hypertension))
bal_hyp   = ovun.sample(hypertension ~ ., data = train_hyp,
                        method = "over", N = N_hyp)$data
bal_hyp$hypertension = as.factor(bal_hyp$hypertension)

# For heart disease
train_hd = train_data %>% dplyr::select(-hypertension)
N_hd     = 2 * max(table(train_hd$heart_disease))
bal_hd   = ovun.sample(heart_disease ~ ., data = train_hd,
                       method = "over", N = N_hd)$data
bal_hd$heart_disease = as.factor(bal_hd$heart_disease)

# ── Align factor levels ───────────────────────────────────────
for(col in names(bal_hyp)) {
  if(is.factor(bal_hyp[[col]])) {
    test_data[[col]] = factor(test_data[[col]],
                              levels = levels(bal_hyp[[col]]))
  }
}

# ── Logistic Regression for both ─────────────────────────────
glm_hyp = glm(hypertension ~ gender + age + ever_married + work_type +
                Residence_type + avg_glucose_level + bmi_category + smoking_status,
              data = bal_hyp, family = binomial)

glm_hd  = glm(heart_disease ~ gender + age + ever_married + work_type +
                Residence_type + avg_glucose_level + bmi_category + smoking_status,
              data = bal_hd, family = binomial)

# ── Predict on SAME test set ──────────────────────────────────
prob_hyp = predict(glm_hyp, newdata = test_data, type = "response")
prob_hd  = predict(glm_hd,  newdata = test_data, type = "response")

# ── Build joint data frame ────────────────────────────────────
joint_df = data.frame(
  prob_hyp   = prob_hyp,
  prob_hd    = prob_hd,
  has_hyp    = as.factor(test_data$hypertension),
  has_hd     = as.factor(test_data$heart_disease),
  age        = test_data$age,
  both       = as.factor(
    ifelse(test_data$hypertension == 1 & test_data$heart_disease == 1, "Both",
           ifelse(test_data$hypertension == 1, "Hyp only",
                  ifelse(test_data$heart_disease == 1, "HD only", "Neither")))
  )
)

# ── Plot ──────────────────────────────────────────────────────
library(ggplot2)

ggplot(joint_df, aes(x = prob_hyp, y = prob_hd, color = both)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c(
    "Both"     = "red",
    "Hyp only" = "orange",
    "HD only"  = "steelblue",
    "Neither"  = "grey60"
  )) +
  labs(
    title = "Joint Predicted Risk: Hypertension vs Heart Disease",
    x     = "P(Hypertension)",
    y     = "P(Heart Disease)",
    color = "True Label"
  ) +
  theme_minimal()

# ── Correlation ───────────────────────────────────────────────
cat("Pearson correlation:", cor(prob_hyp, prob_hd), "\n")
