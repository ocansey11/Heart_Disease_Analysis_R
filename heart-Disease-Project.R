
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purr", repos = "http://cran.us.r-project.org")



heart <- read.csv("https://github.com/ocansey11/Heart_Disease_Analysis_R/blob/main/heart.csv")
# heart <- read.csv(file.choose(), header=TRUE)


# age
# sex
# cp = chest pain type (4 values)
# trestbps = resting blood pressure measured in mm Hg on admission to the hospital.
# chol = serum cholestoral in mg/dl  
# fbs = fasting blood sugar > 120 mg/dl
# restecg = resting electrocardiographic results (values 0= normal,1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertropy by Estes )  
# thalach = maximum heart rate achieved
# exang = exercise induced angina
# oldpeak = ST depression induced by exercise relative to rest
# slope = the slope of the peak exercise ST segment; 1 = upslopping 2 = flat 3 = downslopping 
# ca = number of major vessels (0-3) colored by flourosopy
# thal: 0 = normal; 1 = fixed defect; 2 = reversible defect
# target - the predicted attribute - diagnosis of heart disease ( angiographic disease status) value 0 <= 50% diameter narrowing; and value 1  => 50% diameter narrowing 
# The names and social security numbers of the patients were recently removed from the database, replaced with dummy values.



# SUMMARY
summary(heart)

#  After close look at each attribute i found discrepancies for two predictors. Ca and Thal.
# Following the description provided from Kaggle, recurring values, ca-4 and thal-0 were not accounted for.
# So either the mistake was made dring the description of the data set or made during the collection.


# Cleaning the data 
sum(heart$ca == 4 ) + sum(heart$thal == 0)
# The errors in our data set sum up to 25. we can carry on without them

# Check the correlation between these 'predictor' attributes and our outcome attributes
heart %>% filter(ca == 4) %>% group_by(target) %>% ggplot(aes(ca, fill = target )) + geom_bar()
# ca4 has a strong correlation with target1

heart %>% group_by(target) %>% ggplot(aes(thal, fill = target )) + geom_bar()
# thal0 has a neutral correlation with target.




 heart_clean <- heart %>% filter(ca != 4) %>% filter(thal != 0)
 heart_clean <- heart_clean %>% mutate(ca = droplevels(ca), thal = droplevels(thal))
 heart_clean$ca
 heart_clean$thal

 
 
set.seed(101)

library(tidyverse)
library(dplyr)
library(dslabs)
library(caret)
library(purrr)


#EXPLORATORY DATA
head(heart_clean)


#REFIXING DATA VARIABLES

# We are going to use this new data frame "heart 2" to do some visualization,
# hence we need to change the levels of the values into  readable characters

heart2 <-  heart_clean %>% mutate(sex = if_else(sex == 1, "Male", "Female"),
                            age = if_else(age <= 64 & age >= 24, "Adult", "Elderly"),
                  fbs = if_else( fbs == 1, ">120", "<=120"),
                  exang = if_else(exang ==  1, "Yes", "No"),
                  cp =if_else(cp == 1, "Typical Angina Pain", 
                              if_else(cp == 2,  "Atypical Angina Pain", 
                                     ifelse( cp == 3, "Non-Anginal Pain", "Asymptomatic Pain"))),
                  restecg = ifelse(restecg == 0, "Normal", if_else(restecg ==  1, "Abnormality", "Probable or definite")),
                  slope = if_else( slope == 1,"upslopping", if_else(slope == 2, "Flat", "Downslopping")),
                  ca = as.factor(ca),
                  thal = if_else( thal == 1, "Fixed defect", if_else(thal == 2, "Normal", "Reversible Defect")),
                  target = if_else(target == 1,"Yes", "No")) %>% mutate_if(is.character, as.factor) %>% select(target, sex, fbs, exang, cp, restecg, slope, thal, everything())

summary(heart2)


# VISUALIZATION
# I am going to render some graphs to show the relationships between the values in our data frame
# to have an idea of how Heart Diseases are spread across various groups in our data


# Bar plots 
# Heart Disease
heart2 %>% ggplot(aes(target , fill = target)) + 
  geom_bar() + 
  xlab("Heart Disease") + 
  ggtitle("Presence Of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))



# Heart disease Grouped by Sex
heart2 %>% filter(target == "Yes") %>%  group_by(sex) %>% count() %>%
  ggplot(aes(sex, n, fill = sex)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Sex") +
  xlab("Sex") + ylab("count") 


# Check to see if Males are prevalent in our data.
prop.table(table(heart2$sex))
# There are 2 times more Males than females in the dataset. something to keep in mind 


# Looking at the graph we cannot make a general statement that Males are more likely to suffer from heart diseases,
# because from the data there is prevalence of Males
# Hence we can check the proportion of Heart disease for each gender
prop.table(table(heart2$sex, heart2$target))

# We can represent or findings in a graph like so
heart2 %>% group_by(sex, target) %>% count() %>%
  ggplot(aes(sex, n, fill = target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Sex") +
  xlab("Sex") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))

# We notice although there are more men with heart Disease in this data set. it seems Females have a higher concentration of heart disease looking at only the Female bar in the graph




# SEX AND AGE
heart2 %>% group_by(sex, age, target) %>% count() %>%
  ggplot(aes(sex, n, fill = target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Sex") +
  xlab("Sex") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present")) + facet_wrap(~age)

# This graph shows Heart Disease are prevalent in females across our various ages as well.




#  CHOL
# Comparing the correlation between cholesterol levels and heart disease 
heart2 %>% group_by(chol,target) %>% count() %>%  ggplot(aes(chol,n)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis by Chol") +
  xlab("Cholesterol") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present")) + facet_wrap(~target)

# Zooming on the cholesterol levels between 200 and 300.
heart2 %>% filter(chol > 200 & chol < 300) %>% group_by(chol,target) %>% count() %>%  ggplot(aes(chol,n)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Cholesterol") +
  xlab("Cholesterol") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present")) + facet_wrap(~target)

heart2 %>% filter(chol > 200 & chol < 300) %>% group_by(target) %>% summarize(n())





# FAST BLOOD SUGAR
heart2 %>% group_by(fbs,target) %>% count() %>%  ggplot(aes(fbs,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Fasting Blood Sugar") +
  xlab("FBS") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present")) 



# CHEST PAIN
heart2 %>% group_by(cp,target,sex) %>% count() %>%  ggplot(aes(cp,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Chest Pain") +
  xlab("Chest Pain") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present")) + facet_wrap(~sex )      



# CA
heart2 %>% group_by(ca,target) %>% count() %>%  ggplot(aes(ca,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with CA") +
  xlab("CA") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))



# THAL
heart2 %>% group_by(thal,target) %>% count() %>%  ggplot(aes(thal,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Thal") +
  xlab("THAL") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))



# THALACH
heart2 %>% group_by(thalach,target) %>% count() %>%  ggplot(aes(thalach,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Thalach") +
  xlab("THALACH") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))


#trestbps
heart2 %>% group_by(trestbps,target) %>% count() %>%  ggplot(aes(trestbps,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Trestbps") +
  xlab("Trestbps") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))



# SLOPE

heart2 %>% group_by(slope,target) %>% count() %>%  ggplot(aes(slope,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Slope") +
  xlab("Slope") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))


# OLDPEAK
heart2 %>% group_by(oldpeak,target) %>% count() %>%  ggplot(aes(oldpeak,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with OldPeak") +
  xlab("Oldpeak") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))


# EXANG
heart2 %>% group_by(exang,target) %>% count() %>%  ggplot(aes(exang,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Exang") +
  xlab("Exang") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))



# Restecg
heart2 %>% group_by(restecg,target) %>% count() %>%  ggplot(aes(restecg,n, fill= target)) +
  geom_col()  +
  ggtitle("Heart Disease Analysis with Restecg") +
  xlab("Restecg") + ylab("count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absent", "Present"))








# MACHINE LEARNING
# FIX THE VARIABLE TYPES
heart_clean$sex <-factor(heart_clean$sex)
heart_clean$cp<- factor(heart_clean$cp)
heart_clean$fbs <- factor(heart_clean$fbs)
heart_clean$restecg <- factor(heart_clean$restecg)
heart_clean$exang <- factor(heart_clean$exang)
heart_clean$slope <- factor(heart_clean$slope)
heart_clean$ca <- factor(heart_clean$ca)
heart_clean$thal <-factor(heart_clean$thal)
heart_clean$target <- factor(heart_clean$target)


set.seed(101)
# Split the Data
y <- heart_clean$target
test_index <- createDataPartition(y,times = 1, p = 0.8, list = FALSE)

train_set <- heart_clean[test_index,]
test_set <- heart_clean[-test_index,]



# SIMPLE MODEL

prop.table(table(heart2$sex, heart2$target))  
# 86 females do not have heart disease. whiles 223 females out of a total 319 do. 
# that means, according to our dataset 73 out of 100 Females are likely to have heart disease. 
# Only 41 percent of the males have heart disease.



#  Considering this Lets Try with a simple model where we predict if the patient has heart disease based just on their sex
# target is mutated to be 1 if  sex = 0  which is Female in our dataset
# We chose Females because according to our proportion 
# males are less likely to have heart diseases.

set.seed(101)
y_hat_sex <- factor(ifelse(heart_clean$sex == 0, 1, 0))
confusionMatrix(y_hat_sex, heart_clean$target)$overall["Accuracy"]
# We get an accuracy of 0.629. Not bad, but we applied the model still using the main data set. 

# Applying this Model on the Test set
y_hat_sex_test <- factor(ifelse(test_set$sex == 0, 1, 0))
confusionMatrix(y_hat_sex_test, test_set$target)$overall["Accuracy"]
# we get an accuracy of 0.6582915 on the test_set



# Heres what happens if we used Male. You can Uncomment and run the code below
# y_hat_sex <- factor(ifelse(heart$sex == 1, 1, 0))
# confusionMatrix(y_hat_sex, heart$target)$overall["Accuracy"]


# Update to our simple model by including the ages variable as a predictor
y_hat_sex_age <- factor(if_else(heart_clean$sex == 0, 1, ifelse(heart_clean$age > 64 , 1, 0)))
confusionMatrix(y_hat_sex_age, heart_clean$target)$overall["Accuracy"]
#  When we include ages as a criteria the accuracy reduced from 0.62 to 0.58. keep in mind we did not use the test_set for this 





  # 1. USING GLM

set.seed(101)
# considering conditional probabilities we want to determine the best p value for improving our accuracy
ps <- seq(0.3, 0.7, 0.1)

accuracy_general <- map_df(ps, function(p){
  
fit <- glm(target ~ exang + trestbps + chol + age + sex + thal + restecg + cp + fbs + ca + oldpeak + slope + thalach, data = train_set, family = binomial)
p_hat <- predict(fit, test_set, type = "response")
y_hat <-  factor(ifelse(p_hat > p, 1, 0))
cm <- confusionMatrix(y_hat, as.factor(test_set$target))
acc <- cm$overall["Accuracy"]

f_meas <- F_meas(y_hat, test_set$target)
cm_sensitivity <- sensitivity(y_hat, test_set$target)
cm_specificity <- specificity(y_hat, test_set$target)


tibble( acc, f_meas, cm_sensitivity, cm_specificity)
})  
 
accuracy_results <- cbind( ps, accuracy_general)
accuracy_results
# You can decide which p value cutoff to use. depending on F meas and cm sensitivity 0.6 seems to be the best.



  
  # 2. IMPROVING ON OUR PREVIOUS MODEL USING LOGISTIC REGRESSION AND  K-FOLD CROSS VALIDATION 
  set.seed(101)
  train_control  <- trainControl(method = 'cv', number = 10)

  model_glm <- train(target ~ exang + trestbps + chol + age + sex + thal + restecg + cp + fbs + ca + oldpeak + slope + thalach, data = train_set, family = binomial, method = "glm", trControl = train_control)
  y_hat_glm <- predict(model_glm, test_set)
  cm_glm <-  confusionMatrix(y_hat_glm,test_set$target)
  cm_glm$overall["Accuracy"]
  F_meas(y_hat_glm,test_set$target)
  
  varImp(model_glm)
  
  
# Remember our FBs graph which showed am equal spread of target variables for both case of Fbs( > 120 and  < 120). 
# This was one of the graph with little to no correlation with our target variable, hence i decide to see what happens if i omit it from our model
  model_glm <- train(target ~  thal + restecg + cp  + exang + trestbps + age + sex + chol + ca + thal + oldpeak + slope  + thalach, data = train_set, family = binomial, method = "glm", trControl = train_control)
  y_hat_glm <- predict(model_glm, test_set)
  cm_glm <-  confusionMatrix(y_hat_glm,test_set$target)
  cm_glm$overall["Accuracy"]
  # F_meas slightly better than our initial glm model as well.
  F_meas(y_hat_glm,test_set$target)

  varImp(model_glm)
  
  
  # Final GLM Model
  # selecting the final predictors based on the Variable Importance
  model_glm <- train(target ~  thal +  cp  + exang + trestbps +  sex + chol + ca + thal + oldpeak +slope+ thalach, data = train_set, family = binomial, method = "glm", trControl = train_control)
  y_hat_glm <- predict(model_glm, test_set)
  cm_glm <-  confusionMatrix(y_hat_glm,test_set$target)
  cm_glm$overall["Accuracy"]
  F_meas(y_hat_glm,test_set$target)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #3. USING KNN
  
  set.seed(101)
  model_knn <- train(target ~ thal + restecg + cp  + exang + trestbps + age + sex + fbs + chol + ca + oldpeak + slope + thalach, data = train_set,  method = "knn", trControl = train_control)
  y_hat_knn <- predict(model_knn, test_set)
  cm_knn <-  confusionMatrix(y_hat_knn,test_set$target)
  cm_knn$overall["Accuracy"]
  im_knn <- varImp(model_knn)
  im_knn
  # i noticed fbs had 0.00 varImportance hence we run the Knn model again without it
  
  
  model_knn <- train(target ~ thal + restecg + cp  + exang + trestbps + age + sex + thal + restecg + cp  + chol + ca + oldpeak + slope + thalach, data = train_set,  method = "knn", trControl = train_control)
  y_hat_knn <- predict(model_knn, test_set)
  cm_knn <-  confusionMatrix(y_hat_knn,test_set$target)
  cm_knn$overall["Accuracy"]
  im_knn <- varImp(model_knn)
  im_knn
  
  # We notice trestbps had 0.00 var Importance as well. There seems to be a trend. Also restecg had a very low vaImp of 5.848.
  # We notice after removing sex and age we get a better accuracy. However it is lower than the accuracy from glm model.
  # I continued to chip away predictors until i had an accuracy of 0.8040201. 
  
  model_knn <- train(target ~  oldpeak + thalach + ca + thal , data = train_set,  method = "knn", trControl = train_control)
  y_hat_knn <- predict(model_knn, test_set)
  cm_knn <-  confusionMatrix(y_hat_knn,test_set$target)
  cm_knn$overall["Accuracy"]

  im_knn <- varImp(model_knn)
  im_knn  
  # i noticed thal has a varImp of 0, however if we remove the predictor the total accuracy drops.

  
  
  #4. CLASSIFICATION TREEE
  set.seed(101)
  library(rpart)
  tree_model <- rpart(target ~ thal + restecg + cp  + exang + trestbps +age + sex  + restecg +  fbs + ca + oldpeak + slope + thalach, data = train_set, method = "class")
  varImp(tree_model)
  
  # Rerun the model without fbs, slope, age, sex, trestbps and restecg
 
  tree_model <- rpart(target ~  exang +  thal +  cp  + ca + oldpeak +  thalach, data = train_set, method = "class")
  varImp(tree_model)
  
  plotcp(tree_model)
  
  plot(tree_model, uniform=TRUE,
       main="Classification Tree for Heart Disease")
  text(tree_model, use.n = TRUE, all=TRUE, cex = 0.75)
  
  y_hat_tree <- predict(tree_model, test_set, type = 'class')
  cm_tree <- confusionMatrix(y_hat_tree, test_set$target)
  cm_tree
  
  #  i had a final accuracy of 0.8342
  
  
  #5 RANDOM FOREST
  set.seed(101)
  library(randomForest)
  model_forest <- randomForest(target ~ exang + trestbps + chol + age + sex + thal + restecg + cp + fbs + 
                           ca + oldpeak + slope + thalach, data = train_set, ntree = 50)
  y_hat_forest <- predict(model_forest, test_set, type = 'class')
  cm_forest <- confusionMatrix(y_hat_forest, test_set$target)
  varImp(model_forest)
  
  
  
  # We get a high accuracy between 0.98 and 1
  
 