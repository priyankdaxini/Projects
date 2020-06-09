library(caret)
library(ranger)
library(randomForest)
library(rpart)
library(rpart.plot)
library(xgboost)
library(dplyr)
library(lubridate)
library(e1071)
library(magrittr)
library(vtreat)

getwd()
setwd("C:/Users/HP/Downloads")
wp <- read.csv("tanzania_water_pumps.csv")
head(wp)
status <- read.csv("pumps_status.csv")
head(status)

#Merging
wp2 <- wp %>% 
  left_join(status, by = "id")

#EDA
head(wp2)
str(wp2)

#Cleaning
wp2 <- wp2 %>% 
  select(-id)
wp2$id <- as.factor("id")
wp2$date_recorded <- ymd(wp2$date_recorded)
wp2$num_private <- NULL
wp2$region_code<- as.factor("region_code")
wp2$district_code <- as.factor("district_code")
wp2$date_recorded<-ymd(wp2$date_recorded)
wp2<- wp2[wp2$construction_year!=0,]

#Further cleaning
levels(wp2$status_group)<-c("0","1","2")
levels(wp2$status_group)
wp3<-wp2 %>% 
  mutate(status_group_2=ifelse(status_group==0,0,1)) %>% 
  select(-c(status_group,recorded_by))
head(wp3)
#Train Test Split
rows <- sample(nrow(wp3))
wp4 <- wp3[c(rows),]
head(wp4)

#Split Index
split <- round(nrow(wp4)*0.8)

#creating train and test split
wp4_train <- wp4[1:split,]
wp4_test <- wp4[(split+1):nrow(wp4),]  
head(wp4_train)

rm(wp)
rm(wp1)
rm(wp2)
rm(wp3)

#Log model
model1<-glm(status_group_2~., data= wp4_train,family="Binomial")
summary(model1)

pred1<-predict(model1, newdata=wp4_test,type="response")
head(pred1)
summary(pred1)

#Decision tree

wp_tree<-rpart(status_group2~.,data = wp4_train, method = "anova")
plot(wp_tree)

#predict
pred_train_tree<-predict(wp_tree,newdata=wp4_test)
error_train_tree<-pred_train_tree-wp4_test$status_group2
sqrt(mean(error_train_tree^2))
summary(wp4_test$readmitted)

#Random Forest 
wp_tree2<-ranger(status_group2~.,wp4_train)
plot(wp_tree2)
pred_3<-predict(wp_tree2,wp4_test)
head(pred_3)

error<-pred_3$predictions-wp4_test$status_group2
sqrt(mean(error^2))

#xgboost
#input column: vars
vars <- colnames(wp4_train[,1:37])

#create the treatment plan for training data
treatplan<- designTreatmentsZ(dframe = wp4_train, varlist = vars, verbose = TRUE)
head(treatplan)
treatplan$scoreFrame

treatplan %>% 
  use_series(scoreFrame)

#keep only "clean" and "lev" vars from scroeframe
(newvars<- treatplan %>% 
    use_series(scoreFrame) %>% # this helps use $ in pipe
    filter(code %in% c("clean", "lev")) %>% # keep only clean and lev rows
    use_series(varName)) # return variable names as a character

# create the one hot encoder training data: telco_train_treat
wp4_test_treat <- prepare(treatmentplan = treatplan, dframe = wp4_test, varRestriction = newvars)

# check one hot encoder train and test data
head(wp4_test_treat)
str(wp4_test_treat)

#xgb cv
cv_model<-xgb.cv(data = as.matrix(wp4_train_treat),
                 label = wp4_train$status_group2,
                 nrounds = 100,
                 nfold = 5,
                 objective="binary:logistic", 
                 #eta=0.3
                 #max_depth=6
                 early_stopping_rounds=10,
                 verbose=1)

#get the evaluation log
head(cv_model)

cv_model %>% 
  use_series(evaluation_log) %>% 
  summarise(ntrees_train=which.min(train_error_mean),#find the index of min(train_rmse_mean)
            ntrees_test=which.min(test_error_mean))#find the index of min(test_rmse_mean)

#run xgboost with nrounds=x (consider 16)

xgb_model<-xgboost(data = as.matrix(wp4_train_treat),
                   label = wp4_train$status_group2,
                   nrounds = 16,
                   objective="binary:logistic", #"multi:softprob"
                   eta=0.3,
                   depth=6,
                   verbose=1)

#make predictions
xgb_pred<-predict(xgb_model,newdata=as.matrix(wp4_test_treat))
summary(xgb_pred)
xgb_pred2<-ifelse(xgb_pred>0.3,1,0)

#confusion matrix
xgb_conf_matrix<-table(wp4_test$status_group2,xgb_pred2)
(acc_xgb<-sum(diag(xgb_conf_matrix))/sum(xgb_conf_matrix))
(precision_xgb<-xgb_conf_matrix[2,2]/sum(xgb_conf_matrix[2,2]+xgb_conf_matrix[1,2]))
(recall_xgb <- xgb_conf_matrix[2,2]/sum(xgb_conf_matrix[2,2]+xgb_conf_matrix[2,1]))

