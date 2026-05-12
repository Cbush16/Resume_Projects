library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)  

# Reading in the main dataset
main <- read.csv(file="C:\\Users\\bushc\\Desktop\\DS-450\\2017_18 telemetry lock and dam 19 analysis master.csv",header=TRUE, sep=",")

## CONVERTING DATE COLUMNS
# Converting Date and Time
main$Date<-mdy(main$Date)                 # Convert Date column to Date objects
main$DeployDate<-mdy(main$DeployDate)     # Fixed previous wrong format
main$Up2017.date<-mdy(main$Up2017.date)
main$Up2018.date<-mdy(main$Up2018.date)

main$START_DATETIME<-mdy_hm(main$START_DATETIME)  # Convert Start datetime
main$END_DATETIME<-mdy_hm(main$END_DATETIME)      # Convert End datetime

## CHECKING FOR MISSING DATA
# Columns to check for missing values 
cols_to_check<-c("TRANSMITTERID", "Species", "AGENCY", "Deploy.loc",
                   "Up.passage", "Down.passage", "RESIDENCEEVENT",
                   "DURATION.sec", "DURATION.min", "log.DUR.min", "NUMRECS")

# Removing rows with missing values in key columns
main_clean<-main[complete.cases(main[, cols_to_check]), ]

# Count rows removed due to missing data
n_removed<-nrow(main)-nrow(main_clean)
cat("Rows removed due to missing data:", n_removed, "\n")

## REMOVING DUPLICATES AND UNNEEDED COLUMNS
# Removing Duplicate Measurement Columns
main_clean<-main_clean %>% select(-Stage.Ft, -DURATION.sec)

# Removing Variables With 95% Missing
main_clean<-main_clean %>% select(-Up2017.date, -Up2018.date)

## CONVERTING CATEGORICAL VARIABLES
# Converting categorical variables to factors for easier modeling
main_clean$Season<-as.factor(main_clean$Season)
main_clean$Species<-as.factor(main_clean$Species)
main_clean$AGENCY<-as.factor(main_clean$AGENCY)
main_clean$Deploy.loc<-as.factor(main_clean$Deploy.loc)
main_clean$Up.passage<-as.factor(main_clean$Up.passage)
main_clean$Down.passage<-as.factor(main_clean$Down.passage)
main_clean$UpPass.2017<-as.factor(main_clean$UpPass.2017)
main_clean$UpPass.2018<-as.factor(main_clean$UpPass.2018)

## HANDLING MISSING VALUES
# Handling missing values for Length and Weight using median imputation
main_clean$Length[is.na(main_clean$Length)]<-median(main_clean$Length, na.rm=TRUE)
main_clean$Weight[is.na(main_clean$Weight)]<-median(main_clean$Weight, na.rm=TRUE)

## CREATING LOG-TRANSFORMED VARIABLES BECAUSE OF MAJOR OUTLIERS
# Add log-transformed versions of numeric columns
main_clean$log_DURATION<-log(main_clean$DURATION.min + 1)  # +1 avoids log(0)
main_clean$log_NUMRECS<-log(main_clean$NUMRECS + 1)        
main_clean$log_Weight<-log(main_clean$Weight + 1)         

## CHECKING THE CLEANED DATA
summary(main_clean)
colSums(is.na(main_clean))/nrow(main_clean)

library(randomForest)

## RANDOM FOREST MODEL After poster presentation went in and fixed the model to filter for only invasive carp during the model. Should have done this before but didn't think about it.
# Filtering dataset to only invasive carp species (MATCHES XGBOOST MODEL)
main_clean<-main_clean[main_clean$Species %in% c("SVCP", "BHCP", "GSCP"), ]

# Checking Assumptions
length(unique(main_clean$TRANSMITTERID))
nrow(main_clean)
# Not every entry is unique so will have to do train/test split with all transmitterid that are the same together

numeric_vars<-main_clean[,sapply(main_clean, is.numeric)]
cor(numeric_vars, use = "complete.obs")
#Correlation Matrix which is a little off due to the transmitterid 

table(main_clean$Up.passage)
prop.table(table(main_clean$Up.passage))
# There is a class imbalance, so will have to weight the classes 

# Handling TRANSMITTERID
# Get unique transmitters
unique_transmitters<-unique(main_clean$TRANSMITTERID)

# Randomly assign 70% of transmitters to training
set.seed(123)
train_transmitters<-sample(unique_transmitters, 0.7 * length(unique_transmitters))

# Assigning rows to train/test based on transmitter (WAS NOT SURE ON HOW TO DO THIS)
train_data<-main_clean %>% filter(TRANSMITTERID %in% train_transmitters)
test_data <-main_clean %>% filter(!TRANSMITTERID %in% train_transmitters)


# Making Predictor Formula
rf_formula<-Up.passage~Temp+Stage.m+Week+Season+Species+Length+log_Weight+
  Tot.lock.n + Barge.Tot.n+Rec.Tot.n+log_DURATION+log_NUMRECS

# Balanced Random Forest Model
rf_M<-randomForest(formula = rf_formula,data = train_data,ntree = 500,importance = TRUE,classwt = c("0" = 1, "1" = 3))

rf_M
# This report shows that my model has performed very well on the training data.
# This is because there was very low class error. Also, only 2.6% of the time the model wrongly classifies a row in testing. 

importance(rf_M)
# From this it can be seen that weight, species, length, and week are the variables with the strongest influence.
# Overall biological traits and temporal information help most when predicting passage. 

#Predictions on Train/Test Data
train_preds<-predict(rf_M, newdata = train_data)

# Get probability predictions (needed for threshold tuning)
test_probs<-predict(rf_M, newdata = test_data, type = "prob")
head(test_probs)

# APPLY CUSTOM THRESHOLD (0.2 instead of 0.5)
test_preds<-ifelse(test_probs[,2] > 0.2, 1, 0)

# Convert to factor for evaluation
test_preds<-factor(test_preds, levels=c(0,1))

# Confusion matrix using UPDATED predictions
CM<-table(Predicted=test_preds, Actual=test_data$Up.passage)
CM

# Accuracy
accuracy<-sum(diag(CM))/sum(CM)

# Precision & Recall for class 1 using prop.table
precision<-CM["1","1"] / sum(CM["1",])
recall<-CM["1","1"] / sum(CM[,"1"])
f1_score<- 2 * precision * recall/(precision + recall)

# Quick print in one line
c(Accuracy=round(accuracy,4), Precision=round(precision,4),
  Recall=round(recall,4), F1=round(f1_score,4))   # The 4's just say to round to 4 decimal places

### NEW MODEL AFTER UPDATING IT RESULTS
# The accuracy says that 94.0% of all predictions are correct
# The precision says that 62.1% of predicted 1's are actually correct
# The recall shows that 94.7% of actual 1's are correctly identified
# The F1 score shows a balanced performance between precision and recall at 0.75

# The model now performs  well at identifying minority events (upstream passage)
# Recall is extremely high, meaning very few actual movement events are missed
# The trade-off is a reduction in precision, meaning more false positives are introduced

# The most important predictors are weight, species, length, week.
# The data still contains very few upstream occurrences, which makes classification inherently imbalanced

# By lowering the classification threshold, the model was improved for ecological detection purposes
# If further improvement was needed, threshold tuning using ROC optimization or additional feature engineering
# could further balance precision and recall depending on study priorities




## Visuals
library(pROC)
pred_prob <- predict(rf_M, newdata = test_data, type = "prob")[,2]
roc_obj <- roc(test_data$Up.passage, pred_prob)
plot(roc_obj,col = 'darkblue',lwd = 3,main = "ROC Curve for Upstream Passage Model",xlab = "False Positive Rate",
     ylab = "True Positive Rate",print.auc = TRUE,print.auc.cex = 1.2,grid = TRUE)


# The curve being in the top left corner along with a high AUC shows strong ability of the model to discriminate between upstream passage and non-passage events.
# This indicates that the Random Forest model performs well in ranking risk of movement, even though classification performance depends on threshold selection

library(ggplot2)
ggplot(as.data.frame(table(Predicted=predict(rf_M,newdata=test_data),Actual=test_data$Up.passage)),
       aes(x=Actual,y=Predicted,fill=Freq))+geom_tile(color="black")+geom_text(aes(label=Freq),size=6)+
  scale_fill_gradient(low="#A6CEE3",high="#1F78B4")+labs(title="Confusion Matrix: Random Forest Passage Model",x="Actual Upstream Passage",
                                                            y="Predicted Upstream Passage")+theme_minimal(base_size=14)
# This shows model is very good at predicting when passage doesn't occur.
# Does sometimes predict passage when it doesn't happen. Common for rare-event data sets.
# 792 True Negatives, 46 True Positives, 36 False Positives, 25 False Negatives 

library(ggplot2)
test_probs <- predict(rf_M, newdata = test_data, type = "prob")
test_pred_class <- ifelse(test_probs[,2] > 0.2, 1, 0)
ggplot(as.data.frame(table(Predicted = test_pred_class,
                           Actual = test_data$Up.passage)),
       aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "#A6CEE3", high = "#1F78B4") +
  labs(title = "Confusion Matrix: Random Forest Passage Model (Threshold = 0.2)",
       x = "Actual Upstream Passage",
       y = "Predicted Upstream Passage") +
  theme_minimal(base_size = 14)
# This shows improved detection of upstream passage events after threshold tuning.
# The model now captures more true positives but introduces additional false positives, which is expected in rare-event ecological prediction problems.



importance_df <- as.data.frame(importance(rf_M))
importance_df$Variable <- rownames(importance_df)

top_vars <- head(importance_df[order(-importance_df$MeanDecreaseGini),"Variable"],5)
top_vars

test_data$pred_prob <- test_probs[,2]

aggregate(pred_prob ~ Season, data = test_data, mean)
season_df <- aggregate(pred_prob ~ Season, data = test_data, mean)
ggplot(season_df, aes(x = Season, y = pred_prob)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Predicted Probability by Season",
       x = "Season",
       y = "Mean Predicted Probability") +
  theme_minimal(base_size = 14)


aggregate(pred_prob ~ Species, data = test_data, mean)
species_df <- aggregate(pred_prob ~ Species, data = test_data, mean)
ggplot(species_df, aes(x = Species, y = pred_prob)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Predicted Probability by Species",
       x = "Species",
       y = "Mean Predicted Probability") +
  theme_minimal(base_size = 14)






### GRADIENT BOOSTING MODEL (XGBOOST) FOR INVASIVE CARP PASSAGE
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)

##Data prep
#Using the main_clean data set from earlier processing
xg_data<-main_clean

#Ensuring categorical variables are factors 
char_cols<-sapply(xg_data, is.character)
xg_data[char_cols]<-lapply(xg_data[char_cols], as.factor)

#Convert response variable to numeric 0/1 for XGBoost
xg_data$Up.passage<-as.numeric(as.character(xg_data$Up.passage))

#Removing irrelevant variables
remove<-c("UpPass.2017","UpPass.2018","Down.passage","Weight","START_DATETIME","END_DATETIME","DeployDate","AGENCY", "Deploy.loc")

#Filter for specific carp species of interest
xgcarp_data<-xg_data[xg_data$Species %in% c("SVCP", "BHCP", "GSCP"), ]

# Actively drop the leakage columns from the data frame
xgcarp_data<-xgcarp_data[, !(names(xgcarp_data) %in% remove)]
xgcarp_data<-droplevels(xgcarp_data) 

#Split by TransmitterID so the model doesn't memorize specific fish
id<-xgcarp_data$TRANSMITTERID

#Create the model matrix (removing ID and the intercept)
X<-model.matrix(Up.passage ~ . - TRANSMITTERID - 1,data=xgcarp_data)
y<-xgcarp_data$Up.passage

#Splitting the data
set.seed(123)
unique_fish<-unique(id)
train_fish<-sample(unique_fish, size=0.7 * length(unique_fish))
train_index<-id %in% train_fish

X_train<-X[train_index, ]
X_test<-X[!train_index, ]
y_train<-y[train_index]
y_test<-y[!train_index]

##Handling class imbalance
#Calculate weight for the positive class to handle rare passage events
#Because passage is rare, calculate a weight to tell the model to pay extra attention to movers
neg<-sum(y_train==0)
pos<-sum(y_train==1)
scale_pos_weight<-neg/pos 

# Convert data into XGBoost's high-performance DMatrix format
dtrain<-xgb.DMatrix(data=X_train,label=y_train)
dtest<-xgb.DMatrix(data=X_test,label=y_test)

##Training
params<-list(objective="binary:logistic",eval_metric="aucpr",# Focus on Precision-Recall for imbalanced data
  max_depth=4,eta=0.05,subsample=0.8,colsample_bytree=0.8,scale_pos_weight=scale_pos_weight)
# Max Depth 4 limits tree depth to 4, eta.05 menas the learning rate is slower, subsample 0.8 means 80% of data per tree is used 


#Training the XGBoost model
model<-xgb.train(params=params,data=dtrain,nrounds=200,verbose=0)

#Convert probabilities to classes based on custom threshold of .25 since it is less like for a fish to pass upstream
pred_prob<-predict(model, X_test)
pred_class<-ifelse(pred_prob > 0.25, 1, 0)

##Model Eval
#Confusion matrix
conf_mat<-confusionMatrix(factor(pred_class,levels=c(0,1)),factor(y_test,levels=c(0,1)),positive="1")
print(conf_mat)

cm_table<-as.data.frame(conf_mat$table)
colnames(cm_table)<-c("Predicted", "Actual", "Count")
ggplot(cm_table,aes(x=Actual,y=Predicted,fill=Count))+geom_tile(color="white")+geom_text(aes(label=Count),size=6)+
  scale_fill_gradient(low="lemonchiffon",high="darkgoldenrod")+labs(title="Confusion Matrix: XGBoost Carp Passage Model",
    x="Actual Class",y="Predicted Class")+theme_minimal(base_size = 14)
#The model correctly classified about 93% of all cases overall.
#The model correctly identified about 84% of the actual positive cases.
#The model correctly identified about 94% of the actual negative cases.
#When the model predicted a positive, it was correct about 59% of the time.
#When the model predicted a negative, it was correct about 98% of the time.
#Kappa of 0.6576 means the models agreement between predicted labels and actual ground truth labels is very good.
#The model performs well across both classes, averaging about 89% correct classification per class.





# Calculating key performance metrics: Precision, Recall (Sensitivity), and F1-Score
precision<-conf_mat$byClass["Pos Pred Value"]
recall<-conf_mat$byClass["Sensitivity"]
f1<-2*(precision*recall)/(precision+recall)
metrics<-data.frame(Precision=precision,Recall=recall,F1_Score=f1)
print(metrics)

##Visuals
#Prep results for plotting
test_results<-xgcarp_data[!train_index, ]
test_results$pred_prob<-pred_prob
test_results$actual<-factor(y_test, levels=c(0,1), labels=c("No Passage", "Upstream"))

#Risk vs Weight (Log Scale)
ggplot(test_results, aes(x=log_Weight,y=pred_prob))+geom_point(aes(color=actual),alpha=0.3)+
  geom_smooth(method="loess",color="black") +scale_color_manual(values=c("seashell3", "dodgerblue3"))+
  labs(title="Movement Risk vs Fish Weight", x="Log-Weight (g)", y="Predicted Risk Score")+
  theme_minimal(base_size = 14)

#Risk vs Water Stage
ggplot(test_results,aes(x=Stage.m,y=pred_prob))+geom_point(aes(color=actual),alpha=0.3)+
  geom_smooth(method="loess",color="darkseagreen")+scale_color_manual(values=c("steelblue4","dodgerblue"))+
  labs(title = "Movement Risk vs Water Stage", x = "Stage (m)", y = "Predicted Risk Score") +
  theme_minimal(base_size = 14)

#Seasonal Risk Trends by Temperature
ggplot(test_results, aes(x=Temp,y=pred_prob,color=Season))+geom_smooth(se=FALSE,size=1.5,span=1.0)+
  labs(title="Seasonal Thermal Risk Profiles",x ="Water Temperature (C)",y="Predicted Risk")+theme_minimal(base_size=14)

#ROC Curve
ggroc(roc_obj,colour="dodgerblue3", size=1.2)+geom_abline(slope=1,intercept=1,linetype="dashed",color="grey")+
  labs(title=paste("XGBoost ROC Curve (AUC =",round(auc(roc_obj), 3), ")"))+
  theme_minimal(base_size=14)

#FEATURE IMPORTANCE
# Extract and plot the most influential biological and environmental predictors
importance<-xgb.importance(feature_names = colnames(X_train),model=model)
xgb.plot.importance(importance[1:min(10, nrow(importance)),],col="dodgerblue3",main="Top Predictors of Upstream Movement")





### K Nearest Neighbors Model
# Train a KNN model using cross-validation to find the best value of k (number of neighbors)
# Data is standardized (centered and scaled) because KNN is distance-based and sensitive to scale
knn_model<-train(X_train,factor(y_train),method = "knn",preProcess = c("center", "scale"),
                 tuneLength = 10,  # Tests different numbers of neighbors
                 trControl = trainControl(method = "cv")) # Uses k-fold cross-validation for model tuning

## Predictions
# Get probabilities for the test set
knn_probs<-predict(knn_model,newdata = X_test,type = "prob")[,"1"]

# Convert probabilities into class predictions using a custom threshold (0.25 instead of default 0.5)
# Lower threshold makes the model more sensitive to detecting positives
knn_preds<-ifelse(knn_probs> 0.25, 1, 0)

## Evaluation
#Build a confusion matrix comparing predicted vs actual values
conf_mat_knn <- confusionMatrix(factor(knn_preds, levels = c(0,1)),factor(y_test, levels = c(0,1)),positive = "1")
print(conf_mat_knn) # Print performance metrics (accuracy, sensitivity, specificity, etc.)

# 347 True Negatives the model correctly predicted 0 (no event) 347 times.
# 32 True Positives the model correctly predicted 1 (event happened) 32 times.
# 6 False Negatives the model missed 6 real events (it predicted 0 when it was actually 1).
# 16 False Positives the model incorrectly predicted 1 when it was actually 0.

#The model correctly predicts the outcome about 94.5% of the time overall.
#Kappa of 0.7139 means the model shows good agreement between predictions and actual values beyond chance.
#The model correctly identifies about 84% of actual positive events (1s).
#The model correctly identifies about 95.6% of actual negative cases (0s).


# Create data frame from your KNN matrix results
cm_data<-as.data.frame(conf_mat_knn$table)

# Create heatmap of confusion matrix results
ggplot(cm_data, aes(x=Reference,y =Prediction,fill=Freq)) +
  geom_tile(color="white")+geom_text(aes(label=Freq),size=10,color="white") +
  scale_fill_gradient(low="steelblue",high = "coral1") +scale_x_discrete(labels = c("Actual: No", "Actual: Yes")) +
  scale_y_discrete(labels=c("Pred: No", "Pred: Yes"))+labs(title = "KNN Predictive Accuracy",fill ="Fish Count")+
  theme_minimal(base_size=16) +theme(legend.position="none",panel.grid.major=element_blank())


# Create ROC object using predicted probabilities
roc_knn<-roc(y_test, knn_probs)
# Plot ROC curve
plot(roc_knn,col= "blue",lwd=3,main="ROC Curve - KNN Model")

# Add AUC value to plot
auc_value<-auc(roc_knn)
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)))


# Create dataset
plot_data <- data.frame(Variable = X_test[, "Stage.m"], Prob = knn_probs)

# Plot
ggplot(plot_data, aes(x = Variable, y = Prob)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "coral1", size = 1.5, se = TRUE) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  labs(title = "Risk Curve: Fish Passage Probability",
    subtitle = "How probability of passage changes with river height",
    x = "River Stage (m)",y = "Predicted Probability") +theme_minimal()
