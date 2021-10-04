# Load the lib 
library(ggplot2)
library(dplyr)
library(caret) # Package to perform one hot encoding
library(ggmap)
library(scales)
library(RColorBrewer)
library(caTools)

# Load the dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

# Ddataset used for visualization
dummy <- data.frame(cbind(train,as.factor(labels$status_group))) # Considering the status as factor column
names(dummy)[41] <- 'status_group'
# Analyzing the dataset
dim(train)  # To check the shape of our dataset

# Describing the dataset


# Analyzing the dataset
tb <- with(train,table(extraction_type_class))
lab <- with(labels,table(status_group))

# Analyzing extraction_type_class
ggplot(as.data.frame(tb), aes(factor(extraction_type_class), Freq)) + geom_col(position = 'dodge')

# Analyzing labels
ggplot(data=dummy, aes(x=status_group)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill=brewer.pal(3, "Set1"), alpha = 1/2) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  xlab("Operational Status") + ylab("Percent")


# Visualize Regions
color_label <- c("#66FF00", "#0099CC", "#FF6666")
table(dummy$status_group, dummy$construction_year)
prop.table(table(dummy$status_group, dummy$construction_year), margin = 2)

# "Eliminating" the construction year of 0 (probably indicating missing data for year) for better graph representation
ggplot(subset(dummy, construction_year > 0),
       aes(x = construction_year, fill = status_group)) + 
  geom_bar() +
  xlab("Construction year")  +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  scale_fill_manual(values=color_label)

# Look at distribution of labels per basin
ggplot(dummy,
       aes(x = basin, fill = status_group)) + 
  geom_bar() +
  xlab("Basin")  +
  theme(axis.text.x=element_text(angle = 20, hjust = 1)) +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  theme(legend.position = "top") +
  scale_fill_manual(values=color_label)

# Water Quantity
ggplot(dummy,
       aes(x = quantity, fill = status_group)) + 
  geom_bar() +
  xlab("Basin") +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  theme(legend.position = "top") +
  scale_fill_manual(values=color_label)

# Create bar plot for source_type
qplot(source_type, data=dummy, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  labs(fill = "Status of waterpoint") +
  theme(axis.text.x=element_text(angle = 20, hjust = 1)) + 
  scale_fill_manual(values=color_label) + 
  xlab("Type of water source") +
  ylab("Number of waterpoints")

# Look at distribution of labels per total static head 
ggplot(subset(dummy, amount_tsh <20000 & amount_tsh >0),
       aes(x = amount_tsh)) + 
  geom_histogram(bins = 20) +
  facet_wrap(~ status_group) +
  xlab("Total Static Head") +
  ylab("Number of waterpoints")

# To get the unique values of recorded_by
unique(train['recorded_by'])
# Since we can see that 'recorded_by' column has no unique elements present so we can drop.
train <- train[ , !names(train) %in% c('recorded_by')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('recorded_by')]

# Checking our dataset dimension after dropping a column 
dim(train) 

# Checking our second column amount_tsh
str(train['amount_tsh'])
table(train$amount_tsh)
# From the below table we can say that it consist off more zeros so we can drop it
train <- train[ , !names(train) %in% c('amount_tsh')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('amount_tsh')]

# dim(train) # Checking our dataset dimension

# In public meeting we are removing that column 
train <- train[ , !names(train) %in% c('public_meeting')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('public_meeting')]

dim(train)

# Checking permit column
table(train$permit)
# From below table we can see that there is a empty string and we can't take the mean values as it is boolean value
# So drop that column
train <- train[ , !names(train) %in% c('permit')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('permit')]

dim(train)

# Grouping extraction type,extraction type group and extraction type class
View(data.frame(train$extraction_type,train$extraction_type_group,train$extraction_type_class))
# We can say that three column have a common value and hence we can remove one column
train <- train[ , !names(train) %in% c('extraction_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('extraction_type_group')]

######################################## Get back to it later
# Grouping management and management_group
View(data.frame(train$management,train$management_group))
# We can't drop anything from above mentioned columns as they posses different values

# Compare scheme name and scheme management
View(data.frame(train$scheme_name,train$scheme_management))
table(train$scheme_name) # Checking the table of values for scheme_name
table(train$scheme_management) # Checking the table of values for scheme_management
# Since scheme_name consist of too many categorical values we can eliminate it
train <- train[ , !names(train) %in% c('scheme_name')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('scheme_name')]

dim(train)

# Comparing the payment and payment_type
View(train[c('payment','payment_type')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('payment')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('payment')]

dim(train)
dim(test)

# Comparing the water_quality and quality_group
View(train[c('water_quality','quality_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quality_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quality_group')]

dim(train)
dim(test)

# Comparing the quantity and quantity_group
View(train[c('quantity','quantity_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quantity_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quantity_group')]

# dim(train)

# Comparing the source_type, source and source_class
View(train[c('source_type','source','source_class')])
# By comparing we can say that source_type and source are same so we can drop either of it but source_class defines type of the water source
train <- train[ , !names(train) %in% c('source_type')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('source_type')]

# dim(train)

# Comparing the source_type, source and source_class
View(train[c('waterpoint_type','waterpoint_type_group')])
# By comparing we can say that source_type and source are same so we can drop either of it but source_class defines type of the water source
train <- train[ , !names(train) %in% c('waterpoint_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('waterpoint_type_group')]

# dim(train)

# drop columns
train <- train[ , !names(train) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

# Doing the above for test data
test <- test[ , !names(test) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

dim(train)
dim(test)

# Converting the date to days
train$date_recorded = as.numeric(as.Date(Sys.Date()) - as.Date(c(train$date_recorded)))
test$date_recorded = as.numeric(as.Date(Sys.Date()) - as.Date(c(test$date_recorded)))
View(train)
View(test)

# Replacing all 0 with NA

# After analyzing we can say that 0's are present in two columns that is population and construction year so we consider their mean value
replace_zeros <- function(x){
  # converting that particular columns 0 values to NA and then considering the mean
  x$population[x$population == 0] <- NA
  x$construction_year[x$construction_year == 0] <- NA
  
  # Replacing the NA values to its respective mean
  x$population <- ifelse(is.na(x$population), round(mean(x$population, na.rm=TRUE)), x$population)
  x$construction_year <- ifelse(is.na(x$construction_year),round( mean(x$construction_year, na.rm=TRUE)), x$construction_year)
  x$gps_height <- ifelse(is.na(x$gps_height),round( mean(x$gps_height, na.rm=TRUE)), x$gps_height)
  
  return(x) # returning the dataset
}

df_train <- replace_zeros(train)
df_test <- replace_zeros(test)
View(df_train)
View(df_test)

# Converting the categorical value of to top 4 categorical values 
clean_dataset <- function(x){
  x$basin <- ifelse(x$basin == 'Lake Victoria','lake victoria',x$basin)
  x$basin <- ifelse(x$basin == 'Pangani','pangani',x$basin)
  x$basin <- ifelse(x$basin == 'Rufiji','rufiji',x$basin)
  x$basin <- ifelse(x$basin == 'Internal','internal',x$basin)
  x$basin <- ifelse(x$basin != 'lake victoria' & x$basin != 'pangani' & x$basin != 'rufiji' & x$basin != 'internal' ,'other_basin',x$basin)
  
  # Considering the scheme_management column
  x$scheme_management <- ifelse(x$scheme_management == 'VWC','vwc',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'WUG','wug',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water authority','water_auth',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water Board','water_board',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management != 'vwc' & x$scheme_management != 'wug' & x$scheme_management != 'water_auth' & x$scheme_management != 'water_board' ,'other_schmgt',x$scheme_management)
  
  # Considering the extraction_type column
  x$extraction_type <- ifelse(x$extraction_type != 'gravity','other_exttype',x$extraction_type)
  
  # Considering the extraction_type_class column
  x$extraction_type_class <- ifelse(x$extraction_type_class != 'gravity' & x$extraction_type_class != 'handpump' ,'other_extclass',x$extraction_type_class)
  
  # Considering the management column
  x$management <- ifelse(x$management != 'vwc' & x$management != 'wug' & x$management != 'wua','other_mgt',x$management)
  
  # Considering the management_group column
  x$management_group <- ifelse(x$management_group != 'user-group','other_mgtgroup',x$management_group)
  
  # Considering the payment_type column
  x$payment_type <- ifelse(x$payment_type != 'never pay','other_payemt',x$payment_type)
  
  # Considering the water_quality column
  x$water_quality <- ifelse(x$water_quality != 'soft' ,'other_waterqual',x$water_quality)
  
  # Considering the quantity column
  x$quantity <- ifelse(x$quantity != 'insufficient' & x$quantity != 'enough','other_quantity',x$quantity)
  
  # Considering the source column
  x$source <- ifelse(x$source != 'shallow well' & x$source != 'spring' & x$source != 'machine dbh' & x$source != 'river','other_source',x$source)
  
  # Considering the source_class column and converting unknown to other_sourceClass
  x$source_class <- ifelse(x$source_class == 'unknown','other_sourceClass',x$source_class)
  
  # Considering the waterpoint_type column
  x$waterpoint_type <- ifelse(x$waterpoint_type != 'communal standpipe' & x$waterpoint_type != 'hand pump','other_watertype',x$waterpoint_type)
  return(x) # Returns the dataset
}

labels_rename <- function(x){
  x$status_group <- ifelse(x$status_group == "functional",as.numeric(0),x$status_group)
  x$status_group <- ifelse(x$status_group == "non functional",as.numeric(1),x$status_group)
  x$status_group <- ifelse(x$status_group == "functional needs repair",as.numeric(2),x$status_group)
  return(x)
}

cleaned_train <- clean_dataset(df_train)
cleaned_test <- clean_dataset(df_test)
cleaned_lab <- labels_rename(labels)
View(cleaned_train)
View(cleaned_test)
View(cleaned_lab)

# dmy <- dummyVars(" ~ .", data = customers, fullRank=T)
# trsf <- data.frame(predict(dmy, newdata = customers))

# Performing one hot encoding
# train_1h <- data.frame(predict(dummyVars(" ~ .", data = cleaned_train, fullRank = T), newdata = cleaned_train))
train_dmy <- dummyVars(' ~ .',data = cleaned_train,fullRank = T)
train_1h <- data.frame(predict(train_dmy, newdata = cleaned_train))

test_dmy <- dummyVars(' ~ .',data = cleaned_test,fullRank = T)
test_1h <- data.frame(predict(test_dmy, newdata = cleaned_test))

train_1h <- data.frame(cbind(train_1h,as.factor(cleaned_lab$status_group))) # Considering the status as factor column
names(train_1h)[36] <- 'status_group'

####################
# Machine Learning #
####################
# Performing train test and split the dataset
dt = sort(sample(nrow(train_1h), nrow(train_1h)*.8))
data_train<-train_1h[dt,]
data_test<-train_1h[-dt,]
#################
# Random Forest #
#################

library(randomForest)

# Object random forest model
rfm = randomForest(status_group~.,data = data_train,ntree=100)

# Accuracy checking
Y_pred <- predict(rfm,data_test[,-ncol(data_test)])
Y <- data_test[,ncol(data_test)]

# Building the confusion matrix
confusion_matrix <- table(Y_pred,Y)
confusion_matrix
accuracy_randomForest = sum(diag(confusion_matrix)/sum(confusion_matrix))
paste0('Accuracy Score for random forest using train test split : ',round(accuracy_randomForest*100),'%')

# Hyperparameterizing
accuracy_vec <- array(0,36)
for (i in 1:36){ #print(i)
  model <- randomForest(x=data_train[,-36],
                        y=as.factor(data_train[,36]),
                        xtest=data_test[,-36],
                        ytest=as.factor(data_test[,36]),
                        ntree=i)
  
  accuracy_vec[i] = (model$test$confusion[1,1]+model$test$confusion[2,2])/sum(model$test$confusion)
}
print(accuracy_vec)

#################
# ID3 Algorithm #
#################

library(rpart) # Package that is used to find the DT algorithm
str(train_1h)

# Train test split out dataset
dtree <- rpart(status_group ~ ., data = data_train)

# Predictions probability for DT
dt_prob <- predict(dtree,data_test,type = 'prob') # We get the output as probability where the first one identifies factor 0 ,second one as factor 1 and third one as factor 2

# Predictions for DT
dt_pred <- predict(dtree,data_test)

# Accuracy printing 
pred <- predict(dtree,data_train,type='class')
confusion_matrix_dt <- table(pred,data_train$status_group)
confusion_matrix_dt
accuracy_dt = sum(diag(confusion_matrix_dt)/sum(confusion_matrix_dt))
paste0('Accuracy Score for Decision Tree using train test split : ',accuracy_dt*100,'%')

prunedTree = prune(dtree,cp=0.01)
pred = predict(prunedTree,data_test,type="class")
confusion_matrix_dt_2 <- table(pred, data_test$status_group)
accuracy_dt_2 = sum(diag(confusion_matrix_dt_2)/sum(confusion_matrix_dt_2))
paste0('Accuracy Score of Decision Tree after pruning : ',accuracy_dt_2*100,'%')

# Hyperparameterizing the ID3 algorithm using 10 cross validation
n_trees <- 10 
threshold <- 0.5

spam_idx <- sample(1:nrow(train_1h))
half_split <- floor(nrow(train_1h)/2)
target_variable <- ncol(train_1h)

feature_values <- seq(5,58,5)
accuracy_vec <- numeric()

for (features_per_tree in feature_values)
{
  Y_trees <- numeric()
  
  for(i in 1:n_trees){
    #3.1 Sample the features
    selected_features <- sample(1:(ncol(train_1h)-1),features_per_tree)
    
    #3.2 Take the first half of the dataset as a training data set with bootstrap for each tree
    bootstrap_idx <- sample(1:half_split,replace = T)
    train_data <- train_1h[spam_idx[bootstrap_idx],c(selected_features,target_variable)]
    
    #3.3 Take the second half of the dataset as a hold out or test data set
    test_data <- train_1h[spam_idx[(half_split+1):nrow(train_1h)],c(selected_features,target_variable)]
    
    #3.4 Fit a model on the training set and evaluate it on the test set
    model <- rpart(status_group ~ ., method="class",data=train_data)
    Y_pred <- predict(model,subset(test_data, select=-c(status_group)))
    
    #3.5 Store the prediction of each tree (2 is to take only the P(Y="spam"|x))
    Y_trees <- cbind(Y_trees,Y_pred[,2])
  }
  
  # Calculate the ensemble prediction
  Y_hat <- apply(Y_trees,1,mean)
  Y_hat <- ifelse(Y_hat > threshold,"yes","no") 
  
  # Evaluate the predictions
  Y <- test_data[,"status_group"]
  confusion_matrix <- table(Y_hat,Y)
  confusion_matrix
  
  # From confusion matrix we can calculate the accuracy by summing the daignals 
  # and dividing it by sum of the confusion matrix
  accuracy = (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
  
  # We find the number of incorrect calculations
  misclassification_rate = 1 - accuracy
  accuracy_vec <- cbind(accuracy_vec,accuracy)
  
  # print(paste("[INFO] - Misclassification rate -",features_per_tree,"features :",misclassification_rate))
  print(paste("[INFO] - Accuracy rate -",accuracy_vec,"Misclassification rate :",misclassification_rate))
}

#################
# KNN Algorithm #
#################

library(class)
Ypred_knn=knn(data_train,data_test,data_train$status_group,k=3)
KNN <- table(Ypred_knn,data_test$status_group)
knn_acc <- sum(diag(KNN)/sum(KNN))
paste0('Accuracy Score for KNN using train test split : ',round(knn_acc*100),'%')

dt_pred2 <- as.numeric(as.character(Ypred_knn))
cross_valid_knn <- data.frame(rmse = RMSE(dt_pred2,label1),mae = MAE(dt_pred2,label1))
print(cross_valid_knn)



Data<-train_1h[sample(nrow(train_1h)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(Data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function
  Idx <- which(folds==i,arr.ind=TRUE)
  testData <- Data[Idx, ]
  trainData <- Data[-Idx, ]
  Ypred_knn=knn(trainData,testData,trainData$status_group,k=i)
  KNN <- table(as.numeric(as.character(Ypred_knn)),testData$status_group)
  knn_acc <- sum(diag(KNN)/sum(KNN))
  dj <- sum(diag(dt_matrix)/sum(dt_matrix))
}

##########################
# naive bayes classifier #
##########################
library(e1071)
classifier_cl <- naiveBayes(status_group ~ ., data = data_train)
classifier_cl
y_pred <- predict(classifier_cl, newdata = data_test)

# Confusion Matrix
cm <- table(data_test$status_group, y_pred)
cm
acc_nb <- sum(diag(cm)/sum(cm))
confusionMatrix(cm)
plot(y_pred, data_test$status_group, main="Naive Bayes")

#######################
# Deep Neural Network #
#######################

