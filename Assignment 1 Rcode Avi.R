#import packages;
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(corrgram)
library(GGally)
library(ggthemes) 
library(caret)

data<-read.csv('C:/Users/avije/Desktop/York University/York University/ML1000/Assignment 1 Data/processed.cleveland.data.csv',header = TRUE, na.strings = c("NA","","#NA","?"))
data2<-read.csv('C:/Users/avije/Desktop/York University/York University/ML1000/Assignment 1 Data/processed.hungarian.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
data3<-read.csv('C:/Users/avije/Desktop/York University/York University/ML1000/Assignment 1 Data/processed.switzerland.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
data4<-read.csv('C:/Users/avije/Desktop/York University/York University/ML1000/Assignment 1 Data/processed.va.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
#Add recorid
full_data <- rbind(data,data2,data3,data4)

#check # of rows and columns
ncol(full_data);
nrow(full_data);

#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(full_data,10)
str(full_data)
summary(full_data)

#transform SEX variable to only M and F(currently in int form)
full_data$SEX<-ifelse(full_data$SEX==1,"M","F")

#transform TRAGET variabale to only 0 - 1 (Hungarian had 0 to 4)
full_data$TARGET<-ifelse(full_data$TARGET>=1,1,full_data$TARGET)
table(full_data$TARGET)
head(full_data,10)

#check distribution of target variable
hist(full_data$TARGET);
summary(full_data$TARGET);

#Analyze data types
str(full_data)
#Factor categorical varibales
factor_VARS <- c('SEX','CP','FBS','RESTECG','EXANG','SLOPE','CA','THAL')
full_data[factor_VARS]<- lapply(full_data[factor_VARS],function(x) as.factor(x))

#Analyze missing and erroneous data elements
summary(full_data)
attach(full_data)

#Observations:
#1) THAL, CA and SLOPE have many missing values. Should consider removing these columns
full_data_truncated<-as.data.frame(full_data[, !names(full_data) %in% c("THAL","CA","SLOPE")])
summary(full_data_truncated)

#2) CHOL and TRESTBPS have observations of 0 which might be erroneous. We should replace these values with NA
full_data_truncated$CHOL[full_data_truncated$CHOL == 0] <- NA
summary(full_data_truncated$CHOL)

full_data_truncated$TRESTBPS[full_data_truncated$TRESTBPS == 0] <- NA
summary(full_data_truncated$TRESTBPS)

######################    DATA EXPLORATION: CATEGORICAL     ###########################

#1. Frequency table
attach(full_data_truncated)
freq_tbl_sex=table(SEX)
head(freq_tbl_sex)
head(full_data,10)

#1B. Absolute #s are hard to work with. Let's move to proportion. Input freq table to get prop.
prop.table(freq_tbl_sex)

#we look at SEX w.r.t. our Target Variable...
#2. cross-tab for 2 categorical features - look at it w.r.t our target variable
freq_xtab_sex=xtabs(~SEX+TARGET)
head(freq_xtab_sex)
#2B. cross-tab for 2 categorical features, this time with proportions
prop.table(freq_xtab_sex)

#now we look at C.P w.r.t. our Target Variable as there seems to be a relation between the type of chest pain and target.
attach(full_data_truncated)
freq_tbl_CP=table(CP)
head(freq_tbl_CP)
prop.table(freq_tbl_CP)
freq_xtab_CP=xtabs(~CP+TARGET)
head(freq_xtab_CP)
prop.table(freq_xtab_CP)

#FBS w.r.t target variable 
attach(full_data_truncated)
freq_tbl_FBS=table(FBS)
head(freq_tbl_FBS)
prop.table(freq_tbl_FBS)
freq_xtab_FBS=xtabs(~FBS+TARGET)
head(freq_xtab_FBS)
prop.table(freq_xtab_FBS)

#RestECG w.r.t target variable
attach(full_data_truncated)
freq_tbl_RESTECG=table(RESTECG)
head(freq_tbl_RESTECG)
prop.table(freq_tbl_RESTECG)
freq_xtab_RESTECG=xtabs(~RESTECG+TARGET)
head(freq_xtab_RESTECG)
prop.table(freq_xtab_RESTECG)

#RestECG w.r.t target variable
attach(full_data_truncated)
freq_tbl_EXANG=table(EXANG)
head(freq_tbl_EXANG)
prop.table(freq_tbl_EXANG)
freq_xtab_EXANG=xtabs(~EXANG+TARGET)
head(freq_xtab_EXANG)
prop.table(freq_xtab_EXANG)


####################   VISUALIZATIONS   ###############################
#1A. barplot with absolute counts
barplot(freq_tbl_sex)
barplot(freq_tbl_CP)
barplot(freq_tbl_FBS)
barplot(freq_tbl_RESTECG)
barplot(freq_tbl_EXANG)

#1B. barplot with proportions - easier to read

barplot(freq_xtab_sex, legend=rownames(freq_xtab_sex), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4"), beside=T, las=1)
barplot(freq_xtab_CP, legend=rownames(freq_xtab_CP), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4","navajowhite4","lightsteelblue4"), beside=T, las=1)
barplot(freq_xtab_FBS, legend=rownames(freq_xtab_FBS), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4"), beside=T, las=1)
barplot(freq_xtab_RESTECG, legend=rownames(freq_xtab_RESTECG), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4","navajowhite4"), beside=T, las=1)
barplot(freq_xtab_EXANG, legend=rownames(freq_xtab_EXANG), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4"), beside=T, las=1)

#by doing some research online it is seen that cholestrol has ranges ()
full_data_truncated$CHOLRANGE[full_data_truncated$CHOL < 200] <- 'Desirable'
full_data_truncated$CHOLRANGE[full_data_truncated$CHOL >=200 & full_data_truncated$CHOL <= 239] <- 'Borderline High'
full_data_truncated$CHOLRANGE[full_data_truncated$CHOL >=240] <- 'High'

#convert cholrange to factor in order to plot w.r.t TARGET
full_data_truncated$CHOLRANGE<-factor(full_data_truncated$CHOLRANGE)
attach(full_data_truncated)
freq_tbl_cholrange=table(CHOLRANGE)
head(freq_tbl_cholrange)
prop.table(freq_tbl_cholrange)
freq_xtab_cholrange=xtabs(~CHOLRANGE+TARGET)
head(freq_xtab_cholrange)
prop.table(freq_xtab_cholrange)
barplot(freq_tbl_cholrange)
barplot(freq_xtab_cholrange, legend=rownames(freq_xtab_cholrange), ylab="Number of People", xlab="Target", col=c("mistyrose2","mistyrose4","navajowhite4"), beside=T, las=1)


ggplot(full_data_truncated, aes(TRESTBPS)) + geom_histogram(binwidth = 2)+
  scale_x_continuous("TRESTBPS", breaks = seq(0,270,by = 30))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 20))+
  labs(title = "Histogram")

ggplot(full_data_truncated, aes(CHOL)) + geom_histogram(binwidth = 2)+
  scale_x_continuous("CHOL", breaks = seq(0,270,by = 30))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 20))+
  labs(title = "Histogram")

corrgram(full_data_truncated, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram")

ggpairs(full_data_truncated[, names(full_data_truncated)%in% factor_VARS])
ggpairs(full_data_truncated[, !names(full_data_truncated)%in% factor_VARS])


# 
# 
# #Step 1: Get only numeric colums
# 
# #Step 2: Melt data --> Turn it into skinny LONG table
# melt_data = melt(full_data, id.vars=c("ID"))
# head(melt_data, 10)
# ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
# #Replace missing values, and replace CHOL = 0 with mean and median
# #see pattern of missing data
# #md.pattern(full_data)
# aggr_plot = aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(full_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# 
# library(Hmisc)
# #Check distritbution of CHOL, find high percent with 0 Chol, after research this isnt possible, we chose to impute these, find mean 
# hist(CHOL)
# 
# hist(CHOL, freq=F, main='CHOL: Original Data', 
#      col='darkgreen')
# hist(CHOL, freq=F, main='Age: MICE Output', 
#      col='lightgreen', ylim=c(0,0.04))
# 
# #mIGHT HAVE TO IMPUTE 0 FOR OLDPEAK
# full_data$CHOL[full_data$CHOL == 0] <- NA
# hist(full_data$CHOL)
# summary(full_data$CHOL)
# 
# #Split data into 
# full_data_0 <- full_data[full_data$TARGET==0,]
# full_data_1 <- full_data[full_data$TARGET==1,]
# summary(full_data_0)
# summary(full_data_1)
# 
# imp_CHOL_mean=Hmisc::impute(full_data$CHOL, mean)# replace with mean
# 
# imp_CHOL_mode=Hmisc::impute(full_data$CHOL, median)  # median
# 
# ifelse(full_data$TARGET==0,impute(Ful))
# chol_data <- cbind.data.frame(full_data$ID,full_data$CHOL,imp_CHOL_mean,imp_CHOL_mode)

# colnames(chol_data) <- c("ID","CHOL","CHOL_MEAN","CHOL_MED")
# chol_melt <- melt(chol_data,id.vars=c("ID"))
# head(chol_melt,10)
# ggplot(chol_melt, aes(value, fill = variable)) + geom_histogram(position = "dodge",bins =20)



#hist(table(CA))

#KNN
knn_input=as.data.frame(full_data_truncated[, !names(full_data_truncated) %in% c("TARGET")])
str(knn_input)
sample_knn_input=knn_input
nrow(sample_knn_input)


#R's built in Z-score normalization
#sample_knn_input_zscore = as.data.frame( scale(sample_knn_input ))
#str(sample_knn_input_zscore)
library(DMwR)
#Run KNN imputation for this sample - warning might take a while....
knnOutput = knnImputation(sample_knn_input, k=5,scale=T)

summary(knnOutput)
summary(full_data_truncated)

knnOutput$TARGET <- full_data$TARGET

ggpairs(full_data_truncated[, names(full_data_truncated) %in% factor_VARS])
ggpairs(knnOutput[, names(knnOutput) %in% factor_VARS])
ggpairs(full_data_truncated[, !names(full_data_truncated) %in% factor_VARS])
ggpairs(knnOutput[, !names(knnOutput) %in% factor_VARS])
corrgram(full_data_truncated, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram Before KNN")
corrgram(knnOutput, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram After KNN")

##Model Building
#library(dismo) 
library(randomForest)
train <- knnOutput[1:828,]
test <- knnOutput[829:920,]

# folds <- kfold(knnOutput, k=5, by=knnOutput$TARGET)
# train <- knnOutput[folds[folds<=4],]
# test <- knnOutput[folds[folds>4],]
# str(train)
# summary(train)
# str(test)
# summary(test)
set.seed(123456)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(train$TARGET)~.,data = train)

# Show model error
plot(rf_model)
#legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(test$TARGET,prediction)

# Write the solution to file
write.csv(solution, file = 'C:/Users/benny/OneDrive/Machine Learning/ML1000/Assignments/Assignment 1/rf_mod_Solution.csv', row.names = F)
confusionMatrix(prediction,factor(test$TARGET))


# #Step 3: This data structure is now suitable for a multiplot function
# ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
# 
# 
# attach(full_data)
# 
# freq_tbl=table(THAL)
# head(freq_tbl)
# 
# prop.table(freq_xtab)
# hist(full_data$THAL)
# 
# #aDD TARGET BACK TO imputed DATASET
# 
# knnOutput$TARGET <- full_data$TARGET
# 
# 
# 
# ggpairs(full_data[,!names(full_data) %in% c("ID","SEX","TARGET","CP","THAL","FBS","EXANG","SLOPE","CA","RESTECG")])
# ggpairs(full_data[,!names(knnOutput) %in% c("ID","SEX","TARGET","CP","THAL","FBS","EXANG","SLOPE","CA","RESTECG")])
# summary(full_data$AGE)
# 
# 
# library(gridExtra)
# #Frequency of Factor variables
# 
# # counts
# plot1 <- ggplot(data.frame(full_data$THAL), aes(x=full_data$THAL)) +
#   geom_bar()
# 
# 
# plot2 <- ggplot(data.frame(knnOutput$THAL), aes(x=knnOutput$THAL)) +
#   geom_bar()
# 
# 
# plot3 <- ggplot(data.frame(full_data$CA), aes(x=full_data$CA)) +
#   geom_bar()
# 
# 
# plot4 <- ggplot(data.frame(knnOutput$CA), aes(x=knnOutput$CA)) +
#   geom_bar()
# 
# plot5 <- ggplot(data.frame(full_data$SLOPE), aes(x=full_data$SLOPE)) +
#   geom_bar()
# 
# 
# plot6 <- ggplot(data.frame(knnOutput$SLOPE), aes(x=knnOutput$SLOPE)) +
#   geom_bar()
# grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6 ,nrow = 3,ncol=2)
# 
# 
# #Correalation Plot
# numeric_cols = sapply(knnOutput, is.numeric)
# data_num_only=cbind(knnOutput[,numeric_cols],full_data$TARGET)
# 
# cor(data_num_only)
# 
# ############################ BONUS: ADVANCED CORRELATOIN MATRIX ##################################
# #correlation matrix with statistical significance
# 
# cor_result=rcorr(as.matrix(data_num_subset))
# 
# cor_result$r
# 
# #Link: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# #The output of the function rcorr() is a list containing the following elements :
# #r : the correlation matrix, n : the matrix of the number of observations used, p: p-values of significance of correlations
# 
# # Extract the correlation coefficients
# cor_result$r
# 
# ############################ BONUS: INTRO TO FUNCTIONS ##################################
# # ++++++++++++++++++++++++++++
# # flattenCorrMatrix - makes it easier to read (in my opinion)
# # ++++++++++++++++++++++++++++
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# 
# #Simple method to flatten (if that how you want to look at it)
# cor_result_flat = flattenCorrMatrix(cor_result$r, cor_result$P)
# head(cor_result_flat)
# 
# 
# 
# #Normallize
# 
# numeric_cols <- sapply(knnOutput, is.numeric)
# data_num_only<-knnOutput[,numeric_cols]
# 
# normalize = function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# data_num_only_normal = as.data.frame(lapply(data_num_only[,!names(data_num_only) %in% c('TARGET')], normalize))
# summary(data_num_only_normal)
# 
# fData <- cbind(knnOutput[,!names(knnOutput) %in% names(data_num_only_normal)],data_num_only_normal)
# summary(fData)
# 
# ##Model Building
# require(dismo)
# require(randomForest)
# 
# folds <- kfold(fData, k=3, by=fData$TARGET)
# train <- fData[folds[folds<=7],]
# test <- fData[folds[folds>7],]
# set.seed(754)
# 
# # Build the model (note: not all possible variables are used)
# rf_model <- randomForest(factor(train$TARGET)~.,data = train)
# 
# 
# 
# 
# # Show model error
# plot(rf_model, ylim=c(0,0.36))
# legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# 
# 
# 
# importance    <- importance(rf_model)
# varImportance <- data.frame(Variables = row.names(importance), 
#                             Importance = round(importance[ ,'MeanDecreaseGini'],2))
# 
# # Create a rank variable based on importance
# rankImportance <- varImportance %>%
#   mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# 
# # Use ggplot2 to visualize the relative importance of variables
# ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
#                            y = Importance, fill = Importance)) +
#   geom_bar(stat='identity') + 
#   geom_text(aes(x = Variables, y = 0.5, label = Rank),
#             hjust=0, vjust=0.55, size = 4, colour = 'red') +
#   labs(x = 'Variables') +
#   coord_flip() + 
#   theme_few()
# 
# 
# 
# 
# library(e1071)
# library(rpart)
# 
#  ## split data into a train and test set
# index <- 1:nrow(fData)
# testindex <- sample(index, trunc(length(index)/3))
# testset <- fData[testindex,]
# trainset <- fData[-testindex,]
#  ## svm
# svm.model <- svm(TARGET ~ ., data = trainset, cost = 100, gamma = 1)
# svm.pred <- predict(svm.model, testset[,])
# table(testset$TARGET,ifelse(svm.pred>0.5,1,0))
# 
# 
# #cross validation with SVM

# #see all models able to run 

names(getModelInfo())



# define training control
train_control <- trainControl(method="repeatedcv", number=10)
# train the model
model <- train(factor(TARGET)~., data=knnOutput, trControl=train_control, method="svmLinear")
# summarize results
print(model)
model.pred <- predict(model, knnOutput)
model.pred
confusionMatrix(model.pred,factor(knnOutput$TARGET))
# 
# 
