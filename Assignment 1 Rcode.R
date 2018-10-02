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

getwd()

data=read.csv('YorkU/Assignment 1/processed.cleveland.data.csv',header = TRUE, na.strings = c("NA","","#NA","?"))





data2=read.csv('YorkU/Assignment 1/processed.hungarian.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
data3=read.csv('YorkU/Assignment 1/processed.switzerland.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
data4=read.csv('YorkU/Assignment 1/processed.va.data.csv', header = TRUE, na.strings = c("NA","","#NA","?"))
#Add recorid
full_data <- rbind(data,data2,data3,data4)

#transform target forthis data 
test <- sapply(full_data$TARGET, function(x) ifelse(x >= 1, 1, 0))
new = cbind(full_data$TARGET,test)

#transform TRAGET variabale to only 0 - 1 (Hungarian had 0 to 4)
full_data$TARGET<-ifelse(full_data$TARGET>=1,1,full_data$TARGET)
table(full_data$TARGET)
#Add recordID
#full_data$ID <- seq.int(nrow(full_data))

#attach(full_data)


#Analyze missing values
str(full_data)


#Factor categorical varibales
factor_VARS <- c('SEX','CP','FBS','RESTECG','EXANG','SLOPE','CA','THAL')

full_data[factor_VARS]<- lapply(full_data[factor_VARS],function(x) as.factor(x))
summary(full_data)
str(full_data)

hist(full_data$THALACH);


#pMiss = function(x){sum(is.na(x))/length(x)*100}
#apply(full_data,2,pMiss)

attach(full_data)


#Step 1: Get only numeric colums

#Step 2: Melt data --> Turn it into skinny LONG table
melt_data = melt(full_data, id.vars=c("ID"))
head(melt_data, 10)
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
#Replace missing values, and replace CHOL = 0 with mean and median
#see pattern of missing data
#md.pattern(full_data)
aggr_plot = aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(full_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(Hmisc)
#Check distritbution of CHOL, find high percent with 0 Chol, after research this isnt possible, we chose to impute these, find mean 
hist(CHOL)

hist(CHOL, freq=F, main='CHOL: Original Data', 
     col='darkgreen')
hist(CHOL, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

#mIGHT HAVE TO IMPUTE 0 FOR OLDPEAK
full_data$CHOL[full_data$CHOL == 0] <- NA
hist(full_data$CHOL)
summary(full_data$CHOL)

#Split data into 
full_data_0 <- full_data[full_data$TARGET==0,]
full_data_1 <- full_data[full_data$TARGET==1,]
summary(full_data_0)
summary(full_data_1)

imp_CHOL_mean=Hmisc::impute(full_data$CHOL, mean)# replace with mean

imp_CHOL_mode=Hmisc::impute(full_data$CHOL, median)  # median

ifelse(full_data$TARGET==0,impute(Ful))
chol_data <- cbind.data.frame(full_data$ID,full_data$CHOL,imp_CHOL_mean,imp_CHOL_mode)

colnames(chol_data) <- c("ID","CHOL","CHOL_MEAN","CHOL_MED")
chol_melt <- melt(chol_data,id.vars=c("ID"))
head(chol_melt,10)
ggplot(chol_melt, aes(value, fill = variable)) + geom_histogram(position = "dodge",bins =20)



hist(table(CA))

#KNN
knn_input=as.data.frame(full_data[, !names(full_data) %in% c("ID","TARGET")])
str(knn_input)
sample_knn_input=knn_input
nrow(sample_knn_input)


#R's built in Z-score normalization
#sample_knn_input_zscore = as.data.frame( scale(sample_knn_input ))
#str(sample_knn_input_zscore)
library(DMwR)
#Run KNN imputation for this sample - warning might take a while....
knnOutput = knnImputation(sample_knn_input, k=10,scale=T)

summary(knnOutput)
summary(full_data)
hist(table(knnOutput$CA))




#Step 3: This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')


attach(full_data)

freq_tbl=table(THAL)
head(freq_tbl)

prop.table(freq_xtab)
hist(full_data$THAL)

#aDD TARGET BACK TO imputed DATASET

knnOutput$TARGET <- full_data$TARGET

library(GGally)

ggpairs(full_data[,!names(full_data) %in% c("ID","SEX","TARGET","CP","THAL","FBS","EXANG","SLOPE","CA","RESTECG")])
ggpairs(full_data[,!names(knnOutput) %in% c("ID","SEX","TARGET","CP","THAL","FBS","EXANG","SLOPE","CA","RESTECG")])
summary(full_data$AGE)


library(gridExtra)
#Frequency of Factor variables

# counts
plot1 <- ggplot(data.frame(full_data$THAL), aes(x=full_data$THAL)) +
  geom_bar()


plot2 <- ggplot(data.frame(knnOutput$THAL), aes(x=knnOutput$THAL)) +
  geom_bar()


plot3 <- ggplot(data.frame(full_data$CA), aes(x=full_data$CA)) +
  geom_bar()


plot4 <- ggplot(data.frame(knnOutput$CA), aes(x=knnOutput$CA)) +
  geom_bar()

plot5 <- ggplot(data.frame(full_data$SLOPE), aes(x=full_data$SLOPE)) +
  geom_bar()


plot6 <- ggplot(data.frame(knnOutput$SLOPE), aes(x=knnOutput$SLOPE)) +
  geom_bar()
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6 ,nrow = 3,ncol=2)


#Correalation Plot
numeric_cols = sapply(knnOutput, is.numeric)
data_num_only=cbind(knnOutput[,numeric_cols],full_data$TARGET)

cor(data_num_only)

############################ BONUS: ADVANCED CORRELATOIN MATRIX ##################################
#correlation matrix with statistical significance

cor_result=rcorr(as.matrix(data_num_subset))

cor_result$r

#Link: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#The output of the function rcorr() is a list containing the following elements :
#r : the correlation matrix, n : the matrix of the number of observations used, p: p-values of significance of correlations

# Extract the correlation coefficients
cor_result$r

############################ BONUS: INTRO TO FUNCTIONS ##################################
# ++++++++++++++++++++++++++++
# flattenCorrMatrix - makes it easier to read (in my opinion)
# ++++++++++++++++++++++++++++
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Simple method to flatten (if that how you want to look at it)
cor_result_flat = flattenCorrMatrix(cor_result$r, cor_result$P)
head(cor_result_flat)



#Normallize

numeric_cols <- sapply(knnOutput, is.numeric)
data_num_only<-knnOutput[,numeric_cols]

normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_num_only_normal = as.data.frame(lapply(data_num_only[,!names(data_num_only) %in% c('TARGET')], normalize))
summary(data_num_only_normal)

fData <- cbind(knnOutput[,!names(knnOutput) %in% names(data_num_only_normal)],data_num_only_normal)
summary(fData)

##Model Building
require(dismo)
require(randomForest)

folds <- kfold(fData, k=3, by=fData$TARGET)
train <- fData[folds[folds<=7],]
test <- fData[folds[folds>7],]
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(train$TARGET)~.,data = train)




# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)



importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
```




library(e1071)
library(rpart)

 ## split data into a train and test set
index <- 1:nrow(fData)
testindex <- sample(index, trunc(length(index)/3))
testset <- fData[testindex,]
trainset <- fData[-testindex,]
 ## svm
svm.model <- svm(TARGET ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,])
table(testset$TARGET,ifelse(svm.pred>0.5,1,0))






