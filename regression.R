#Example 1: predicting weight by gender and height

library(ggplot2)
library(reshape2)
library(plyr)
library(car)
ht_weight_df <- read.csv(file.choose(), header=TRUE)
attach(ht_weight_df)
str(ht_weight_df)
plot(x = ht_weight_df$Height, y = ht_weight_df$Weight)
lm_ht_weight <- lm(Weight ~ Height, data = ht_weight_df)
summary(lm_ht_weight)
male_df <- subset(ht_weight_df, Gender == "Male")
female_df <- subset(ht_weight_df, Gender == "Female")
# Get the summary values of height
summary(male_df$Height)
summary(female_df$Height)
ddply(ht_weight_df, .(Gender), function(df) summary(df$Height))
plot(density(male_df$Height))
plot(density(female_df$Height))
dens_by_gender <- ggplot(data = ht_weight_df, aes(x = Height, color = ht_weight_df$Gender)) + 
  geom_density()
dens_by_gender

qq_by_gender <- ggplot(data = ht_weight_df, aes(sample = Height)) + geom_point(stat = "qq") + 
  facet_wrap(~Gender)
qq_by_gender

ht_wt_pt_gender <- ggplot(data = ht_weight_df, aes(x = Height, y = Weight, color = Gender)) + 
  geom_point(alpha = 0.2)
ht_wt_pt_gender

fit <- lm(Weight ~ Height + Gender, data = ht_weight_df)
summary(fit)# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)

lm_ht_wt_by_gender <- lm(Weight ~ Height * Gender, data = ht_weight_df)
summary(lm_ht_wt_by_gender)
newdata <- ht_weight_df[1:2,1:2]
predict(lm_ht_wt_by_gender, newdata, interval = "confidence")


# Assessing Outliers
outlierTest(lm_ht_wt_by_gender) # Bonferonni p-value for most extreme obs
qqPlot(lm_ht_wt_by_gender, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(lm_ht_wt_by_gender) # leverage plots

# Influential Observations
# added variable plots 
avPlots(lm_ht_wt_by_gender)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(ht_weight_df)-length(lm_ht_wt_by_gender$coefficients)-2)) 
plot(lm_ht_wt_by_gender, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(lm_ht_wt_by_gender,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(lm_ht_wt_by_gender, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(lm_ht_wt_by_gender) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lm_ht_wt_by_gender)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(lm_ht_wt_by_gender)

# Evaluate Collinearity
vif(lm_ht_wt_by_gender) # variance inflation factors 
sqrt(vif(lm_ht_wt_by_gender)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(lm_ht_wt_by_gender)


# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(lm_ht_wt_by_gender) 
summary(gvmodel)


#Example 3: predicting mpg for an auto
#data: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/
auto <- na.omit(read.table(file.choose()))
colnames(auto) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year","origin","car_name")
auto$horsepower <- as.numeric(levels(auto$horsepower))[auto$horsepower]
auto <- na.omit(auto)
attach(auto)

#Use this command to plot pairwise scatter plots in RStudio and inspect the result for 
#relationships between the independent variable mpg and the numerical dependent variables.
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + model_year+origin)


auto.fit <- lm(mpg ~ . -car_name,data=auto)

summary(auto.fit)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(auto.fit)

#P-values for coefficients of cylinders, horsepower and acceleration are all greater than 0.05. 
#This means that the relationship between the dependent and these independent variables is not 
#significant at the 95% certainty level. I'll drop 2 of these variables and try again. High 
#p-values for these independent variables do not mean that they definitely should not be used 
#in the model. It could be that some other variables are correlated with these variables and 
#making these variables less useful for prediction

auto.fit1 <- lm(mpg ~ displacement + horsepower + weight , data=auto)
summary(auto.fit1)

#Here we see that both Multiple R-squared and Adjusted R-squared have fallen. 
#When comparing models, use Adjusted R-squared. That's because R-squared will increase or 
#stay the same (never decrease) when more independent variables are added. The formula for 
#Adjusted R-squared includes an adjustment to reduce R-squared. If the additional variable 
#adds enough predictive information to the model to counter the negative adjustment then 
#Adjusted R-squared will increase. If the amount of predictive information added is not 
#valuable enough, Adjusted R-squared will reduce.

auto.fit2 <- lm(mpg ~ acceleration + horsepower + weight, data = auto)
summary(auto.fit2)

#Trying more combinations as acceleration has very high p-value.

auto.fit3 <- lm(mpg ~ model_year + horsepower + weight, data = auto)
summary(auto.fit3)


auto.fit4 <- lm(mpg ~ model_year + horsepower + weight + origin, data = auto)
summary(auto.fit4)


auto.fit5 <- lm(mpg ~ model_year + acceleration + weight + origin, data = auto)
summary(auto.fit5)

auto.fit6 <- lm(mpg ~ model_year + weight + origin, data = auto)
summary(auto.fit6)
#Now we're getting somewhere as all the coefficients have a small p-value.

auto.fit7 <- lm(mpg ~ I(displacement^2) + model_year + weight + origin, data = auto)
summary(auto.fit7)
#Let's try some non-linear combinations with different exponents for horsepower.
auto.fit8 <- lm(mpg ~ I(horsepower^1) + I(horsepower^2) + I(horsepower^3) +
                  model_year + weight + origin, data = auto)
summary(auto.fit8)

#The Adjusted R-squared is the highest so far. Another thing to note is that even though 
#the p-value for horsepower^3 is very small (relationship is significant), the coefficient 
#is tiny. So we should consider removing it unless horsepower^3 has an intuitive or business 
#meaning to us in the given context.

#While creating models we should always bring business understanding into consideration. 
#If the context dictates that that particular variable is important to explaining the outcome,
#we will retain it in the model even if the coefficient is very small.

#If the effect is small and we are not able to explain why the independent variable should
#affect the dependent variable in a particular way, we may be risking overfitting to our 
#particular sample of data. Such a model may not generalize.

auto.fit9 <- lm(mpg ~ horsepower + model_year + weight + origin, data = auto)

summary(auto.fit9)

#Adjusted R-squared reduced. We can do better.

auto.fit10 <- lm(mpg ~ model_year + weight + origin + poly(horsepower,2) , data=auto)
summary(auto.fit10)

#There we go. This is a good enough model. Adjusted R-squared is the second highest. 
#None of the coefficients seem miniscule. The coefficient for weight is quite small 
#compared to the others but the weight values are in thousands. So, the effect in reality 
#is quite significant. Also, intuition dictates that mpg should be dependent on the weight 
#of the vehicle. So even if the effect were small, we would have kept it.

#mpg = - 21.77 + 0.7456e*model_year - 0.004393*weight + 1.046*origin - 26.15*horsepower^1 + 29.98*horsepower^2

#We were able to explain 85% of the variance in miles per gallon using the regression model.
#Intuition agrees with this model as weight and horsepower would definitely affect mpg. 
#The relationship is non-linear between mpg and horsepower.
#A surprising finding was that model_year and origin had significant effects also.

#To calculate 95% prediction interval of mpg for a given set of values for the independent variables, use the code below.

newdata <- data.frame(model_year=70, weight=3691, origin=1, horsepower = 200)
predict(auto.fit10, newdata, interval = "confidence")
