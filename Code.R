attach(chocolate)
View(chocolate)

install.packages("ggplot2")
library(ggplot2)


## EXPLORATORY ANALYSIS

# VISUALIZING RELATIONSHIPS BETWEEN VARIABLES

# Simple Plots
ggplot(chocolate, aes(x=Cocoa_Percent, y=Rating))+geom_point(color="darkgreen")+labs(x= "Cocoa Percent", y="Rating", title="Rating & Cocoa Percent")+theme(plot.title = element_text(hjust = 0.5))

# Boxplots
# par(mfrow = c(2, 2))
ggplot(chocolate, aes(y=Cocoa_Percent))+geom_boxplot(fill='orange')+labs(y= "Cocoa Percent", title="Cocoa Percent Boxplot")+theme(plot.title = element_text(hjust = 0.5))
ggplot(chocolate, aes(y=Rating))+geom_boxplot(fill='lightblue')+labs(y= "Rating", title="Rating Boxplot")+theme(plot.title = element_text(hjust = 0.5))
ggplot(chocolate, aes(y=Review_Date))+geom_boxplot(fill='lightpink')+labs(y= "Year", title="Review Date Boxplot")+theme(plot.title = element_text(hjust = 0.5))

# Histograms

ggplot(chocolate, aes(x=Cocoa_Percent))+geom_histogram(bins=50, fill='orange')+labs(y= "Cocoa Percent", title="Cocoa Percent Histogram")+theme(plot.title = element_text(hjust = 0.5))
ggplot(chocolate, aes(x=Rating))+geom_histogram(bins=50, fill='lightblue')+labs(y= "Rating", title="Rating Histogram")+theme(plot.title = element_text(hjust = 0.5))
ggplot(chocolate, aes(x=Review_Date))+geom_histogram(bins=50, fill='lightpink')+labs(y= "Year", title="Review Date Histogram")+theme(plot.title = element_text(hjust = 0.5))
mean(Cocoa_Percent)
quantile(Cocoa_Percent)
mean(Rating)
quantile(Rating)
quantile(Review_Date)



# Examine the correlation coefficient between Y and each predictor, xi;

cor(Cocoa_Percent,Rating) # -0.146428
cor(Review_Date,Rating) # 0.07540066
cor(Cocoa_Percent,Review_Date) # 0.04047251

# In this case, our target variable is categorical, making linear regressions unsuited even when factorizign
# However, to still understand the data better, let's use a numerical target variable: Rating
# Run simple linear regressions between numerical Y and each predictor xi.

reg1=lm(Rating~Company)
summary(reg1)

# P-value: < 0.00000000000000022
# R-squared: 0.4392

reg2=lm(Rating~Specific_Bean_Origin)
summary(reg2)

# P-value: 0.000002068
# R-squared: 0.6286

reg3=lm(Rating~REF)
summary(reg3)

# P-value: 0.000885
# R-squared: 0.006412

reg4=lm(Rating~Review_Date)
summary(reg4)

# P-value: 0.001747
# R-squared: 0.005685

reg5=lm(Rating~Cocoa_Percent)
summary(reg5)

# P-value: 0.000000000001041
# R-squared: 0.02144

reg6=lm(Rating~Company_Location)
summary(reg6)

# P-value: 1.533e-06
# R-squared: 0.07085

reg7=lm(Rating~Bean_Type)
summary(reg7)

# P-value: 2.73e-05
# R-squared: 0.05055

reg8=lm(Rating~Broad_Bean_Origin)
summary(reg8)

# P-value: 0.01523
# R-squared: 0.07465




# CHECKING FOR HETEROSKEDASTICITY

# Flag potential heteroskedastic predictors.
# Run residual plots

install.packages("car")
library(car)

residualPlot(reg1,quadratic=FALSE) # FUNNEL
residualPlot(reg2,quadratic=FALSE) # FUNNEL
residualPlot(reg3,quadratic=FALSE) 
residualPlot(reg4,quadratic=FALSE) # FUNNEL
residualPlot(reg5,quadratic=FALSE) 
residualPlot(reg6,quadratic=FALSE) # FUNNEL
residualPlot(reg7,quadratic=FALSE) # FUNNEL
residualPlot(reg8,quadratic=FALSE) 

# Step 2: If residuals have funnel shape, there is heteroskedasticity
# Numerical approach where p-value < 0.05 has heteroskedasticity

ncvTest(reg1) # p-value: 0.14842
ncvTest(reg2) # p-value: 6.1674e-13 -> heteroskedasticity
ncvTest(reg3) # p-value: 2.593e-15 -> heteroskedasticity
ncvTest(reg4) # p-value: 2.4175e-16 -> heteroskedasticity
ncvTest(reg5) # p-value: 0.029936 -> heteroskedasticity
ncvTest(reg6) # p-value: 0.059112
ncvTest(reg7) # p-value: 4.896e-07 -> heteroskedasticity
ncvTest(reg8) # p-value: 0.01411 -> heteroskedasticity



# CHECKING FOR MULTI-COLLINEARITY

# Examine correlations between all predictors
# Use a correlation matrix and look at the correlation coefficients. Take note of possible collinearity.

install.packages("psych")
require(psych)

quantvars=chocolate[,c(3,4,5,7)]  # select only quant variables
corr_matrix=cor(quantvars)
round(corr_matrix,2)
#                REF Review_Date Cocoa_Percent Rating
# REF           1.00        0.99          0.04   0.08
# Review_Date   0.99        1.00          0.04   0.08
# Cocoa_Percent 0.04        0.04          1.00  -0.15
# Rating        0.08        0.08         -0.15   1.00

# Collinearity is problematic when absolute value is higher than around 0.8 
# There's collinearity between Review Date and REF
# Let's only use Review_Date since it's easier to interpret

checkcat=aov(Rating~Company+Company_Location, data=chocolate)
summary(checkcat)



# CHECKING FOR NON-LINEARITY
# Even though we won't be doing regression, we'll get better insight into the data 
# if we know whether the predictors are linear or not

# When the residual has no pattern, it indicates linearity

reglin=lm(Rating~Review_Date+Cocoa_Percent)
residualPlots(reglin)

#                 Test stat Pr(>|Test stat|)  
# Review_Date      0.9514           0.3415    
# Cocoa_Percent   -8.0712        1.296e-15 ***
# Tukey test      -8.7197        < 2.2e-16 ***

# Cocoa percent is clear non-linear, while review date is more linear




## MODELLING

# RANDOM FOREST 

install.packages("randomForest")
library(randomForest)

# Let's start with a simple numerical predictor as the target variable
myforest=randomForest(Rating~Broad_Bean_Origin+Cocoa_Percent+Company_Location+Review_Date+Bean_Type, ntree=500, data=chocolate, importance=TRUE, na.action=na.omit)
myforest
# Mean of squared residuals: 0.1883389
# % Var explained: 17.55

# Now let's predict the rating for a chocolate bar with the following details
predict(myforest,data.frame(Broad_Bean_Origin="Peru",Cocoa_Percent=0.4,Company_Location="U.S.A.", Review_Date=2016, Bean_Type="Criollo"))
# Rating is predicted to be 3.219646

importance(myforest)
#                    %IncMSE IncNodePurity
# Broad_Bean_Origin 23.86563      39.90567
# Cocoa_Percent     38.10881      56.08207
# Company_Location  31.89318      35.95413
# Review_Date       30.51864      34.91308
# Bean_Type         31.85642      26.39312

# Cocoa Percent is most important bc if removed it increases RSS by 38%
# But what if we want to predict where is the best place to source beans? 
# It doesn't seem like the bean origin has a huge impact on ratings
# But does ratings influence where companies choose to source their beans?

# Plot the importance
varImpPlot(myforest)


# CROSS VALIDATION FOR NUMERICAL (OUT-OF-BAG)

myforest=randomForest(Rating~Broad_Bean_Origin+Cocoa_Percent+Company_Location+Review_Date+Bean_Type, ntree=500, data=chocolate, importance=TRUE, na.action=na.omit, do.trace=50)
# Do trace shows how worth it is to add more trees
# |      Out-of-bag   |
# Tree |      MSE  %Var(y) |
#   50 |   0.1951    85.54 |
#  100 |   0.1923    84.32 |
#  150 |   0.1908    83.69 |
#  200 |   0.1903    83.47 |
#  250 |   0.1899    83.27 |
#  300 |   0.1895    83.10 |
#  350 |   0.1894    83.04 |
#  400 |   0.1893    83.01 |
#  450 |   0.1893    83.00 |
#  500 |   0.1891    82.92 |





# CLASSIFICATION TREES WITH RANDOM FOREST

set.seed(0)

table(Broad_Bean_Origin)
# The places with the most instances are:
# Venezuela: 214 
# Ecuador: 193
# Peru: 165
# Dominican Republic: 141+25=166 (There is a typo Domincan Republic)

chocolate=chocolate[!is.na(chocolate$Broad_Bean_Origin), ]
chocolate=droplevels(chocolate)

# Classification with Random Forest
classifiedforest=randomForest(as.factor(Broad_Bean_Origin)~Cocoa_Percent+Rating+Review_Date, data=chocolate, cp=0.01, na.action=na.omit)
classifiedforest
# No. of variables tried at each split: 1
# OOB estimate of  error rate: 85.28%
importance(classifiedforest)
#               MeanDecreaseGini
# Cocoa_Percent         137.8608
# Rating                104.6276
# Review_Date           114.2134


classifiedforest2=randomForest(as.factor(Company)~Broad_Bean_Origin+Cocoa_Percent+Rating+Review_Date+Bean_Type, data=chocolate, cp=0.01, na.action=na.omit)
classifiedforest2
# No. of variables tried at each split: 2
# OOB estimate of error rate: 83.44%
importance(classifiedforest2)
#                   MeanDecreaseGini
# Broad_Bean_Origin         369.1263
# Cocoa_Percent             263.9273
# Rating                    263.0677
# Review_Date               246.4423
# Bean_Type                 187.1633


classifiedforest3=randomForest(as.factor(Broad_Bean_Origin)~Cocoa_Percent+Rating+Review_Date+Company_Location, data=chocolate, cp=0.01, na.action=na.omit)
classifiedforest3
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 78.73%
importance(classifiedforest3)
#                  MeanDecreaseGini
# Cocoa_Percent            286.3734
# Rating                   235.5006
# Review_Date              198.8179
# Company_Location         312.8101


# Adding Company
classifiedforest4=randomForest(as.factor(Broad_Bean_Origin)~Cocoa_Percent+Rating+Review_Date+Company+Company_Location, data=chocolate, cp=0.01, na.action=na.omit)
classifiedforest4
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 76.58%
importance(classifiedforest4)
#                  MeanDecreaseGini
# Cocoa_Percent            245.2787
# Rating                   236.2145
# Review_Date              193.6285
# Company                  367.3562
# Company_Location         242.8198
varImpPlot(classifiedforest4, main="Model Variable Importance")


# Using Specific 
classifiedforest5=randomForest(as.factor(Specific_Bean_Origin)~Cocoa_Percent+Rating+Review_Date, data=chocolate, cp=0.01, na.action=na.omit)
classifiedforest5
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 97%
importance(classifiedforest5)
#               MeanDecreaseGini
# Cocoa_Percent        117.78791
# Rating                85.94664
# Review_Date           91.07353




# CHECKING FIRST FEW MODELS

predict(classifiedforest,data.frame(Cocoa_Percent=0.6, Rating=4.5, Review_Date=2023))
# Result: Peru

predict(classifiedforest2,data.frame(Broad_Bean_Origin="Peru",Cocoa_Percent=0.4, Rating=4.5, Review_Date=2016, Bean_Type="Criollo"))
# Result: Altus aka Cao Artisan 





# BOOSTING

# The individual boosting trees are weak learners, but all together they are powerful
# All together they become an ensemble of weak learners to turn into a very strong model
# We want weak learners to avoid overfitting


install.packages("gbm")
library(gbm)

set.seed(1) # making it so we always get the same results


# BOOSTING FOR NUMERICAL TARGET VARIABLE

boosted=gbm(Rating~as.factor(Broad_Bean_Origin)+Cocoa_Percent+as.factor(Company_Location)+Review_Date+as.factor(Bean_Type), data=chocolate, distribution="gaussian", n.trees=10000, interaction.depth = 4)
summary(boosted)
# Using gaussian for numerical (use bernoulli for classification)
# Output gives the relative importance

# var   rel.inf
# as.factor(Broad_Bean_Origin) as.factor(Broad_Bean_Origin) 62.012637
# as.factor(Company_Location)   as.factor(Company_Location) 26.506001
# as.factor(Bean_Type)                 as.factor(Bean_Type)  5.695877
# Cocoa_Percent                               Cocoa_Percent  3.254047
# Review_Date                                   Review_Date  2.531438

predicted_score=predict(boosted, newdata=chocolate, ntrees=10000)
mean((predicted_score-Rating)^2)
# MSE: 0.05951122


# BOOSTING FOR CATEGORICAL TARGET VARIABLE

boosted2=gbm(Broad_Bean_Origin~Rating+Cocoa_Percent+as.factor(Company_Location)+Review_Date+as.factor(Bean_Type), data=chocolate, distribution="bernoulli", n.trees=10000, interaction.depth = 4)
summary(boosted2)
# We can't test boosting for this classification because there's more than 2 categories






# FINAL MODEL PREDICTIONS

# Dark Chocolate Prediction

# Where would be the best place to source for Ambrosia, a Canadian Company:
predict(classifiedforest4,data.frame(Cocoa_Percent=0.7, Rating=4.5, Review_Date=2023, Company="Ambrosia", Company_Location="Canada"))
# Result: Colombia

# Now let's check for Arete, a company in the USA:
predict(classifiedforest4,data.frame(Cocoa_Percent=0.8, Rating=4.0, Review_Date=2023, Company="Arete", Company_Location="U.S.A."))
# Result: Peru


# Milk Chocolate Predictions

# Soma company in Canada producing highly rated milk chocolate
predict(classifiedforest4,data.frame(Cocoa_Percent=0.4, Rating=4.2, Review_Date=2023, Company="Soma", Company_Location="Canada"))
# Sao Tome

# Batch company in the USA producing highly rated milk chocolate
predict(classifiedforest4,data.frame(Cocoa_Percent=0.3, Rating=4.7, Review_Date=2023, Company="Batch", Company_Location="U.S.A."))
# Sao Tome








# FINAL MODEL CROSS VALIDATION

# Out of bag
classifiedforest4=randomForest(as.factor(Broad_Bean_Origin)~Cocoa_Percent+Rating+Review_Date+Company+Company_Location, ntree=500, data=chocolate, importance=TRUE, na.action=na.omit, do.trace=50)
# Do trace shows how worth it is to add more trees
# While it's hard to see all the individual values, overall performance is still decent throughout all potential tree additions
# The higher tree additions perform slightly worse overall

# Error rate
classifiedforest4$err.rate[,1]
# Overall, 
plot(classifiedforest4$err.rate[,1], type = "l", col= "brown", xlab="Final Model", ylab="Model Error Rate", main="Validation Error Rate")





