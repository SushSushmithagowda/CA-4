#the first dataset has the details of business expenditure on technology
Business_expenditure_on_technology_details <- read.csv("business_expenditure_on_technology.csv")

#the first dataset has the details of crop yield
crop_yield_details<- read.csv("crop_yield.csv")
data <- merge(Business_expenditure_on_technology_details,crop_yield_details)
str()
head(data)
# The estimates of the beta coefficients
# the standard errors (SE), which defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.
# Plotting business expenditure on technology and crop yield variable to see relationship between the response(crop yield) and
# predictor (business expenditure on technology) variable

plot(data$Crop_Yield_per_Hectare_Tonnes,data$Business_expenditure_on_Technology,
     xlab="Business_expenditure_on_Technology",
     ylab="Crop_Yield_per_Hectare_Tonnes",
     main = "Scatter plot showing regression line
     for Crop_Yield_per_Hectare_Tonnes predicted from Business_expenditure_on_Technology")
abline(LinearModel)

#Correlation Test for dataset
cor(data$Crop_Yield_per_Hectare_Tonnes, data$Business_expenditure_on_Technology)
# Scatter plots helps to visualise any linear relationships between the 
# dependent (response) business expenditure variable and independent (predictor) crop yield variables

scatter.smooth(x = data$Business_expenditure_on_Technology, 
               y = data$Crop_Yield_per_Hectare_Tonnes, 
               main = "Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology",
               xlab = "Business_expenditure_on_Technology",
               ylab = "Crop_Yield_per_Hectare_Tonnes")


# Box Plot
par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(data$Business_expenditure_on_Technology, main = "Business_expenditure_on_Technology", 
        sub = paste("Outlier rows: ", boxplot.stats(data$Business_expenditure_on_Technology)$out)) # box plot for 'Income'
boxplot(data$Crop_Yield_per_Hectare_Tonnes, main = "Crop_Yield_per_Hectare_Tonnes", 
        sub = paste("Outlier rows: ", boxplot.stats(data$Crop_Yield_per_Hectare_Tonnes)$out)) # box plot for 'Rent'


# Skewness function to examine normality of data
# install.packages("e1071")
# Density Plot
library(e1071)
# Divide graph area in 2 columns
par(mfrow = c(1, 2))

# Density plot for Crop_Yield_per_Hectare_Tonnes
par(mfrow = c(1, 2))

plot(density(data$Crop_Yield_per_Hectare_Tonnes), 
     main = "Density Plot :Crop_Yield_per_Hectare_Tonnes",
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(data$Crop_Yield_per_Hectare_Tonnes), 2)))
polygon(density(data$Crop_Yield_per_Hectare_Tonnes), col = "yellow")


# Density plot for Business_expenditure_on_Technology
plot(density(data$Business_expenditure_on_Technology),
     main = "Density Plot :data$Business_expenditure_on_Technology",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(data$Business_expenditure_on_Technology), 2)))
polygon(density(data$Business_expenditure_on_Technology), col = "orange")




# Calculating correlation test between Business_expenditure_on_Technology and Crop_Yield_per_Hectare_Tonnes
cor(data$Crop_Yield_per_Hectare_Tonnes, data$Business_expenditure_on_Technology)

# build linear regression model on full data
LinearModel <- lm(Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology, data = data)
LinearModel
# Examining the 95% confidence intervals of the model

confint(LinearModel)

#summary of linear model
summary(LinearModel)

Model_Summary <- summary(LinearModel)

# model coefficients
Model_Coefficients <- Model_Summary$coefficients


# get beta estimate for Business_expenditure_on_Technology
Beta.Estimate <- Model_Coefficients["Business_expenditure_on_Technology", "Estimate"]
Beta.Estimate

# get std.error for Business_expenditure_on_Technology
Std_Error <- Model_Coefficients["Business_expenditure_on_Technology", "Std. Error"]
Std_Error

# calc t statistic
t_value <- Beta.Estimate / Std_Error
t_value
p_value <- 2 * pt(-abs(t_value), df = nrow(data) - ncol(data)) # calc p Value
p_value



# sample chooses a random sample
# from 1:all records from data, 80% of rows
#Sample data for Training and Testing 
Records_taken <- sample(1:nrow(data), 0.8 * nrow(data))
Training_Data <- data[Records_taken,]
Testing_Data <- data[-Records_taken,]

#Model for Linear training model
LMModel <- lm(Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology, 
              data = Training_Data)
LMpred <- predict(LMModel, Testing_Data)
LMpred

summary(LMpred)

LMActual_preds <- data.frame(cbind(actuals = Testing_Data$Crop_Yield_per_Hectare_Tonnes, 
                                   predicted = LMpred))
LMActual_preds
head(LMActual_preds)



AIC(LinearModel)

BIC(LinearModel)

Correlation_Accuracy <- cor(LMActual_preds)
Correlation_Accuracy

#Min_Max Accuracy
LNminmaxaccuracy <- mean(apply(LMActual_preds, 1, min) / apply(LMActual_preds, 1, max))
LNminmaxaccuracy

#Mape validation
LMmape <- mean(abs(LMActual_preds$predicted - LMActual_preds$actuals) / LMActual_preds$actuals)
LMmape


# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)


#Building Polynomial Model 
# # sample chooses a random sample
# # from 1:all records from data, 80% of rows
# no_of_records <- sample(1:nrow(data), 0.8 * nrow(data))
# # model training data
# training_data <- data[Records_taken,]
# training_data
# # test data
# testing_data <- data[-Records_taken,]
# testing_data
# polymodel <- lm(Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology + 
I(Business_expenditure_on_Technology^2), data = data)

# Build the Polynomial model on training data
# lm(formula, data) where
# formula describes the model to be fit
PolynomialModel <- lm(Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology + 
                        I(Crop_Yield_per_Hectare_Tonnes ^ 2), data = data)
print(PolynomialModel)
summary(PolynomialModel)


Poly_Model_Summary <- summary(PolynomialModel)

# model coefficients
Poly_Model_Coefficients <- Poly_Model_Summary$coefficients


# get beta estimate for Business_expenditure_on_Technology
Beta.Estimate_poly <- Poly_Model_Coefficients["Business_expenditure_on_Technology", "Estimate"]
Beta.Estimate_poly
# get std.error for Business_expenditure_on_Technology
Std_Error_poly <- Poly_Model_Coefficients["Business_expenditure_on_Technology", "Std. Error"]
Std_Error_poly

# calc t statistic
t_value_poly <- Beta.Estimate_poly / Std_Error_poly
t_value_poly
p_value_poly <- 2 * pt(-abs(t_value_poly), df = nrow(data) - ncol(data)) # calc p Value
p_value_poly



#Model for Polynomial Model[Second Order]
Poly_model <- lm(Crop_Yield_per_Hectare_Tonnes ~ Business_expenditure_on_Technology + 
                   I(Business_expenditure_on_Technology ^ 2), data = Training_Data)
Polypred <- predict(Poly_model, Testing_Data)
Polypred


PolyActual_preds <- data.frame(cbind(actuals = Testing_Data$Crop_Yield_per_Hectare_Tonnes, 
                                     predicted = Polypred))
PolyActual_preds

AIC(PolynomialModel)
BIC(PolynomialModel)


#Correlation Accuracy
PLcorrelationaccuracy <- cor(PolyActual_preds)
PLcorrelationaccuracy

# Min - max accuracy
PLminmaxaccuracy <- mean(apply(PolyActual_preds, 1, min) 
                         / apply(PolyActual_preds, 1, max))
PLminmaxaccuracy

# MAPE
Polymape <- mean(abs(PolyActual_preds$predicted - PolyActual_preds$actuals) / PolyActual_preds$actuals)
Polymape

summary(Poly_model)

