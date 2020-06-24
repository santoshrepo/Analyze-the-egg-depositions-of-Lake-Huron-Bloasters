---
title: "Time Series Assignment2"
author: "Santosh Kumaravel Sundaravadivelu (S3729461)"
output:
  html_document:
    color: blue
    number_sections: yes
    theme: united
    toc: yes
  word_document:
    toc: yes
---

# Abstract

The purpose of this assignment is to analyse of the egg depositions of age-3 Lake Huron Bloaters (Coregonus hoyi) across the 1982-1996 period and then forecast the depositions for the upcoming five years with the best fit model. Initially, Model is selected by analysing between various models such as trend models, Arima models, and then choosing the best fit model based on the various predictions performed on the later part of the assignment. The best model is chosen to forecast the egg depositions for the upcoming five years.

# Introduction

Coregonus hoyi is a fish kind found in the underwater slopes in lakes such as lake huran. It is also known as bloater which is in the family Salmonidae. It is around 25.5 cms long and silver in color. In this assignment, the analysis is done for the depositions of its eggs across the period(1982-1996).

## Importing Libraries

The libraries such as TSA,time-series is imported into the environment to deal with time-series data. lmtest is used for testing Linear Regression Models. Forecast library is used to forecast the values for the upcoming years.

```{r message = FALSE, warnings = FALSE}

library(tseries)
library(TSA)
library(FitAR)
library(fUnitRoots)
library(forecast)
library(lmtest)

```

## Data Description

The data is originally loaded into the environment by the read.csv function. The Head function displays the top values present in the data frame.

```{r }
data_egg <‐ read.csv('eggs.csv', header = TRUE)
```

```{r }
head(data_egg)
```

Once the data frame is loaded into the environment, it needs to be converted into time-series data to proceed with the assignment. ts() function will be used to convert the data frame into time series data and for further analysis. class function confirms the data frame is converted to time-series data. 

```{r }
data_egg <‐ ts(as.vector(data_egg[,2]), start=1981, end=1996)
class(data_egg)
```

## Descriptive Analysis

The next step is to do a descriptive analysis over the time series data to find any kind of seasonality or trend present.

```{r }
# Ploting Time series graph
plot(data_egg,type='o',ylab='Egg depositions',xlab = 'Time(Year)',main = 'Fig 1: Timeseries plot of Egg depositions')
```

From the above time series plot, insights could be gained such as,

* Seasonality:  There is no seasonality present in the above plot.
* Trend: It is quite visible that the graph exhibits an upward trend.
* Changepoint: There is no particular change in points, but the spike is observed in 1990 and then the plot comes to normal.
* Relationship between the points -  There is a mix of autoregressive is observed. Moving average nature is also observed.

```{r }
plot(y=data_egg,x=zlag(data_egg),ylab='Egg depositions',xlab = 'Time(Year)',main = 'Fig 2: Scatter plot of Egg depositions')

```

From the above scatterplot, insights could be gained are,

* It is strongly co-related because of the positive upward trend present. it is calculated with lag 1. Lags mean how far the series offset present. In this case, we could say that it is shifted by one because it is lag 1. The positive matches are decreased when there is an increase in the number of lags

```{r}
par(mfrow=c(2,1) , cex.lab = 0.6, cex.main=0.6, cex.axis = 0.6,mar=c(2.5,2.5,2.5,2.5))
acf(data_egg, main = "ACF plot for Egg depositions")
pacf(data_egg, main = "PACF plot for Egg depositions")
mtext("Fig 3: ACF and PACF plots", outer=TRUE, cex=1, line=‐1.0)

```

From the above plots, we could infer,

* There is a significant lag present in the ACF and PACF plots, which implies there is moving average and autoregressive present in the time series data.

```{r}
y = data_egg
x = zlag(data_egg)
index = 2:length(x)
cor(y[index],x[index])

```

The above co-relation value confirms the presence of co-relation which was inferred before in the scatter plot. The co-relation score is around 74.4% with lag 1.

# Model Selection

Model Selection is an essential part of the assignment. In this section, all the various models are analysed and the best fit model is chosen for the forecasting process.

## Trend Models

From the above Descriptive analysis, deterministic behavior was not obtained from the analysis. To find the deterministic behavior of the time series data we proceed to the Linear and Quadratic model.

### Linear Trend Model

The linear model is backed up by the formula Y=mx+c, m is the slope, Y is the mean, X is time and C is intercept. In this model, the focus is on p-value and R^2 value. If the p-value is less than 0.05 then the null hypothesis is rejected or else vice versa. On the other hand, R^2 value is considered as the goodness coefficient explains the variation of the series by the model, in other words, the efficiency of the model.


```{r}
linear_Model = lm(data_egg~time(data_egg))
summary(linear_Model)

```

From the above Summary statistics of the Linear model, insights could be gained are,

* P-value is 0.046, which implies it could reject the null hypothesis since the value is less than 0.05.
* The model fits the series since the null hypothesis is rejected.
* Adjusted R^2 value is 40.74% which doubts the efficiency of the linear model to be considered for the best fit.

### Quadratic Regression Model

Quadratic Regression is backed up with the formula: Y=ax+bx+c, where X is time, c is intercept, b is the quadratic trend, a is the linear trend, Y is the mean function. WE focus on the p-value and R^2 value to determine the best fit model.

```{r}
t = time(data_egg)
t2 = t^2
quadractic_Model = lm(data_egg ~ t + t2)
summary(quadractic_Model)

```

From the above summary statistics, insights can be gained are, 

* p-value is 0.0028, which is less than 0.05 thus the null hypothesis is rejected. 
* Since the p-value is less the model is significant.
* Although, the Adjusted R^2 value is higher than the linear model. It is not efficient to be the best fit model.

### Model Fitting

```{r}
plot(ts(as.vector(data_egg)),ylab='Egg Depositions',xlab='Time(Year)',type='o', col = c("Black"),lwd = 2,
     main = "Fig 4: Fitted Linear and Quadratic to Egg Deposition")

lines(as.vector(fitted(linear_Model)), col="red", lwd = 2)
lines(as.vector(fitted(quadractic_Model)), col="blue", lwd = 2)

legend("topleft",lty=1, text.width = 20, col=c("Black","red","blue"), bty = "n",
       c("Egg Deposition", "Fitted Linear Curve", "Fitted Quadratic Curve"))

```

From the above figure, we can see the models are not a good fit model for forecasting. Although the Linear model is fitted well visually, the R^2 value is very low.

## Residual Analysis for Trend Models

### Residual Analysis for Linear Model

```{r}

par(mfrow=c(3,2),cex.lab =0.8,cex.main=0.8,cex.axis = 0.8,mar=c(2,2,4,2))
acf(rstudent(linear_Model), main="ACF Plot")
pacf(rstudent(linear_Model), main="PACF Plot")

plot(y=rstudent(linear_Model),
x=as.vector(time(data_egg)),
ylab='Standardized Residuals',
xlab='Year',
type='o',
main = "Standardized residuals")
hist(rstudent(linear_Model),
xlab='Standardized Residuals',
main = "Standardized residuals")
plot(y=rstudent(linear_Model),
x=fitted(linear_Model),
ylab='Standardized Residuals',
xlab='Fitted Trend Line Values',
type='p',
main = "Standardized residuals")
qqnorm(rstudent(linear_Model), main="QQ Plot ")
qqline(rstudent(linear_Model), col = 2, lwd = 1, lty = 2)
mtext("Fig 5: Linear Model Residual Analysis", outer=TRUE, cex=1,
line=‐1.25)

```

```{r}
#Shapiro wilk test
shapiro.test(rstudent(linear_Model))

```

```{r}
#Independence test
runs(rstudent(linear_Model))

```

From the above residual analysis of Linear model, insights can be gained are,

* The P-value in the Shapiro-Wilk normality test is 0.001205 which is less than that of 0.05 which implies the residuals are not normal.
* In independence test the value is greater than 0.05, which means the residuals are dependent.
* Normality of the qqplot is questioned because of the alignment of the straight line on the data points. 
* The Distribution is not normal in the histogram.
* In ACF and PACF plots there is successive lags with implies correlation is significant.
* There is a change in variance in the residuals vs fitted trend plot.

### Residual analysis of Quadratic Model

```{r}
par(mfrow=c(3,2),cex.lab =0.8,cex.main=0.8,cex.axis = 0.8,mar=c(2,2,4,2))
acf(rstudent(quadractic_Model), main="ACF Plot")
pacf(rstudent(quadractic_Model), main="PACF Plot")
plot(y=rstudent(quadractic_Model),
x=as.vector(time(data_egg)),
ylab='Standardized Residuals',
xlab='Year',
type='o',
main = "Standardized residuals")
hist(rstudent(quadractic_Model),
xlab='Standardized Residuals',
main = "Standardized residuals")

plot(y=rstudent(quadractic_Model),
x=fitted(quadractic_Model),
ylab='Standardized Residuals',
xlab='Fitted Trend Line Values',
type='p',
main = "Standardized residuals")
qqnorm(rstudent(quadractic_Model), main="QQ Plot")
qqline(rstudent(quadractic_Model), col = 2, lwd = 1, lty = 2)
mtext("Fig 6: Quadratic Regression Model Residual Analysis", outer=TRUE, cex=1,
line=‐1.25)

```

```{r}
#shapiro wilk test
shapiro.test(rstudent(quadractic_Model))

```



```{r}
#Independence test
runs(rstudent(quadractic_Model))
```

From the above residual analysis of Quadratic model, insights can be gained are,

* In Shaprio Test same as the linear model the p-value is less which implies the residuals are not normal.
* In Independence test since the value is greater the residuals are dependent.
* same as the linear model, doubting the normality of the residuals.
* The Distribution is not normal in the histogram.
* In ACF and PACF plots there is successive lags with implies correlation is significant.
* There is a change in variance in the residuals vs fitted trend plot.

The quadratic model is good compared to the linear model, but the predicting capabilities are very less which means it is not the right fit to forecast the upcoming five years. Since the R^2 value is low in both the models are not the right fit for forecasting. Since the conclusion for the best fit model is not determined in the deterministic behavior. it is proceeded to the stochastic trend analysis and make use of the ARIMA models to find the best fit to forecast the egg deposition for the upcoming 5 years.

## ARIMA Models

To proceed with the ARIMA models, the first step is to check the time series data is stationary or not. It can be determined with the ADF(Augmented Dickey-Fuller) test function and then normality is accessed by the Shapiro Test and proceeded with the analysis.

### ADF Test

```{r}

adf.test(data_egg)
```
From the above ADF test, the p-value is greater than 0.05 which implies it has failed to reject the null hypothesis. Thus the time series is non-stationary.

```{r}
#shapiro wilk test
shapiro.test(data_egg)
```

From the above Shapiro-Wilk normality test, the p-value is observed to be greater than 0.05 which means it has failed to reject the null hypothesis and thus it is depicted normality.

### Transformation

To remove the non-stationarity in the time series data, BoxCox transformation is used. the yule-walker method is used in the BoxCox transformation.

```{r }
data_egg.transform = BoxCox.ar(data_egg, method = "yule‐walker")
mtext("Fig 7: BoxCox Treansformation", outer=TRUE, cex=1,line=‐3)
```


```{r}
data_egg.transform$ci
```

Lambda value ranges from 0.1 to 0.8 with a 95% confidence interval and the center value is 0.45 which will be used to transform the data.

```{r}
lambda = 0.45
BC.data_egg.transform = (data_egg^lambda‐1)/lambda

```

```{r}

plot(BC.data_egg.transform, type = "o",xlab="Time(Year)", ylab = "Egg depositions", main = "Fig 8: Timeseries plot with Transformed Egg depositions")

```

The time series plot is plotted for the lambda value=0.45 which was found in the previous step of Transformation.

```{r}

#QQ plot
qqnorm(BC.data_egg.transform,main="Fig 9: QQplot with Transformed Egg depositions")
qqline(BC.data_egg.transform, col = 2) 

```


```{r}
#shapiro Test
shapiro.test(BC.data_egg.transform)
```

The normality of the transformed time series data can be determined by the above Shapiro-Wilk normality test and Q-Q plot. The p-value is greater than 0.05 in the Shapiro-Wilk test and thus failed to reject the null hypothesis. The data points are partially arranged on the straight line in QQplot.

To check the stationarity of the transformed data ADF test is performed.

```{r}
#Dicker fuller test
adf.test(BC.data_egg.transform)
```

The P-value of the ADF test is greater than 0.05 which implies the transformed data is still not stationary and thus differencing is performed to remove the non-stationarity in the time series data.

### Differencing

The Differencing is done to remove the non-stationarity in the time series data.

```{r}
#Order 1 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 1)
```

Differencing of 1 is performed to check if the non-stationarity is removed.

```{r}
#plotting difference 1
plot(diff.data_egg,
type='o',
ylab='Egg depositions',
xlab = 'Time(Year)',
main = 'Fig 10: First Difference of Egg depositions')

```

It is quite evident that there is the presence of a trend in the above time series plot.

```{r}

#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

```

The P-value of the ADF test is around 0.3469 which implies it has failed to reject the null hypothesis. Also, there is a trend present and thus is differenced further for the second order.

```{r}
diff.data_egg = diff(BC.data_egg.transform, differences = 2)
```

Differencing of 2 is performed to check if the non-stationarity is removed.

```{r}
#plotting difference 2
plot(diff.data_egg,
type='o',
ylab='Egg depositions',
xlab = 'Time(Year)',
main = 'Fig 11: Second Difference of Egg depositions')

```

From the above plot, there is no trend present in the second-order of difference, it is validated further by the ADF test.

```{r}
#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

```

P-value is around 0.1098 which is less than first-order differencing and it is greater than 0.05, implies it failed to reject the null hypothesis. It is being further differenced to third order.

```{r}
#Order 3 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 3)
```

Differencing of 3 is performed to check if the non-stationarity is removed.

```{r}

#plotting difference 3
plot(diff.data_egg,
type='o',
ylab='Egg Depositions',
xlab = 'Time(Year)',
main = 'Fig 12: Third Difference of Egg depositions')

```

From the above time series plot, the trend is not present in the 3rd order differencing. The stationarity is further analysed using the ADF test.

```{r}
#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

```

The p-value is around 0.1836 is greater than 0.05 and failed to reject the null hypothesis. Thus the series is being further differenced into the 4th order of differencing.

```{r}

#Order 4 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 4)

```

Differencing of 4 is performed to check if the non-stationarity is removed.

```{r}

#plotting difference 4
plot(diff.data_egg,type='o',
ylab='Egg Depositions',
xlab = 'Time(Year)',
main ='Fig 13: Forth Difference of Egg depositions') 
```

From the above plots, even in the 4th Order of differencing the trend is not present. The stationarity is analysed using the ADF test.

```{r}
#Dicker-Fuller Unit Test
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

```

The p-value is around 0.02265 which is less than 0.05 which rejects the null hypothesis. Thus the time series is stationary with differencing order of 4.

### Possible candidate models

In the previous step, the time series data was transformed into stationery with the differencing order of 4. Further ACF, PACF is plotted to find the possible candidate for the models.

```{r}
#ACF and PACF Plots
par(mfrow=c(2,1) , cex.lab = 0.7, cex.main=0.7, cex.axis = 0.7,mar=c(3,3,2.5,1))
acf(diff.data_egg, main = "ACF plot")
pacf(diff.data_egg, main = "PACF plot")
mtext("Fig 14: ACF and PACF plots for Egg depositions", outer=TRUE, cex=1, line=‐1.0)
```

It is quite evident that from the above ACF and PACF plots, there is one significant lag and two significant lag in the PACF plot. Hence, the ARIMA(2,4,1) model is added to the candidate set of models.

```{r}
#EACF test with maximum order of 2
eacf(diff.data_egg,ar.max = 2, ma.max = 2)
```

The presence of white noise in vertex(0,0). The adjucent points are captured for plausible candidate model ARIMA(0,4,1),ARIMA(0,4,2),ARIMA(1,4,1) and ARIMA(1,4,1).

```{r}
#Computing BIC
res4 = armasubsets(y=diff.data_egg,nar=2,nma=3,y.name='test',ar.method='ols')
plot(res4)
mtext("Fig 15: BIC Table", outer=TRUE, cex=1, line=‐1.25)
```

From the above table, it is evident that there is the presence of AR(1), MA(1) and MA(2), which implies the following ARIMA models could be considered for the set of candidate models ARIMA(1,4,2) and ARIMA(1,4,1)


To conclude the set of plausible candidate models are,

* ARIMA(0,4,2)
* ARIMA(0,4,1)
* ARIMA(1,4,0)
* ARIMA(1,4,1)
* ARIMA(1,4,2)
* ARIMA(2,4,1)

### Parameter Estimation

```{r}
# ARIMA(0,4,2) CSS Method
model_css_042 = arima(BC.data_egg.transform, order = c(0,4,2), method = 'CSS')
coeftest(model_css_042)
```


```{r}
# ARIMA(0,4,2) ML Method
model_ml_042 = arima(BC.data_egg.transform, order = c(0,4,2), method = 'ML')
coeftest(model_ml_042)
```

From the above AIRMA(0,4,2) model, MA(1), MA(2) are significant with CSS and ML methods

```{r}
# ARIMA(0,4,1) CSS Method
model_css_041 = arima(BC.data_egg.transform, order = c(0,4,1), method = 'CSS')
coeftest(model_css_041)
```

```{r}
# ARIMA(0,4,1) ML Method
model_ml_041 = arima(BC.data_egg.transform, order = c(0,4,1), method = 'ML')
coeftest(model_ml_041)
```

From the AIRMA(0,4,1) model, conponent MA is significant in both the methods.

```{r}
# ARIMA(1,4,1) CSS Method
model_css_141 = arima(BC.data_egg.transform, order = c(1,4,1), method = 'CSS')
coeftest(model_css_141)
```

```{r}
# ARIMA(1,4,1) ML Method
model_ml_141 = arima(BC.data_egg.transform, order = c(1,4,1), method = 'ML')
coeftest(model_ml_141)

```

From the above ARIMA(1,4,1) model, both the MA and AR components results are significant with the p-value.

```{r}
# ARIMA(1,4,0) CSS Method
model_css_140 = arima(BC.data_egg.transform, order = c(1,4,0), method = 'CSS')
coeftest(model_css_140)
```


```{r}
# ARIMA(1,4,0) ML Method
model_ml_140 = arima(BC.data_egg.transform, order = c(1,4,0), method = 'ML')
coeftest(model_ml_140)
```

The above AIRMA(1,4,0) model, AR is significant in both the models.

```{r}
# ARIMA(2,4,1) CSS Method
model_css_241 = arima(BC.data_egg.transform, order = c(2,4,1), method = 'CSS')
coeftest(model_css_241)

```

```{r}
# ARIMA(2,4,1) ML Method
model_ml_241 = arima(BC.data_egg.transform, order = c(2,4,1), method = 'ML')
coeftest(model_ml_241)

```

From the above AIRMA(2,4,1) model, AR(1),MA(1) is significant in ML method and AR(1) is significant in CSS method.

```{r}
# ARIMA(1,4,2) CSS Method
model_css_142 = arima(BC.data_egg.transform, order = c(1,4,2), method = 'CSS')
coeftest(model_css_142)
```

```{r}
# ARIMA(1,4,2) ML Method
model_ml_142 = arima(BC.data_egg.transform, order = c(1,4,2), method = 'ML')
coeftest(model_ml_142)
```

From the above AIRMA(1,4,2) model, AR(1) component is significant in CSS, and MA(1), MA(2) is significant in the ML method.

In the set of possible candidate ARIMA models, the models with the least AIC and BIC score will be considered as the best fit model.

```{r include=FALSE}

# Find best model on the basis AIC and BIC
par(mfrow=c(1,1) , cex.lab = 0.7, cex.main=0.7, cex.axis = 0.7)
sort.score <‐ function(x, score = c("bic", "aic")){
if (score == "aic"){
x[with(x, order(AIC)),]
} else if (score == "bic") {
x[with(x, order(BIC)),]
} else {
warning('score = "x" only accepts valid arguments ("aic","bic")')
}
}

```

```{r}
sort.score(AIC(model_ml_041,model_ml_042,model_ml_140,model_ml_141,model_ml_142, model_ml_241), score = "aic")
```

```{r}
# Using BIC
sort.score(BIC(model_ml_041,model_ml_042,model_ml_140,model_ml_141,model_ml_142, model_ml_241), score = "bic")
```

From the AIC and BIC table, it is very clear that ARIMA(0,4,2) has the least value in both the tables and considered the best fit model to forecast the egg depositions for the upcoming 5 years.

### Overfitting Analysis

Overfitting needs to be checked as the ARIMA(0,4,2) model is selected to forecast. MA and AR components are tuned to analyse the overfitting of the model. We re considering ARIMA(0,4,3) and ARIMA(1,4,2) models to analyse the overfitting. Since ARIMA(1,4,2) is analysed previously, ARIMA(0,4,3) model is taken now,

```{r}
# ARIMA(0,4,3) CSS Method
model_css_043 = arima(BC.data_egg.transform, order = c(0,4,3), method = 'CSS')
coeftest(model_css_043)

```

```{r}
# ARIMA(0,4,3) ML Method
model_ml_043 = arima(BC.data_egg.transform, order = c(0,4,3), method = 'ML')
coeftest(model_ml_043)
```

From the above two methods such as CSS and ML, it is evident that MA(1) and MA(2) component is significant when CSS method is used and only MA(1) is found significant while using ML method. This ARIMA(0,4,3) is overfitting. Only (0,4,2) is considered as a best-fit model.

## Residual Analysis for ARIMA Model

```{r include=FALSE}

residual.analysis <‐ function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA‐GARCH")[1]){
# If you have an output from arima() function use class = "ARIMA"
# If you have an output from garch() function use class = "GARCH"
# If you have an output from ugarchfit() function use class = "ARMA‐GARCH"
if (class == "ARIMA"){
if (std == TRUE){
res.model = rstandard(model)
}else{
res.model = residuals(model)
}
}else if (class == "GARCH"){
res.model = model$residuals[start:model$n.used]
}else if (class == "ARMA‐GARCH"){
  res.model = model@fit$residuals
}else {
stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
}
  
par(mfrow=c(3,2),cex.lab =0.7,cex.main=0.7,cex.axis = 0.7,mar=c(2,2.5,4,2))
plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot")
abline(h=0)
acf(res.model,main="ACF plot")
hist(res.model,main="Histogram")
qqnorm(res.model,main="QQ plot ")
pacf(res.model,main="PACF plot")
qqline(res.model, col = 2)
k=0
LBQPlot(res.model, lag.max = length(model$residuals)‐1, StartLag = k + 1, k = 0, SquaredQ =
FALSE)
mtext("Fig 16: Residual Analysis of ARIMA model", outer=TRUE, cex=1, line=‐1.5)
}
  
```


```{r}
residual.analysis(model = model_ml_042)
```


```{r}
#shapiro wilk test
shapiro.test(rstandard(model_ml_042))
```

From the above residual analysis of ARIMA(0,4,2) Model, insights can be gained are,

* There is no change in variance or Trend present in the residual plot of the ARIMA(0,4,2) model.
* No significant correlation between the ACF and PACF plots.
* Normality of the residuals can be justified from the QQplot, where the straight line is aligned on most of the data points.
* Histogram implies it is normally distributed.
* In Ljung Box Test, all the points are above the threshold value line which implies the ARIMA model chosen is the right fit.

Thus, the ARIMA(0,4,2) model defines the best fit model for forecasting the egg depositions.

# Forecasting

The Best fit Model ARIMA(0,4,2) is used to predict the egg depositions for the upcoming 5 years.

```{r}

fit = Arima(BC.data_egg.transform, model = model_ml_042)
predicted = forecast(fit, h = 5)
#Print forecasting
print(predicted)

```

```{r}
#plotting forecasted data
plot(predicted, ylim = c(‐2,3), main = "Fig 17: Predicted ARIMA(0,4,2)",xlab="Time(Year)",ylab="Egg depositions")
```

In the above-forecasted plot, the blue line is represented by the forecasted value for the depositions of the egg (80%) and the grey region is 95%

# Conclusion

The data was imported into the environment for predicting the forecast for the next 5 years. At first, descriptive analysis was done to check the presence of a trend. Then the Trend model was used to find the best fit to forecast it. Since the deterministic Model(Trend Model) was not the right fit, the Stotachic model(ARIMA) was used to find the best fit. ADF test was conducted to find the time series data is stationary or not. Since it was not stationary, it was transformed into BoxCox transformation. Since there was a trend present differencing was done, it was further differenced until there was no trend and the p-value is less than 0.05. Based on the AIC and BIC values the best model was nominated for the best fit. Residual Analysis was performed to confirm the best fit model for the forecasting. ARIMA(0,4,2) Model was chosen as the best model and it was used to forecast the egg depositions for the next 5 years.

# Appendix

```{r eval=FALSE}

library(tseries)
library(TSA)
library(FitAR)
library(fUnitRoots)
library(forecast)
library(lmtest)

data_egg <‐ read.csv('eggs.csv', header = TRUE)

head(data_egg)

data_egg <‐ ts(as.vector(data_egg[,2]), start=1981, end=1996)
class(data_egg)

# Ploting Time series graph
plot(data_egg,type='o',ylab='Egg depositions',xlab = 'Time(Year)',main = 'Fig 1: Timeseries plot of Egg depositions')

plot(y=data_egg,x=zlag(data_egg),ylab='Egg depositions',xlab = 'Time(Year)',main = 'Fig 2: Scatter plot of Egg depositions')

par(mfrow=c(2,1) , cex.lab = 0.6, cex.main=0.6, cex.axis = 0.6,mar=c(2.5,2.5,2.5,2.5))
acf(data_egg, main = "ACF plot for Egg depositions")
pacf(data_egg, main = "PACF plot for Egg depositions")
mtext("Fig 3: ACF and PACF plots", outer=TRUE, cex=1, line=‐1.0)

y = data_egg
x = zlag(data_egg)
index = 2:length(x)
cor(y[index],x[index])

linear_Model = lm(data_egg~time(data_egg))
summary(linear_Model)

t = time(data_egg)
t2 = t^2
quadractic_Model = lm(data_egg ~ t + t2)
summary(quadractic_Model)

plot(ts(as.vector(data_egg)),ylab='Egg Depositions',xlab='Time(Year)',type='o', col = c("Black"),lwd = 2,
     main = "Fig 4: Fitted Linear and Quadratic to Egg Deposition")

lines(as.vector(fitted(linear_Model)), col="red", lwd = 2)
lines(as.vector(fitted(quadractic_Model)), col="blue", lwd = 2)
legend("topleft",lty=1, text.width = 20, col=c("Black","red","blue"), bty = "n",
       c("Egg Deposition", "Fitted Linear Curve", "Fitted Quadratic Curve"))
	   
	   
par(mfrow=c(3,2),cex.lab =0.8,cex.main=0.8,cex.axis = 0.8,mar=c(2,2,4,2))
acf(rstudent(linear_Model), main="ACF Plot")
pacf(rstudent(linear_Model), main="PACF Plot")

plot(y=rstudent(linear_Model),
x=as.vector(time(data_egg)),
ylab='Standardized Residuals',
xlab='Year',
type='o',
main = "Standardized residuals")
hist(rstudent(linear_Model),
xlab='Standardized Residuals',
main = "Standardized residuals")
plot(y=rstudent(linear_Model),
x=fitted(linear_Model),
ylab='Standardized Residuals',
xlab='Fitted Trend Line Values',
type='p',
main = "Standardized residuals")
qqnorm(rstudent(linear_Model), main="QQ Plot ")
qqline(rstudent(linear_Model), col = 2, lwd = 1, lty = 2)
mtext("Fig 5: Linear Model Residual Analysis", outer=TRUE, cex=1,
line=‐1.25)

#Shapiro wilk test
shapiro.test(rstudent(linear_Model))

#Independence test
runs(rstudent(linear_Model))

par(mfrow=c(3,2),cex.lab =0.8,cex.main=0.8,cex.axis = 0.8,mar=c(2,2,4,2))
acf(rstudent(quadractic_Model), main="ACF Plot")
pacf(rstudent(quadractic_Model), main="PACF Plot")
plot(y=rstudent(quadractic_Model),
x=as.vector(time(data_egg)),
ylab='Standardized Residuals',
xlab='Year',
type='o',
main = "Standardized residuals")
hist(rstudent(quadractic_Model),
xlab='Standardized Residuals',
main = "Standardized residuals")
plot(y=rstudent(quadractic_Model),
x=fitted(quadractic_Model),
ylab='Standardized Residuals',
xlab='Fitted Trend Line Values',
type='p',
main = "Standardized residuals")
qqnorm(rstudent(quadractic_Model), main="QQ Plot")
qqline(rstudent(quadractic_Model), col = 2, lwd = 1, lty = 2)
mtext("Fig 6: Quadratic Regression Model Residual Analysis", outer=TRUE, cex=1,
line=‐1.25)

#shapiro wilk test
shapiro.test(rstudent(quadractic_Model))

#Independence test
runs(rstudent(quadractic_Model))


adf.test(data_egg)

#shapiro wilk test
shapiro.test(data_egg)

data_egg.transform = BoxCox.ar(data_egg, method = "yule‐walker")
mtext("Fig 7: BoxCox Treansformation", outer=TRUE, cex=1,line=‐3)

data_egg.transform$ci

lambda = 0.45
BC.data_egg.transform = (data_egg^lambda‐1)/lambda

plot(BC.data_egg.transform, type = "o",xlab="Time(Year)", ylab = "Egg depositions", main = "Fig 8: Timeseries plot with Transformed Egg depositions")

#QQ plot
qqnorm(BC.data_egg.transform,main="Fig 9: QQplot with Transformed Egg depositions")
qqline(BC.data_egg.transform, col = 2) 

#shapiro Test
shapiro.test(BC.data_egg.transform)

#Dicker fuller test
adf.test(BC.data_egg.transform)

#Order 1 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 1)

#plotting difference 1
plot(diff.data_egg,
type='o',
ylab='Egg depositions',
xlab = 'Time(Year)',
main = 'Fig 10: First Difference of Egg depositions')

#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

diff.data_egg = diff(BC.data_egg.transform, differences = 2)

#plotting difference 2
plot(diff.data_egg,
type='o',
ylab='Egg depositions',
xlab = 'Time(Year)',
main = 'Fig 11: Second Difference of Egg depositions')

#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

#Order 3 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 3)

#plotting difference 3
plot(diff.data_egg,
type='o',
ylab='Egg Depositions',
xlab = 'Time(Year)',
main = 'Fig 12: Third Difference of Egg depositions')

#Dicker‐Fuller Unit‐Root
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

#Order 4 of differencing
diff.data_egg = diff(BC.data_egg.transform, differences = 4)

#plotting difference 4
plot(diff.data_egg,type='o',
ylab='Egg Depositions',
xlab = 'Time(Year)',
main ='Fig 13: Forth Difference of Egg depositions') 

#Dicker-Fuller Unit Test
order = ar(diff(diff.data_egg))$order
adfTest(diff.data_egg, lags = order, title =NULL, description = NULL)

#ACF and PACF Plots
par(mfrow=c(2,1) , cex.lab = 0.7, cex.main=0.7, cex.axis = 0.7,mar=c(3,3,2.5,1))
acf(diff.data_egg, main = "ACF plot")
pacf(diff.data_egg, main = "PACF plot")
mtext("Fig 14: ACF and PACF plots for Egg depositions", outer=TRUE, cex=1, line=‐1.0)

#EACF test with maximum order of 2
eacf(diff.data_egg,ar.max = 2, ma.max = 2)

#Computing BIC
res4 = armasubsets(y=diff.data_egg,nar=2,nma=3,y.name='test',ar.method='ols')
plot(res4)
mtext("Fig 15: BIC Table", outer=TRUE, cex=1, line=‐1.25)

# ARIMA(0,4,2) CSS Method
model_css_042 = arima(BC.data_egg.transform, order = c(0,4,2), method = 'CSS')
coeftest(model_css_042)

# ARIMA(0,4,2) ML Method
model_ml_042 = arima(BC.data_egg.transform, order = c(0,4,2), method = 'ML')
coeftest(model_ml_042)

# ARIMA(0,4,1) CSS Method
model_css_041 = arima(BC.data_egg.transform, order = c(0,4,1), method = 'CSS')
coeftest(model_css_041)

# ARIMA(0,4,1) ML Method
model_ml_041 = arima(BC.data_egg.transform, order = c(0,4,1), method = 'ML')
coeftest(model_ml_041)

# ARIMA(1,4,1) CSS Method
model_css_141 = arima(BC.data_egg.transform, order = c(1,4,1), method = 'CSS')
coeftest(model_css_141)

# ARIMA(1,4,1) ML Method
model_ml_141 = arima(BC.data_egg.transform, order = c(1,4,1), method = 'ML')
coeftest(model_ml_141)

# ARIMA(1,4,0) CSS Method
model_css_140 = arima(BC.data_egg.transform, order = c(1,4,0), method = 'CSS')
coeftest(model_css_140)

# ARIMA(1,4,0) ML Method
model_ml_140 = arima(BC.data_egg.transform, order = c(1,4,0), method = 'ML')
coeftest(model_ml_140)

# ARIMA(2,4,1) CSS Method
model_css_241 = arima(BC.data_egg.transform, order = c(2,4,1), method = 'CSS')
coeftest(model_css_241)

# ARIMA(2,4,1) ML Method
model_ml_241 = arima(BC.data_egg.transform, order = c(2,4,1), method = 'ML')
coeftest(model_ml_241)

# ARIMA(1,4,2) CSS Method
model_css_142 = arima(BC.data_egg.transform, order = c(1,4,2), method = 'CSS')
coeftest(model_css_142)

# ARIMA(1,4,2) ML Method
model_ml_142 = arima(BC.data_egg.transform, order = c(1,4,2), method = 'ML')
coeftest(model_ml_142)

# Find best model on the basis AIC and BIC
par(mfrow=c(1,1) , cex.lab = 0.7, cex.main=0.7, cex.axis = 0.7)
sort.score <‐ function(x, score = c("bic", "aic")){
if (score == "aic"){
x[with(x, order(AIC)),]
} else if (score == "bic") {
x[with(x, order(BIC)),]
} else {
warning('score = "x" only accepts valid arguments ("aic","bic")')
}
}

sort.score(AIC(model_ml_041,model_ml_042,model_ml_140,model_ml_141,model_ml_142, model_ml_241), score = "aic")

# Using BIC
sort.score(BIC(model_ml_041,model_ml_042,model_ml_140,model_ml_141,model_ml_142, model_ml_241), score = "bic")

# ARIMA(0,4,3) CSS Method
model_css_043 = arima(BC.data_egg.transform, order = c(0,4,3), method = 'CSS')
coeftest(model_css_043)

# ARIMA(0,4,3) ML Method
model_ml_043 = arima(BC.data_egg.transform, order = c(0,4,3), method = 'ML')
coeftest(model_ml_043)

residual.analysis <‐ function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA‐GARCH")[1]){
# If you have an output from arima() function use class = "ARIMA"
# If you have an output from garch() function use class = "GARCH"
# If you have an output from ugarchfit() function use class = "ARMA‐GARCH"
if (class == "ARIMA"){
if (std == TRUE){
res.model = rstandard(model)
}else{
res.model = residuals(model)
}
}else if (class == "GARCH"){
res.model = model$residuals[start:model$n.used]
}else if (class == "ARMA‐GARCH"){
  res.model = model@fit$residuals
}else {
stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
}
par(mfrow=c(3,2),cex.lab =0.7,cex.main=0.7,cex.axis = 0.7,mar=c(2,2.5,4,2))
plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot")
abline(h=0)
acf(res.model,main="ACF plot")
hist(res.model,main="Histogram")
qqnorm(res.model,main="QQ plot ")
pacf(res.model,main="PACF plot")
qqline(res.model, col = 2)
k=0
LBQPlot(res.model, lag.max = length(model$residuals)‐1, StartLag = k + 1, k = 0, SquaredQ =
FALSE)
mtext("Fig 16: Residual Analysis of ARIMA model", outer=TRUE, cex=1, line=‐1.5)
}
  
residual.analysis(model = model_ml_042)

#shapiro wilk test
shapiro.test(rstandard(model_ml_042))


fit = Arima(BC.data_egg.transform, model = model_ml_042)
predicted = forecast(fit, h = 5)
#Print forecasting
print(predicted)

#plotting forecasted data
plot(predicted, ylim = c(‐2,3), main = "Fig 17: Predicted ARIMA(0,4,2)",xlab="Time(Year)",ylab="Egg depositions")

```

