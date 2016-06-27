Galton's Height Prediction
========================================================
author: Daniel Rosquete
date: 27 Jun 2016

The Data
========================================================

Galton's Height data was taken from:   
The University of Alabama in Huntsville
http://www.math.uah.edu/stat/data/Galton.html  
  
This data was cleaned and processed to obtain 865 observations
with 4 features: Father, Mother, Gender and Height, where Height
was the value to predict, in centimeters.  


```r
set.seed(787878)

# inches to centimeters
datos[,1]<-round(datos[,1]*2.54,0)
...
# Removing the data that was sparsed
datos <- datos[datos$Father <= 190 & datos$Father >= 150,]
...
```

The model
========================================================

First it was splitted by 75% to train and 25% to test.  
Then, the model was setted as:

```r
inTrain <- createDataPartition(y=datos$Height,p=0.75,list = FALSE)
training<-datos[inTrain,]
test <- datos[-inTrain,]
#Training model
modelFit <- lm(Height~.,data = training)
#Prediction
pred<-predict(modelFit,newdata = test)
```
The coefficients are: 41.15 (Intercept) 0.43 Father
0.28 Mother and 13.33 Gender

The results
========================================================
To obtain the basic data:

```r
result<-RMSE(pred,test$Height)
mn<-mean(result)
s<-sd(result)
```
To obtain the quality of results:

```r
#The confidence interval
ic<-mn+c(-1,1)*qt(.975,length(pred)-1)*s / 
    sqrt(length(pred))
t.test(result)
```
The 95 percent confidence interval is between 4.07 and 4.98  
P-value < 2.2e-16

References
========================================================

The working app is in:  
https://dhrosquete.shinyapps.io/dashBoardTest/  
  
The source codes are in:
