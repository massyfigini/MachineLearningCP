# Machine Learning Course Project
Massimiliano Figini  
17/02/2016  



## Course Project Instruction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively.
Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

The training data for this project are available here:  
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv  
The test data are available here:  
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv  
The data for this project come from this source:   http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.  

*The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.*


## Getting and cleaning data

Load data and packages


```r
TrainDownload <- "C:/Users/figinim/Documents/Studies/Machine Learning/Course Project/pml-training.csv"
TestDownload <- "C:/Users/figinim/Documents/Studies/Machine Learning/Course Project/pml-testing.csv"
train <- read.csv(TrainDownload)
test <- read.csv(TestDownload)
library(dplyr)
library(caret)
```

Number of records per classe


```r
summarize(group_by(train, classe), Training=n())
```

```
## # A tibble: 5 × 2
##   classe Training
##   <fctr>    <int>
## 1      A     5580
## 2      B     3797
## 3      C     3422
## 4      D     3216
## 5      E     3607
```

There is a lot of data for every "classe" to do the algorithm.  
  
Find columns with lot NA or blank values


```r
na <-sapply(train, function(y) sum(length(which(is.na(y)))))
na <- data.frame(na)
na <- tibble::rownames_to_column(na)
blank <- sapply(train, function(y) sum(length(which(y==""))))
blank <- data.frame(blank)
blank <- tibble::rownames_to_column(blank)
na_blank <- full_join(na, blank, by=c("rowname","rowname"))
na_blank <- mutate(na_blank, NaBlank=na+blank, NaBlankPerc=(na+blank)/nrow(train)*100)
filter(na_blank, NaBlank > 0)
```

```
##                      rowname    na blank NaBlank NaBlankPerc
## 1         kurtosis_roll_belt     0 19216   19216    97.93089
## 2        kurtosis_picth_belt     0 19216   19216    97.93089
## 3          kurtosis_yaw_belt     0 19216   19216    97.93089
## 4         skewness_roll_belt     0 19216   19216    97.93089
## 5       skewness_roll_belt.1     0 19216   19216    97.93089
## 6          skewness_yaw_belt     0 19216   19216    97.93089
## 7              max_roll_belt 19216     0   19216    97.93089
## 8             max_picth_belt 19216     0   19216    97.93089
## 9               max_yaw_belt     0 19216   19216    97.93089
## 10             min_roll_belt 19216     0   19216    97.93089
## 11            min_pitch_belt 19216     0   19216    97.93089
## 12              min_yaw_belt     0 19216   19216    97.93089
## 13       amplitude_roll_belt 19216     0   19216    97.93089
## 14      amplitude_pitch_belt 19216     0   19216    97.93089
## 15        amplitude_yaw_belt     0 19216   19216    97.93089
## 16      var_total_accel_belt 19216     0   19216    97.93089
## 17             avg_roll_belt 19216     0   19216    97.93089
## 18          stddev_roll_belt 19216     0   19216    97.93089
## 19             var_roll_belt 19216     0   19216    97.93089
## 20            avg_pitch_belt 19216     0   19216    97.93089
## 21         stddev_pitch_belt 19216     0   19216    97.93089
## 22            var_pitch_belt 19216     0   19216    97.93089
## 23              avg_yaw_belt 19216     0   19216    97.93089
## 24           stddev_yaw_belt 19216     0   19216    97.93089
## 25              var_yaw_belt 19216     0   19216    97.93089
## 26             var_accel_arm 19216     0   19216    97.93089
## 27              avg_roll_arm 19216     0   19216    97.93089
## 28           stddev_roll_arm 19216     0   19216    97.93089
## 29              var_roll_arm 19216     0   19216    97.93089
## 30             avg_pitch_arm 19216     0   19216    97.93089
## 31          stddev_pitch_arm 19216     0   19216    97.93089
## 32             var_pitch_arm 19216     0   19216    97.93089
## 33               avg_yaw_arm 19216     0   19216    97.93089
## 34            stddev_yaw_arm 19216     0   19216    97.93089
## 35               var_yaw_arm 19216     0   19216    97.93089
## 36         kurtosis_roll_arm     0 19216   19216    97.93089
## 37        kurtosis_picth_arm     0 19216   19216    97.93089
## 38          kurtosis_yaw_arm     0 19216   19216    97.93089
## 39         skewness_roll_arm     0 19216   19216    97.93089
## 40        skewness_pitch_arm     0 19216   19216    97.93089
## 41          skewness_yaw_arm     0 19216   19216    97.93089
## 42              max_roll_arm 19216     0   19216    97.93089
## 43             max_picth_arm 19216     0   19216    97.93089
## 44               max_yaw_arm 19216     0   19216    97.93089
## 45              min_roll_arm 19216     0   19216    97.93089
## 46             min_pitch_arm 19216     0   19216    97.93089
## 47               min_yaw_arm 19216     0   19216    97.93089
## 48        amplitude_roll_arm 19216     0   19216    97.93089
## 49       amplitude_pitch_arm 19216     0   19216    97.93089
## 50         amplitude_yaw_arm 19216     0   19216    97.93089
## 51    kurtosis_roll_dumbbell     0 19216   19216    97.93089
## 52   kurtosis_picth_dumbbell     0 19216   19216    97.93089
## 53     kurtosis_yaw_dumbbell     0 19216   19216    97.93089
## 54    skewness_roll_dumbbell     0 19216   19216    97.93089
## 55   skewness_pitch_dumbbell     0 19216   19216    97.93089
## 56     skewness_yaw_dumbbell     0 19216   19216    97.93089
## 57         max_roll_dumbbell 19216     0   19216    97.93089
## 58        max_picth_dumbbell 19216     0   19216    97.93089
## 59          max_yaw_dumbbell     0 19216   19216    97.93089
## 60         min_roll_dumbbell 19216     0   19216    97.93089
## 61        min_pitch_dumbbell 19216     0   19216    97.93089
## 62          min_yaw_dumbbell     0 19216   19216    97.93089
## 63   amplitude_roll_dumbbell 19216     0   19216    97.93089
## 64  amplitude_pitch_dumbbell 19216     0   19216    97.93089
## 65    amplitude_yaw_dumbbell     0 19216   19216    97.93089
## 66        var_accel_dumbbell 19216     0   19216    97.93089
## 67         avg_roll_dumbbell 19216     0   19216    97.93089
## 68      stddev_roll_dumbbell 19216     0   19216    97.93089
## 69         var_roll_dumbbell 19216     0   19216    97.93089
## 70        avg_pitch_dumbbell 19216     0   19216    97.93089
## 71     stddev_pitch_dumbbell 19216     0   19216    97.93089
## 72        var_pitch_dumbbell 19216     0   19216    97.93089
## 73          avg_yaw_dumbbell 19216     0   19216    97.93089
## 74       stddev_yaw_dumbbell 19216     0   19216    97.93089
## 75          var_yaw_dumbbell 19216     0   19216    97.93089
## 76     kurtosis_roll_forearm     0 19216   19216    97.93089
## 77    kurtosis_picth_forearm     0 19216   19216    97.93089
## 78      kurtosis_yaw_forearm     0 19216   19216    97.93089
## 79     skewness_roll_forearm     0 19216   19216    97.93089
## 80    skewness_pitch_forearm     0 19216   19216    97.93089
## 81      skewness_yaw_forearm     0 19216   19216    97.93089
## 82          max_roll_forearm 19216     0   19216    97.93089
## 83         max_picth_forearm 19216     0   19216    97.93089
## 84           max_yaw_forearm     0 19216   19216    97.93089
## 85          min_roll_forearm 19216     0   19216    97.93089
## 86         min_pitch_forearm 19216     0   19216    97.93089
## 87           min_yaw_forearm     0 19216   19216    97.93089
## 88    amplitude_roll_forearm 19216     0   19216    97.93089
## 89   amplitude_pitch_forearm 19216     0   19216    97.93089
## 90     amplitude_yaw_forearm     0 19216   19216    97.93089
## 91         var_accel_forearm 19216     0   19216    97.93089
## 92          avg_roll_forearm 19216     0   19216    97.93089
## 93       stddev_roll_forearm 19216     0   19216    97.93089
## 94          var_roll_forearm 19216     0   19216    97.93089
## 95         avg_pitch_forearm 19216     0   19216    97.93089
## 96      stddev_pitch_forearm 19216     0   19216    97.93089
## 97         var_pitch_forearm 19216     0   19216    97.93089
## 98           avg_yaw_forearm 19216     0   19216    97.93089
## 99        stddev_yaw_forearm 19216     0   19216    97.93089
## 100          var_yaw_forearm 19216     0   19216    97.93089
```

100 of 160 variable are missing for 98% of the values. I exclude these and the other useless variable.


```r
# Select only column with values
not_na <- filter(na_blank, NaBlank == 0)
Col <- not_na$rowname
train <- select(train,one_of(Col))
# exclude other useless variables
train = train[-c(1,3,4,5,6,7)]
```

These are the columns used for the model.


```r
str(train)
```

```
## 'data.frame':	19622 obs. of  54 variables:
##  $ user_name           : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```


## Split data

I split the data in two group: one for build the model with 75% of cases, the second with the other 25% for test it.


```r
inTrain = createDataPartition(train$classe, p = 0.75, list=FALSE)
MyTrain = train[inTrain,]
MyTest = train[-inTrain,]
```

Now I use the training data for building the model


## Models

### Classification Tree

First, I try with a Classification Tree model.


```r
set.seed(26587)
modelTree <- train(classe~., data = MyTrain[-1], method="rpart")
modelTree
```

```
## CART 
## 
## 14718 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 14718, 14718, 14718, 14718, 14718, 14718, ... 
## Resampling results across tuning parameters:
## 
##   cp          Accuracy   Kappa     
##   0.03541251  0.5039932  0.35239439
##   0.05993861  0.4178394  0.21068340
##   0.11573151  0.3169400  0.04934575
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.03541251.
```

The accuracy is only 51%.  
I test the model.  


```r
predTree <- predict(modelTree, newdata = MyTest)
table(predTree,MyTest$classe)
```

```
##         
## predTree    A    B    C    D    E
##        A 1284  395  404  375  128
##        B   12  322   31  130  117
##        C   97  232  420  299  256
##        D    0    0    0    0    0
##        E    2    0    0    0  400
```

```r
TruePredictionTree <- data.frame(classe=MyTest$classe,esito=predTree==MyTest$classe)
frequenzeTree <- table(TruePredictionTree$classe,TruePredictionTree$esito)
frequenzeTree
```

```
##    
##     FALSE TRUE
##   A   111 1284
##   B   627  322
##   C   435  420
##   D   804    0
##   E   501  400
```

```r
prop.table(frequenzeTree,1)*100
```

```
##    
##          FALSE       TRUE
##   A   7.956989  92.043011
##   B  66.069547  33.930453
##   C  50.877193  49.122807
##   D 100.000000   0.000000
##   E  55.604883  44.395117
```

```r
colSums(frequenzeTree)
```

```
## FALSE  TRUE 
##  2478  2426
```

```r
colSums(frequenzeTree)/sum(frequenzeTree)
```

```
##     FALSE      TRUE 
## 0.5053018 0.4946982
```

As expected seeing the accuracy, not very well prediction, only about 50% true. Very bad in particular for classe D.


### Random Forest

I try with a Random Forest model, that should be more accurate.


```r
# NB: almost two hours of calculation for this model with my i5 2.20 GHz 8 GB RAM
set.seed(26587)
modelRF <- train(classe~., data = MyTrain[-1], method="rf")
modelRF
```

```
## Random Forest 
## 
## 14718 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 14718, 14718, 14718, 14718, 14718, 14718, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9888636  0.9859054
##   27    0.9886212  0.9855999
##   52    0.9790813  0.9735263
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

I test the Random Forest model.


```r
# Prediction on test with Random Forest model
predRF <- predict(modelRF, newdata = MyTest)
table(predRF,MyTest$classe)
```

```
##       
## predRF    A    B    C    D    E
##      A 1392    3    0    0    0
##      B    3  943    5    0    0
##      C    0    3  848    9    0
##      D    0    0    2  795    4
##      E    0    0    0    0  897
```

```r
TruePredictionRF <- data.frame(classe=MyTest$classe,esito=predRF==MyTest$classe)
frequenzeRF <- table(TruePredictionRF$classe,TruePredictionRF$esito)
frequenzeRF
```

```
##    
##     FALSE TRUE
##   A     3 1392
##   B     6  943
##   C     7  848
##   D     9  795
##   E     4  897
```

```r
prop.table(frequenzeRF,1)*100
```

```
##    
##          FALSE       TRUE
##   A  0.2150538 99.7849462
##   B  0.6322445 99.3677555
##   C  0.8187135 99.1812865
##   D  1.1194030 98.8805970
##   E  0.4439512 99.5560488
```

```r
colSums(frequenzeRF)
```

```
## FALSE  TRUE 
##    29  4875
```

```r
colSums(frequenzeRF)/sum(frequenzeRF)
```

```
##      FALSE       TRUE 
## 0.00591354 0.99408646
```

Random forest give us a very better prediction, 99,4% true: only 33 error on 4904 records.


## Prediction

I make the prediction on the 20 cases provided.


```r
# Final Prediction on the 20 test cases with Random Forest model
predict(modelRF, newdata = test)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
20*0.994
```

```
## [1] 19.88
```

I expect to have only 0.6% of error, so 20 on 20 or at least 19 on 20 of correct predictions.
