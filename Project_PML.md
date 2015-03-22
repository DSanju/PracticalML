---
title: "Project_PracticalML"
author: "Sanjeev Kumar"
date: "Sunday, March 22, 2015"
output: html_document
---
Devices such as Jawbone Up, Nike FuelBand, and Fitbit enable us to collect data about personal physical activities in realtime fashion.  In this project, goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set that can use any of the other variables to predict with.

#Loading data

```r
#packages required for analysis
require(caret)
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
require(ggplot2)
require(corrplot)
```

```
## Loading required package: corrplot
```

```r
require(randomForest)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
#Uploading data in R
rm(list = ls())
setwd("C:/Users/Sanjeev/PracticalML")
PML = read.csv("pml-training.csv", na.strings = c("NA", "")) #adding NA for missing value
PML_test <- read.csv("pml-testing.csv", na.strings = c("NA", "")) #adding NA for missing value
# Understanding data variable for future manipulations
sapply(PML,class)
```

```
##                        X                user_name     raw_timestamp_part_1 
##                "integer"                 "factor"                "integer" 
##     raw_timestamp_part_2           cvtd_timestamp               new_window 
##                "integer"                 "factor"                 "factor" 
##               num_window                roll_belt               pitch_belt 
##                "integer"                "numeric"                "numeric" 
##                 yaw_belt         total_accel_belt       kurtosis_roll_belt 
##                "numeric"                "integer"                 "factor" 
##      kurtosis_picth_belt        kurtosis_yaw_belt       skewness_roll_belt 
##                 "factor"                 "factor"                 "factor" 
##     skewness_roll_belt.1        skewness_yaw_belt            max_roll_belt 
##                 "factor"                 "factor"                "numeric" 
##           max_picth_belt             max_yaw_belt            min_roll_belt 
##                "integer"                 "factor"                "numeric" 
##           min_pitch_belt             min_yaw_belt      amplitude_roll_belt 
##                "integer"                 "factor"                "numeric" 
##     amplitude_pitch_belt       amplitude_yaw_belt     var_total_accel_belt 
##                "integer"                 "factor"                "numeric" 
##            avg_roll_belt         stddev_roll_belt            var_roll_belt 
##                "numeric"                "numeric"                "numeric" 
##           avg_pitch_belt        stddev_pitch_belt           var_pitch_belt 
##                "numeric"                "numeric"                "numeric" 
##             avg_yaw_belt          stddev_yaw_belt             var_yaw_belt 
##                "numeric"                "numeric"                "numeric" 
##             gyros_belt_x             gyros_belt_y             gyros_belt_z 
##                "numeric"                "numeric"                "numeric" 
##             accel_belt_x             accel_belt_y             accel_belt_z 
##                "integer"                "integer"                "integer" 
##            magnet_belt_x            magnet_belt_y            magnet_belt_z 
##                "integer"                "integer"                "integer" 
##                 roll_arm                pitch_arm                  yaw_arm 
##                "numeric"                "numeric"                "numeric" 
##          total_accel_arm            var_accel_arm             avg_roll_arm 
##                "integer"                "numeric"                "numeric" 
##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
##                "numeric"                "numeric"                "numeric" 
##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
##                "numeric"                "numeric"                "numeric" 
##           stddev_yaw_arm              var_yaw_arm              gyros_arm_x 
##                "numeric"                "numeric"                "numeric" 
##              gyros_arm_y              gyros_arm_z              accel_arm_x 
##                "numeric"                "numeric"                "integer" 
##              accel_arm_y              accel_arm_z             magnet_arm_x 
##                "integer"                "integer"                "integer" 
##             magnet_arm_y             magnet_arm_z        kurtosis_roll_arm 
##                "integer"                "integer"                 "factor" 
##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
##                 "factor"                 "factor"                 "factor" 
##       skewness_pitch_arm         skewness_yaw_arm             max_roll_arm 
##                 "factor"                 "factor"                "numeric" 
##            max_picth_arm              max_yaw_arm             min_roll_arm 
##                "numeric"                "integer"                "numeric" 
##            min_pitch_arm              min_yaw_arm       amplitude_roll_arm 
##                "numeric"                "integer"                "numeric" 
##      amplitude_pitch_arm        amplitude_yaw_arm            roll_dumbbell 
##                "numeric"                "integer"                "numeric" 
##           pitch_dumbbell             yaw_dumbbell   kurtosis_roll_dumbbell 
##                "numeric"                "numeric"                 "factor" 
##  kurtosis_picth_dumbbell    kurtosis_yaw_dumbbell   skewness_roll_dumbbell 
##                 "factor"                 "factor"                 "factor" 
##  skewness_pitch_dumbbell    skewness_yaw_dumbbell        max_roll_dumbbell 
##                 "factor"                 "factor"                "numeric" 
##       max_picth_dumbbell         max_yaw_dumbbell        min_roll_dumbbell 
##                "numeric"                 "factor"                "numeric" 
##       min_pitch_dumbbell         min_yaw_dumbbell  amplitude_roll_dumbbell 
##                "numeric"                 "factor"                "numeric" 
## amplitude_pitch_dumbbell   amplitude_yaw_dumbbell     total_accel_dumbbell 
##                "numeric"                 "factor"                "integer" 
##       var_accel_dumbbell        avg_roll_dumbbell     stddev_roll_dumbbell 
##                "numeric"                "numeric"                "numeric" 
##        var_roll_dumbbell       avg_pitch_dumbbell    stddev_pitch_dumbbell 
##                "numeric"                "numeric"                "numeric" 
##       var_pitch_dumbbell         avg_yaw_dumbbell      stddev_yaw_dumbbell 
##                "numeric"                "numeric"                "numeric" 
##         var_yaw_dumbbell         gyros_dumbbell_x         gyros_dumbbell_y 
##                "numeric"                "numeric"                "numeric" 
##         gyros_dumbbell_z         accel_dumbbell_x         accel_dumbbell_y 
##                "numeric"                "integer"                "integer" 
##         accel_dumbbell_z        magnet_dumbbell_x        magnet_dumbbell_y 
##                "integer"                "integer"                "integer" 
##        magnet_dumbbell_z             roll_forearm            pitch_forearm 
##                "numeric"                "numeric"                "numeric" 
##              yaw_forearm    kurtosis_roll_forearm   kurtosis_picth_forearm 
##                "numeric"                 "factor"                 "factor" 
##     kurtosis_yaw_forearm    skewness_roll_forearm   skewness_pitch_forearm 
##                 "factor"                 "factor"                 "factor" 
##     skewness_yaw_forearm         max_roll_forearm        max_picth_forearm 
##                 "factor"                "numeric"                "numeric" 
##          max_yaw_forearm         min_roll_forearm        min_pitch_forearm 
##                 "factor"                "numeric"                "numeric" 
##          min_yaw_forearm   amplitude_roll_forearm  amplitude_pitch_forearm 
##                 "factor"                "numeric"                "numeric" 
##    amplitude_yaw_forearm      total_accel_forearm        var_accel_forearm 
##                 "factor"                "integer"                "numeric" 
##         avg_roll_forearm      stddev_roll_forearm         var_roll_forearm 
##                "numeric"                "numeric"                "numeric" 
##        avg_pitch_forearm     stddev_pitch_forearm        var_pitch_forearm 
##                "numeric"                "numeric"                "numeric" 
##          avg_yaw_forearm       stddev_yaw_forearm          var_yaw_forearm 
##                "numeric"                "numeric"                "numeric" 
##          gyros_forearm_x          gyros_forearm_y          gyros_forearm_z 
##                "numeric"                "numeric"                "numeric" 
##          accel_forearm_x          accel_forearm_y          accel_forearm_z 
##                "integer"                "integer"                "integer" 
##         magnet_forearm_x         magnet_forearm_y         magnet_forearm_z 
##                "integer"                "numeric"                "numeric" 
##                   classe 
##                 "factor"
```

#Data manipulation

```r
#Removing variables not important
PML_1 <- PML[, -(grep("timestamp|X|user_name|num_window|new_window", names(PML)))] #removed variable 1 to 7 that was not required for analysis
PML_test1 <- PML_test[, -(grep("timestamp|X|user_name|num_window|new_window", names(PML_test)))]
#Identifying NAs from dataset for removing it later
NAs <- apply(PML_1, 2, function(x) {
    sum(is.na(x))})
PML_final <- PML_1[, which(NAs == 0)]
PML_test_final <- PML_test1[, which(NAs == 0)]
head(PML_final)#No NA in the observation
```

```
##   roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x gyros_belt_y
## 1      1.41       8.07    -94.4                3         0.00         0.00
## 2      1.41       8.07    -94.4                3         0.02         0.00
## 3      1.42       8.07    -94.4                3         0.00         0.00
## 4      1.48       8.05    -94.4                3         0.02         0.00
## 5      1.48       8.07    -94.4                3         0.02         0.02
## 6      1.45       8.06    -94.4                3         0.02         0.00
##   gyros_belt_z accel_belt_x accel_belt_y accel_belt_z magnet_belt_x
## 1        -0.02          -21            4           22            -3
## 2        -0.02          -22            4           22            -7
## 3        -0.02          -20            5           23            -2
## 4        -0.03          -22            3           21            -6
## 5        -0.02          -21            2           24            -6
## 6        -0.02          -21            4           21             0
##   magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm
## 1           599          -313     -128      22.5    -161              34
## 2           608          -311     -128      22.5    -161              34
## 3           600          -305     -128      22.5    -161              34
## 4           604          -310     -128      22.1    -161              34
## 5           600          -302     -128      22.1    -161              34
## 6           603          -312     -128      22.0    -161              34
##   gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y accel_arm_z
## 1        0.00        0.00       -0.02        -288         109        -123
## 2        0.02       -0.02       -0.02        -290         110        -125
## 3        0.02       -0.02       -0.02        -289         110        -126
## 4        0.02       -0.03        0.02        -289         111        -123
## 5        0.00       -0.03        0.00        -289         111        -123
## 6        0.02       -0.03        0.00        -289         111        -122
##   magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell
## 1         -368          337          516      13.05217      -70.49400
## 2         -369          337          513      13.13074      -70.63751
## 3         -368          344          513      12.85075      -70.27812
## 4         -372          344          512      13.43120      -70.39379
## 5         -374          337          506      13.37872      -70.42856
## 6         -369          342          513      13.38246      -70.81759
##   yaw_dumbbell total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y
## 1    -84.87394                   37                0            -0.02
## 2    -84.71065                   37                0            -0.02
## 3    -85.14078                   37                0            -0.02
## 4    -84.87363                   37                0            -0.02
## 5    -84.85306                   37                0            -0.02
## 6    -84.46500                   37                0            -0.02
##   gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
## 1             0.00             -234               47             -271
## 2             0.00             -233               47             -269
## 3             0.00             -232               46             -270
## 4            -0.02             -232               48             -269
## 5             0.00             -233               48             -270
## 6             0.00             -234               48             -269
##   magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
## 1              -559               293               -65         28.4
## 2              -555               296               -64         28.3
## 3              -561               298               -63         28.3
## 4              -552               303               -60         28.1
## 5              -554               292               -68         28.0
## 6              -558               294               -66         27.9
##   pitch_forearm yaw_forearm total_accel_forearm gyros_forearm_x
## 1         -63.9        -153                  36            0.03
## 2         -63.9        -153                  36            0.02
## 3         -63.9        -152                  36            0.03
## 4         -63.9        -152                  36            0.02
## 5         -63.9        -152                  36            0.02
## 6         -63.9        -152                  36            0.02
##   gyros_forearm_y gyros_forearm_z accel_forearm_x accel_forearm_y
## 1            0.00           -0.02             192             203
## 2            0.00           -0.02             192             203
## 3           -0.02            0.00             196             204
## 4           -0.02            0.00             189             206
## 5            0.00           -0.02             189             206
## 6           -0.02           -0.03             193             203
##   accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z
## 1            -215              -17              654              476
## 2            -216              -18              661              473
## 3            -213              -18              658              469
## 4            -214              -16              658              469
## 5            -214              -17              655              473
## 6            -215               -9              660              478
##   classe
## 1      A
## 2      A
## 3      A
## 4      A
## 5      A
## 6      A
```

```r
#Splitting PML data set for training and test
inTrain <- createDataPartition(PML_final$classe, p = 0.6, list = FALSE)
training <- PML_final[inTrain, ]
test <- PML_final[-inTrain, ]
```
#Building up and testing the model

```r
set.seed(33833)
rf <- train(training$classe~.,method="rf",data=training, prof = TRUE,
            trControl = trainControl(method = "boot", number = 5, allowParallel = TRUE)) #randomForest
print(rf)
```

```
## Random Forest 
## 
## 11776 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## 
## Summary of sample sizes: 11776, 11776, 11776, 11776, 11776 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9875048  0.9841996  0.001769857  0.002216375
##   27    0.9870473  0.9836238  0.003152194  0.003962155
##   52    0.9745953  0.9678774  0.004293509  0.005386406
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```r
vi1 <- varImp(rf,type=2) # importance of variable in the model
test$rightprediction <- predict(rf, test) == test$classe
summary(test$rightprediction)
```

```
##    Mode   FALSE    TRUE    NA's 
## logical      75    7771       0
```

```r
table(predict(rf, test), test$classe)
```

```
##    
##        A    B    C    D    E
##   A 2232   18    0    0    0
##   B    0 1496    7    0    0
##   C    0    4 1361   40    1
##   D    0    0    0 1246    5
##   E    0    0    0    0 1436
```

```r
CrossValidated<- postResample((predict(rf, test)), test$classe)
CrossValidated
```

```
##  Accuracy     Kappa 
## 0.9904410 0.9879052
```

#Trying the ConfussionMatrix


```r
set.seed(33833)
CrossValidatedError <- confusionMatrix((predict(rf, test)), test$classe)
CrossValidatedError
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232   18    0    0    0
##          B    0 1496    7    0    0
##          C    0    4 1361   40    1
##          D    0    0    0 1246    5
##          E    0    0    0    0 1436
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9904         
##                  95% CI : (0.988, 0.9925)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9879         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9855   0.9949   0.9689   0.9958
## Specificity            0.9968   0.9989   0.9931   0.9992   1.0000
## Pos Pred Value         0.9920   0.9953   0.9680   0.9960   1.0000
## Neg Pred Value         1.0000   0.9965   0.9989   0.9939   0.9991
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1907   0.1735   0.1588   0.1830
## Detection Prevalence   0.2868   0.1916   0.1792   0.1594   0.1830
## Balanced Accuracy      0.9984   0.9922   0.9940   0.9841   0.9979
```
#PartB: The 20 cases to predict

```r
Model.Prediction2 <- predict(rf, PML_test_final)
Model.Prediction2
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
