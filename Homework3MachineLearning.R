##Homework 3 Linear Models Ryan Leveille

#The dataset I have chosen is from the New York City Ozone dataset (airquality).
#a)

library(datasets)
attach(airquality)
library(tree)
library(randomForest)
airquality

airquality = na.omit(airquality)

set.seed(1)
train <- sample(1:nrow(airquality), .5*nrow(airquality))

ozone_tree <- tree(Ozone ~., data = airquality, subset = train)
plot(ozone_tree)
text(ozone_tree, pretty = 0)

ozone_test <- airquality[-train,'Ozone']

ozone_predict <- predict(ozone_tree, newdata = airquality[-train,])
#ozone MSE
mean((ozone_predict - ozone_test)^2)
#before pruning the MSE is 862.7823

#Pruning
cv_tree <- cv.tree(ozone_tree)
plot(cv_tree)
#Based off the plot the optimal tree is 3

prune_tree <- prune.tree(ozone_tree, best = 3)
plot(prune_tree)
text(prune_tree , pretty = 0)

predict_prune <- predict(prune_tree, newdata = airquality[-train,])

mean((predict_prune - ozone_test)^2)
#After pruning the MSE went down to 806.0138

#b) Bagging

p <- ncol(airquality)-1
B <- 1500

bagmodel <- randomForest(Ozone ~., data = airquality, subset = train, ntree = B, mtry = p, importance = TRUE)

bag_predict <- predict(bagmodel, newdata = airquality[-train,])

mean((bag_predict-ozone_test)^2)
#The MSE is now 724.8521
print(bagmodel)
table(bagmodel$oob.times)

plot(bagmodel$oob.times)
plot(bagmodel$mse)

# table(bagmodel$oob.times)
#503 512 515 522 523 526 529 531 533 534 536 537 538 539 540 541 542 543 544 545 546 548 549 551 552 
#1   1   3   1   1   1   1   1   1   1   1   3   1   1   2   1   2   1   1   2   1   2   1   3   1 
#553 554 555 556 557 558 560 561 563 564 566 570 580 
#2   2   3   1   2   2   1   1   1   2   1   1   1 

##include plot

#c)

set.seed(1)
random_ozone <- randomForest(Ozone ~., data=airquality, subset = train, mtry = p/4, importance = TRUE)
predict_random_ozone <- predict(random_ozone, newdata = airquality[-train,])
mean((predict_random_ozone-ozone_test)^2)
#The MSE for mtry = p is [1] 725.9452
#The MSE for mtry = p/2 is [1] 537.3218
#The MSE for mtry = p/3 is [1] 537.3218
#The MSE for mtry = sqrt(p) is [1] 537.3218
#The MSE for mtry =  p/4 is [1] 524.6814

#d)
#The model where MSE was lowest was randomforest

#Problem 4
linear_nueron <- function(input,weight,bias){
  
  for (i in seq(input)){
    x <- input[i]*weight[i] 
    sum_vec <- sum_vec + x
    i <- i + 1
   
  }  
  y <- sum_vec + bias
  print(y)
  
}

sum_vec <- 0
input <- c(4,-3,7,5,-1)
weight <- c(0.2, -.54, -.21, -.1, .33)
bias = .14

linear_nueron(input,weight,bias)
#linear_nueron(input,weight,bias)
#[1] 0.26






