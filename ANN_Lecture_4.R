#########
### http://www.bzst.com/2014/03/the-use-of-dummy-variables-in.html
#########

###########################################################################
### ANN for CLASSIFICATION:    Auto data example   ########################
##########################################################################


library(keras)

library(ISLR)
head(Auto,2)

Auto$name <- NULL

n <- nrow(Auto)
p <- ncol(Auto)-1

set.seed(1)
train <- sample(n, 0.8*n)

train_data <- Auto[train, 1:p]   
train_labels <- Auto[train, p+1]  

test_data <- Auto[-train, 1:p]   
test_labels <- Auto[-train, p+1]   




# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_data <- scale(train_data) 
train_data
train_data[1, ] # First training sample, normalized

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")


test_data <- scale(test_data, 
                   center = col_means_train, 
                   scale = col_stddevs_train)


#####
## For CLASSIFICATION, we got to ONE-HOT ENCODE the RESPONSE VARIABLE
#####

# One hot encode training response values
train_labels <- to_categorical(as.numeric(train_labels)-1)
# One hot encode testing response values
test_labels <- to_categorical(as.numeric(test_labels)-1)



###########
## SETTING THE SEED FOR NEURAL NETWORKS
###########

use_session_with_seed(1)

######
## Setting up layers
## First hidden layer - as usual.
## But the OUTPUT LAYER:  a softmax with 3 output nodes.
#######

model <- keras_model_sequential(layers=
                                  list(
                                    layer_dense(units = 5, activation = "relu",
                                                input_shape = dim(train_data)[2]),
                                    layer_dense(units = ncol(train_labels), activation = "softmax")
                                  ))

model

#kernel_initializer=initializer_random_uniform(

compile(model,
        loss = 'categorical_crossentropy',
        optimizer = 'adam',
        metrics = 'accuracy')



# Store the fitting history in `history` 
history <- fit(model,
               train_data, 
               train_labels, 
               epochs = 200,
               batch_size = 32, 
               validation_split = 0.2
)

# Plot the history
plot(history, smooth=F)

# Print training performance metrics
history


#### TEST PREDICTION

# Evaluate the model
score <- evaluate(model,
                  test_data, 
                  test_labels)

# Print the score
print(score)



################
### MORE EPOCHS with EARLY STOPPING
#################

use_session_with_seed(1)   # SETTING THE SEED

early_stop <- callback_early_stopping(monitor = "val_loss", 
                                      patience = 20)

model_earlystop <- keras_model_sequential(layers=list(
  layer_dense(units = 5, activation = "relu",
              input_shape = dim(train_data)[2]),
  layer_dense(units = ncol(train_labels), activation = "softmax")
))

model_earlystop

compile(model_earlystop,
        loss = 'categorical_crossentropy',
        optimizer = 'adam',
        metrics = 'accuracy'
)



# Store the fitting history in `history` 
history_earlystop <- fit(model_earlystop,
                         train_data, 
                         train_labels, 
                         epochs = 1000,
                         batch_size = 32, 
                         validation_split = 0.2,
                         callbacks = list(early_stop))

# Plot the history
plot(history_earlystop, smooth=F)

# Print training performance metrics
print(history_earlystop)

# Evaluate the model
score <- evaluate(model_earlystop,
                  test_data, 
                  test_labels)
# batch_size = 128)

# Print the score
print(score)




##############
## INCREASING # OF NODES + EARLY STOPPING
###############

use_session_with_seed(1)   # SETTING THE SEED

early_stop <- callback_early_stopping(monitor = "val_loss", 
                                      patience = 20)

model_IncrNodes <- keras_model_sequential(layers=
                                            list(
                                              layer_dense(units = 10, activation = "relu",
                                                          input_shape = dim(train_data)[2]),
                                              layer_dense(units = ncol(train_labels), activation = "softmax")
                                            ))

model_IncrNodes

#kernel_initializer=initializer_random_uniform(

compile(model_IncrNodes,
        loss = 'categorical_crossentropy',
        optimizer = 'adam',
        metrics = 'accuracy'
)



# Store the fitting history in `history` 
history_IncrNodes <- fit(model_IncrNodes,
                         train_data, 
                         train_labels, 
                         epochs = 1000,
                         batch_size = 32, 
                         validation_split = 0.2,
                         callbacks = list(early_stop))

# Plot the history
plot(history_IncrNodes, smooth=F)

# Print training performance metrics
print(history_IncrNodes)


#### TEST PREDICTION


# Evaluate the model

score <- evaluate(model_IncrNodes,
                  test_data, 
                  test_labels)
print(score)



### CONFUSION MATRIX of TEST PREDICTIONS

#### PREDICT ON TEST DATA

test.pred <- predict(model_IncrNodes, test_data)
head(test.pred,2)
tail(test.pred,2)

# Predict the classes for the test data
classes <- predict_classes(model_IncrNodes, test_data)
head(classes)

table(classes)

test_labels_num <- as.numeric(Auto[-train, p+1] )-1
# Confusion matrix
table(test_labels_num, 
      classes) 

