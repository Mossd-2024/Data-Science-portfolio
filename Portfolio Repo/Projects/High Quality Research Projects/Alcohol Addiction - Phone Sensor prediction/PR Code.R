##ML Syntax placeholder for pre-registation
###Passive Sensing registration as code
library(ranger)
library(caret)
library(magrittr)
library(Metrics)
library(tuneRanger)
library(glmnet)
library(tensorflow)
library(keras)
library(dplyr)
library(magrittr)
set.seed(13024)
#Read in data
data<-readRDS("sms.passive.rds")

#Define Data variables
features<-c()#Here we will add a list of aggregated features from the passive sensing data

out<-c("audit") #Outcome

##Split sample
data<-data[complete.cases(data[,c(out,features)]),c(out,features)]
smp_size <- floor(0.70 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
df_training<- data[train_ind, ]
df_test <- data[-train_ind, ]

#Scale the predictors
df_training[,3:nrow(data)]<-df_training[,3:nrow(data)]%>%scale(.)
df_test[,3:nrow(data)]<-df_test[,3:nrow(data)]%>%scale(.)


#Create the cross-validation folds
cv_folds <- sample.int(10, size = nrow(df_training), replace = TRUE) #This assigns each row to a CV fold

#Random Forest with tuneRanger and ranger
rfRes <- list()  #Empty list

#Create RF formula
formula.2<-reformulate(features, response = out)

#N of trees to use
ntree<-1000
#N of vars to try for each tree
mtry  <- round(sqrt(ncol(df_training)))

cv_split <- trainControl(method = "cv", 
                         number = 10,
                         search="grid",
                         index = lapply(1:max(cv_folds), #Applies existing cv folds
                         function(i){which(cv_folds == i)}), 
                         verboseIter = TRUE, 
                         allowParallel = TRUE) 
#Model training
cv_model <- caret::train(formula.2, 
                         data = df_training,
                         method = "ranger",
                         num.trees = ntree,
                         trControl = cv_split, 
                         verbose = T,
                         importance = "permutation")
rfRes[["final_model"]] <- cv_model
ggplot(rfRes$final_model) #Plots model performance across tuning parameters

#Estimate RMSE on new data and store in the list
rf_pred <- predict(rfRes$final_model, df_test)
fit<-c(rfRes$final_model$finalModel$mtry,
       rfRes$final_model$finalModel$min.node.size,
       rfRes$final_model$finalModel$prediction.error,
       rfRes$final_model$finalModel$prediction.error%>%sqrt(),
       rfRes$final_model$finalModel$r.squared,
       postResample(pred = rf_pred, obs = df_test$c.abmrf.QF))
rfRes[["fit"]] <-fit
names(rfRes$fit) <- c("mtry", "min.node.size","mse", "train_rmse", "train_r2",
                      paste0("test_", c("rmse", "r2","MAE")))

#Accuracy of predictions
df_test %>%
mutate(predicted = predict(rfRes$final_model, df_test)) %>%
  ggplot(aes(predicted, c.abmrf.QF)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") +  theme_bw(18)

#Plotting the relative Importance of each predictor
#https://brunaw.com/slides/rladies-dublin/RF/intro-to-rf.html#30
varimp <- varImp(cv_model)$importance #Variable importance
rfRes[["varimp"]]  <-varimp
imps <- data.frame(var = features,
                   imps = rfRes[["varimp"]][[1]])
imps %>%
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)


#LASSO step 1. In the training data, select the value of lambda using 10 fold CV
lassoRes<-list()
lasso.tune<-cv.glmnet(x=df_training[,features]%>%as.matrix(),
                       y=df_training[,out]%>%as.matrix(),
                       foldid = cv_folds,
                       nlambda=100,
                       alpha=1,
                       family= "poisson")
lassoRes[["tuning"]]<-lasso.tune
#LASSO step 2. Then, using the same CV folds in the training data, use the CV estimated value of lambda to obtain lasso estimates
lasso.train<-glmnet(x=df_training[,features]%>%as.matrix(),
                      y=df_training[,out]%>%as.matrix(),
                      foldid = cv_folds,
                      lambda=lasso.tune$lambda.1se,
                      alpha=1,
                      family= "poisson")
lassoRes[["final_model"]]<-lasso.train
#df_training RMSE
pred.lasso.1se<-predict(lassoRes[["final_model"]],newx = df_training[,features]%>%as.matrix(),type="response")
train.lasso.rmse.1se<-rmse(df_training[,out]%>%as.matrix(),pred.lasso.1se)

#LASSO step 3. Predict values of the outcome in the new data and compute RMSE
pred.lasso.1se.test<-predict(lassoRes[["final_model"]],newx = df_test[,features]%>%as.matrix(),type="response")
train.lasso.rmse.1se.test<-rmse(df_test[,out]%>%as.matrix(),pred.lasso.1se.test)

lassoRes[["fit"]] <-c(lasso.tune$lambda.1se,
                      train.lasso.rmse.1se,
                      train.lasso.rmse.1se.test,
                      coef(lassoRes[["final_model"]]))
  
names(lassoRes$fit) <- c("lambda", "train_rmse", "test_rmse","coef_matrix")



##Deep Neural Network with multiple inputs 
# Separate the target value—the “label”—from the features. This label is the value that you will train the model to predict.

train_features <- df_training[, -match(out, names(df_training))]
test_features <- df_test[, -match(out, names(df_test))]

train_labels <- df_training[[out]]
test_labels <- df_test[[out]]

normalizer <- layer_normalization(axis = -1L)
# Then, fit the state of the preprocessing layer to the data by calling adapt():
adapt(normalizer, as.matrix(train_features))

# Calculate the mean and variance, and store them in the layer:
 
print(normalizer$mean)

# When the layer is called, it returns the input data, with each feature independently normalized.
first <- as.matrix(train_features[1,])

first <- as.matrix(train_features[1,])
build_and_compile_model <- function(norm) {
  model <- keras_model_sequential() |>
    norm() |>
    layer_dense(64, activation = 'relu') |>
    layer_dense(64, activation = 'relu') |>
    layer_dense(1)
 
  model |> compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(0.001)
  )
 
  model
}

dnn_model <- build_and_compile_model(normalizer)
summary(dnn_model)

history <- dnn_model |> fit(
  as.matrix(train_features),
  as.matrix(train_labels),
  validation_split = 0.2,
  verbose = 0,
  epochs = 100
)

plot(history)

# Collect the results on the test set:
dnnRes <- list()
dnnRes[['dnn_model']] <- dnn_model |> evaluate(
    as.matrix(test_features),
    as.matrix(test_labels),
    verbose = 0
  )

#Visualize predictions
ggplot(data.frame(pred = as.numeric(test_predictions), audit = test_labels)) +
  geom_point(aes(x = pred, y = audit)) +
  geom_abline(intercept = 0, slope = 1, color = "blue")

#Compute RMSE
train.dnn.rmse<-rmse(as.matrix(train_labels),
                           predict(dnn_model, as.matrix(train_features)))
test.dnn.rmse<-rmse(as.matrix(test_labels),
                           predict(dnn_model, as.matrix(test_features)))

dnnRes[['fit']]<-c(train.dnn.rmse, test.dnn.rmse)
names(dnnRes$fit) <- c("train_rmse", "test_rmse")
  

#Collect RMSE across final models and compare in training and test sets
rfRes[["fit"]][c(4,6)]
lassoRes[["fit"]][2:3]
dnnRes[['fit']]