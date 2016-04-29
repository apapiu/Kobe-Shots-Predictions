library(ranger)
library(caret)
library(ggplot2)
library(xgboost)


load("data.Rdata")

sam <- sample(1:dim(train)[1], 20000)

model_ranger <- ranger(shot_made_flag ~
                           combined_shot_type +
                           shot_distance +
                           zerosecs +
                           lastmin +
                           old +
                           lon +
                           lat +
                           opponent +
                           DateTime +
                           action_type +
                           shot_zone_basic,
                       #importance = "impurity",
                       #probability = TRUE,
                       data = train,
                       num.trees = 50,
                       write.forest = TRUE)


#don't trust the OOB when you have a factor with lots of levels and few examples:
confusionMatrix(predict(model_ranger, train)$predictions, train$shot_made_flag)

predict(model_ranger, train)$predictions-> temp
confusionMatrix(1*(temp[,2] > 1/2), train$shot_made_flag)

model_ranger$variable.importance %>% sort() %>% barplot()

predict(model_ranger, test)$predictions -> rf_preds
solutions <- data.frame(shot_id = test$shot_id, shot_made_flag = preds[,2])
write.csv(solutions, "ranger3.csv", row.names = FALSE)


#Error Diagnostics:
train$preds <- predict(model_ranger, train)$predictions

ggplot(data = train, aes(x = lon, y = lat)) +
    geom_point(aes(color = preds), alpha = 0.7, size = 2) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void()

train$correct <- as.factor(train$preds == train$shot_made_flag)

ggplot(data = train[1:5000,], aes(x = lon, y = lat)) +
    geom_point(aes(color = correct), alpha = 0.7, size = 2) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void()

#~~~~~~~~~
# xgboost:
#~~~~~~~~~

X <- model.matrix(~ combined_shot_type +
                      #shot_distance +
                      ref_dist + #big improvment here from shot_dist
                      dist +
                      zerosecs +
                      close +
                      lastmin +
                      old +
                      lat +
                      lon +
                      playoffs +
                      away +
                      shot_type + 
                      season +
                      opponent +
                      DateTime +
                      action_type +
                      shot_zone_basic, data = train)[,-1]

X_test <- model.matrix(~  combined_shot_type +
                           #shot_distance +
                           ref_dist + #big improvment here from shot_dist
                           dist +
                           zerosecs +
                           close +
                           lastmin +
                           old +
                           lat +
                           lon +
                           playoffs +
                           away +
                           shot_type + 
                           season +
                           opponent +
                           DateTime +
                           action_type +
                           shot_zone_basic, data = test)[,-1]

y <- as.numeric(train$shot_made_flag) - 1

nrounds = 400
xgb.cv(data = X, label = y, 
       objective = "binary:logistic",
       eval_metric = "logloss",
       #eval_metric = "error",
       eta = 0.03, 
       max_depth = 6,
       nrounds = nrounds,
       gamma = 1,
       subsample = 0.8,
       colsample_bytree = 0.5,
       min_child_weight = 10,
       nfold = 10) -> cv.error



min(cv.error$test.logloss.mean)
which.min(cv.error$test.logloss.mean)
#6009 with ref_distance instead of shot_dist 
#6037 with 
#6043 with eta = 0.05, 250 trees
#6052 if you add time + 4 trees

#100 rounds, 4 depth, eta = 0.1, gamma = 1,  logloss = 0.609117
#100 rounds, 4 depth, eta = 0.1, gamma = 0.1,   logloss = 0.608691


ggplot(cv.error, aes(x = c(1: nrounds))) +
    geom_line(aes(y = train.logloss.mean), color = "green") +
    geom_line(aes(y = test.logloss.mean), color = "blue")

model_xgb <- xgboost(data = X, label = y, 
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     eta = 0.03, 
                     max_depth = 6,
                     nrounds = 260,
                     gamma = 0.5,
                     subsample = 0.8,
                     colsample_bytree = 0.5)

xgb.importance(colnames(X),model = model_xgb) -> imp_xgb

setdiff(colnames(X),imp_xgb$Feature)

predict(model_xgb, X_test) -> xgb_preds
hist(xgb_preds, breaks = 100)

xgb_preds[xgb_preds > 0.97] <- xgb_preds[xgb_preds > 0.97] - 0.03

solutions <- data.frame(shot_id = test$shot_id, shot_made_flag = xgb_preds)
write.csv(solutions, "xgb11.csv", row.names = FALSE)

#a bit of diagnostics:
predict(model_xgb, X) -> train_sols
train$preds <- train_sols

qplot(data = train, x= train_sols , binwidth = 0.01, fill = shot_made_flag)

train[train$action_type != "Jump Shot",] -> special_shot

plotly::ggplotly(
ggplot(special_shot, aes(x= preds, fill = factor(action_type))) + 
    geom_histogram(aes(y=..count../sum(..count..)))
    #scale_fill_brewer(palette = "Set1")
)
caret::confusionMatrix(1*(train$preds > 1/2), train$shot_made_flag)


#~~~~~~~~~~~~~~~~~~
#ensembling glmnet:
#~~~~~~~~~~~~~~~~~
set.seed(134)
replicate(50, {
    model_xgb <- xgboost(data = X, label = y, 
                         objective = "binary:logistic",
                         eval_metric = "logloss",
                         eta = 0.03, 
                         max_depth = sample(7:8,1),
                         nrounds = 240,
                         gamma = 0.1,
                         subsample = sample(c(0.8,1), 1),
                         colsample_bytree = sample(c(0.5,0.8), 1))
    predict(model_xgb, X_test)}) -> ensem_preds

rowMeans(ensem_preds) -> ensemble_predictions

hist(ensemble_predictions, breaks = 100)

ensemble_predictions[ensemble_predictions > 0.97] <- 
    ensemble_predictions[ensemble_predictions > 0.97] - 0.03

solutions <- data.frame(shot_id = test$shot_id, shot_made_flag = ensemble_predictions)
write.csv(solutions, "xgb3_ensem.csv", row.names = FALSE)


#~~~~~~~
#glmnet:
#~~~~~~
library(glmnet)

model_glm <- cv.glmnet(x = X, y = train$shot_made_flag,
                    family = "binomial")

model_glm$lambda.min
plot(model_glm)

predict(model_glm, s = model_glm$lambda.min, newx = X_test, type = "response") -> temp

preds <- as.numeric((temp + xgb_preds)/2)

solutions <- data.frame(shot_id = test$shot_id, shot_made_flag = as.numeric(temp))
write.csv(solutions, "xgbglmnetplusdate.csv", row.names = FALSE)

qplot(data = solutions, x = shot_made_flag, binwidth = 0.01)


#~~~~
#knn:
#~~~~
library(class)
library(e1071)

geo <- train[,c("lat","lon")]

geo$lat <- geo$lat + rnorm(n = dim(geo)[1], sd = 1/100)

model_knn <- knn(geo, test = test[,c("lat","lon")],
                 cl = as.factor(y), k = 158, prob = TRUE)

model_knn <- knn(geo, test = geo, cl = as.factor(y), k = 100, prob = TRUE)



#some xgb+rf ensemble:
hist(xgb_preds)
hist(rf_preds[,2])
preds <- (xgb_preds+rf_preds[,2])/2
hist(preds)
solutions <- data.frame(shot_id = test$shot_id, shot_made_flag = preds)
write.csv(solutions, "xgb8+rf.csv", row.names = FALSE)
#not useful
