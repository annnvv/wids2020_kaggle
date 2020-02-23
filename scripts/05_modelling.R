  # https://towardsdatascience.com/feature-selection-using-regularisation-a3678b71e499
  
  rm(list = ls())
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  library(e1071)
  library(ranger)
  library(parallel)
  library(doParallel)

  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  # train_all_sub <- read.csv(paste0(proj_path, "/clean_data/train_cleaned_sub.csv"), stringsAsFactors = FALSE)
  train_all <- read.csv(paste0(proj_path, "/clean_data/train_cleaned.csv"), stringsAsFactors = FALSE)
  str(train_all[0:10])
  
  # test_all_sub <- read.csv(paste0(proj_path, "/clean_data/test_cleaned_sub.csv"), stringsAsFactors = FALSE)
  test_all <- read.csv(paste0(proj_path, "/clean_data/test_cleaned.csv"), stringsAsFactors = FALSE)
  str(test_all[0:10])  
  
  # create raw_train/test identifier
  train_all$cat <- "train"
  test_all$cat <- "test"
  
  #append all of the rows together
  both <- rbind(train_all, test_all)
  rm(train_all, test_all)
  
  # change hostpital_id and icu_id to factors
  both$hosp_death <- as.factor(both$hosp_death)
  both$hosp_id <- as.factor(both$hosp_id)
  both$icu_id <- as.factor(both$icu_id)
  both$gender <- as.factor(both$gender)
  str(both[0:10])
  
  train_all <- both[both$cat == "train", ]
  train_all <- train_all[ , c(1:length(train_all)-1)]
  
  test_all <- both[both$cat == "test", ]
  test_all <- test_all[ , c(1:length(test_all)-1)]
  
  rm(both)
  
  # create train and validation sets
  set.seed(14441)
  index <- createDataPartition(train_all$hosp_death, p=0.5, list = FALSE)
  train <- train_all[index,]
  val <- train_all[-index,]
  rm(index)
  
  form <- as.formula(paste0(paste((names(train)[2]), collapse = " + "), "~", 
                            paste0((names(train)[3:length(train)]), collapse = " + ")))
  
  no_cats <- as.formula(paste0(paste((names(train)[2]), collapse = " + "), "~", 
                               paste0((names(train)[c(4:8, 10:length(train))]), collapse = " + ")))
  
  #### functions ----
  caret_train <- function(model_method, tune_len, trainCntrl = NA) {
    fit <- train(form, data = train_all, method = model_method,
                 trControl = trainCntrl, preProcess = c("center","scale"),
                 tuneLength = tune_len, 
                 importance = TRUE)
  }
  
  train_val <- function(model, type){
    dt_predTrain <- predict(model, train, type = type)
    # Checking classification accuracy
    print(prop.table(table(dt_predTrain, train$hosp_death))*100)
    
    dt_predVal <- predict(model, val, type = type)
    # Checking classification accuracy - out of sample
    print(prop.table(table(dt_predVal, val$hosp_death))*100)  
  }
  
  submission_func <- function(pred_obj, file_name){
    # Purpose: a function to create a submission for the kaggle competition
    # Input: a prediction object and file name for .csv
    # Output: is a .csv file
    
    test_all$hosp_death <- pred_obj
    submission_dt <- cbind.data.frame(test_all$encounter_id, pred_obj)  
    names(submission_dt) <- c("encounter_id", "hospital_death")
    submission_dt$hospital_death <- as.numeric(as.character(submission_dt$hospital_death))
    write.csv(submission_dt, paste0(proj_path, "/submissions/", file_name), row.names = FALSE)  
  }
  
  train_all2 <- train_all
  train_all2$hosp_death <- as.numeric(as.character(train_all2$hosp_death))
  
  # #### Linear Regression ----
  lmFit <- lm(no_cats, data = train_all2[, c(2,4:8,10:length(train_all2))],)
  summary(lmFit)
  
  lm_predTest <- predict(lmFit, test_all, type = "response")
  
  submission_func(lm_predTest, "linear_reg_submission_baseline_20200222.csv") #kaggle auc: 0.87171
  # lm_predTest_bound <- lm_predTest
  # lm_predTest_bound[lm_predTest_bound<0] <- 0
  # lm_predTest_bound[lm_predTest_bound>1] <- 1
  # submission_func(lm_predTest_bound, "linear_reg_submission_bound_20200217.csv") #kaggle auc:0.87340
  
  saveRDS(lmFit, paste0(proj_path, "/models/linear_reg_submission_baseline_20200222.RDS"))
  
  # rm(lmFit, lm_predTest)
  
  #### Logistic Regression ----
  glmFit <- glm(no_cats, data = train_all[, c(2,4:8, 10:length(train_all))],  
                method = "glm.fit", family = "binomial")
  summary(glmFit)
  
  glm_predTest <- stats::predict(glmFit, test_all, type = "response")
  
  submission_func(glm_predTest, "logit_submission_baseline_20200222.csv")  #kaggle auc: 0.88355
  saveRDS(glmFit, paste0(proj_path, "/models/logit_submission_baseline_20200222.RDS"))
  
  #### Decision Trees ----
  dfFit <- rpart(form, method = "class", data = train_all, cp = 0.01)
  beep()
  rpart.plot(dfFit, shadow.col="gray", nn = TRUE)
  result.opt <- as.data.frame(printcp(dfFit))
  
  train_val(dfFit, type = "class")
  
  dt_predTest <- predict(dfFit, test_all, type = "prob")
  
  submission_func(dt_predTest[,2], "decision_tree_submission_baseline_20200222.csv") #kaggle auc: 0.71434 
  saveRDS(dfFit, paste0(proj_path, "/models/decision_tree_submission_baseline_20200222.RDS"))
  
  rm(result.opt, dfFit, dt_predTest)
  
  #ctrl <- trainControl(method="repeatedcv", repeats = 3) #classProbs=TRUE, summaryFunction = twoClassSummary
  
  # #### KNN ----
  # # knnFit <- caret_train("knn", 5, ctrl)
  # # knnFit <- train(form, data = train_all, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 11)
  # 
  # knn <- train(no_cats, data = train_all, method = "knn", 
  #              metric = "accuracy", tuneGrid = data.frame(k=1))
  # saveRDS(knn, paste0(proj_path, "/models/knn1_submission_20200219.RDS"))
  # 
  # knn
  # train_val(knn, type= "raw")
  # 
  # knn_predTest <- predict(knn, test_all, type = "prob")
  # 
  # submission_func(knn_predTest[,2], "knn1_submission_20200219.csv") #kaggle auc: 0.56314
  
  #### Support Vector Machine (SVM) Radial ####  ----
  svmRFit <- svm(no_cats, data = train_all, kernel = "radial", 
                 scale = FALSE, probability = TRUE)
  saveRDS(svmRFit, paste0(proj_path, "/models/svmR_submission_20200219.RDS"))
  
  train_val(svmRFit, type = "raw")
  
  svmR_predTest <- predict(svmRFit, test_all[, c(4:8, 10:length(test_all))], type = "class")
  svmR_predTest2 <- predict(svmRFit, test_all[, c(4:8, 10:length(test_all))], type = "prob")
  submission_func(svmR_predTest2, "svmR_submission_20200219.csv")  #kaggle auc: 0.50000
  svmR_predTest3 <- predict(svmRFit, test_all[, c(4:8, 10:length(test_all))], type = "raw")
  submission_func(svmR_predTest3, "svmR_submission2_20200219.csv")  
  
  # plot(svmRFit, train_all, no_cats)
  
  #### Random Forest ---- 
  # ctrl_par <- trainControl(method="repeatedcv", repeats = 3, allowParallel = TRUE)
  # rfFit <- caret_train("rf", 9, ctrl_par)
  #Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data
  #https://arxiv.org/pdf/1508.04409.pdf
  threads <-  detectCores()-1
  # rfFit <- ranger(no_cats, data = train_all, num.trees = 3, probability = TRUE,  
  #                seed = 14441, num.threads = threads) #classification = TRUE
  # saveRDS(rfFit, paste0(proj_path, "/models/ranger_test_20200221.RDS"))
  # rf_test_predTest <- predict(rfFit, test_all)
  # View(rf_test_predTest$predictions)
  # submission_func(rf_test_predTest$predictions[,2], "range_test_20200221.csv")  #kaggle auc: 0.77093
  # 
  # rfFit <- ranger(no_cats, data = train_all, num.trees = 50, probability = TRUE,  
  #                 seed = 14441, num.threads = threads) 
  # saveRDS(rfFit, paste0(proj_path, "/models/ranger_submission_20200221.RDS"))
  # rf_test_predTest <- predict(rfFit, test_all)
  # View(rf_test_predTest$predictions)
  # submission_func(rf_test_predTest$predictions[,2], "ranger_submission_20200221.csv")  #kaggle auc:0.87902
  # rfFit200 <- ranger(no_cats, data = train_all, num.trees = 200, probability = TRUE,  
  #                 seed = 14441, num.threads = threads) 
  # saveRDS(rfFit200, paste0(proj_path, "/models/ranger_200_submission_20200221.RDS"))
  # rf200_test_predTest <- predict(rfFit200, test_all)
  # View(rf200_test_predTest$predictions)
  # submission_func(rf200_test_predTest$predictions[,2], "ranger_200_submission_20200221.csv")  #kaggle auc:0.88480
  # rfFit500 <- ranger(no_cats, data = train_all, num.trees = 500, probability = TRUE,  
  #                    seed = 14441, num.threads = threads) 
  # saveRDS(rfFit500, paste0(proj_path, "/models/ranger_500_submission_20200221.RDS"))
  # rf500_test_predTest <- predict(rfFit500, test_all)
  # View(rf500_test_predTest$predictions)
  # submission_func(rf500_test_predTest$predictions[,2], "ranger_500_submission_20200221.csv")  #kaggle auc:0.88632
  
  rfFit1000 <- ranger(no_cats, data = train_all, num.trees = 1000, probability = TRUE,  
                     seed = 14441, num.threads = threads) 
  saveRDS(rfFit1000, paste0(proj_path, "/models/ranger_1000_submission_20200222.RDS"))
  rf1000_test_predTest <- predict(rfFit1000, test_all)
  View(rf1000_test_predTest$predictions)
  submission_func(rf1000_test_predTest$predictions[,2], "ranger_1000_submission_20200222.csv")  #kaggle auc:0.89242
  
  #### Naive Bayes ----
  set.seed(14441)
  nbFit <- train(no_cats, data = train_all, method = "nb")
  saveRDS(nbFit, paste0(proj_path, "/models/nb_submission_20200223.RDS")) 
  
  nb_predTest <- predict(nbFit, test_all, type = "prob")
  
  submission_func(nb_predTest[,2], "nb_submission_20200223.csv")  #kaggle auc: 0.82686
  
  #### XGBoost ---- 
  set.seed(14441)
  xgbFit <- train(no_cats, data = train_all, method = "xgbLinear")
  saveRDS(xgbFit, paste0(proj_path, "/models/xgboost_submission_20200222.RDS"))
  
  xgb_predTest <- predict(xgbFit, test_all, type = "prob")
  train_val(xgbFit, type = "raw")
  
  submission_func(xgb_predTest[,2], "xgboost_submission_20200222.csv") #kaggle auc:0.89372 
  
  #https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster  
  registerDoSEQ()
  
  # #### Neural Net ----
  # set.seed(14441)
  # nnetFit <- train(no_cats, data = train_all, method = "nnet")
  # saveRDS(nnetFit, paste0(proj_path, "/models/nnet_submission_20200219.RDS"))
  # train_val(nnetFit, type = "raw")
  # nnet_predTest <- predict(nnetFit, test_all, type = "prob")
  # 
  # submission_func(nnet_predTest[,2], "nnet_submission_20200220.csv") #kaggle auc: 0.50000
  
  
  
  
  #### misc work ---
  
  # cp.opt.num <- which.min(abs(result.opt$CP - (result.opt[which.min(result.opt[,4]), 4] + result.opt[which.min(result.opt[,4]), 5])))
  # cp.opt <- result.opt[cp.opt.num,1]
  # 
  # fit.opt <- rpart(form, method = "class", data = train, cp = cp.opt)
  # rpart.plot(fit.opt, shadow.col="gray", nn = TRUE)
  # printcp(fit.opt)
  # 
  # train_val(fit.opt)
  # 
  # rm(cp.opt.num, cp.opt, fit.opt)
  
  # #### RANDOM FOREST
  # form_rf <- as.formula(paste0(paste((names(train)[2]), collapse = " + "), "~", 
  #                              paste0((names(train)[c(4:8, 10:length(train))]), collapse = " + ")))
  # # Run Random Forest
  # mc <- makeCluster(detectCores()-1)
  # registerDoParallel(mc)
  # set.seed(14441)
  # # fit.rf <-  randomForest(form_rf, data = train)
  # fit.rf <- caret::train(form_rf, data = train, method = "rf", 
  #                        preProc = c("scale", "center"), importance = TRUE)
  # stopCluster(mc)
  # 
  # # Warning message:
  # #   In train.default(x, y, weights = w, ...) :
  # #   You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.
  # # Check OOB error
  # fit.rf
  # # Check tree decay
  # plot(fit.rf)
  # 
  # train_val(fit.rf)
  # 
  # # Tune random forest
  # fit.tune <- tuneRF(train[ ,c(-1, -2)], train$hosp_death, ntreeTry = 250, 
  #                    mtryStart = 1, stepFactor = 2, 
  #                    improve = 0.001, trace = TRUE, plot = TRUE)
  # fit.tune
  # tune.param <- fit.tune[fit.tune[, 2] == min(fit.tune[, 2]), 1]
  # 
  # fit.rf.tuned <-  randomForest(form_rf, data = train, mytry = tune.param)
