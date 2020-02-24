  rm(list = ls())
  library(caret)
  library(randomForest)
  library(e1071)
  library(ranger)
  library(doParallel)
  library(parallel)
  library(beepr)

  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  #submit function
  submission_func <- function(pred_obj, file_name){
    # Purpose: a function to create a submission for the kaggle competition
    # Input: a prediction object and file name for .csv
    # Output: is a .csv file
    
    test_preds$hosp_death <- pred_obj
    submission_dt <- cbind.data.frame(test_preds$encounter_id, pred_obj)  
    names(submission_dt) <- c("encounter_id", "hospital_death")
    submission_dt$hospital_death <- as.numeric(as.character(submission_dt$hospital_death))
    write.csv(submission_dt, paste0(proj_path, "/submissions/", file_name), row.names = FALSE)  
  } 
  
  # train_all <- read.csv(paste0(proj_path, "/clean_data/train_cleaned.csv"), stringsAsFactors = FALSE)
  # test_all <- read.csv(paste0(proj_path, "/clean_data/test_cleaned.csv"), stringsAsFactors = FALSE)
  # 
  # # create raw_train/test identifier
  # train_all$cat <- "train"
  # test_all$cat <- "test"
  # 
  # #append all of the rows together
  # both <- rbind(train_all, test_all)
  # rm(train_all, test_all)
  # 
  # # change hostpital_id and icu_id to factors
  # both$hosp_death <- as.factor(both$hosp_death)
  # both$hosp_id <- as.factor(both$hosp_id)
  # both$icu_id <- as.factor(both$icu_id)
  # # # both$gender <- as.factor(both$gender)
  # str(both[0:10])
  # 
  # train_all <- both[both$cat == "train", ]
  # train_all <- train_all[ , c(1:length(train_all)-1)]
  # 
  # test_all <- both[both$cat == "test", ]
  # test_all <- test_all[ , c(1:length(test_all)-1)]
  # 
  # rm(both)
  
  ##read in models
  xgb <- readRDS(paste0(proj_path, "/models/xgboost_submission_20200222.RDS"))
  ranger <- readRDS(paste0(proj_path, "/models/ranger_1000_submission_20200222.RDS"))
  logit <- readRDS(paste0(proj_path, "/models/logit_submission_baseline_20200222.RDS"))
  lm <- readRDS(paste0(proj_path, "/models/linear_reg_submission_baseline_20200222.RDS"))
  nb <- readRDS(paste0(proj_path, "/models/nb_submission_20200223.RDS"))
  dt <- readRDS(paste0(proj_path, "/models/decision_tree_submission_baseline_20200222.RDS"))
  # knn1 <- readRDS(paste0(proj_path, "/models/knn1_submission_20200219.RDS"))   
  train_preds <- c()
  train_preds$encounter_id <- train_all$encounter_id
    train_preds$hospital_death <- train_all$hosp_death
  train_preds <- as.data.frame(train_preds)
  
  xgb_pred <- predict(xgb, train_all, type = "prob")
    train_preds$xgb_pred <- xgb_pred[,2]
  ranger_pred <- predict(ranger, train_all)
    train_preds$rf_pred <- ranger_pred$predictions[,2]
  logit_pred <- predict(logit, train_all, type = "response")
    train_preds$logit_pred <- logit_pred
  lm_pred <- predict(lm, train_all, type = "response")
    train_preds$lm_pred <- lm_pred
    train_preds$lm_pred[train_preds$lm_pred<0] <- 0
    train_preds$lm_pred[train_preds$lm_pred>1] <- 1
  nb_pred <- predict(nb, train_all, type = "prob")
    train_preds$nb_pred <- nb_pred[, 2]
  dt_pred <- predict(dt, train_all, type = "prob")
    train_preds$dt_pred <- dt_pred[ , 2]
  write.csv(train_preds, paste0(proj_path, "/models/train_all_preds.csv"), row.names = FALSE)
  rm(xgb_pred, ranger_pred, logit_pred, lm_pred, nb_pred, dt_pred)
  rm(dt, lm, logit, nb, ranger, xgb)
  
  train_preds <- read.csv(paste0(proj_path, "/models/train_all_preds.csv"))
  names(train_preds)
  train_preds$hospital_death <- as.factor(train_preds$hospital_death)
  
  test_preds <- read.csv(paste0(proj_path, "/models/test_all_preds.csv"))
  names(test_preds)
  
  #formula
  form <- as.formula(paste0(paste((names(train_preds)[2]), collapse = " + "), "~", 
                            paste0((names(train_preds)[c(3:length(train_preds))]), collapse = " + ")))
  form
  
  #train control
  trC <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
  
  #### xgboost ----
  mc <- makeCluster(detectCores()-1)
  registerDoParallel(mc)
  set.seed(14441)
  xgbFit_ens <- train(form, data = train_preds, method = "xgbLinear", 
                      trControl = trC, importance=TRUE)
  stopCluster(mc)
  beep()
  varImp(xgbFit_ens)
  
  saveRDS(xgbFit_ens, paste0(proj_path, "/models/xgboost_ens_submission_20200224.RDS"))
  
  xgb_ens_pred <- predict(xgbFit_ens, test_preds, type = "prob")
  table(is.na(xgb_ens_pred))
  summary(xgb_ens_pred)

  submission_func(xgb_ens_pred[,2], "xgboost_ens_submission_20200224.csv") #kaggle auc: 0.75716
  
  mc <- makeCluster(detectCores()-1)
  registerDoParallel(mc)
  set.seed(14441)
  xgbFit_tree_ens <- train(form, data = train_preds, method = "xgbTree", 
                      trControl = trC, importance=TRUE)
  stopCluster(mc)
  beep()
  varImp(xgbFit_tree_ens)
  saveRDS(xgbFit_tree_ens, paste0(proj_path, "/models/xgboost_tree_ens_submission_20200224.RDS"))
  
  xgbt_ens_pred <- predict(xgbFit_tree_ens, test_preds, type = "prob")
  table(is.na(xgbt_ens_pred))
  summary(xgbt_ens_pred)
  
  submission_func(xgbt_ens_pred[,2], "xgboost_tree_ens_submission_20200224.csv") #kaggle auc: 0.71937
  
  #### random forest ----
  threads <- detectCores()-1
  rfFit500_ens <- ranger(form, data = train_preds, num.trees = 500, importance = "impurity",
                      probability = TRUE, seed = 14441, num.threads = threads) 
  
  saveRDS(rfFit500_ens, paste0(proj_path, "/models/ranger500_ens_submission_20200224.RDS")) 
  rfFit500_ens$variable.importance
  beep()
  
  rfFit500_ens_pred <- predict(rfFit500_ens, test_preds)
  summary(rfFit500_ens_pred$predictions[,2])
  submission_func(rfFit500_ens_pred$predictions[,2], "ranger500_ens_submission_20200224.csv") #kaggle auc: 0.76783
  
  #### support vector machine - radial ----
  svmRFit <- svm(form, data = train_preds, kernel = "radial", 
                 scale = FALSE, probability = TRUE)
  saveRDS(svmRFit, paste0(proj_path, "/models/svmR_submission_20200224.RDS"))
  
  svmR_ens_pred <- predict(svmRFit, test_preds, decision.values = TRUE, probability = TRUE)
  head(attr(svmL_ens_pred, "probabilities"))
  table(svmL_ens_pred)
  
  attr(svmL_ens_pred, "probabilities")[,2]
  submission_func(attr(svmR_ens_pred, "probabilities")[,2], "svmR_ens_submission_20200224.csv") #kaggle auc: 0.85887
  
  #### support vechtor machine - linear ---- 
  svmLFit <- svm(form, data = train_preds, kernel = "linear", 
                 scale = FALSE, probability = TRUE)
  saveRDS(svmLFit, paste0(proj_path, "/models/svmRL_submission_20200224.RDS"))

  svmL_ens_pred <- predict(svmLFit, test_preds, decision.values = TRUE, probability = TRUE)
  head(attr(svmL_ens_pred, "probabilities"))
  table(svmL_ens_pred)
  
  attr(svmL_ens_pred, "probabilities")[,2]
  submission_func(attr(svmL_ens_pred, "probabilities")[,2], "svmL_ens_submission_20200224.csv") #kaggle auc:0.86252
  
  #### logit -----
  glm_ens_Fit <- glm(form, data = train_preds, method = "glm.fit", family = "binomial")
  summary(glm_ens_Fit)
  
  glm_ens_pred <- stats::predict(glm_ens_Fit, test_preds, type= "response")
  
  submission_func(glm_ens_pred, "logit_ens_submission_20200224.csv")  #kaggle auc: 0.66851
  saveRDS(glm_ens_Fit, paste0(proj_path, "/models/logit_ens_submission_20200224.RDS"))
  
  