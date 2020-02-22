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
  # xgb <- readRDS(paste0(proj_path, "/models/xgboost_submission_20200219.RDS"))
  # ranger <- readRDS(paste0(proj_path, "/models/ranger_1000_submission_20200221.RDS"))
  # logit <- readRDS(paste0(proj_path, "/models/logit_submission_baseline_20200216.RDS"))
  # lm <- readRDS(paste0(proj_path, "/models/linear_reg_submission_baseline_20200216.RDS"))
  # nb <- readRDS(paste0(proj_path, "/models/nb_submission_20200219.RDS"))
  # dt <- readRDS(paste0(proj_path, "/models/decision_tree_submission_baseline2c_20200215.RDS"))
  # knn1 <- readRDS(paste0(proj_path, "/models/knn1_submission_20200219.RDS"))   
  # 
  # train_preds <- as.data.frame(train_all$encounter_id, train_all$hosp_death)
  #   names(train_preds)<- c("encounter_id", "hospital_death")
  # xgb_pred <- predict(xgb, train_all, type = "prob")
  #   train_preds$xgb_pred <- xgb_pred[,2]
  # ranger_pred <- predict(ranger, train_all)
  #   train_preds$rf_pred <- ranger_pred$predictions[,2]
  # logit_pred <- predict(logit, train_all, type = "response")  
  #   train_preds$logit_pred <- logit_pred
  # lm_pred <- predict(lm, train_all, type = "response")  
  #   train_preds$lm_pred <- lm_pred
  # nb_pred <- predict(nb, train_all, type = "prob")  
  #   train_preds$nb_pred <- nb_pred[, 2]
  # dt_pred <- predict(dt, train_all, type = "prob")
  #   train_preds$dt_pred <- dt_pred[ , 2]
  # write.csv(train_preds, paste0(proj_path, "/models/train_all_preds.csv"), row.names = FALSE)
  # rm(xgb_pred, ranger_pred, logit_pred, lm_pred, nb_pred, dt_pred)
  # rm(dt, knn1, lm, logit, nb, ranger, xgb)
  
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
  
  saveRDS(xgbFit_ens, paste0(proj_path, "/models/xgboost_ens_submission_20200222.RDS"))
  
  xgb_ens_pred <- predict(xgbFit_ens, test_preds)
  table(is.na(xgb_ens_pred))
  summary(xgb_ens_pred)

  submission_func(xgb_ens_pred, "xgboost_ens_submission_20200222.csv") #kaggle auc: 0.89376
  
  #### random forest ----
  threads <- detectCores()-1
  rfFit500_ens <- ranger(form, data = train_preds, num.trees = 500, 
                      probability = TRUE, seed = 14441, num.threads = threads) 
  
  saveRDS(rfFit500_ens, paste0(proj_path, "/models/ranger500_ens_submission_20200222.RDS"))
  
  #### support vector machine - radial ----
  
  
  #### support vechtor machine - linear ---- 
  
  