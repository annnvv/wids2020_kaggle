  # https://towardsdatascience.com/feature-selection-using-regularisation-a3678b71e499
  
  rm(list = ls())
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  library(parallel)
  library(doParallel)
  
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  train_all <- read.csv(paste0(proj_path, "/clean_data/train_cleaned.csv"), stringsAsFactors = FALSE)
  str(train_all[0:10])
  
  test_all <- read.csv(paste0(proj_path, "/clean_data/test_cleaned.csv"), stringsAsFactors = FALSE)
  str(test_all[0:10])  
  
  # create raw_train/test identifier
  train_all$cat <- "train"
  test_all$cat <- "test"
  
  #append all of the rows together
  both <- rbind(train_all, test_all)
  rm(train_all, test_all)
  
  # change hostpital_id and icu_id to factors
  both$hosp_id <- as.factor(both$hosp_id)
  both$icu_id <- as.factor(both$icu_id)
  both$gender <- as.factor(both$gender)
  str(both[0:10])
  
  train_all <- both[both$cat == "train", ]
  train_all <- train_all[ , c(1:length(train_all)-1)]
  
  test_all <- both[both$cat == "test", ]
  test_all <- test_all[ , c(1:length(test_all)-1)]
  
  rm(both)
  
  set.seed(14441)
  # create train and validation sets
  index <- createDataPartition(train_all$hosp_death, p=0.5, list = FALSE)
  train <- train_all[index,]
  val <- train_all[-index,]
  
  rm(train_all, index)  
  
  form <- as.formula(paste0(paste((names(train)[2]), collapse = " + "), "~", 
                            paste0((names(train)[3:length(train)]), collapse = " + ")))
  
  # Decision Trees
  fit <- rpart(form, method = "class", data = train, cp = 0.01)
  rpart.plot(fit, shadow.col="gray", nn = TRUE)
  result.opt <- as.data.frame(printcp(fit))
  
  train_val <- function(model){
    dt_predTrain <- predict(model, train, type = "class")
    # Checking classification accuracy
    print(prop.table(table(dt_predTrain, train$hosp_death))*100)
    
    dt_predVal <- predict(model, val, type = "class")
    # Checking classification accuracy - out of sample
    print(prop.table(table(dt_predVal, val$hosp_death))*100)  
  }
  
  train_val(fit)
  
  dt_predTest <- predict(fit, test_all, type = "prob")
  
  submission_func <- function(pred_obj, file_name){
    # Purpose: a function to create a submission for the kaggle competition
    # Input: a prediction object and file name for .csv
    # Output: is a .csv
    
    test_all$hosp_death <- pred_obj
    submission_dt <- cbind.data.frame(test_all$encounter_id, pred_obj)  
    names(submission_dt) <- c("encounter_id", "hospital_death")
    submission_dt$hospital_death <- as.numeric(as.character(submission_dt$hospital_death))
    write.csv(submission_dt, paste0(proj_path, "/submissions/", file_name), row.names = FALSE)  
  }
  
  submission_func(dt_predTest[,2], "decision_tree_submission_baseline2b_20200201.csv")  
  
  
  cp.opt.num <- which.min(abs(result.opt$CP - (result.opt[which.min(result.opt[,4]), 4] + result.opt[which.min(result.opt[,4]), 5])))
  cp.opt <- result.opt[cp.opt.num,1]
  
  fit.opt <- rpart(form, method = "class", data = train, cp = cp.opt)
  rpart.plot(fit.opt, shadow.col="gray", nn = TRUE)
  printcp(fit.opt)
  
  train_val(fit.opt)
  
  rm(cp.opt.num, cp.opt, fit.opt)
  
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
