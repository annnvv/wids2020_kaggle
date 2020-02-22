  #simple average
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  xgb_pred <- read.csv(paste0(proj_path, "/submissions/xgboost_submission_20200220.csv"), 
                       stringsAsFactors = FALSE) #kaggle auc: 0.89499
  names(xgb_pred)[2] <- "xgb_pred"
  hist(xgb_pred$xgb_pred)
  
  rf_pred <- read.csv(paste0(proj_path, "/submissions/ranger_1000_submission_20200221.csv"), 
                      stringsAsFactors = FALSE)  #kaggle auc:0.88714
  names(rf_pred)[2] <- "rf_pred"
  hist(rf_pred$rf_pred)
  
  logit_pred <- read.csv(paste0(proj_path, "/submissions/logit_submission_baseline_20200216.csv"), 
                         stringsAsFactors = FALSE)  #kaggle auc: 0.88346
  names(logit_pred)[2] <- "logit_pred"
  hist(logit_pred$logit_pred)
  
  lm_pred <- read.csv(paste0(proj_path, "/submissions/linear_reg_submission_baseline_20200216.csv"), 
                      stringsAsFactors = FALSE) #kaggle auc: 0.87360
  names(lm_pred)[2] <- "lm_pred"
  hist(lm_pred$lm_pred)
  
  nb_pred <- read.csv(paste0(proj_path, "/submissions/nb_submission_20200219.csv"), 
                      stringsAsFactors = FALSE)  #kaggle auc: 0.81191
  names(nb_pred)[2] <- "nb_pred"
  hist(nb_pred$nb_pred)
  summary(nb_pred$nb_pred)
  
  dt_pred <- read.csv(paste0(proj_path, "/submissions/decision_tree_submission_baseline2c_20200215.csv"), 
                      stringsAsFactors = FALSE)   #kaggle auc: 0.70422
  names(dt_pred)[2] <- "dt_pred"
  hist(dt_pred$dt_pred)
  
  knn_pred <- read.csv(paste0(proj_path, "/submissions/knn1_submission_20200219.csv"), 
                       stringsAsFactors = FALSE) #kaggle auc: 0.56314
  names(knn_pred)[2] <- "knn_pred"
  hist(knn_pred$knn_pred)

  all_pred  <- merge(xgb_pred, rf_pred, by = "encounter_id")
  all_pred  <- merge(all_pred, logit_pred, by = "encounter_id")
  all_pred  <- merge(all_pred, lm_pred, by = "encounter_id")
  all_pred  <- merge(all_pred, nb_pred, by = "encounter_id")
  all_pred  <- merge(all_pred, dt_pred, by = "encounter_id")
  all_pred  <- merge(all_pred, knn_pred, by = "encounter_id")
  rm(xgb_pred, rf_pred, logit_pred, lm_pred, nb_pred, dt_pred, knn_pred)

  all_pred$average_all <- rowMeans(all_pred[ ,c(2:8)], na.rm=TRUE)
  sa_v1 <- all_pred[, c(1,9)]
  names(sa_v1)[2] <- "hospital_death" 
  write.csv(sa_v1, paste0(proj_path, "/submissions/simple_average_v1.csv"), row.names = F)  #kaggle auc: 0.88705
  
  all_pred$average_best3 <- rowMeans(all_pred[ ,c(2:4)], na.rm=TRUE)
  sa_v2 <- all_pred[, c(1,10)]
  names(sa_v2)[2] <- "hospital_death" 
  write.csv(sa_v2, paste0(proj_path, "/submissions/simple_average_best3_v2.csv"), row.names = F)  #kaggle auc:0.90145
  
  