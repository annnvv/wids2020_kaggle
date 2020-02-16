  # Feature Selection
  #https://beta.vu.nl/nl/Images/werkstuk-fonti_tcm235-836234.pdf

  rm(list = ls())
  
  library(glmnet)
  library(caret)
  
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


  #https://stackoverflow.com/questions/21858124/r-error-in-glmnet-na-nan-inf-in-foreign-function-call
  x <- model.matrix( ~ -1 + ., train_all[ ,c(3:length(train_all))])
  
  #### LASSO Feature Selection ####
  fit <- cv.glmnet(x, as.factor(train_all$hosp_death), 
                family = "binomial", alpha=1, folds = 5, measure = "auc")
  plot(fit)
  coef(fit)
  print(fit)
  
  #code based on: https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
  c <- coef(fit, extract = TRUE)
  inds <- which(c!=0)
  variables <- row.names(c)[inds]
  `%ni%` <- Negate(`%in%`)
  variables <-variables[variables %ni% '(Intercept)']
  
  variables <- variables[!grepl("^hosp_id", variables)]
  variables <- variables[!grepl("^icu_id", variables)]
  variables <- c("hosp_id", "icu_id", variables)  

  variables  
  rm(c, inds)
  
  # subset and write clean csvs
  train_all_sub <- train_all[ , variables]
  write.csv(train_all_sub, paste0(proj_path, "/clean_data/train_cleaned_sub.csv"), 
            row.names = FALSE)
  
  test_all_sub <- test_all[ , variables]
  write.csv(test_all_sub, paste0(proj_path, "/clean_data/test_cleaned_sub.csv"), row.names = FALSE)
  
  # for (i in 1:length(train_all)){
  #   if (table(is.na(train_all[,i]))){
  #     print(names(train_all[, i]))
  #   }
  # }
  