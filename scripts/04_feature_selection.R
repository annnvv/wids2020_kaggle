  # Feature Selection
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

    