  rm(list = ls())
  
  library(fastDummies)
  
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  train <- read.csv(paste0(proj_path, "/data/training.csv"))
  
  train$apache_2_bodysystem[train$apache_2_bodysystem == "Undefined Diagnoses"] <- "Undefined diagnoses"
  table(train$apache_2_bodysystem)
  train$apache_2_bodysystem <- droplevels(train$apache_2_bodysystem)
  
  # test <- read.csv(paste0(proj_path, "/data/unlabeled.csv"))
  # # create train/test identifier
  # train$cat <- "train"
  # test$cat <- "test"
  # table(test$hospital_death) #make sure this is empty
  # #append all of the rows together
  # full <- rbind(train, test)
  # rm(train, test)
  
  # can drop "patient_id" and "encounter_id" as explantory variables, they are unique!
  train <- train[ , -which(names(train) %in% c("encounter_id", "patient_id"))]
  # remove variables with zero variance
  train <- train[ - as.numeric(which(apply(train, 2, var) == 0))] #only one var, readmit_status
  
  # drop variables with invasive in their name! (They have a lot of missing data)
  inv <- names(train)[grep("_invasive_", names(train))]
  train <- train[ , -which(names(train) %in% c(inv))]
  ninety <- c("h1_albumin_max", "h1_albumin_min", "h1_bilirubin_max", "h1_bilirubin_min", "h1_lactate_max", "h1_lactate_min")
  train <- train[ , -which(names(train) %in% c(ninety))]
  rm(inv, ninety)
  
  # factor variables
  factor_df <- train[ , sapply(train,is.factor)]
  names(factor_df)
  
  train <- fastDummies::dummy_cols(train, select_columns=names(factor_df)[c(1, 3:8)], 
                                   ignore_na = TRUE, remove_selected_columns = TRUE)
  names(train) <- gsub("_source_", "_src_", names(train))
  names(train) <- gsub("hospital", "hosp", names(train))
  names(train) <- gsub("-", "_", names(train))
  names(train) <- gsub(" ", "_", names(train))
  names(train) <- tolower(names(train))
  
  names(train)[165:length(train)]
  
  #drop variables that end with an underscore (i.e. missing data)
  miss <- names(train)[grep("_$", names(train))]
  miss <- miss[2:5]
  train <- train[ , -which(names(train) %in% c(miss))]
  
  
  # for (i in 165:length(train)){
  #   print(names(train[i]))
  #   print(mean(train[i], na.rm = TRUE))
  #   print(table(is.na(train[i])))
  #   
  # }
  
  # change hostpital_id and icu_id to factors!
  train$hosp_id <- as.factor(train$hosp_id)
  train$icu_id <- as.factor(train$icu_id)
  
  #deal with missing data!
  #### demographic data
  hist(train$age) #replace with mean
  
  train$age[is.na(train$age)] <- mean(train$age, na.rm = TRUE)
  
  hist(train$bmi) #replace with median
  # tapply(train$bmi, train$gender, median, na.rm = TRUE)
  train$bmi[is.na(train$bmi)&train$gender == "F"] <- tapply(train$bmi, train$gender, median, na.rm = TRUE)[2]
  train$bmi[is.na(train$bmi)&train$gender == "M"] <- tapply(train$bmi, train$gender, median, na.rm = TRUE)[3]
  
  hist(train$height) #replace with median
  # mean(train$height, na.rm = TRUE)
  # tapply(train$height, train$gender, mean, na.rm = TRUE) # women tend to be shorter
  train$height[is.na(train$height)&train$gender == "F"] <- tapply(train$height, train$gender, mean, na.rm = TRUE)[2]
  train$height[is.na(train$height)&train$gender == "M"] <- tapply(train$height, train$gender, mean, na.rm = TRUE)[3]
  
  hist(train$weight) #replace with median
  # tapply(train$weight, train$gender, mean, na.rm = TRUE) # men tend to weight more
  train$weight[is.na(train$weight)&train$gender == "F"] <- tapply(train$weight, train$gender, median, na.rm = TRUE)[2]
  train$weight[is.na(train$weight)&train$gender == "M"] <- tapply(train$weight, train$gender, median, na.rm = TRUE)[3]
  
  # need to do something with ethnicity and gender
  
  # dealing with numeric data!
  with_miss <- c()
  for (i in names(train)){
    if (sum(is.na(train[ , i]))/nrow(train)>0){
      with_miss <- c(with_miss, i)
    }
  }
  rm(i)
  
  train2 <- train
  
  for (i in with_miss){
    train[is.na(train2[i]), i] <- mean(train2[ , i], na.rm = TRUE)
  }
  
  train[is.na(train2["bmi"]), "bmi"] <- mean(train2[ , "bmi"], na.rm = TRUE)
  
  
  View(train)

  
  form2 <- as.formula("hosp_death ~ age + weight + height + gender + elective_surgery + leukemia + ethnicity_caucasian")
  
  lm2 <- lm(form2, data = train)
  summary(lm2)
  
  glm2 <- glm(form2, data = train, family = "binomial")
  summary(glm2)

  
  form1 <- as.formula("hosp_death ~ .")
  
  lm1 <- lm(form1, data = train)
  summary(lm1)
  
  # ###lasso 
  # library(glmnet)
  # cv_output <- cv.glmnet(x_vars[train,], y_var[train], 
  #                        alpha = 1, lambda = lambda_seq)
  
  # tapply(train$pre_icu_los_days, train$hospital_death, mean)
  # tapply(train$albumin_apache, train$hospital_death, mean, na.rm = TRUE)
  # replace_miss <- function(var){
  #   train[var][is.na(train[var])&train$hospital_death == 0] <- tapply(train[,var], train$hospital_death, mean, na.rm = TRUE)[1] #survived
  #   train[var][is.na(train[var])&train$hospital_death == 1] <- tapply(train[,var], train$hospital_death, mean, na.rm = TRUE)[2] #died
  # }
  # train$albumin_apache <- replace_miss("albumin_apache")
  ## can't use the function above because test data does not have the target variable!
  
