  rm(list = ls())
  
  library(fastDummies)
  
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  raw_train <- read.csv(paste0(proj_path, "/raw_data/training.csv"))
  
  raw_train$apache_2_bodysystem[raw_train$apache_2_bodysystem == "Undefined Diagnoses"] <- "Undefined diagnoses"
  table(raw_train$apache_2_bodysystem)
  raw_train$apache_2_bodysystem <- droplevels(raw_train$apache_2_bodysystem)

  # can drop "patient_id" as explantory variables, they are unique!
  # need "encounter_id"
  raw_train <- raw_train[ , -which(names(raw_train) %in% c("patient_id"))] #"hosp_id", "icu_id" 
  # remove variables with zero variance
  raw_train <- raw_train[ - as.numeric(which(apply(raw_train, 2, var) == 0))] #only one var, readmit_status
  
  # drop variables with invasive in their name! (They have a lot of missing data)
  inv <- names(raw_train)[grep("_invasive_", names(raw_train))]
  raw_train <- raw_train[ , -which(names(raw_train) %in% c(inv))]
  ninety <- c("h1_albumin_max", "h1_albumin_min", "h1_bilirubin_max", "h1_bilirubin_min", "h1_lactate_max", "h1_lactate_min")
  raw_train <- raw_train[ , -which(names(raw_train) %in% c(ninety))]
  rm(inv, ninety)
  
  # factor variables
  factor_df <- raw_train[ , sapply(raw_train,is.factor)]
  names(factor_df)
  
  raw_train <- fastDummies::dummy_cols(raw_train, select_columns=names(factor_df)[c(1, 3:8)], 
                                   ignore_na = TRUE, remove_selected_columns = TRUE)
  rm(factor_df)
  
  names(raw_train) <- gsub("_source_", "_src_", names(raw_train))
  names(raw_train) <- gsub("hospital", "hosp", names(raw_train))
  names(raw_train) <- gsub("-", "_", names(raw_train))
  names(raw_train) <- gsub(" ", "_", names(raw_train))
  names(raw_train) <- tolower(names(raw_train))
  
  names(raw_train)[165:length(raw_train)]
  
  #drop variables that end with an underscore (i.e. missing data)
  miss <- names(raw_train)[grep("_$", names(raw_train))]
  miss
  miss <- miss[2:5]
  raw_train <- raw_train[ , -which(names(raw_train) %in% c(miss))]
  rm(miss)
  
  # for (i in 165:length(raw_train)){
  #   print(names(raw_train[i]))
  #   print(mean(raw_train[i], na.rm = TRUE))
  #   print(table(is.na(raw_train[i])))
  # }
  
  # change hostpital_id and icu_id to characters!
  raw_train$hosp_id <- as.character(raw_train$hosp_id)
  raw_train$icu_id <- as.character(raw_train$icu_id)
  
  #deal with missing data!
  #### demographic data
  hist(raw_train$age) #replace with mean
  raw_train$age[is.na(raw_train$age)] <- mean(raw_train$age, na.rm = TRUE)
  prop.table(table(is.na(raw_train$age)))*100
  
  hist(raw_train$bmi) #replace with median
    tapply(raw_train$bmi, raw_train$gender, median, na.rm = TRUE)
  raw_train$bmi[is.na(raw_train$bmi)&raw_train$gender == ""] <- tapply(raw_train$bmi, raw_train$gender, median, na.rm = TRUE)[1]
  raw_train$bmi[is.na(raw_train$bmi)&raw_train$gender == "F"] <- tapply(raw_train$bmi, raw_train$gender, median, na.rm = TRUE)[2]
  raw_train$bmi[is.na(raw_train$bmi)&raw_train$gender == "M"] <- tapply(raw_train$bmi, raw_train$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_train$bmi)))*100
  
  hist(raw_train$height) #replace with median
  # mean(raw_train$height, na.rm = TRUE)
  tapply(raw_train$height, raw_train$gender, median, na.rm = TRUE) # women tend to be shorter
  raw_train$height[is.na(raw_train$height)&raw_train$gender == ""] <- tapply(raw_train$height, raw_train$gender, median, na.rm = TRUE)[1]
  raw_train$height[is.na(raw_train$height)&raw_train$gender == "F"] <- tapply(raw_train$height, raw_train$gender, median, na.rm = TRUE)[2]
  raw_train$height[is.na(raw_train$height)&raw_train$gender == "M"] <- tapply(raw_train$height, raw_train$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_train$height)))*100
  
  hist(raw_train$weight) #replace with median
    tapply(raw_train$weight, raw_train$gender, mean, na.rm = TRUE) # men tend to weight more
  raw_train$weight[is.na(raw_train$weight)&raw_train$gender == ""] <- tapply(raw_train$weight, raw_train$gender, median, na.rm = TRUE)[1]
  raw_train$weight[is.na(raw_train$weight)&raw_train$gender == "F"] <- tapply(raw_train$weight, raw_train$gender, median, na.rm = TRUE)[2]
  raw_train$weight[is.na(raw_train$weight)&raw_train$gender == "M"] <- tapply(raw_train$weight, raw_train$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_train$weight)))*100
  
  # need to do something with gender
  table(raw_train$gender) # based on averages of missing above, they are more similar to men
  prop.table(table(raw_train$gender))*100
  raw_train$gender[raw_train$gender == ""] <- "M"
  raw_train$gender <- droplevels(raw_train$gender)
  
  # dealing with numeric data!
  with_miss <- c()
  for (i in names(raw_train)){
    if (sum(is.na(raw_train[ , i]))/nrow(raw_train)>0){
      with_miss <- c(with_miss, i)
    }
  }
  rm(i)
  
  for (i in with_miss){
    raw_train[is.na(raw_train[i]), i] <- mean(raw_train[ , i], na.rm = TRUE)
  }
  rm(i, with_miss)
  #raw_train[is.na(raw_train["bmi"]), "bmi"] <- mean(raw_train[ , "bmi"], na.rm = TRUE)
  
  complete <- raw_train[complete.cases(raw_train), ] ## complete cases, hurrah!
  rm(complete)
  
  #re-order to have hosp_death as first var!
  raw_train <- raw_train[ , c(1, 3, 2, 4:length(raw_train))]
  
  #drop this factor variable because it doesn't exist in the test set!
  raw_train <- raw_train[ , -which(names(raw_train) %in% c("hosp_admit_src_observation"))]
  
  # #remove death probability rates from data
  # raw_train <- raw_train[ , -which(names(raw_train) %in% c("apache_4a_hosp_death_prob", "apache_4a_icu_death_prob" ))]
  
  write.csv(raw_train, paste0(proj_path, "/clean_data/train_cleaned.csv"), row.names = FALSE)

  # form1 <- as.formula("hosp_death ~ .")
  # lm1 <- lm(form1, data = raw_train)
  # summary(lm1)
    
  # form2 <- as.formula("hosp_death ~ age + weight + height + gender + elective_surgery + leukemia + ethnicity_caucasian")
  # lm2 <- lm(form2, data = raw_train)
  # summary(lm2)
  # 
  # glm2 <- glm(form2, data = raw_train, family = "binomial")
  # summary(glm2)


  # ###lasso 
  # library(glmnet)
  # cv_output <- cv.glmnet(x_vars[raw_train,], y_var[raw_train], 
  #                        alpha = 1, lambda = lambda_seq)
  
  # tapply(raw_train$pre_icu_los_days, raw_train$hospital_death, mean)
  # tapply(raw_train$albumin_apache, raw_train$hospital_death, mean, na.rm = TRUE)
  # replace_miss <- function(var){
  #   raw_train[var][is.na(raw_train[var])&raw_train$hospital_death == 0] <- tapply(raw_train[,var], raw_train$hospital_death, mean, na.rm = TRUE)[1] #survived
  #   raw_train[var][is.na(raw_train[var])&raw_train$hospital_death == 1] <- tapply(raw_train[,var], raw_train$hospital_death, mean, na.rm = TRUE)[2] #died
  # }
  # raw_train$albumin_apache <- replace_miss("albumin_apache")
  ## can't use the function above because test data does not have the target variable!
  
