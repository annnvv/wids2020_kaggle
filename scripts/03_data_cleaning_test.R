  rm(list = ls())
  
  library(fastDummies)
  
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  
  raw_test <- read.csv(paste0(proj_path, "/raw_data/unlabeled.csv"))
  
  table(raw_test$apache_2_bodysystem)
  raw_test$apache_2_bodysystem[raw_test$apache_2_bodysystem == "Undefined Diagnoses"] <- "Undefined diagnoses"
  table(raw_test$apache_2_bodysystem)
  raw_test$apache_2_bodysystem <- droplevels(raw_test$apache_2_bodysystem)
  
  # raw_test <- read.csv(paste0(proj_path, "/data/unlabeled.csv"))
  # # create raw_test/raw_test identifier
  # raw_test$cat <- "raw_test"
  # raw_test$cat <- "raw_test"
  # table(raw_test$hospital_death) #make sure this is empty
  # #append all of the rows together
  # full <- rbind(raw_test, raw_test)
  # rm(raw_test, raw_test)
  
  # can drop "patient_id" as explantory variables, they are unique!
  # need "encounter_id" for submission
  raw_test <- raw_test[ , -which(names(raw_test) %in% c("patient_id"))] #, "hosp_id", "icu_id" 
  # remove variables with zero variance
  raw_test <- raw_test[ , -which(names(raw_test) %in% c("readmission_status"))]
  
  # drop variables with invasive in their name! (They have a lot of missing data)
  inv <- names(raw_test)[grep("_invasive_", names(raw_test))]
  raw_test <- raw_test[ , -which(names(raw_test) %in% c(inv))]
  ninety <- c("h1_albumin_max", "h1_albumin_min", "h1_bilirubin_max", "h1_bilirubin_min", "h1_lactate_max", "h1_lactate_min")
  raw_test <- raw_test[ , -which(names(raw_test) %in% c(ninety))]
  rm(inv, ninety)
  
  # factor variables
  raw_test_factor_df <- raw_test[ , sapply(raw_test,is.factor)]
  names(raw_test_factor_df)
  
  raw_test <- fastDummies::dummy_cols(raw_test, select_columns=names(raw_test_factor_df)[c(1, 3:8)], 
                                   ignore_na = TRUE, remove_selected_columns = TRUE)
  rm(raw_test_factor_df)
  
  names(raw_test) <- gsub("_source_", "_src_", names(raw_test))
  names(raw_test) <- gsub("hospital", "hosp", names(raw_test))
  names(raw_test) <- gsub("-", "_", names(raw_test))
  names(raw_test) <- gsub(" ", "_", names(raw_test))
  names(raw_test) <- tolower(names(raw_test))
  
  names(raw_test)[159:length(raw_test)]
  
  #drop variables that end with an underscore (i.e. missing data)
  miss <- names(raw_test)[grep("_$", names(raw_test))]
  miss
  miss <- miss[2:5]
  raw_test <- raw_test[ , -which(names(raw_test) %in% c(miss))]
  rm(miss)
  
  # for (i in 165:length(raw_test)){
  #   print(names(raw_test[i]))
  #   print(mean(raw_test[i], na.rm = TRUE))
  #   print(table(is.na(raw_test[i])))
  # }
  
  # change hostpital_id and icu_id to factors!
  raw_test$hosp_id <- as.character(raw_test$hosp_id)
  raw_test$icu_id <- as.character(raw_test$icu_id)
  
  #deal with missing data!
  #### demographic data
  hist(raw_test$age) #replace with mean
  raw_test$age[is.na(raw_test$age)] <- mean(raw_test$age, na.rm = TRUE)
  prop.table(table(is.na(raw_test$age)))*100
  
  hist(raw_test$bmi) #replace with median
  tapply(raw_test$bmi, raw_test$gender, median, na.rm = TRUE)
  median(raw_test$bmi, na.rm = TRUE)
  raw_test$bmi[is.na(raw_test$bmi)&raw_test$gender == ""] <- tapply(raw_test$bmi, raw_test$gender, median, na.rm = TRUE)[1]
  raw_test$bmi[is.na(raw_test$bmi)&raw_test$gender == "F"] <- tapply(raw_test$bmi, raw_test$gender, median, na.rm = TRUE)[2]
  raw_test$bmi[is.na(raw_test$bmi)&raw_test$gender == "M"] <- tapply(raw_test$bmi, raw_test$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_test$bmi)))*100
  
  hist(raw_test$height) #replace with median
  # mean(raw_test$height, na.rm = TRUE)
  tapply(raw_test$height, raw_test$gender, median, na.rm = TRUE) # women tend to be shorter
  raw_test$height[is.na(raw_test$height)&raw_test$gender == ""] <- tapply(raw_test$height, raw_test$gender, median, na.rm = TRUE)[1]
  raw_test$height[is.na(raw_test$height)&raw_test$gender == "F"] <- tapply(raw_test$height, raw_test$gender, median, na.rm = TRUE)[2]
  raw_test$height[is.na(raw_test$height)&raw_test$gender == "M"] <- tapply(raw_test$height, raw_test$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_test$height)))*100
  
  hist(raw_test$weight) #replace with median
  tapply(raw_test$weight, raw_test$gender, mean, na.rm = TRUE) # men tend to weight more
  raw_test$weight[is.na(raw_test$weight)&raw_test$gender == ""] <- tapply(raw_test$weight, raw_test$gender, median, na.rm = TRUE)[1]
  raw_test$weight[is.na(raw_test$weight)&raw_test$gender == "F"] <- tapply(raw_test$weight, raw_test$gender, median, na.rm = TRUE)[2]
  raw_test$weight[is.na(raw_test$weight)&raw_test$gender == "M"] <- tapply(raw_test$weight, raw_test$gender, median, na.rm = TRUE)[3]
  prop.table(table(is.na(raw_test$weight)))*100
  
  # need to do something with gender
  table(raw_test$gender) # based on averages of missing above, they are more similar to men
  prop.table(table(raw_test$gender))*100
  raw_test$gender[raw_test$gender == ""] <- "M"
  raw_test$gender <- droplevels(raw_test$gender)
  
  # dealing with numeric data!
  with_miss <- c()
  for (i in names(raw_test)){
    if (sum(is.na(raw_test[ , i]))/nrow(raw_test)>0){
      with_miss <- c(with_miss, i)
    }
  }
  rm(i)
  
  for (i in with_miss){
    raw_test[is.na(raw_test[i]), i] <- mean(raw_test[ , i], na.rm = TRUE)
  }
  rm(i, with_miss)
  #raw_test[is.na(raw_test["bmi"]), "bmi"] <- mean(raw_test[ , "bmi"], na.rm = TRUE)
  
  #re-order to have hosp_death as first var!
  raw_test <- raw_test[ , c(1, 3, 2, 4:length(raw_test))]
  
  # #remove death probability rates from data
  # raw_test <- raw_test[ , -which(names(raw_test) %in% c("apache_4a_hosp_death_prob", "apache_4a_icu_death_prob" ))]
  
  
  raw_test2 <- raw_test[, c(1, 3:length(raw_test))]
  complete <- raw_test2[complete.cases(raw_test2), ] ## complete cases, hurrah!
  rm(complete, raw_test2)
  
  #View(raw_test)
  
  write.csv(raw_test, paste0(proj_path, "/clean_data/test_cleaned.csv"), row.names = FALSE)
  