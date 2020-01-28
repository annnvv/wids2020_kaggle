  rm(list = ls())
  
  library(caret)
  
  proj_path <- "C:/Users/Anna V/Documents/GitHub/wids2020_kaggle"
  train <- read.csv(paste0(proj_path, "/data/training.csv"), 
                    header = TRUE, 
                    stringsAsFactors = FALSE)
  names(train) <- gsub("invasive", "inv", names(train))
  
  # remove empty rows!
  # train <- train[rowSums(is.na(train)) != ncol(train), ]
  
  # names(train)
  
  # comp <- train[complete.cases(train), ] #wooomp, woomp, only 27 complete rows!
  # head(comp)
  # rm(comp)
  
  str(train)  
  
  #number of unique patients
  length(unique(train$patient_id))
  #whether unique patients represent the dataset
  length(unique(train$patient_id))==nrow(train)
  length(unique(train$encounter_id))==nrow(train)

  #need to figure out relationship between hospital_id and icu_id! weird that they don't match!
  length(unique(train$hospital_id)) # 147 hospitals
  # table(train$hospital_id)
  length(unique(train$icu_id)) # 241 icu's
  # table(train$icu_id)
  
  # can drop "patient_id" and "encounter_id" as explantory variables, they are unique!
  train <- train[ , -which(names(train) %in% c("encounter_id", "patient_id"))]
  
  ## check for zero variance variables!
  caret::nearZeroVar(train, saveMetrics = TRUE)
  
  # remove variables with zero variance
  # https://rpubs.com/sediaz/Zero_Variance
  train <- train[ - as.numeric(which(apply(train, 2, var) == 0))]
  
  # target variable
  prop.table(table(train$hospital_death))*100 #very imbalanced dataset!

  # Missing data - NA values
  count_miss <- function(prop){
    count <- 0
    for (i in names(train)){
      if ((sum(is.na(train[ , i]))/nrow(train))>prop){
        print(i)
        count <- count + 1
      }
      # print(count)
    }
    return(count)
  }
  
  count_miss(0.75)
  # 45 variables are missing more than three quarters of their data!
  count_miss(0.25)
  # 74 variables missing more than a quarter of their data

  count_miss(0.9)
  
  
  # factor variables
  factor_df <- train[ , sapply(train,is.character)]
  
  # frequency tables for factor variables
  for (i in 1:8){
    print(names(factor_df[i]))
    # print(table(factor_df[i]))
    print(prop.table(table(factor_df[i]))*100)
    print("")
  }
  
  #ethnicity" 77% Caucasian, 1.5% NA
  #gender: 54% Female, 46% Male, 0.03% NA
  #hospital_admit_source: 40% Emergency Department, 10% Operating Room, 8.8%Floor, 7% Direct Admit, 23% NA
  #icu_admit_source
  #icu_stay_type # can probably drop! #prop.table(table(train$icu_stay_type))*100
  #icu_type
  #apache_3j_bodysystem
  #apache_2_bodysystem
  
  ## cross-tabs
  prop.table(table(train$hospital_death, train$gender))*100
  prop.table(table(train$hospital_death, train$ethnicity))*100
  prop.table(table(train$hospital_death[train$ethnicity== "Caucasian"], train$ethnicity[train$ethnicity== "Caucasian"]))*100
  
  # change hostpital_id and icu_id to factors!
  train$hospital_id <- as.factor(train$hospital_id)
  train$icu_id <- as.factor(train$icu_id)
  
  # summarize death_probabilities by actual outcomes!
  tapply(train$apache_4a_hospital_death_prob, train$hospital_death, summary)
  tapply(train$apache_4a_icu_death_prob, train$hospital_death, summary)
  # probably want to 
  
  # numeric variables
  num_df <- train[ , sapply(train,is.numeric)]
  names(num_df)
  
  hist(train$age) #replace with mean
  hist(train$bmi) #replace with median
  hist(train$height) #replace with median
  hist(train$weight) #replace with median
  
  # summary statistics for numeric variables
  for (i in 1:35){
    print(names(num_df[i]))
    # print(table(factor_df[i]))
    print(summary(num_df[i]))
    print("")
  }
  
  #NEED change ID variables to strings!
  # 4228 age observations are missing! (min age is 16, missings might be minors, 
  # but not likely based on height/weight)
  # View(train[is.na(train$age), ])
  
  
  table(train$readmission_status) # drop readmission status
  
  for (i in 35:66){
    print(names(num_df[i]))
    # print(table(factor_df[i]))
    print(summary(num_df[i]))
    print("")
  }
  
  for (i in 67:92){
    print(names(num_df[i]))
    # print(table(factor_df[i]))
    print(summary(num_df[i]))
    print("")
  }
  
  
  for (i in 152:length(num_df)){
    print(names(num_df[i]))
    # print(table(factor_df[i]))
    print(summary(num_df[i]))
    print("")
  }
  # might want to delete these vars!
  
  #numeric stuff
  # sumByGroup <- function(df,var){
  #   tapply(df$var, train$hospital_death, summary)
  # }
  # sumByGroup(train, age)
  
  tapply(train$age, train$hospital_death, summary) #older people tend to die in hospitals
  t.test(train$age, train$hospital_death)  #statistically significant
  
  tapply(train$bmi, train$hospital_death, summary) #people with a lower bmi tend to die!
  t.test(train$bmi, train$hospital_death)  #statistically significant
  
  
  # View(train[ , (!grepl("h1_", names(train)) & !grepl("d1_", names(train)))]) 
  ## 58 columns that do not start with d1 or h1
  
  #View(train[, c("apache_2_bodysystem", "apache_2_diagnosis", "apache_3j_bodysystem", "apache_3j_diagnosis")])
  #table(train$apache_2_bodysystem, train$apache_2_diagnosis)
  #View(train[train$apache_2_bodysystem == "Cardiovascular",  ])
  #View(train[,c("height", "weight", "bmi", "gender")  ])
  
    
  # train$apache_2_bodysystem2 <- train$apache_2_bodysystem
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == ""] <- "Missing"
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == "Undefined Diagnoses"] <- "Undefined diagnoses"
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == "Haematologic"] <- "Other"
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == "Haematologic"] <- "Other"
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == "Renal/Genitourinary"] <- "Other"
  # train$apache_2_bodysystem2[train$apache_2_bodysystem2 == "Trauma"] <- "Other"