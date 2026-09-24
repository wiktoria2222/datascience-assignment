# Preprocessing - where we pre process the data
#
# Steps:
# 1. Load the Math and Portuguese datasets.
# 2. Check duplicates, missing values, and summaries.
# 3. Remove students without both alcohol measures.
# 4. Remove students aged 20 or older
# 5. Impute missing values with MICE.
# 6. Calculate the average alcohol score.
# 7. Create dummy variables for later modelling.

library(dplyr)
library(mice)
library(fastDummies)

# Fields selected for the research question.
model_inputs <- c(
  "sex", "age", "address",
  "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "famrel",
  "activities", "romantic", "freetime", "goout"
)

# Categorical fields changed to factors before using MICE.
factor_inputs <- c(
  "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob", "guardian",
  "famsup", "internet", "activities", "romantic"
)

# Same preprocessing steps for each dataset.
preprocess_data <- function(input_file, output_file, dataset_name, mice_version) {
  dataset <- read.csv(
    input_file,
    na.strings = c("", "NA"),
    check.names = FALSE
  )

  # Check the raw data.
  cat("\n", dataset_name, " dataset\n", sep = "")
  # Check for fully identical rows.
  cat("Duplicates: ", sum(duplicated(dataset)), "\n", sep = "")
  cat("\nstr dataset: \n", "", sep="")
  str(dataset)
  cat("\nNumber of NAs for each column: \n", "", sep="")
  print(colSums(is.na(dataset)))
  cat("\nSummary: \n", "", sep="")
  print(summary(dataset))

  # Remove students without both alcohol measures.
  dataset <- dataset %>% filter(!is.na(Dalc) & !is.na(Walc))


  # Keep selected fields and remove excluded fields.
  dataset <- dataset %>% select(all_of(c(model_inputs, "Dalc", "Walc")))

  # Change categorical variables to factors for MICE.
  dataset <- dataset %>% mutate(across(all_of(factor_inputs), as.factor))

  # Create five MICE versions with 20 iterations.
  initial_mice <- mice(dataset, maxit = 0, printFlag = FALSE)
  mice_methods <- initial_mice$method
  # Don't impute the alcohol consumption columns. Just to be safe.
  # It shouldn't happen because we already removed rows with missing Dalc/Walc.
  mice_methods[c("Dalc", "Walc")] <- ""

  imputed_data <- mice(
    dataset,
    method = mice_methods,
    m = 5,
    maxit = 20,
    seed = 123,
    printFlag = FALSE
  )

  # Inspect observed and imputed values.
  cat("\n", dataset_name, ": age\n", sep = "")
  print(summary(dataset$age))
  print(imputed_data$imp$age)

  cat("\n", dataset_name, ": going out with friends\n", sep = "")
  print(summary(dataset$goout))
  print(imputed_data$imp$goout)

  cat("\n", dataset_name, ": sex\n", sep = "")
  print(summary(dataset$sex))
  print(imputed_data$imp$sex)

  # Use the version with the smallest average difference from observed values.
  #  Math uses version 4, Portuguese uses version 5.
  completed_data <- complete(imputed_data, mice_version)

  # Make sure no missing values after imputation
  cat("\n", dataset_name, ": missing values after imputation\n", sep = "")
  print(colSums(is.na(completed_data)))

  # Give workday and weekend alcohol scores equal weight.
  completed_data <- completed_data %>%
    mutate(alc_score = (Dalc + Walc) / 2)

  # Create dummy variables. One category is removed from each variable.
  model_data <- dummy_cols(
    completed_data,
    select_columns = factor_inputs,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

  # Check and save the final data.
  print(sapply(model_data, is.numeric))
  write.csv(model_data, output_file, row.names = FALSE)
}

# Preprocess the Math dataset.
preprocess_data(
  input_file = "data/raw/student_mat.csv",
  output_file = "data/processed/Math.csv",
  dataset_name = "Math",
  mice_version = 1
)

# Preprocess the Portuguese dataset.
preprocess_data(
  input_file = "data/raw/student_por.csv",
  output_file = "data/processed/Lang.csv",
  dataset_name = "Portuguese",
  mice_version = 5
)


#Splitting into training and test sets 
#Same steps applied for the portugese set
#The glmnet standardizes automatically so there's no need for manual
install.packages("caTools")
library(caTools)
set.seed(123)

split <- sample.split(
  seq_len(nrow(Math)),
  SplitRatio = 0.7
)

Math_train <- subset(Math, split == TRUE)
Math_test <- subset(Math, split == FALSE)

#Ridge regression training model
?cv.glmnet
x_train <- Math_train %>%
  select(-alc_score) %>%
  as.matrix()
y_train <- Math_train$alc_score

Ridgemodel <- cv.glmnet(
  x_train,
  y_train,
  alpha = 0,
  standardize = TRUE
)

plot(Ridgemodel)
Ridgemodel$lambda.min
Ridgemodel$lambda.1se
ridge_coef <- coef(Ridgemodel, s = "lambda.min")
ridge_coef

#Ridge regression test model

x_test <- Math_test %>% 
  select(-alc_score) %>% 
  as.matrix() 
 y_test <- Math_test$alc_score

Ridge_pred <- predict(
  Ridgemodel,
  newx = x_test,
  s = "lambda.min"
)

#Ridge regression evaluation MAE
Ridge_mae <- mean(abs(Ridge_pred - y_test)) 
Ridge_mae

#Lasso training model
Lassomodel <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1,
  standardize = TRUE
)

plot(Lassomodel)
Lassomodel$lambda.min
Lassomodel$lambda.1se
lasso_coef <- coef(Lassomodel, s = "lambda.min")
lasso_coef

#LASSO test model
x_test <- Math_test %>% 
  select(-alc_score) %>% 
  as.matrix() 
y_test <- Math_test$alc_score

Lasso_pred <- predict(
  Lassomodel,
  newx = x_test,
  s = "lambda.min"
)

#LASSO regression evaluation MAE
Lasso_mae <- mean(abs(Lasso_pred - y_test)) 
Lasso_mae
