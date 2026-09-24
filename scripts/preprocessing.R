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

  # Remove students older than 20. Keep missing ages for MICE.
  cat("Students aged 20+ removed: ", sum(dataset$age >= 20, na.rm = TRUE), "\n", sep = "")
  dataset <- dataset %>% filter(is.na(age) | age < 20)

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
  # After removing age 22: Math uses version 1, Portuguese uses version 5.
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



#Statistical tests for predictors
#Dummy variables "_other" are a reference point
Math_regression <- Math%>%
  select(-Dalc, -Walc, -totalconsumption, -Mjob_other,
         -Fjob_other,
         -reason_other,
         -guardian_other)

Reg_model <- lm(
  meantotal ~ ., 
  data = Math_regression
)
summary(Reg_model)
