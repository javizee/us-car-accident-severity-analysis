# functions.R
# Helper functions for the US Car Accident Severity Analysis
#
# Each function includes a Roxygen-style comment block explaining:
# 1. what the function does,
# 2. why the analytical choice was made,
# 3. its inputs, and
# 4. its output.

#' Group detailed weather descriptions into broader categories
#'
#' Converts the original Weather_Condition field, which contains many highly
#' specific labels, into a smaller set of interpretable weather groups.
#'
#' This analytical choice reduces sparsity and makes the weather variable easier
#' to summarize and model. Closely related labels such as "Light Rain",
#' "Rain Showers", and "Rain / Windy" are treated as part of the same broader
#' weather condition instead of as separate rare categories.
#'
#' @param weather_condition A character vector containing the original weather
#'   descriptions.
#'
#' @return A factor with the levels Clear, Cloudy, Rain, Snow, Low Visibility,
#'   Dust, Hail, Windy, and Other.
create_weather_group <- function(weather_condition) {
  weather_groups <- list(
    Clear = c("Clear", "Fair"),
    Cloudy = c(
      "Partly Cloudy", "Mostly Cloudy", "Overcast",
      "Scattered Clouds", "Cloudy"
    ),
    Rain = c(
      "Rain", "Light Rain", "Heavy Rain",
      "Light Drizzle", "Drizzle", "Heavy Drizzle",
      "Rain Showers", "Rain Shower",
      "Light Rain Showers", "Light Rain Shower",
      "Showers in the Vicinity", "Light Rain with Thunder",
      "Thunderstorms and Rain", "Light Thunderstorms and Rain",
      "Heavy Thunderstorms and Rain", "Thunderstorm",
      "Light Thunderstorm", "T-Storm", "Heavy T-Storm",
      "Thunder in the Vicinity", "Thunder", "Heavy Rain Shower",
      "Rain Shower / Windy", "Light Rain Shower / Windy",
      "Light Rain / Windy", "Rain / Windy", "Heavy Rain / Windy",
      "Heavy T-Storm / Windy", "T-Storm / Windy", "Thunder / Windy"
    ),
    Snow = c(
      "Snow", "Light Snow", "Heavy Snow",
      "Light Snow Showers", "Light Snow Shower", "Blowing Snow",
      "Wintry Mix", "Wintry Mix / Windy", "Light Freezing Rain",
      "Snow / Windy", "Light Snow / Windy", "Heavy Snow / Windy",
      "Snow and Thunder", "Light Snow with Thunder"
    ),
    `Low Visibility` = c(
      "Fog", "Patches of Fog", "Shallow Fog", "Partial Fog",
      "Light Freezing Fog", "Mist", "Haze", "Light Haze",
      "Smoke", "Heavy Smoke", "Fog / Windy", "Mist / Windy",
      "Haze / Windy", "Smoke / Windy"
    ),
    Dust = c(
      "Blowing Sand", "Blowing Dust", "Blowing Dust / Windy",
      "Widespread Dust", "Widespread Dust / Windy",
      "Duststorm", "Dust Whirls", "Sand / Windy", "Volcanic Ash"
    ),
    Hail = c("Hail", "Small Hail", "Light Hail", "Thunder and Hail"),
    Windy = c(
      "Fair / Windy", "Partly Cloudy / Windy",
      "Mostly Cloudy / Windy", "Cloudy / Windy",
      "Squalls", "Squalls / Windy",
      "Drizzle / Windy", "Light Drizzle / Windy"
    )
  )
  
  # Preserve genuinely missing weather observations instead of treating them
  # as a recorded but uncommon condition.
  grouped <- rep("Other", length(weather_condition))
  grouped[is.na(weather_condition)] <- NA_character_
  
  for (group_name in names(weather_groups)) {
    matched <- !is.na(weather_condition) &
      weather_condition %in% weather_groups[[group_name]]
    grouped[matched] <- group_name
  }
  
  factor(
    grouped,
    levels = c(
      "Clear", "Cloudy", "Rain", "Snow",
      "Low Visibility", "Dust", "Hail", "Windy", "Other"
    )
  )
}


#' Create a binary severe-accident outcome
#'
#' Converts the original four-level Severity variable into a binary outcome:
#' severity levels 1-2 are labeled non-severe and levels 3-4 are labeled severe.
#'
#' The binary outcome matches the project's main question, which is whether the
#' available conditions can distinguish severe crashes from non-severe crashes.
#' It also allows performance to be evaluated with recall, precision, F1 score,
#' balanced accuracy, and AUC.
#'
#' @param severity A numeric or integer vector containing severity levels 1-4.
#'
#' @return A factor with levels "0" for non-severe and "1" for severe.
create_severe_outcome <- function(severity) {
  factor(ifelse(severity >= 3, "1", "0"), levels = c("0", "1"))
}


#' Summarize missing values in each variable
#'
#' Calculates the number and percentage of missing observations for every column.
#'
#' Missingness was examined before modeling because variables with substantial
#' missing data may require removal, imputation, or a clearly documented
#' assumption. Reporting both counts and percentages makes the scale of each
#' missing-data problem easier to interpret.
#'
#' @param data A data frame.
#'
#' @return A data frame sorted from highest to lowest missing-data percentage.
summarize_missingness <- function(data) {
  result <- data.frame(
    Variable = names(data),
    Missing = colSums(is.na(data)),
    Percent = round(colMeans(is.na(data)) * 100, 2),
    row.names = NULL
  )
  
  result[order(-result$Percent), ]
}


#' Replace missing precipitation values with zero
#'
#' Replaces NA values in the precipitation column with zero.
#'
#' This choice treats a missing precipitation measurement as no recorded
#' precipitation. It preserves observations that would otherwise be removed.
#' Because this is a substantive assumption, it should be stated explicitly and
#' reconsidered in future work using alternative imputation methods.
#'
#' @param data A data frame containing the precipitation variable.
#' @param precipitation_col The precipitation column name.
#'
#' @return The input data frame with missing precipitation values replaced by 0.
impute_precipitation_zero <- function(
    data,
    precipitation_col = "Precipitation(in)"
) {
  if (!precipitation_col %in% names(data)) {
    stop("The precipitation column was not found in the data.")
  }
  
  data[[precipitation_col]][is.na(data[[precipitation_col]])] <- 0
  data
}


#' Remove rows missing required weather predictors
#'
#' Drops observations with missing values in the selected core weather fields.
#'
#' Complete cases were used for these variables so both models would be trained
#' and evaluated on the same set of observed predictor values. This avoids model
#' errors caused by missing inputs, although it may reduce the sample and should
#' be acknowledged as a limitation.
#'
#' @param data A data frame.
#' @param required_cols Character vector of columns that must be complete.
#'
#' @return A data frame containing complete observations for the required fields.
drop_missing_model_rows <- function(
    data,
    required_cols = c(
      "Temperature(F)",
      "Humidity(%)",
      "Visibility(mi)",
      "Wind_Speed(mph)"
    )
) {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "The following required columns were not found: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  data[stats::complete.cases(data[, required_cols, drop = FALSE]), ]
}


#' Select the variables used in the accident-severity models
#'
#' Keeps the weather variables, roadway indicators, weather group, and binary
#' outcome used by the logistic regression and Random Forest models.
#'
#' Restricting the analysis to a documented feature set makes the comparison
#' between models consistent and avoids using identifiers, coordinates, or
#' variables with substantial missingness that were not part of the research
#' question.
#'
#' @param data A prepared accident data frame.
#' @param features Character vector of variables to retain.
#'
#' @return A data frame containing only the selected modeling variables.
select_model_features <- function(
    data,
    features = c(
      "Temperature(F)",
      "Humidity(%)",
      "Visibility(mi)",
      "Wind_Speed(mph)",
      "Precipitation(in)",
      "Weather_Group",
      "Bump",
      "Crossing",
      "Give_Way",
      "Junction",
      "Railway",
      "Roundabout",
      "Stop",
      "Traffic_Signal",
      "Traffic_Calming",
      "Severe"
    )
) {
  missing_cols <- setdiff(features, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "The following modeling columns were not found: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  data[, features, drop = FALSE]
}


#' Split accident data into training and test sets
#'
#' Creates a stratified train-test split using the binary severity outcome.
#'
#' The training set is used to fit the models, while the untouched test set is
#' used to estimate performance on unseen observations. Stratification preserves
#' the severe/non-severe proportion in both sets, which is important because the
#' target is highly imbalanced.
#'
#' @param data A prepared modeling data frame.
#' @param outcome Name of the outcome column.
#' @param prop Proportion of observations assigned to training.
#' @param seed Random seed for reproducibility.
#'
#' @return A named list containing train and test data frames.
split_accident_data <- function(
    data,
    outcome = "Severe",
    prop = 0.80,
    seed = 42
) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for split_accident_data().")
  }
  
  if (!outcome %in% names(data)) {
    stop("The outcome column was not found in the data.")
  }
  
  set.seed(seed)
  
  index <- caret::createDataPartition(
    data[[outcome]],
    p = prop,
    list = FALSE
  )
  
  list(
    train = data[index, , drop = FALSE],
    test = data[-index, , drop = FALSE]
  )
}


#' Convert categorical modeling variables to factors
#'
#' Converts the outcome, weather group, and roadway indicator variables to
#' factors using consistent levels in the training and test data.
#'
#' Logistic regression and Random Forest treat categorical predictors
#' differently from numeric variables. Converting these fields to factors makes
#' their role explicit and helps keep category encoding consistent across model
#' fitting and evaluation.
#'
#' @param train Training data frame.
#' @param test Test data frame.
#' @param factor_cols Character vector of columns to convert.
#'
#' @return A named list containing updated train and test data frames.
factorize_model_data <- function(
    train,
    test,
    factor_cols = c(
      "Weather_Group",
      "Bump",
      "Crossing",
      "Give_Way",
      "Junction",
      "Railway",
      "Roundabout",
      "Stop",
      "Traffic_Signal",
      "Traffic_Calming",
      "Severe"
    )
) {
  missing_train <- setdiff(factor_cols, names(train))
  missing_test <- setdiff(factor_cols, names(test))
  
  if (length(c(missing_train, missing_test)) > 0) {
    stop("One or more requested factor columns were not found.")
  }
  
  for (column in factor_cols) {
    # Learn categorical levels from the training data only. This keeps the test
    # set completely separate from preprocessing decisions.
    train_levels <- unique(as.character(train[[column]]))
    train_levels <- train_levels[!is.na(train_levels)]
    
    unseen_test_levels <- setdiff(
      unique(as.character(test[[column]])),
      train_levels
    )
    unseen_test_levels <- unseen_test_levels[!is.na(unseen_test_levels)]
    
    if (length(unseen_test_levels) > 0) {
      stop(
        "The test data contain unseen levels in '", column, "': ",
        paste(unseen_test_levels, collapse = ", ")
      )
    }
    
    train[[column]] <- factor(train[[column]], levels = train_levels)
    test[[column]] <- factor(test[[column]], levels = train_levels)
  }
  
  if ("Severe" %in% factor_cols) {
    train$Severe <- factor(train$Severe, levels = c("0", "1"))
    test$Severe <- factor(test$Severe, levels = c("0", "1"))
  }
  
  list(train = train, test = test)
}


#' Balance severe and non-severe classes in the training data
#'
#' Randomly downsamples each outcome class to the same number of observations.
#'
#' The original training data contain many more non-severe than severe crashes.
#' Training directly on that distribution caused the models to favor the
#' majority class and made high accuracy possible without detecting severe
#' accidents. Balancing gives the Random Forest equal exposure to both outcomes
#' so it can learn patterns associated with severe crashes.
#'
#' Balancing is applied only to the training set. The test set must remain
#' unchanged so evaluation reflects the real-world class distribution and does
#' not produce an artificially optimistic performance estimate.
#'
#' @param data Training data frame.
#' @param outcome Name of the binary outcome column.
#' @param n_per_class Number of observations sampled from each class.
#' @param seed Random seed for reproducibility.
#'
#' @return A shuffled data frame with equal class sizes.
balance_classes <- function(
    data,
    outcome = "Severe",
    n_per_class = 100000,
    seed = 42
) {
  if (!outcome %in% names(data)) {
    stop("The outcome column was not found in the training data.")
  }
  
  class_counts <- table(data[[outcome]])
  
  if (length(class_counts) != 2) {
    stop("balance_classes() requires a binary outcome.")
  }
  
  if (any(class_counts < n_per_class)) {
    stop(
      "n_per_class is larger than the available observations in at least ",
      "one outcome class."
    )
  }
  
  set.seed(seed)
  
  sampled <- lapply(names(class_counts), function(class_label) {
    class_rows <- data[as.character(data[[outcome]]) == class_label, , drop = FALSE]
    class_rows[sample(seq_len(nrow(class_rows)), n_per_class), , drop = FALSE]
  })
  
  balanced <- do.call(rbind, sampled)
  balanced <- balanced[sample(seq_len(nrow(balanced))), , drop = FALSE]
  rownames(balanced) <- NULL
  balanced
}


#' Fit a logistic regression classifier
#'
#' Fits a binomial logistic regression model using all selected predictors.
#'
#' Logistic regression provides an interpretable baseline model for determining
#' whether weather and roadway features contain predictive information. Its
#' performance also provides a useful comparison with the more flexible Random
#' Forest model.
#'
#' @param train Training data frame.
#' @param outcome Name of the binary outcome variable.
#'
#' @return A fitted glm object.
fit_logistic_model <- function(train, outcome = "Severe") {
  formula <- stats::as.formula(paste(outcome, "~ ."))
  
  stats::glm(
    formula,
    data = train,
    family = stats::binomial()
  )
}


#' Fit a Random Forest accident-severity classifier
#'
#' Trains a Random Forest using the selected weather and roadway predictors.
#'
#' Random Forest was chosen because accident severity may depend on nonlinear
#' relationships and interactions that logistic regression cannot represent
#' directly. Variable importance is enabled so the model can also identify the
#' predictors that contributed most to classification.
#'
#' @param train Balanced training data frame.
#' @param outcome Name of the binary outcome variable.
#' @param ntree Number of trees.
#' @param seed Random seed for reproducibility.
#'
#' @return A fitted randomForest object.
fit_random_forest <- function(
    train,
    outcome = "Severe",
    ntree = 100,
    seed = 42
) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required for fit_random_forest().")
  }
  
  train <- train
  names(train) <- make.names(names(train))
  
  formula <- stats::as.formula(
    paste(make.names(outcome), "~ .")
  )
  
  set.seed(seed)
  
  randomForest::randomForest(
    formula,
    data = train,
    ntree = ntree,
    importance = TRUE
  )
}


#' Evaluate a binary classification model
#'
#' Calculates a confusion matrix and ROC AUC from observed outcomes, predicted
#' classes, and predicted probabilities.
#'
#' Multiple metrics are used because accuracy is misleading when severe crashes
#' are rare. Recall measures how many severe accidents are detected, precision
#' measures how often severe predictions are correct, balanced accuracy gives
#' equal weight to both classes, and AUC evaluates ranking performance across
#' possible probability thresholds.
#'
#' @param truth Observed binary outcomes.
#' @param predicted_class Predicted outcome classes.
#' @param predicted_probability Predicted probability of the severe class.
#' @param positive Label representing a severe accident.
#'
#' @return A list containing the confusion matrix, ROC object, and numeric AUC.
evaluate_classifier <- function(
    truth,
    predicted_class,
    predicted_probability,
    positive = "1"
) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for evaluate_classifier().")
  }
  
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required for evaluate_classifier().")
  }
  
  truth <- factor(truth, levels = c("0", "1"))
  predicted_class <- factor(predicted_class, levels = c("0", "1"))
  
  confusion <- caret::confusionMatrix(
    predicted_class,
    truth,
    positive = positive
  )
  
  roc_object <- pROC::roc(
    response = truth,
    predictor = predicted_probability,
    levels = c("0", "1"),
    direction = "<",
    quiet = TRUE
  )
  
  list(
    confusion_matrix = confusion,
    roc = roc_object,
    auc = as.numeric(pROC::auc(roc_object))
  )
}


#' Convert model evaluation results into a comparison row
#'
#' Extracts the main classification metrics from the output of
#' evaluate_classifier().
#'
#' A standardized row makes it possible to compare models using the same
#' measures rather than relying on overall accuracy alone.
#'
#' @param model_name Name to display for the model.
#' @param evaluation Result returned by evaluate_classifier().
#'
#' @return A one-row data frame containing accuracy, recall, precision, F1,
#'   balanced accuracy, and AUC.
evaluation_row <- function(model_name, evaluation) {
  cm <- evaluation$confusion_matrix
  
  data.frame(
    Model = model_name,
    Accuracy = unname(cm$overall["Accuracy"]),
    Recall = unname(cm$byClass["Sensitivity"]),
    Precision = unname(cm$byClass["Pos Pred Value"]),
    F1 = unname(cm$byClass["F1"]),
    Balanced_Accuracy = unname(cm$byClass["Balanced Accuracy"]),
    AUC = evaluation$auc,
    row.names = NULL
  )
}
