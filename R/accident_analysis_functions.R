# accident_analysis_functions.R
# Helper functions for the US Car Accident Severity Analysis

load_accident_data <- function(path = "data/US_Accidents_March23.csv") {
  if (!file.exists(path)) {
    stop("Could not find the accident CSV at: ", path)
  }
  readr::read_csv(path, show_col_types = FALSE)
}

drop_constant_predictors <- function(data, outcome = "Severe") {
  predictor_names <- setdiff(names(data), outcome)
  
  keep_predictors <- predictor_names[
    vapply(
      data[predictor_names],
      function(column) {
        length(unique(stats::na.omit(column))) > 1
      },
      logical(1)
    )
  ]
  
  data[, c(keep_predictors, outcome), drop = FALSE]
}

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
      "Rain Showers", "Rain Shower", "Light Rain Showers",
      "Light Rain Shower", "Showers in the Vicinity",
      "Light Rain with Thunder", "Thunderstorms and Rain",
      "Light Thunderstorms and Rain", "Heavy Thunderstorms and Rain",
      "Thunderstorm", "Light Thunderstorm", "T-Storm", "Heavy T-Storm",
      "Thunder in the Vicinity", "Thunder", "Heavy Rain Shower",
      "Rain Shower / Windy", "Light Rain Shower / Windy",
      "Light Rain / Windy", "Rain / Windy", "Heavy Rain / Windy",
      "Heavy T-Storm / Windy", "T-Storm / Windy", "Thunder / Windy"
    ),
    Snow = c(
      "Snow", "Light Snow", "Heavy Snow", "Light Snow Showers",
      "Light Snow Shower", "Blowing Snow", "Wintry Mix",
      "Wintry Mix / Windy", "Light Freezing Rain", "Snow / Windy",
      "Light Snow / Windy", "Heavy Snow / Windy",
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
  
  grouped <- rep("Other", length(weather_condition))
  for (group_name in names(weather_groups)) {
    grouped[weather_condition %in% weather_groups[[group_name]]] <- group_name
  }
  
  factor(
    grouped,
    levels = c(
      "Clear", "Cloudy", "Rain", "Snow",
      "Low Visibility", "Dust", "Hail", "Windy", "Other"
    )
  )
}

create_severe_outcome <- function(severity) {
  factor(ifelse(severity >= 3, "1", "0"), levels = c("0", "1"))
}

prepare_accident_outcomes <- function(data) {
  data |>
    dplyr::mutate(
      Weather_Group = create_weather_group(Weather_Condition),
      Severe = create_severe_outcome(Severity)
    )
}

summarize_missingness <- function(data) {
  result <- data.frame(
    Variable = names(data),
    Missing = colSums(is.na(data)),
    Percent = round(colMeans(is.na(data)) * 100, 2),
    row.names = NULL
  )
  result[order(-result$Percent), ]
}

impute_precipitation_zero <- function(
    data,
    precipitation_col = "Precipitation(in)"
) {
  if (!precipitation_col %in% names(data)) {
    stop("The precipitation column was not found.")
  }
  data[[precipitation_col]][is.na(data[[precipitation_col]])] <- 0
  data
}

drop_missing_model_rows <- function(
    data,
    required_cols = c(
      "Temperature(F)", "Humidity(%)",
      "Visibility(mi)", "Wind_Speed(mph)"
    )
) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  data[stats::complete.cases(data[, required_cols, drop = FALSE]), ]
}

select_model_features <- function(
    data,
    features = c(
      "Temperature(F)", "Humidity(%)", "Visibility(mi)",
      "Wind_Speed(mph)", "Precipitation(in)", "Weather_Group",
      "Bump", "Crossing", "Give_Way", "Junction", "Railway",
      "Roundabout", "Stop", "Traffic_Signal", "Traffic_Calming",
      "Severe"
    )
) {
  missing_cols <- setdiff(features, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing modeling columns: ", paste(missing_cols, collapse = ", "))
  }
  data[, features, drop = FALSE]
}

split_accident_data <- function(
    data,
    outcome = "Severe",
    prop = 0.80,
    seed = 42
) {
  set.seed(seed)
  index <- caret::createDataPartition(data[[outcome]], p = prop, list = FALSE)
  list(
    train = data[index, , drop = FALSE],
    test = data[-index, , drop = FALSE]
  )
}

factorize_model_data <- function(
    train,
    test,
    factor_cols = c(
      "Weather_Group", "Bump", "Crossing", "Give_Way", "Junction",
      "Railway", "Roundabout", "Stop", "Traffic_Signal",
      "Traffic_Calming", "Severe"
    )
) {
  available_factor_cols <- intersect(
    factor_cols,
    intersect(names(train), names(test))
  )
  
  for (column in available_factor_cols) {
    train_levels <- unique(as.character(train[[column]]))
    train_levels <- train_levels[!is.na(train_levels)]
    
    train[[column]] <- factor(
      train[[column]],
      levels = train_levels
    )
    
    test[[column]] <- factor(
      test[[column]],
      levels = train_levels
    )
  }
  
  if ("Severe" %in% names(train)) {
    train$Severe <- factor(train$Severe, levels = c("0", "1"))
  }
  
  if ("Severe" %in% names(test)) {
    test$Severe <- factor(test$Severe, levels = c("0", "1"))
  }
  
  list(
    train = train,
    test = test
  )
}

balance_classes <- function(
    data,
    outcome = "Severe",
    n_per_class = 100000,
    seed = 42
) {
  class_counts <- table(data[[outcome]])
  if (any(class_counts < n_per_class)) {
    stop("n_per_class exceeds the available rows in a class.")
  }
  
  set.seed(seed)
  sampled <- lapply(names(class_counts), function(label) {
    rows <- data[as.character(data[[outcome]]) == label, , drop = FALSE]
    rows[sample(seq_len(nrow(rows)), n_per_class), , drop = FALSE]
  })
  
  result <- do.call(rbind, sampled)
  result <- result[sample(seq_len(nrow(result))), , drop = FALSE]
  rownames(result) <- NULL
  result
}

fit_logistic_model <- function(train, outcome = "Severe") {
  stats::glm(
    stats::as.formula(paste(outcome, "~ .")),
    data = train,
    family = stats::binomial()
  )
}

fit_random_forest <- function(
    train,
    outcome = "Severe",
    ntree = 100,
    seed = 42
) {
  train <- train
  names(train) <- make.names(names(train))
  set.seed(seed)
  
  randomForest::randomForest(
    stats::as.formula(paste(make.names(outcome), "~ .")),
    data = train,
    ntree = ntree,
    importance = TRUE
  )
}

evaluate_classifier <- function(
    truth,
    predicted_class,
    predicted_probability,
    positive = "1"
) {
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

# -----------------------------
# Plotting functions
# -----------------------------

plot_severity_distribution <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = factor(Severity))) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = "Distribution of Accident Severity",
      x = "Severity Level",
      y = "Number of Accidents"
    ) +
    ggplot2::theme_minimal()
}

plot_top_states <- function(data, n_states = 15) {
  plot_data <- data |>
    dplyr::count(State, sort = TRUE) |>
    dplyr::slice_head(n = n_states)
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = reorder(State, n), y = n)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", n_states, "States by Number of Reported Accidents"),
      x = "State",
      y = "Number of Accidents"
    ) +
    ggplot2::theme_minimal()
}

plot_binary_severity_distribution <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = Severe)) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = "Distribution of Severe and Non-Severe Accidents",
      x = "Severe Accident",
      y = "Count"
    ) +
    ggplot2::theme_minimal()
}

plot_numeric_distribution <- function(data, variable, title, x_label, bins = 30) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[variable]])
  ) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(title = title, x = x_label, y = "Count") +
    ggplot2::theme_minimal()
}

plot_numeric_by_severity <- function(data, variable, title, y_label) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = factor(Severe), y = .data[[variable]])
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      title = title,
      x = "Severe Accident",
      y = y_label
    ) +
    ggplot2::theme_minimal()
}

plot_temperature_distribution <- function(data) {
  plot_numeric_distribution(
    data, "Temperature(F)",
    "Temperature Distribution", "Temperature (°F)"
  )
}

plot_temperature_by_severity <- function(data) {
  plot_numeric_by_severity(
    data, "Temperature(F)",
    "Temperature by Severity", "Temperature (°F)"
  )
}

plot_humidity_distribution <- function(data) {
  plot_numeric_distribution(
    data, "Humidity(%)",
    "Humidity Distribution", "Humidity (%)"
  )
}

plot_humidity_by_severity <- function(data) {
  plot_numeric_by_severity(
    data, "Humidity(%)",
    "Humidity by Severity", "Humidity (%)"
  )
}

plot_visibility_distribution <- function(data) {
  plot_numeric_distribution(
    data, "Visibility(mi)",
    "Visibility Distribution", "Visibility (mi)"
  )
}

plot_visibility_by_severity <- function(data) {
  plot_numeric_by_severity(
    data, "Visibility(mi)",
    "Visibility by Severity", "Visibility (mi)"
  )
}

plot_wind_speed_distribution <- function(data) {
  plot_numeric_distribution(
    data, "Wind_Speed(mph)",
    "Wind Speed Distribution", "Wind Speed (mph)"
  )
}

plot_wind_speed_by_severity <- function(data) {
  plot_numeric_by_severity(
    data, "Wind_Speed(mph)",
    "Wind Speed by Severity", "Wind Speed (mph)"
  )
}

plot_precipitation_distribution <- function(data) {
  plot_numeric_distribution(
    data, "Precipitation(in)",
    "Precipitation Distribution", "Precipitation (in)"
  )
}

plot_precipitation_by_severity <- function(data) {
  plot_numeric_by_severity(
    data, "Precipitation(in)",
    "Precipitation by Severity", "Precipitation (in)"
  )
}

plot_severity_proportion <- function(
    data,
    x_variable,
    title,
    x_label,
    rotate_labels = FALSE
) {
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = factor(.data[[x_variable]]),
      fill = factor(Severe)
    )
  ) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = "Proportion",
      fill = "Severe"
    ) +
    ggplot2::theme_minimal()
  
  if (rotate_labels) {
    plot <- plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
  
  plot
}

plot_severity_by_weather_group <- function(data) {
  plot_severity_proportion(
    data,
    "Weather_Group",
    "Severity by Weather Group",
    "Weather Group",
    rotate_labels = TRUE
  )
}

plot_severity_by_traffic_signal <- function(data) {
  plot_severity_proportion(
    data,
    "Traffic_Signal",
    "Severity by Traffic Signal",
    "Traffic Signal"
  )
}

plot_severity_by_junction <- function(data) {
  plot_severity_proportion(
    data,
    "Junction",
    "Severity by Junction",
    "Junction"
  )
}

plot_severity_by_crossing <- function(data) {
  plot_severity_proportion(
    data,
    "Crossing",
    "Severity by Crossing",
    "Crossing"
  )
}

plot_severity_by_stop <- function(data) {
  plot_severity_proportion(
    data,
    "Stop",
    "Severity by Stop Sign",
    "Stop Sign Present"
  )
}

plot_numeric_correlation <- function(data) {
  num_vars <- data[, c(
    "Temperature(F)", "Humidity(%)", "Visibility(mi)",
    "Wind_Speed(mph)", "Precipitation(in)"
  )]
  
  corr_matrix <- stats::cor(num_vars, use = "complete.obs")
  corrplot::corrplot(
    corr_matrix,
    method = "color",
    type = "upper",
    tl.col = "black"
  )
}
