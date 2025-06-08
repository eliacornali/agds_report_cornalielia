eval_knn_diff_k <- function(k_values, df_train, df_test, recipe) {
  
  # Data frame for results
  results_df <- data.frame()

  # For loop to fit different k values
  for (k in k_values) {
    
    # Fit KNN model for given k
    mod_knn <- caret::train(
      recipe,
      data = df_train |> drop_na(),
      method = "knn",
      trControl = caret::trainControl(method = "none"),
      tuneGrid = data.frame(k = k),
      metric = "RMSE"
    )
    
    # add predictions to the data frames
    df_train <- df_train |> 
      drop_na()
    df_train$fitted <- predict(mod_knn, newdata = df_train)
    
    df_test <- df_test |> 
      drop_na()
    df_test$fitted <- predict(mod_knn, newdata = df_test)
    
    # get metrics tables
    metrics_train <- df_train |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted)
    
    metrics_test <- df_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted)
    
    # extract values from metrics tables
    rmse_train <- metrics_train |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_train <- metrics_train |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    rmse_test <- metrics_test |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_test <- metrics_test |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    # Add to results
    results_df <- bind_rows(
      results_df,
      tibble(k = k,
             rmse_train = rmse_train,
             rmse_test = rmse_test,
             rsq_train = rsq_train,
             rsq_test = rsq_test)
    )
  }
  
  return(results_df)
}
