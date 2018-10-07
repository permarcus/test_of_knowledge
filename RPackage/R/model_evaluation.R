#' Compute evaluation metrics for a given model
#'
#' @param test_model a \code{prospecting_leads} object for the validation set.
#' @param train_data a \code{train_data_set} object with the variable \code{validate_set} that indicates if a validate set or training set.
#' @param cfg a \code{prospecting_bot_config} object.
#'
#' @export
model_evaluation <- function(test_model, test_data, cfg){
  checkmate::assert_class(test_model, "test_model")
  checkmate::assert_class(cfg, "test_bot_config")
  
  # Create validation set and predict on validation set
  validate_set_prob <- predict_func(test_model, data_set = test_data, cfg)
  # cfg$model$no_leads <- no_leads
  
  # Add true values to validation set
  validate_set_prob <- dplyr::left_join(validate_set_prob, test_data[,c("company_id", "y")], by = "company_id")
  # names(validate_set_prob)[3] <- "true_value"

  # Compute evaluation metrics  
  eval_list <- list()
  eval_list$top <- sum(validate_set_prob$y)
  eval_list$threshold <- mean(train_data$y)
  eval_list$rmse <- sqrt(mean((validate_set_prob$y - validate_set_prob$score)^2))
  
  validate_set_prob$pred_class <- as.numeric(validate_set_prob$score > eval_list$threshold)
  eval_list$confusion_matrix <- caret::confusionMatrix(factor(round(validate_set_prob$pred_class), levels = c("0","1")), factor(round(validate_set_prob$y), levels = c("0","1")))
  
  eval_list$rank_measures <- list()
  #eval_list$rank_measures$mean_validation_rank <- 
  #  rank_evaluation(full_data = validation_full_data, normalized = TRUE, fold = "validation_set", FUN = mean)$y
  #eval_list$rank_measures$mean_train_rank <- 
  #  rank_evaluation(validation_full_data, normalized = TRUE, fold = "full_training_set", FUN = mean)$y
  #eval_list$rank_measures$sd_validation_rank <- 
  #  rank_evaluation(validation_full_data, normalized = TRUE, fold = "validation_set", FUN = stats::sd)$y
  #eval_list$rank_measures$sd_train_rank <- 
  #  rank_evaluation(validation_full_data, normalized = TRUE, fold = "full_training_set", FUN = stats::sd)$y  
  #
  #eval_list$rank_measures$mean_validation_rank_sale_process <- 
  #  rank_evaluation(full_data = validation_full_data, normalized = TRUE, fold = "validation_set", use_y = FALSE, FUN = mean)
  #eval_list$rank_measures$mean_train_rank_sale_process <- 
  #  rank_evaluation(validation_full_data, normalized = TRUE, fold = "full_training_set", use_y = FALSE, FUN = mean)
  
  #eval_list$validate_set <- list(size = sum(train_data$validate_set), 
  #                               table_y = table(as.factor(train_data$y[train_data$validate_set])))
  
  class(eval_list) <- c("model_evaluation", "list")
  eval_list
}

