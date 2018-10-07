#' Predict best thingies by model and data
#'
#' @description 
#' A function to use predict on model used for testing
#'  
#' @param test_model a \code{prospecting_model} object.
#' @param data_set a \code{company_data} object.
#' @param cfg a \code{prospecting_bot_config} object.
#'
#' @export
predict_func <- function(test_model, data_set, cfg){
  checkmate::assert_class(test_model, "test_model")
  checkmate::assert_class(data_set, c("tbl_df", "tbl", "data.frame"))
  checkmate::assert_class(cfg, "test_bot_config")
  
 # Remove company_to and company_from
  data_set <- data_set[, -which(colnames(data_set) %in% c("company_to", "company_from", "y"))]  
  
  leads_data <- 
    switch(class(test_model)[2],
           "lognet" = prospect.lognet(model, data_set, cfg),
           "glm" = prospect.glm(model, data_set, cfg),
           "svm.formula" = prospect.svm(model, data_set, cfg),
           "xgb.Booster" = prospect.xgboost(model, data_set, cfg))
  
  # Sort by best prob 
  leads_data <- leads_data[order(leads_data$score, decreasing = TRUE),]
  
  return(leads_data)
}

#' @rdname prospect
prospect.lognet <-function(model, data_set, cfg){
  
  x_matrix <- stats::model.matrix( ~ . , data = data_set[, -1])
  
  leads_df <- data_set[, 1]
  
  leads_df$score <- stats::predict(test_model, newx = x_matrix, type = "response")
  
  leads_df
}

#' @rdname prospect
prospect.glm <-function(model, data_set, cfg){
  x_matrix <- stats::model.matrix( ~ . , data = data_set[, -1])
  
  leads_df <- data_set[, 1]
  
  leads_df$score <- stats::predict(test_model, newx = x_matrix, type = "response")
  
  leads_df
  
}

#' @rdname prospect
prospect.svm <-function(model, data_set, cfg){
  checkmate::assert_class(model, "prospecting_model")
  checkmate::assert_class(model, "svm")
  checkmate::assert_class(data_set, "tbl_df")
  checkmate::assert_class(cfg, "prospecting_bot_config")
  checkmate::assert_true(!any(names(data_set) %in% c("sale_process_stage", "company_to", "company_from")))
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "company_id")))
  }
  checkmate::assert_string(names(data_set)[1], "company_id")
  
  # Create a leads object
  leads_df <- data_set[, 1]
  pred_mat <- predict(model, newdata = data_set[, -1], probability = TRUE)
  leads_df$score <- attr(pred_mat, "probabilities")[, "1"]
  
  leads_df
}


#' @rdname prospect
prospect.xgboost <-function(model, data_set, cfg){
  checkmate::assert_class(model, "prospecting_model")
  checkmate::assert_class(data_set, "tbl_df")
  checkmate::assert_class(cfg, "prospecting_bot_config")
  checkmate::assert_true(!any(names(data_set) %in% c("sale_process_stage", "company_to", "company_from")))
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "company_id")))
  }
  checkmate::assert_string(names(data_set)[1], "company_id")
  
  x_matrix <- Matrix::sparse.model.matrix(company_id ~ . -1, data = data_set)
  
  leads_df <- data_set[, 1]
  class(model) <- class(model)[2]
  leads_df$score <- stats::predict(model, x_matrix, type = "response")
  
  leads_df
}

