#' Train model
#'
#' @param data_set a training data set
#' @param cfg a \code{test_config} object.
#' 
#' @return a \code{model} object
#' 
#' @export
train_model <- function(data_set, cfg){
  checkmate::assert_class(data_set, c("tbl_df", "tbl", "data.frame"))
  checkmate::assert_class(cfg, "test_bot_config")
  
  # Remove company_id, sale_process_stage, company_to and company_from
  data_set <- data_set[, -which(colnames(data_set) %in% c("company_id", "company_to", "company_from", "sale_process_stage"))]
  
  mod <- 
    switch(cfg$model$model,
           "log_reg" = train_prospecting_model.log_reg(data_set, cfg),
           "log_lasso" = train_prospecting_model.log_enet(data_set, cfg, alpha = 1),           
           "log_elastic_net" = train_prospecting_model.log_elastic_net(data_set, cfg),
           "svm_e1071" = train_prospecting_model.svm_e1071(data_set, cfg),
           "xgboost" = train_prospecting_model.xgboost(data_set, cfg))
  
  class(mod) <- c("test_model", class(mod))
  mod 
}


#' Implemented models
#' @rdname train_prospecting_model
implemented_prospecting_models <- function(){
  c("log_reg",
    "log_lasso",
    "log_lasso_lambda_zero",
    "log_enet",
    "log_enet_0.9",
    "log_ridge",
    "log_elastic_net",
    "svm_e1071",
    "xgboost")
}

#' @rdname train_prospecting_model
train_prospecting_model.log_reg <- function(data_set, cfg){
  checkmate::assert_integerish(data_set$y)
  checkmate::assert_true(!any(names(data_set) %in% c("company_id", "sale_process_stage", "company_to", "company_from")))
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "y")))
  }
  
  
  X <- stats::model.matrix(y ~ . , data = data_set)
  checkmate::assert(ncol(X) < nrow(X))
  
  y <- as.factor(data_set$y)
  log_reg_mod <- stats::glm(y ~ X, family = stats::binomial(link = "logit"))
  
  return(log_reg_mod)
}

#' @param alpha Elastic net parameter.
#' @param lambda Penalty parameter for the elastic net.
#' 
#' @rdname train_prospecting_model
train_prospecting_model.log_enet <- function(data_set, cfg, alpha, lambda = NULL){
  checkmate::assert_integerish(data_set$y)
  checkmate::assert_true(!any(names(data_set) %in% c("company_id", "sale_process_stage", "company_to", "company_from")))
  checkmate::assert_numeric(alpha, lower = 0, upper = 1)
  checkmate::assert_number(lambda, lower = 0, null.ok = TRUE)
  if(!is.null(lambda)) checkmate::assert_number(alpha, lower = 0, upper = 1)
  
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "y")))
  }
  
  # Create model matrix
  xfacts_gen <- stats::model.matrix(y ~ . , data = data_set)
  y <- as.factor(data_set$y)
  # dim(xfacts_gen)
  
  # Train LASSO
  ## Lasso is created with non-constant lambda, find optimal lambda and alpha 
  if(is.null(lambda)){
    cv_list <- list()
    min_cvm <- numeric(length(alpha))
    foldid <- sample(1:10, size=length(y), replace=TRUE)
    for(i in seq_along(alpha)){
      cv_list[[i]] <- glmnet::cv.glmnet(x=xfacts_gen, foldid = foldid,
                                        y=y, alpha=alpha[i], family='binomial')
      min_cvm[i] <- cv_list[[i]]$cvm[which(cv_list[[i]]$lambda == cv_list[[i]]$lambda.min)]
    }
    idx <- which.min(min_cvm)
    lambda <- cv_list[[idx]]$lambda.min
    alpha <- alpha[idx]
  }
  
  # Using optimal lambda and alpha - model for prediction
  glmmod_lambda <- 
    glmnet::glmnet(x=xfacts_gen,
                   y=y, alpha=alpha, family='binomial', lambda = lambda)
  glmmod_lambda$cv_best_parameters <- list("alpha" = alpha)
  
  return(glmmod_lambda)
}

#' @rdname train_prospecting_model
train_prospecting_model.svm_e1071 <- function(data_set, cfg){
  checkmate::assert_integerish(data_set$y)
  checkmate::assert_true(!any(names(data_set) %in% c("company_id", "sale_process_stage", "company_to", "company_from")))
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "y")))
  }
  
  model_parameters <- extract_model_parameters(cfg)
  checkmate::assert_numeric(model_parameters$cost, lower = 0)
  checkmate::assert_numeric(model_parameters$gamma, lower = 0)
  
  svm_tune <- e1071::best.tune("svm",
                               as.factor(y) ~ . , data = data_set,
                               probability = TRUE,
                               kernel="radial", 
                               ranges=list(cost = model_parameters$cost, 
                                           gamma = model_parameters$gamma))
  return(svm_tune)
}





#' @rdname train_prospecting_model
train_prospecting_model.log_elastic_net <- function(data_set, cfg){
  checkmate::assert_class(data_set, "tbl_df")
  checkmate::assert_class(cfg, "prospecting_bot_config")
  
  model_parameters <- extract_model_parameters(cfg)
  checkmate::assert_numeric(model_parameters$alpha, lower = 0, upper = 1)
  checkmate::assert_number(model_parameters$lambda, lower = 0, null.ok = TRUE)
  
  train_prospecting_model.log_enet(data_set, cfg, alpha = model_parameters[["alpha"]], lambda = model_parameters[["lambda"]])
}



#' @rdname train_prospecting_model
train_prospecting_model.xgboost <- function(data_set, cfg){
  checkmate::assert_integerish(data_set$y)
  checkmate::assert_true(!any(names(data_set) %in% c("company_id", "sale_process_stage", "company_to", "company_from")))
  if(!is.null(cfg$model$use_company_variables)) {
    checkmate::assert_true(all(cfg$model$use_company_variables %in% names(data_set)))
    checkmate::assert_true(all(names(data_set) %in% c(cfg$model$use_company_variables, "y")))
  }
  
  model_parameters <- extract_model_parameters(cfg)
  param_grid <-  expand.grid(model_parameters[-1])
  if(nrow(param_grid) > 1){
    fitControl <- caret::trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 1)
    cv_train <- suppressPackageStartupMessages(caret::train(as.factor(y) ~ ., data = data_set, 
                                                            method = "xgbTree", 
                                                            trControl = fitControl, 
                                                            verbose = FALSE, 
                                                            ## Now specify the exact models 
                                                            ## to evaluate:
                                                            tuneGrid = param_grid))
    max_idx <- which.max(cv_train$results$Accuracy)
    xgb_params <- cv_train$results[max_idx,]
  } else {
    xgb_params <- param_grid
  }
  
  # Set missing as class
  # data_set[is.na(data_set)] <- "Missing" 
  
  # one hot coding
  labels <- data_set$y
  new_tr <- stats::model.matrix( ~ . + 0, data = data_set[, -1], with = FALSE) 
  
  dtrain <- xgboost::xgb.DMatrix(data = new_tr, label = labels) 
  
  # Setting params
  params <- list(booster = model_parameters$booster, 
                 objective = "binary:logistic", 
                 eta = xgb_params$eta, 
                 gamma = xgb_params$gamma, 
                 max_depth = xgb_params$max_depth, 
                 subsample = xgb_params$subsample, 
                 colsample_bytree = xgb_params$colsample_bytree,
                 min_child_weight = xgb_params$min_child_weight,
                 nrounds = xgb_params$nrounds)
  
  # Setting best run for perfomance, cross validation
  xgbcv <- xgboost::xgb.cv(params = params, data = dtrain, nrounds = 200, nfold = 10, 
                           showsd = TRUE, stratified = TRUE, verbose = cfg$verbose, early_stopping_rounds = 20, maximize = FALSE)
  
  xgb_model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = xgbcv$best_iteration, 
                                  watchlist = list(train=dtrain), verbose = cfg$verbose, early_stopping_rounds = 20, maximize = FALSE , eval_metric = "error")
  
  xgb_model$cv_best_parameters <- as.list(xgb_params[names(param_grid)])
  return(xgb_model)
}

