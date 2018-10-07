#' Train test split
#' 
#' @param data data set to be used in train test split
#' @param cfg a test bot config
#' 
#' @export
train_test_split <- function(data, cfg){
  checkmate::assert_class(data, c("tbl_df", "tbl", "data.frame"))
  checkmate::assert_class(cfg, c("test_bot_config", "list"))
  
    data$y <- sample(c(0:(cfg$params$number_of_factors - 1)), replace=TRUE, size=nrow(data))
    
    data <- dplyr::select(data, names(data)[1], y, names(data))
    
    train <- dplyr::sample_frac(tbl = data, size = cfg$params$train_proportion)
    test <- dplyr::anti_join(x = data, y = train)

    data <- list()
    data$train <- train
    data$test <- test
  
  return(data)    
}