#' Get data test config 
#'
#' @description 
#' Function to get and parse config
#'
#' @param cfg_file config file path
#' 
#' @return 
#' A \code{data_test_config} object.
#' 
#' @export
get_test_bot_config <- function(cfg_file){
  checkmate::assert_file_exists(cfg_file)
  
  json_string <- paste(readLines(cfg_file), collapse = "\n")
  
  cfg <- yaml::read_yaml(cfg_file)
  
  cfg <- test_bot_config(cfg)
  
  return(cfg)
}