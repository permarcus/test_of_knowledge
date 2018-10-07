#' Create a \code{data_cleaner_bot_config} object
#' 
#' @param cfg an \code{tbl_df} to convert to a \code{test_bot_config} object.
#' 
#' @export
test_bot_config <- function(cfg){
  checkmate::assert_class(cfg, "list")
  class(cfg) <- c("test_bot_config", "list")
  assert_data_cleaner_bot_config(cfg)
  cfg
}

#' Assert a \code{data_cleaner_bot_config}
#' 
#' @param cfg an object to check
#' @keywords internal
assert_data_cleaner_bot_config <- function(cfg){
  checkmate::assert_class(cfg, "test_bot_config")
  checkmate::assert_class(cfg, "list")
  
}

