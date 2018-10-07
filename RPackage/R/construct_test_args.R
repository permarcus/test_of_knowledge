#' Assert test bash arguments
#' 
#' @description 
#' If bash is used to call a function, assertion is of use since they will help one find
#' errors as early as possible in the process. This function is used to assert that the 
#' arguments from bash is correct.
#' 
#' @param args character vector to assert.
#' 
#' @export
test_args <- function(args){
  class(args) <- c("test_args", class(args))
  assert_test_args(args)
  args
}  

#' Assert test bash arguments
#' 
#' @description 
#' Function to assert the arguments sent from bash to the production script.
#' 
#' @param args character vector to assert.
#' @keywords internal
assert_test_args <- function(args){
  checkmate::assert_class(args, "test_args")
  checkmate::assert_character(args, len = 2)
  checkmate::assert_choice(args[1], "--config")
  checkmate::assert_file_exists(args[2])
}