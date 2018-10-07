#' Function to assist with reading data
#' 
#' @param cfg config containing file path to data
#' 
#' @export
read_data <- function(cfg){
  checkmate::assert_file_exists(cfg$io$file_path)
  
  # Read in data if the data has ending .csv
  if(stringr::str_detect(tolower(cfg$io$file_path), "\\.csv$")){
    dat <- dplyr::as_data_frame(utils::read.csv2(cfg$io$file_path, stringsAsFactors = FALSE))
  }
  # Read in data if the data has ending .rda
  if(stringr::str_detect(tolower(cfg$io$file_path), "\\.rda$")){
    # Use built function for assert naming of inported file
    dat <- data_import(cfg$io$file_path)
  }
  return(dat)
}


#' data import
#' @rdname data_import
#' 
#' @param path a \code{path} to old data to be parsed with new data
data_import <- function(path){
  checkmate::assert_file_exists(path)
  
  dat <- load(path)
  dat <- get(dat)
  
  return(dat)
}

#' @rdname data_connection
#' @keywords Internal
local_dc_type <- function(path){
  checkmate::assert_string(path)
  if(stringr::str_detect(tolower(path), "\\.csv$")) return("csv")
  if(stringr::str_detect(tolower(path), "\\.rda$")) return("rda")
  if(stringr::str_detect(tolower(path), "\\.")) return("file")
  return("folder")
}


