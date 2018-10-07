#' Compute rank evaluation metrics
#' 
#' @details 
#' This functions computes rank evaluations on the full \code{company_data}.
#'
#' @param full_data A \code{prospecting_leads} object with variables \code{company_id}, \code{score}, \code{true_value} and \code{validation_set}.
#' @param normalized Should the rank be normalized from 0 to 1?
#' @param fold For what fold should the rank be computed. \code{full_training_set} or \code{validation_set}.
#' @param use_y Use \code{y} variable to compute rank. If \code{FALSE}, use unique values in \code{sale_process_stage}.
#' @param FUN Function to compute the rank statistic for the 1:s. 
#' @param ... Further arguments to \code{FUN}.
#' 
rank_evaluation <- function(full_data, normalized = TRUE, fold = "validation_set", use_y = TRUE, FUN = mean, ...){
  checkmate::assert_class(full_data, "tbl_df")
  checkmate::assert_names(c("company_id", "score", "y", "sale_process_stage", "validate_set"), subset.of = names(full_data))
  #  TolveProspectingBot:::assert_company_id(full_data$company_id)
  assert_company_id(full_data$company_id)
  checkmate::assert_flag(normalized)
  checkmate::assert_choice(fold, choices = c("validation_set", "full_training_set"))
  checkmate::assert_class(FUN, "function")
  
  # Ungroup
  full_data <- dplyr::ungroup(full_data)
  
  # Order the dataset according to score (randomize ties)
  full_data$runif <- stats::runif(nrow(full_data))
  full_data <- dplyr::arrange(full_data, dplyr::desc(score), runif)
  full_data$rank <- 1:nrow(full_data)
  if(normalized) full_data$rank <- full_data$rank / nrow(full_data)
  
  # Compute rank statistics
  if(use_y){
    if(fold == "validation_set"){
      subset_data <- dplyr::filter(full_data, y == 1, validate_set)
    }
    if(fold == "full_training_set"){
      subset_data <- dplyr::filter(full_data, y == 1)
    }
    
    # Compute statistic
    return(list(y = FUN(subset_data$rank, ...)))
    
  } else { # Compute rank statistics for all classes
    res <- list()
    
    if(fold == "validation_set"){
      full_validation_data <- dplyr::filter(full_data, validate_set)
      sale_process_stage_categories <- unique(full_validation_data$sale_process_stage)
      for(i in seq_along(sale_process_stage_categories)){
        subset_data <- dplyr::filter(full_validation_data, sale_process_stage == sale_process_stage_categories[i])
        res[[sale_process_stage_categories[i]]] <- FUN(subset_data$rank, ...)
      }
    }
    if(fold == "full_training_set"){
      sale_process_stage_categories <- unique(full_data$sale_process_stage)
      sale_process_stage_categories <- sale_process_stage_categories[!is.na(sale_process_stage_categories)]
      for(i in seq_along(sale_process_stage_categories)){
        subset_data <- dplyr::filter(full_data, sale_process_stage == sale_process_stage_categories[i])
        res[[sale_process_stage_categories[i]]] <- FUN(subset_data$rank, ...)
      }
    }
    return(res)
  }
  stop("Error!!! This should not happen!")
}
utils::globalVariables(c("score", "y", "sale_process_stage", "validate_set"))
