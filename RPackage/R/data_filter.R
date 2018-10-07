#' Function for filtering out unwanted data
#' 
#' @param dat a dataset containg information about companies one wants to extract
#' @param net_sales parameter which one uses to filter the extisting data frame
#' @param tot_turnover parameter which one uses to filter the extisting data frame
#' @param nbr_emp parameter which one uses to filter the extisting data frame
#' @param growth_emp parameter which one uses to filter the extisting data frame
#' @param growth_turnover parameter which one uses to filter the extisting data frame
#' @param prof_margh parameter which one uses to filter the extisting data frame
#' @param laen parameter which one uses to filter the extisting data frame
#' @param city parameter which one uses to filter the extisting data frame
#' @param sector parameter which one uses to filter the extisting data frame
#' 
#' @export
data_filter <- function(dat,
                        net_sales_over = NULL,
                        tot_turnover_over = NULL,
                        nbr_emp_over = NULL,
                        growth_emp_over = NULL,
                        growth_turnover_over = NULL,
                        prof_marg_over = NULL,
                        net_sales_under = NULL,
                        tot_turnover_under = NULL,
                        nbr_emp_under = NULL,
                        growth_emp_under = NULL,
                        growth_turnover_under = NULL,
                        prof_marg_under = NULL,
                        laen = NULL,
                        city = NULL,
                        sector = NULL
){
  checkmate::assert_numeric(net_sales_over, null.ok = TRUE)
  checkmate::assert_numeric(tot_turnover_over, null.ok = TRUE)
  checkmate::assert_numeric(nbr_emp_over, null.ok = TRUE)
  checkmate::assert_numeric(growth_emp_over, null.ok = TRUE)
  checkmate::assert_numeric(growth_turnover_over, null.ok = TRUE)
  checkmate::assert_numeric(prof_marg_over, null.ok = TRUE)
  checkmate::assert_numeric(net_sales_under, null.ok = TRUE)
  checkmate::assert_numeric(tot_turnover_under, null.ok = TRUE)
  checkmate::assert_numeric(nbr_emp_under, null.ok = TRUE)
  checkmate::assert_numeric(growth_emp_under, null.ok = TRUE)
  checkmate::assert_numeric(growth_turnover_under, null.ok = TRUE)
  checkmate::assert_numeric(prof_marg_under, null.ok = TRUE)
  checkmate::assert_character(laen, null.ok = TRUE)
  checkmate::assert_character(city, null.ok = TRUE)
  checkmate::assert_character(sector, null.ok = TRUE)
  
  dat <- dplyr::filter(dat, COUNTRY == "SE")
  
  if(!is.null(net_sales_over)) dat <- dplyr::filter(dat, NET_SALES >= net_sales_over)
  if(!is.null(tot_turnover_over)) dat <-dplyr::filter(dat, TOT_TURNOVER >= tot_turnover_over)
  if(!is.null(nbr_emp_over)) dat <- dplyr::filter(dat, NBR_EMP >=nbr_emp_over)
  if(!is.null(growth_emp_over)) dat <-dplyr::filter(dat, NBR_EMP_GROWTH >= growth_emp_over)
  if(!is.null(growth_turnover_over)) dat <- dplyr::filter(dat, TURNOVER_GROWTH >= growth_turnover_over)
  if(!is.null(prof_marg_over)) dat <- dplyr::filter(dat,PROF_MARG >= prof_marg_over)
  if(!is.null(net_sales_under)) dat <-dplyr::filter(dat, NET_SALES < net_sales_under)
  if(!is.null(tot_turnover_under)) dat <- dplyr::filter(dat, TOT_TURNOVER < tot_turnover_under)
  if(!is.null(nbr_emp_under)) dat <-dplyr::filter(dat, NBR_EMP < nbr_emp_under)
  if(!is.null(growth_emp_under)) dat <- dplyr::filter(dat, NBR_EMP_GROWTH < growth_emp_under)
  if(!is.null(growth_turnover_under)) dat <-dplyr::filter(dat, TURNOVER_GROWTH < growth_turnover_under)
  if(!is.null(prof_marg_under)) dat <-dplyr::filter(dat, PROF_MARG < prof_marg_under)
  if(!is.null(laen)) dat <- dplyr::filter(dat, TERR == laen)
  if(!is.null(city)) dat <- dplyr::filter(dat, CNTY == city)
  if(!is.null(sector)) dat <- dplyr::filter(dat, tolve_manual_sector == sector)
  
  return(dat) 
  
}



