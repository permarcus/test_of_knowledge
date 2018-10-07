# This is a scripts to show data handling process.
# It will process ML split of train and test/validation data with the ue of the caret package
# All other data-mangling will be done by either dplyr or in-built thingies

# Setting seed for consistence
set.seed(4711)

# Imported packages (they are also set as dependencies within the DESCRIPTION file in package root.)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(caret))

# Install the local package (this one)
# devtools::install_local(path = "RPackage")

# Load local library
suppressPackageStartupMessages(library(KnowledgeTest))

# I usually work with configs since they minimize change within package and therefor less
# room for errors

args <- c("--config", "tests/configs/data_handling_config.yaml")
args <- test_args(args)

# Read in config ----
cfg <- get_test_bot_config(cfg_file = args[2])
if(cfg$verbose) cat(args, "\n")
if(cfg$verbose) cat("Read config object\n")

# I like to work with try and checkmate validation for easy understandng 
# of where errors may have occured

try_result <- try({
 # Read in data----
  if(cfg$verbose) cat("Read data\n")
  data <- read_data(cfg)
 
  #  Filter data based on set criterium for what we want and dont want
  #if(cfg$verbose) cat("Filter data based on pre-set conditions\n")
  #data <- data_filter(dat = dat_undisc, net_sales_over = NULL, tot_turnover_over = NULL, nbr_emp_over = NULL, growth_emp_over = NULL, growth_turnover_over = NULL, prof_marg_over = NULL, net_sales_under = NULL, tot_turnover_under = NULL, nbr_emp_under = NULL, growth_emp_under = NULL, growth_turnover_under = NULL, prof_marg_under = NULL, laen = NULL, city = NULL, sector = NULL)
  
  # Train test split, and setting of dependent number of groups (y)----
  if(cfg$verbose) cat("Train test split data\n")
  data_list <- train_test_split(data, cfg)
   
    train_data <- data_list$train
    test_data <- data_list$test
    
  if(cfg$verbose) cat("Train model\n")
    test_model <- train_model(data_set = train_data, cfg)
  
  if(cfg$verbose) cat("Prospect\n")
    predicted_data <- predict_func(test_model, train_data, cfg)
      
  if(cfg$verbose) cat("Compute evaluation metrics\n")
    model_eval <- model_evaluation(test_model, test_data, cfg)
    
    
    
})
model_eval

if(inherits(try_result, "try-error")){
  quit(save = "no", status = 1)
}



