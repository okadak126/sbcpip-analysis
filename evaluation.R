##
## The build_prediction table fails beyond date 2018-10-30
## So way to fix is to delete all output and reports and logs after that date.
## to run the dashboard
##
library(SBCpip)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
options(warn = 1)

# set the appropriate config params based on the path to the main folder containing logs, outputs,
# reports, and data folders.
standard_config <- function(main_folder) {
  set_config_param(param = "output_folder", value = paste0(main_folder, "/Blood_Center_Outputs"))
  set_config_param(param = "log_folder", value = paste0(main_folder, "/Blood_Center_Logs"))
  set_config_param(param = "report_folder", value = paste0(main_folder, "/Blood_Center_Reports"))
  config <- set_config_param(param = "data_folder", value = paste0(main_foldr, "/Blood_Center"))
}

# process seed data from the seed dataset folder and save to RDS file for day before prediction
process_seed_dataset <- function(config, last_date) {
  cbc <- process_all_cbc_files(data_folder = config$data_folder,
                             cbc_abnormals = config$cbc_abnormals,
                             cbc_vars = config$cbc_vars)
  census <- process_all_census_files(data_folder = config$data_folder,
                                   locations = config$census_locations)
  transfusion <- process_all_transfusion_files(data_folder = config$data_folder)
  saveRDS(list(cbc = cbc, census = census, transfusion = transfusion),
          file = file.path(config$output_folder,
                           sprintf(config$output_filename_prefix, as.Character(last_date))))
}

# build a prediction on a range of dates
predict_on_incremental <- function(config, main_folder, start_date, end_date) {
  config <- set_config_param(param = "data_folder", value = paste0(main_folder, "/Blood_Center_inc"))
  config$history_window <- 200
  start_date <- start_date
  end_date <- end_date
  all_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  
  prediction <- NULL
  for (i in seq_along(all_dates)) {
    date_str <- as.character(all_dates[i])
    result <- predict_for_date(config = config, date = date_str)
    prediction <- rbind(prediction,result)
  }
  prediction
}

# summarize transfusion files to obtain true transfusion values.
process_all_transfusion_files_no_reports <- function(data_folder,
                                                     pattern = "LAB-BB-CSRP-Transfused*") {
  fileList <- list.files(data_folder, pattern = pattern , full.names = TRUE)
  names(fileList) <- basename(fileList)
  raw_transfusion <- lapply(fileList, read_one_transfusion_file)
  
  Reduce(f = rbind,
         lapply(raw_transfusion, function(x) x$transfusion_data)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(date)
}

actual_transfusions <- function(config) {
  transfusion_inc <- process_all_transfusion_files_no_reports(data_folder = config$data_folder)
  transfusion_dates <- sapply(transfusion_inc$date, function(x) {as.character(x, format = "%Y-%m-%d")})
  
  transfusion_used <- as.numeric(transfusion_inc$used)
  plt_used_df <- data.frame(transfusion_dates, transfusion_used)
  plt_used_tbl <- as_tibble(plt_used_df[5:nrow(plt_used_df),])
  plt_used_tbl$transfusion_dates %<>% as.Date
  
  names(plt_used_tbl) = c("date", "d_true")
  
  pred_used_tbl %>% mutate(dlead1 = lead(pred_used_tbl$d_true, 1)) %>%
    mutate(dlead2 = lead(pred_used_tbl$d_true, 2)) %>% 
    mutate(t_true = d_true + dlead1 + dlead2) %>% 
    select(date, t_true) -> true
  
  pred_used_tbl
}

# Set up predictions
main_folder <- "/Users/kaiokada/Desktop/Research/platelet_data_full"
config <- standard_config(main_folder)
start_date <- as.Date("2018-04-13")
end_date <- as.Date("2020-11-02")
process_seed_dataset(config, start_date - 1)
predicted_transfusions <- predict_on_incremental(config, main_folder, start_date, end_date)
actual_transfusions <- actual_transfusions(config)

predicted_transfusions %>% left_join(actual_transfusions, by="date") -> pred_and_true
ggplot(data=predicted_transfusions[5:nrow(pred_and_true),]) + geom_line(aes(x=date, y=t_pred)) + 
  geom_line(aes(x=date, y=t_true))

