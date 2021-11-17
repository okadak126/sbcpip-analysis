##
## The build_prediction table fails beyond date 2018-10-30
## So way to fix is to delete all output and reports and logs after that date.
## to run the dashboard
##
library(SBCpip)
library(DBI)
#library(RSQLite)
library(duckdb)
library(magrittr)
options(warn = 1)

# Set my params
set_config_param(param = "output_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Outputs")
set_config_param(param = "log_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Logs")


set_config_param(param = "report_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Reports")

config <- set_config_param(param = "data_folder",
                           value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_inc")

config$history_window <- 100
config$initial_collection_data <- c(60, 60, 60)
config$initial_expiry_data <- c(0,0)

config$census_locations = c('B1','B2', 'B3', 'C1', 'C2', 'C3',
                     'CAPR XFER OVERFL', 'CATH PACU', 'CDU-CLIN DEC UNIT',
                     'D1CC', 'D1CS', 'D2', 'D3', 'DGR',
                     'E1', 'E2-ICU', 'E29-ICU', 'E3',
                     'EMERGENCY DEPARTMENT',
                     'F3', 'FGR', 'G1', 'G2P', 'G2S', 'H1', 'H2') # 2019 set


config$census_locations = c('B1','B2', 'B3', 'C1', 'C2', 'C3',
                            #'CAPR XFER OVERFL', 'CATH PACU', 'CDU-CLIN DEC UNIT',
                            'D1CC', 'D1CS', 'D2', 'D3', 'DGR',
                            'E1', 'E2-ICU', 'E29-ICU', 'E3',
                            'EMERGENCY DEPARTMENT',
                            'F3', 'FGR', 'G1', 
                            #'G2P', 
                            #'G2S', 'H1', 'H2', 
                            "VCP 1 WEST", "VCP 2 NORTH", "VCP 2 WEST", "VCP 3 WEST", 
                            "VCP CCU 1", "VCP CCU 2", "VCP NICU", "VCP NURSERY") # 2019 set w/ VCP

config$census_locations = c("B1", "B2", "B3", "C1", "C2", "C3", "E2-ICU", 
                            "EGR", 
                            "E3", "F3", "FGR", "G2S", "CDU-CLIN DEC UNIT", 
                            "E1", "J2", "J4", "J5", "J6", "J7", "K4", "K5", "K6", 
                            "K7", "L4", "L5", "L6", "L7", "M4", "M5", "M6", "M7", 
                            "VCP 1 WEST", "VCP 2 NORTH", "VCP 2 WEST", "VCP 3 WEST", 
                            "VCP CCU 1", "VCP CCU 2", "VCP NICU", "VCP NURSERY") # 2020 set

config$census_locations = c("B1", "B2", "B3", "C1", "C2", "C3", "E2-ICU", "E1", 
                            "J2", "J4", "J5", "J6", "J7", "K4", "K5", "K6", 
                            "K7", "L4", "L5", "L6", "L7", "M4", "M5", "M6", "M7", 
                            "VCP 1 WEST", "VCP 2 WEST",
                            "VCP 2 NORTH",  "VCP 3 WEST", 
                            "VCP CCU 1", 
                            "VCP CCU 2", 
                            "VCP NICU", "VCP NURSERY") # modified 2020 set

#config$census_locations = c(
#  "B1", "B2", "B3", "E2-ICU",
#  "E1", "J2", "J4", "J5", "J6", "J7", "K4", "K5", "K6", 
#  "K7", "M4", "M5", "M6", "M7", 
#  "VCP 2 NORTH",  "VCP 2 WEST", 
#  "VCP CCU 1", 
#  "VCP NICU", "VCP NURSERY") # reduced 2020 set

config$surgery_services = NULL

config$surgery_services = c("Transplant", 
                            "Cardiac", 
                            "Bone Marrow Transplant",
                            "Neurosurgery", 
                            "Thoracic",
                            "Vascular",
                            "Hepatology",
                            "General", 
                            "Interventional Radiology")

pred_start_date <- as.Date('2020-06-20')
num_days <- 100

pred_end_date <- pred_start_date + num_days
config$l1_bounds <- seq(from = 50, to = 2, by = -2)
config$lag_bounds <- c(-1, 10)
config$c0 <- 15
config$start <- 10
config$penalty_factor <- 5
config$lo_inv_limit <- 20
config$hi_inv_limit <- 60
config$min_inventory <- 0

# DB version
#conn = dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)
conn = dbConnect(duckdb::duckdb(), "/Users/kaiokada/Desktop/Research/pip.duckdb", read_only = FALSE)

conn %>% sbc_build_and_save_seed_db(pred_start_date, config)

prediction <- conn %>% sbc_predict_for_range_db(pred_start_date, num_days, config)
full_pred_table_db <- conn %>% build_prediction_table_db(config, 
                                                         pred_start_date, 
                                                         pred_end_date)
pred_table_analysis(full_pred_table_db, config)
full_coef_table_db <- conn %>% build_coefficient_table_db(pred_start_date, num_days)
print(coef_table_analysis(full_coef_table_db, config)$other, n = 62)
dbDisconnect(conn, shutdown = TRUE)

# RDS Version
sbc_build_and_save_seed_data(pred_start_date, config)
prediction <- sbc_predict_for_range(pred_start_date, num_days, config)
full_pred_table <- build_prediction_table(config, pred_start_date, pred_end_date)
pred_table_analysis(full_pred_table, config)
full_coef_table <- build_coefficient_table(pred_start_date, num_days, config)
print(coef_table_analysis(full_coef_table, config)$other, n = 62)

write.csv(full_pred_table_db, "pred_table_2020jun_ko_fix_db.csv")
write.csv(full_coef_table_db, "coef_table_2020jun_ko_fix_db.csv")



## Working function - Analyze surgeries against transfusions
transfusions_per_surgery <- function(pred_start_date, num_days, config) {
  
  pred_end_date <- pred_start_date + num_days
  all_pred_dates <- seq.Date(from = pred_start_date, to = pred_end_date, by = 1L)
  
  surgery_transfusion_df <- NULL
  
  for (i in seq_along(all_pred_dates)) {
    date_str <- as.character(all_pred_dates[i])
    transfusion_file <- list.files(path = config$data_folder,
                                   pattern = sprintf(config$transfusion_filename_prefix, date_str),
                                   full.names = TRUE)
    surgery_file <- list.files(path = config$data_folder,
                               pattern = sprintf(config$surgery_filename_prefix, date_str),
                               full.names = TRUE)
    
    surgery <- read_one_surgery_file(surgery_file, config$surgery_services)
    transfusion <- read_one_transfusion_file(transfusion_file)
    
    surgery_data_cleaned <- surgery$raw_data %>% 
      distinct(.keep_all = TRUE) #%>%
      #filter(CASE_CLASS == "Elective")
    transfusion_data_cleaned <- transfusion$raw_data %>% 
      distinct(.keep_all = TRUE) %>%
      filter(.data$Type == "PLT") %>%
      mutate(PAT_MRN_ID = `Recip. MRN`)
    
    surgery_plus_transfusion <- surgery_data_cleaned %>% 
      left_join(transfusion_data_cleaned, by = "PAT_MRN_ID") %>%
      mutate(transfused = !is.na(`Issue Date/Time`))

    surgery_transfusion_df <- rbind(surgery_transfusion_df, surgery_plus_transfusion)
  }
  surgery_transfusion_df %>% 
    group_by(OR_SERVICE, transfused) %>%
    summarize(count = n()) %>%
    tidyr::pivot_wider(names_from = transfused, values_from = count, values_fill = 0) %>% 
    replace(., is.na(.), 0) %>%
    mutate(tf_percentage = `TRUE` / (`FALSE` + `TRUE`)) %>%
    arrange(desc(tf_percentage))
}

tfps <- transfusions_per_surgery(as.Date("2019-02-01"), 600, config) 
write.csv(tfps, "transfusions_by_surgery_type5.csv")



# Working code - DuckDB database construction


# Test Table
conn = dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)
sbc_build_and_save_seed_db(conn, pred_start_date, config)
predict_for_date_db(conn, config, date = "2020-03-10")
predict_for_date_db(conn, config, date = "2020-03-11")
conn %>% dplyr::tbl("model")

conn %>% build_coefficient_table_db_2(pred_start_date, num_days)
dates <- seq.Date(from = as.Date("2020-06-20"), to = as.Date("2020-09-28"), by = 1L)
prediction_df <- data.frame(date = dates, t_pred = rep(0, length(dates)))
prediction_df$t_pred = c(161, 178, 181, 178, 181, 166, 162, 165, 177, 190, 187, 190, 163, 162, 171, 172, 188, 191, 180, 166, 153, 167, 170, 180, 171, 174, 167, 161, 160, 170, 186, 186, 180, 173, 167, 162, 180, 193, 182, 168, 155, 151, 173, 177, 184, 181, 185, 172, 163, 153, 165, 176, 176, 186, 179, 176, 165, 181, 187, 195, 201, 192, 174, 164, 174, 192, 190, 201, 192, 175, 168, 168, 184, 190, 199, 188, 175, 163, 170, 179, 180, 188, 174, 165, 166, 166, 178, 181, 178, 160, 155, 154, 170, 182, 176, 179, 169, 172, 148, 159, 170)
prediction_df
config$min_inventory = 0
config$history_window = 150
config$c0 = 15
config$start = 10

full_pred_tbl <- db %>% build_prediction_table_db(config, start_date = as.Date("2020-06-20"), end_date = as.Date("2020-09-28"), pred_tbl = prediction_df)
pred_table_analysis(full_pred_tbl, config)
write.csv(full_pred_tbl, "150")
coef_tbl <- db %>% build_coefficient_table_db(as.Date("2020-06-20"), 100)
coef_table_analysis(coef_tbl, config)
