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
library(reshape2)
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

config$history_window <- 200

# Predict 100 days out from June 19, 2020 (seed data from 200 days prior)
pred_start_date <- as.Date("2020-06-19")
pred_end_date <- pred_start_date + 100
pred_end_date
seed_start_date <- pred_start_date - 201
seed_end_date <- pred_start_date - 1
all_seed_dates <- seq.Date(from = seed_start_date, to = seed_end_date, by = 1)

# In November 2019, Stanford Hospital underwent drastic changes in census locations,
# We need to determine which of the new locations is the best proxy for requiring
# blood transfusions. The pip code currently has a cap on 26 location variables, so I
# only include new locations beginning with J and M (there are also K and L series, 
# and M4 is missing)
config$census_locations <- c("FGR", "E2-ICU", "C3", "E3",  "B3", "B2", "F3", 
                             "C2", "E1", "B1", "H2", "VCP 3 WEST",
                             "VCP 2 WEST", "VCP 1 WEST", "VCL SKILLED NURSING FACILITY", 
                             "G2P", "G2S", "C1", "J2", "J4", "J5", "J6", "J7", "M5", "M6", "M7")
config$census_locations

# Process Seed Data over specified date range
# (This likely will not work on files that contain information for multiple days - pre 4/2018)
seed_data <- list(cbc = NULL, census = NULL, transfusion = NULL, inventory = NULL)
for (i in seq_along(all_seed_dates)) {
  date_str <- as.character(all_seed_dates[i])
  data_single_day <- process_data_for_date(config = config, date = date_str)
  seed_data$cbc %<>% rbind(data_single_day$cbc)
  seed_data$census %<>% rbind(data_single_day$census)
  seed_data$transfusion %<>% rbind(data_single_day$transfusion)
  seed_data$inventory %<>% rbind(data_single_day$inventory)
}

# Save the seed dataset
saveRDS(object = seed_data,
        file = file.path(config$output_folder,
                         sprintf(config$output_filename_prefix, as.character(seed_end_date))))

# Predict for specified date range
all_pred_dates <- seq.Date(from = pred_start_date, to = pred_end_date, by = 1)
all_pred_dates
prediction <- NULL
for (i in seq_along(all_pred_dates)) {
  date_str <- as.character(all_pred_dates[i])
  print(date_str)
  result <- predict_for_date(config = config, date = date_str)
  prediction <- rbind(prediction,result)
}

# Retrieve true transfusion data from the files (this fetches data from all files in folder)
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

transfusion_inc <- process_all_transfusion_files_no_reports(data_folder = config$data_folder)
transfusion_dates <- sapply(transfusion_inc$date, function(x) {as.character(x, format = "%Y-%m-%d")})
transfusion_used <- as.numeric(transfusion_inc$used)
plt_used_df <- data.frame(transfusion_dates, transfusion_used)
plt_used_tbl <- as_tibble(plt_used_df[1:nrow(plt_used_df),])
plt_used_tbl$transfusion_dates %<>% as.Date
names(plt_used_tbl) = c("date", "d_true")
plt_used_tbl %<>% filter(date >= pred_start_date) %<>% filter(date <= pred_end_date)

prediction %>% left_join(plt_used_tbl, by="date") -> pred_and_true
pred_and_true %>%
  mutate(dlead1 = lead(pred_and_true$d_true, 1)) %>%
  mutate(dlead2 = lead(pred_and_true$d_true, 2)) %>% 
  mutate(dlead3 = lead(pred_and_true$d_true, 3)) %>%
  mutate(t_true = dlead1 + dlead2 + dlead3) %>%
  mutate(surplus = t_pred - t_true) -> pred_and_true
pred_and_true %>% filter(is.na(t_true))
pred_and_true <- pred_and_true[2:(nrow(pred_and_true)-2),] # remove NAs

# Plots of predicted platelet usage vs. actual platelet usage, to compare high level trends
ggplot(data=pred_and_true[2:(nrow(pred_and_true)-2),]) + 
  geom_line(aes(x=date, y=t_pred)) + 
  geom_line(data=pred_and_true[1:(nrow(pred_and_true)-2),], aes(x=date, y=t_true)) +
  geom_vline(xintercept=c(as.Date("2019-09-01")))

# Generate prediction table
full_pred_table <- build_prediction_table(config, pred_start_date, pred_end_date)
write.csv(full_pred_table, "predictions_2020.csv")

# Collect model coefficients over all output files (only need one set per week)
output_files <- list.files(path = config$output_folder,
                           pattern = paste0("^",
                                            substring(config$output_filename_prefix, first = 1, last = 10)),
                           full.names = TRUE)
model_coefs <- NULL
print(paste("Number of days to process:", length(output_files)))
for (i in seq(1, length(output_files), 7)) {
  d <- readRDS(output_files[i])
  model_coefs %>% rbind(c(d$model$coefs, lambda = d$model$lambda)) -> model_coefs
  print(paste("Added model coefs for day", i))
}
  
coef.df <- data.frame(model_coefs)
max(prediction$date)

# Plot time-varying coefficients by week (Lambda is 0 between these dates)
all_coef_dates <- seq.Date(from = pred_start_date, to = pred_start_date + length(output_files) - 6, by = 7)
coef.df$date = all_coef_dates
coef.tbl <- as.tibble(coef.df)
coef.tbl %>% relocate(date) -> coef.tbl

write.csv(coef.tbl, "coefs_2020.csv")
coef.tbl

# Plot Day of Week coefficient series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=Sun, col='Sun')) + 
  geom_line(mapping=aes(x=date, y=Mon, col='Mon')) + 
  geom_line(mapping=aes(x=date, y=Tue, col='Tue')) +
  geom_line(mapping=aes(x=date, y=Wed, col='Wed')) + 
  geom_line(mapping=aes(x=date, y=Thu, col='Thu')) + 
  geom_line(mapping=aes(x=date, y=Fri, col='Fri')) +
  geom_line(mapping=aes(x=date, y=Sat, col='Sat'))

# Check DOW coefficient magnitudes over time
coef.tbl2 <- coef.tbl
coef.tbl2[,!(colnames(coef.tbl) == "date")] <- abs(coef.tbl2[,!(colnames(coef.tbl) == "date")])
coef.tbl2 %>% mutate(avg = (Mon + Tue + Wed + Thu + Fri + Sat + Sun) / 7) -> coef.tbl2
ggplot(data=coef.tbl2) + 
  geom_line(mapping=aes(x=date, y=avg, col='dow_avg'))

# Check moving average of daily usage, lambda
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=lag, col='lag')) +
  geom_line(mapping=aes(x=date, y=lambda, col='lambda'))

# Nq Series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=MCV_Nq, col='MCV_Nq')) + 
  geom_line(mapping=aes(x=date, y=PLT_Nq, col='PLT_Nq')) +
  geom_line(mapping=aes(x=date, y=RBC_Nq, col='RBC_Nq')) + 
  geom_line(mapping=aes(x=date, y=RDW_Nq, col='RDW_Nq')) + 
  geom_line(mapping=aes(x=date, y= WBC_Nq, col='WBC_Nq'))

# Census Series
config$census_locations

# The below code would be more convenient, but unfortunately census locations that
# contain spaces and other special characters are modified such that the character
# is replaced by "."

# df <- tidyr::gather(coef.tbl, type, value, config$census_locations)
# ggplot(df, aes(date, value, color = type)) + geom_line() +

# Plotting census locations in groups of 6 for clarity
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=FGR, col='FGR')) + 
  geom_line(mapping=aes(x=date, y=E2.ICU, col='E2.ICU')) +
  geom_line(mapping=aes(x=date, y=C3, col='C3')) + 
  geom_line(mapping=aes(x=date, y=E3, col='E3')) + 
  geom_line(mapping=aes(x=date, y=B3, col='B3')) + 
  geom_line(mapping=aes(x=date, y=B2, col='B2'))

ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=F3, col='F3')) + 
  geom_line(mapping=aes(x=date, y=C2, col='C2')) +
  geom_line(mapping=aes(x=date, y=E1, col='E1')) + 
  geom_line(mapping=aes(x=date, y=B1, col='B1')) + 
  geom_line(mapping=aes(x=date, y=H2, col='H2')) + 
  geom_line(mapping=aes(x=date, y=VCP.3.WEST, col='VCP.3.West'))

ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=VCP.2.WEST, col='VCP.2.West')) + 
  geom_line(mapping=aes(x=date, y=VCP.1.WEST, col='VCP.1.West')) +
  geom_line(mapping=aes(x=date, y=VCL.SKILLED.NURSING.FACILITY, col='VCL.Skilled.Nursing.Facility')) + 
  geom_line(mapping=aes(x=date, y=G2P, col='G2P')) + 
  geom_line(mapping=aes(x=date, y=G2S, col='G2S')) + 
  geom_line(mapping=aes(x=date, y=C1, col='C1'))

ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=J2, col='J2')) + 
  geom_line(mapping=aes(x=date, y=J4, col='J4')) +
  geom_line(mapping=aes(x=date, y=J5, col='J5')) + 
  geom_line(mapping=aes(x=date, y=J6, col='J6')) + 
  geom_line(mapping=aes(x=date, y=J7, col='J7')) + 
  geom_line(mapping=aes(x=date, y=M5, col='M5')) + 
  geom_line(mapping=aes(x=date, y=M6, col='M6')) + 
  geom_line(mapping=aes(x=date, y=M7, col='M7')) 

# Full dataset generation
pred_inputs <- list(cbc = NULL, census = NULL, transfusion = NULL, inventory = NULL)
pred_start_date
pred_end_date
all_pred_dates <- seq.Date(from = pred_start_date, to = pred_end_date, by = 1)
for (i in seq_along(all_pred_dates)) {
  date_str <- as.character(all_pred_dates[i])
  print(date_str)
  inputs_single_day <- process_data_for_date(config = config, date = date_str)
  pred_inputs$cbc %<>% rbind(inputs_single_day$cbc)
  pred_inputs$census %<>% rbind(inputs_single_day$census)
  pred_inputs$transfusion %<>% rbind(inputs_single_day$transfusion)
  pred_inputs$inventory %<>% rbind(inputs_single_day$inventory)
}
full_dataset <- create_cbc_features(pred_inputs$cbc, config$cbc_quantiles)
pred_inputs$inventory$date %<>% as.Date()
full_dataset %>% left_join(pred_inputs$census, by="date") %>% 
  left_join(pred_inputs$transfusion, by="date") %>%
  left_join(pred_inputs$inventory, by="date") -> full_dataset
full_dataset %<>% left_join(prediction, by="date")

ggplot(data=full_dataset) +
  geom_line(mapping=aes(x=date, y=t_pred, col='t_pred')) + geom_vline(xintercept=as.Date("2019-05-13"))

# Should mimic actual dataset generation more closely
full_dataset %>% distinct() %>%
  dplyr::rename(plt_used = .data$used) %>%
  dplyr::mutate(lag = ma(.data$plt_used, window_size = 7L)) -> full_dataset #%>%
  
# Look at correlation of with day of week using Chi-square (is there a better method?)
full_dataset %<>% mutate(dow = weekdays(date))
chisqs.dow <- sapply(full_dataset %>% select(-t_pred), function(x) chisq.test(full_dataset$dow, x, simulate.p.value = TRUE)$p.value)
sorted.chisqs <- sort(chisqs.dow, decreasing = TRUE)
sorted.chisqs
noncor.features <- names(sorted.chisqs[1:10])
noncor.features
noncor.features.long <- reshape2::melt(full_dataset, id = "date", measure = noncor.features )
ggplot(noncor.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-04-15"), as.Date("2019-05-13"), as.Date("2019-05-15"), as.Date("2019-05-24")))
