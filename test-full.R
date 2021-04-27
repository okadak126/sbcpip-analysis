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

set_config_param(param = "output_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Outputs")
set_config_param(param = "log_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Logs")


set_config_param(param = "report_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Reports")

config <- set_config_param(param = "data_folder",
                           value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_inc")

config$history_window <- 200

# Predict 106 days out from January 1, 2019
pred_start_date <- as.Date("2019-01-01")
pred_end_date <- pred_start_date + 380
seed_start_date <- pred_start_date - 201
seed_end_date <- pred_start_date - 1
all_seed_dates <- seq.Date(from = seed_start_date, to = seed_end_date, by = 1)
pred_end_date
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
prediction <- NULL
for (i in seq_along(all_pred_dates)) {
  date_str <- as.character(all_pred_dates[i])
  result <- predict_for_date(config = config, date = date_str)
  prediction <- rbind(prediction,result)
}
ggplot(data=prediction) + geom_line(aes(x=date, y=t_pred))
prediction

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
  mutate(t_true = d_true + dlead1 + dlead2) %>%
  mutate(surplus = t_pred - t_true) -> pred_and_true
pred_and_true %>% filter(is.na(t_true))

# Plots of predicted platelet usage vs. actual platelet usage, as well as "surplus"
ggplot(data=pred_and_true[1:(nrow(pred_and_true)-2),]) + 
  geom_line(aes(x=date, y=t_pred)) + 
  geom_line(aes(x=date, y=t_true))
ggplot(data=pred_and_true[1:(nrow(pred_and_true)-2),]) + 
  geom_line(aes(x=date, y=surplus)) 

# Attempt to generate prediction table (does not work - actually platelet usage returns NA )
full_pred_table <- build_prediction_table(config, pred_start_date, pred_end_date)
write.csv(full_pred_table, "predictions.csv")

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

# Plot time-varying coefficients by week (Lambda is 0 between these dates)
all_coef_dates <- seq.Date(from = pred_start_date, to = pred_start_date + 362, by = 7)
coef.df$date = all_coef_dates
coef.tbl <- as.tibble(coef.df)
coef.tbl %>% relocate(date) -> coef.tbl
coef.tbl %>% filter(date < as.Date("2019-11-10") & date > as.Date("2019-06-01"))

# (Important) Day of Week series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=Sun, col='Sun')) + 
  geom_line(mapping=aes(x=date, y=Mon, col='Mon')) + 
  geom_line(mapping=aes(x=date, y=Tue, col='Tue')) +
  geom_line(mapping=aes(x=date, y=Wed, col='Wed')) + 
  geom_line(mapping=aes(x=date, y=Thu, col='Thu')) + 
  geom_line(mapping=aes(x=date, y=Fri, col='Fri')) + 
  geom_line(mapping=aes(x=date, y=lambda, col='lambda'))

# Nq Series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=MCV_Nq, col='MCV_Nq')) + 
  geom_line(mapping=aes(x=date, y=PLT_Nq, col='PLT_Nq')) +
  geom_line(mapping=aes(x=date, y=RBC_Nq, col='RBC_Nq')) + 
  geom_line(mapping=aes(x=date, y=RDW_Nq, col='RDW_Nq')) + 
  geom_line(mapping=aes(x=date, y= WBC_Nq, col='WBC_Nq'))

# B and C series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=B1, col='B1')) + 
  geom_line(mapping=aes(x=date, y=B2, col='B2')) +
  geom_line(mapping=aes(x=date, y=B3, col='B3')) + 
  geom_line(mapping=aes(x=date, y=C1, col='C1')) + 
  geom_line(mapping=aes(x=date, y= C2, col='C2')) + 
  geom_line(mapping=aes(x=date, y= C3, col='C3'))

# D and E series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=D1CC, col='D1CC')) + 
  geom_line(mapping=aes(x=date, y=D1CS, col='D1CS')) +
  geom_line(mapping=aes(x=date, y=D2, col='D2')) + 
  geom_line(mapping=aes(x=date, y=D3, col='D3')) + 
  geom_line(mapping=aes(x=date, y= DGR, col='DGR')) + 
  geom_line(mapping=aes(x=date, y= E1, col='E1')) + 
  geom_line(mapping=aes(x=date, y= E2.ICU, col='E2.ICU')) + 
  geom_line(mapping=aes(x=date, y= E29.ICU, col='E29.ICU')) + 
  geom_line(mapping=aes(x=date, y= E3, col='E3'))

# FGH series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=EMERGENCY.DEPARTMENT, col='EMERGENCY.DEPARTMENT')) + 
  geom_line(mapping=aes(x=date, y=D1CC, col='D1CC')) +
  geom_line(mapping=aes(x=date, y=F3, col='F3')) +
  geom_line(mapping=aes(x=date, y=FGR, col='FGR')) + 
  geom_line(mapping=aes(x=date, y=G1, col='G1')) + 
  geom_line(mapping=aes(x=date, y= E3, col='E3')) + 
  geom_line(mapping=aes(x=date, y= G2P, col='G2P')) + 
  geom_line(mapping=aes(x=date, y= G2S, col='G2S')) + 
  geom_line(mapping=aes(x=date, y= H1, col='H1')) + 
  geom_line(mapping=aes(x=date, y= H2, col='H2')) + 
  geom_vline(xintercept = c(as.Date("2019-05-13"), as.Date("2019-05-15")))

# Miscellaneous Series (what do these variables mean?)
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=EMERGENCY.DEPARTMENT, col='EMERGENCY.DEPARTMENT')) + 
  geom_line(mapping=aes(x=date, y=CAPR.XFER.OVERFL, col='CAPR.XFER.OVERFL')) +
  geom_line(mapping=aes(x=date, y=CATH.PACU, col='CATH.PACU')) + 
  geom_line(mapping=aes(x=date, y=CDU.CLIN.DEC.UNIT, col='CDU.CLIN.DEC.UNIT')) + 
  geom_line(mapping=aes(x=date, y=seven_lag, col='seven_lag')) 

# Lambda (this is 0 for the whole range)
ggplot(data=coef.tbl) + geom_line(mapping=aes(x=date, y=lambda, col='lambda'))

# Full dataset generation
pred_start_date <- as.Date("2019-01-01")
pred_end_date <- pred_start_date + 358
pred_inputs <- list(cbc = NULL, census = NULL, transfusion = NULL, inventory = NULL)
all_pred_dates <- seq.Date(from = pred_start_date, to = pred_end_date, by = 1)
all_pred_dates
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

# See which features varied the most over time and plot its values
devs <- sapply(data.frame(full_dataset %>% select(-t_pred) %>% filter(date != as.Date("2019-05-13") & date != as.Date("2019-05-15"))), function(x) sd(x))
sorted.devs <- sort(devs, decreasing=TRUE) 
var.features <- names(sorted.devs[2:7])
var.features.long <- reshape2::melt(full_dataset, id = "date", measure = var.features )
ggplot(var.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-04-15"), as.Date("2019-05-13"), as.Date("2019-05-15"), as.Date("2019-05-24")))

# Second most "varying" group of features.
var2.features<- names(sorted.devs[9:15])
var2.features[3] <- names(sorted.devs[17])
var2.features
var2.features.long <- reshape2::melt(full_dataset, id = "date", measure = var2.features )
ggplot(var2.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-04-15"), as.Date("2019-05-13"), as.Date("2019-05-15"), as.Date("2019-05-24")))

# Features that vary the least over the prediction period
nonvar.features <- names(sorted.devs[(length(sorted.devs) - 5):length(sorted.devs)])
nonvar.features[4] = 'CAPR XFER OVERFL'
nonvar.features.long <- reshape2::melt(full_dataset, id = "date", measure = nonvar.features )
ggplot(nonvar.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-04-15"), as.Date("2019-05-13"), as.Date("2019-05-15"), as.Date("2019-05-24")))

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


# Isolate problem areas
full_dataset %>% filter(date > as.Date("2019-11-14")) -> full_dataset_problem


# Plots illustrating strange behavior in November 2019
zeroed.vars <- c('EMERGENCY DEPARTMENT', 'CAPR XFER OVERFL', 'CATH PACU', 'D1CC', 'G1', 'D1CS', 'D2', 'D3', 'DGR', 'E29-ICU', 'H1')
dropped.vars <- c('E2-ICU', 'FGR', 'F3', 'E3', 'C3', 'C2', 'B3', 'E1', 'B1', 'B2')
r.vars <- c('r1', 'r2', 'r3_plus')
zeroed.features.long <- reshape2::melt(full_dataset, id = "date", measure = zeroed.vars)
ggplot(zeroed.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-11-15")))

dropped.features.long <-reshape2::melt(full_dataset, id = "date", measure = dropped.vars)
ggplot(dropped.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-11-15")))

r.features.long <-reshape2::melt(full_dataset, id = "date", measure = r.vars)
ggplot(r.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-11-15")))

other.vars <- setdiff(names(full_dataset), c(zeroed.vars, dropped.vars, r.vars, 'date'))
other.features.long <-reshape2::melt(full_dataset, id = "date", measure = other.vars)
ggplot(other.features.long, aes(date, value, colour = variable)) + geom_line() + 
  geom_vline(xintercept=c(as.Date("2019-11-15"))) + 
  geom_vline(xintercept = c(as.Date("2019-05-13"), as.Date("2019-05-15"))) + 
  geom_vline(xintercept=c(as.Date("2019-05-24")))

# Issue seems to be in Census files (sudden change from 11/17/2019 to 11/18/2019)
