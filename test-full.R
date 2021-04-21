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

set_config_param(param = "output_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Outputs")
set_config_param(param = "log_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Logs")


set_config_param(param = "report_folder",
                 value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_Reports")

config <- set_config_param(param = "data_folder",
                           value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center")


## This is for creating the seed dataset
cbc <- process_all_cbc_files(data_folder = config$data_folder,
                             cbc_abnormals = config$cbc_abnormals,
                             cbc_vars = config$cbc_vars)

census <- process_all_census_files(data_folder = config$data_folder,
                                   locations = config$census_locations)

transfusion <- process_all_transfusion_files(data_folder = config$data_folder)
config
## Save the SEED dataset so that we can begin the process of building the model.
## This gets us to date 2018-04-09-08-01-54.txt, so we save data using that date
saveRDS(list(cbc = cbc, census = census, transfusion = transfusion),
        file = file.path(config$output_folder,
                         sprintf(config$output_filename_prefix, "2018-04-12")))

## End of Seed Creation

## Incremental Data
config <- set_config_param(param = "data_folder",
                           value = "/Users/kaiokada/Desktop/Research/platelet_data_full/Blood_Center_inc")

config$history_window <- 200

# Predict from 2018-04-13 all the way to 2020-01-02
start_date <- as.Date("2018-10-10")
end_date <- as.Date("2019-04-15")

all_dates <- seq.Date(from = start_date, to = end_date, by = 1)

prediction <- NULL

for (i in seq_along(all_dates)) {
  date_str <- as.character(all_dates[i])
  print(date_str)
  
  result <- predict_for_date(config = config, date = date_str)
  
  print(result)
  prediction <- rbind(prediction,result)
}

ggplot(data=prediction) + geom_line(aes(x=date, y=t_pred))

# Retrieve true transfusion data from the files
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

# 
transfusion_inc <- process_all_transfusion_files_no_reports(data_folder = config$data_folder)
transfusion_dates <- sapply(transfusion_inc$date, function(x) {as.character(x, format = "%Y-%m-%d")})
transfusion_used <- as.numeric(transfusion_inc$used)
plt_used_df <- data.frame(transfusion_dates, transfusion_used)
plt_used_tbl <- as_tibble(plt_used_df[1:nrow(plt_used_df),])
plt_used_tbl$transfusion_dates %<>% as.Date
names(plt_used_tbl) = c("date", "d_true")
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
full_pred_table <- build_prediction_table(config, start_date, end_date)
full_pred_table
write.csv(full_pred_table, "predictions.csv")

# Collect model coefficients over all output files (only need one set per week)
config$output_folder
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

# Plot time-varying coefficients by week
nrow(coef.df) * 7
start_date <- as.Date("2018-10-10")
end_date <- as.Date("2019-04-14")

all_dates <- seq.Date(from = start_date, to = end_date, by = 7)
coef.df$date = all_dates
coef.tbl <- as.tibble(coef.df)
coef.tbl %>% relocate(date) -> coef.tbl
coef.tbl %>% filter(date < as.Date("2019-11-10") & date > as.Date("2019-06-01"))

# Day of Week series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=Sun, col='Sun')) + 
  geom_line(mapping=aes(x=date, y=Mon, col='Mon')) + 
  geom_line(mapping=aes(x=date, y=Tue, col='Tue')) +
  geom_line(mapping=aes(x=date, y=Wed, col='Wed')) + 
  geom_line(mapping=aes(x=date, y=Thu, col='Thu')) + 
  geom_line(mapping=aes(x=date, y=Fri, col='Fri'))

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
  geom_line(mapping=aes(x=date, y=F3, col='F3')) +
  geom_line(mapping=aes(x=date, y=FGR, col='FGR')) + 
  geom_line(mapping=aes(x=date, y=G1, col='G1')) + 
  geom_line(mapping=aes(x=date, y= G2P, col='G2P')) + 
  geom_line(mapping=aes(x=date, y= G2S, col='G2S')) + 
  geom_line(mapping=aes(x=date, y= H1, col='H1')) + 
  geom_line(mapping=aes(x=date, y= H2, col='H2'))

# Miscellaneous Series
ggplot(data=coef.tbl) +
  geom_line(mapping=aes(x=date, y=EMERGENCY.DEPARTMENT, col='EMERGENCY.DEPARTMENT')) + 
  geom_line(mapping=aes(x=date, y=CAPR.XFER.OVERFL, col='CAPR.XFER.OVERFL')) +
  geom_line(mapping=aes(x=date, y=CATH.PACU, col='CATH.PACU')) + 
  geom_line(mapping=aes(x=date, y=CDU.CLIN.DEC.UNIT, col='CDU.CLIN.DEC.UNIT')) + 
  geom_line(mapping=aes(x=date, y=seven_lag, col='seven_lag')) 

# Lambda
ggplot(data=coef.tbl) + geom_line(mapping=aes(x=date, y=lambda, col='lambda'))
