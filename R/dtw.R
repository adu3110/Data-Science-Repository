library(data.table)
library(dtw)
library(ggplot2)

data_df <- read.csv("<data_path>", header=T)
model_df <- read.csv("<model_path>", header=T)

setDT(model_df)
setDT(data_df)

ComputeDTW <- function(model_dt_row){
  curr_var_dt <- data_df[, c(model_dt_row$INDEPVAR1[1],
                             model_dt_row$INDEPVAR2[1],
                             model_dt_row$INDEPVAR3[1]), with=FALSE]
  curr_var_mat <- data.matrix(curr_var_dt)
  coeff_mat <- matrix(data=c(model_dt_row$INDEPVAR1_BETA[1],
                             model_dt_row$INDEPVAR2_BETA[1],
                             model_dt_row$INDEPVAR3_BETA[1]), nrow=3, ncol=1)
  logodd_predictions <-  curr_var_mat %*% coeff_mat + model_dt_row$INTERCEPT[1]
  
  pd_predictions <- 1/(1+exp(-logodd_predictions[,1]))
  
  curr_dtw <- dtw(pd_predictions, data_df$PD)
  dtw_dist <- curr_dtw$distance
  return(dtw_dist) 
}

model_dtw <- model_df[, list(dtw_dist = ComputeDTW(.SD)), by=(MODEL_NUM)]

model_dtw <- model_dtw[order(dtw_dist)]

PlotTrajectories <- function(model_dt_row){
  curr_var_dt <- data_df[, c(model_dt_row$INDEPVAR1[1],
                             model_dt_row$INDEPVAR2[1],
                             model_dt_row$INDEPVAR3[1]), with=FALSE]
  curr_var_mat <- data.matrix(curr_var_dt)
  coeff_mat <- matrix(data=c(model_dt_row$INDEPVAR1_BETA[1],
                             model_dt_row$INDEPVAR2_BETA[1],
                             model_dt_row$INDEPVAR3_BETA[1]), nrow=3, ncol=1)
  logodd_predictions <-  curr_var_mat %*% coeff_mat + model_dt_row$INTERCEPT[1]
  
  pd_predictions <- 1/(1+exp(-logodd_predictions[,1]))
  
  pd_dt <- data.table(date = data_df$DATE, pd= data_df$PD, pd_pred = pd_predictions)
  
  p <- ggplot(pd_dt) + geom_line(aes(date, pd, color='red')) +
    geom_line(aes(date, pd_pred, color='green')) + 
    scale_color_identity(guide='legend', labels=c('Predicted_PD', 'PD'))
  p
  return(p) 
}

PlotTrajectories(model_df[MODEL_NUM == model_dtw$MODEL_NUM[1]])

