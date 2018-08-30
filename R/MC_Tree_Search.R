library(data.table)
library(usdm)
library(datasets)

Data_full <- read.csv(file = "/Users/adititiwari/Desktop/Mape_Backward_Selection.csv", header = TRUE )

setDT(Data_full)

categories<- data.table(IndepVariable = colnames(Data_full)[4:ncol(Data_full)])

categories$Category <- "OTHERS"
categories[which(grepl('UP', IndepVariable))]$Category <- 'UP'
categories[which(grepl('GDP', IndepVariable))]$Category <- 'GDP'
categories[which(grepl('CPI', IndepVariable) | 
                   grepl('BREAKINF10', IndepVariable))]$Category <- 'CPI'
categories[which(grepl('PEDY', IndepVariable))]$Category <- 'PEDY'
categories[which(grepl('IP', IndepVariable))]$Category <- 'IP'
categories[which(grepl('X', IndepVariable))]$Category <- 'X'
categories[which(grepl('RSH', IndepVariable)|
                   grepl('RBASE', IndepVariable))]$Category <- 'INTEREST'
categories[which(grepl('PSH', IndepVariable))]$Category <- 'PSH'
categories[which(grepl('PHHX', IndepVariable))]$Category <- 'PH'
categories[which(grepl('WPO', IndepVariable))]$Category <- 'WPO'
categories[which(grepl('PESR', IndepVariable))]$Category <- 'PESR'

categories$sign <- 'None'
categories[Category %in% c('UP','INTEREST', 'CPI')]$sign<- 'pos'
categories[Category %in% c('GDP', 'WAGE', 'PSH', 'PEDY',
                           'IP', 'X', 'PESR', 'WPO', 'PH')]$sign<- 'neg'


GetMAPE <- function(y, yhat){
  pd <- 1/(1+exp(-y))
  pd_fit <- 1/(1+exp(-yhat))
  mape <- mean(abs((pd - pd_fit)/pd))
  return(mape)
}


varlist <- categories$IndepVariable

dt <- Data_full[, c('LOGODD_PD', varlist), with=FALSE]

while(length(varlist) > 70){
  print(length(varlist))
  mape_list <- c()
  for (i in 1:length(varlist)){
    varlist_curr <- varlist[-i]
    data_curr <- dt[, c('LOGODD_PD', varlist_curr), with=FALSE]
    model1 <- lm(LOGODD_PD ~ ., data = data_curr)
    LOGODD_PD_Fit <- predict(model1, newdata =data_curr)
    mape_curr <- GetMAPE(data_curr$LOGODD_PD, LOGODD_PD_Fit)
    mape_list <- c(mape_list, mape_curr)
  }
  removed_var_index <- which.min(mape_list)
  varlist <- varlist[-removed_var_index]
}

reduced_var_cat <- categories[IndepVariable %in% varlist]

reduced_var_cat[, Correlation := sapply(IndepVariable,
                                        function(x){
                                          cor(dt[, c('LOGODD_PD', x),
                                                 with=FALSE])[1,2]})]
reduced_var_cat<- reduced_var_cat[(sign=='pos' & Correlation >0) |
                                    (sign=='neg' & Correlation < 0) |
                                    sign == 'None']

tree_var_list <- reduced_var_cat$IndepVariable

train_data <- Data_full[, c('PD', 'LOGODD_PD', tree_var_list), with =FALSE]

trajectory_data <- read.csv("<>", header=T)
setDT(trajectory_data)

target_MAPE <- 1
target_Rsq <- 0.8
target_VIF <- curr_train_dat[, indep_Vars, with=FALSE]
target_peak <-max(trajectory_data$PD)
important_categories <- c('GDP', 'INTEREST')

Num_vars <- length(tree_var_list)

Tree <- list()

Tree$num_simulations <- 0
Tree$num_success <- 0
Tree$weight <- 0

Tree$Nodes <- list()

for (first_var in 1:(Num_vars-2)){
  Tree$Nodes[[first_var]] <- list()
  
  Tree$Nodes[[first_var]]$var_name <- tree_var_list[first_var]
  Tree$Nodes[[first_var]]$var_num <- first_var
  Tree$Nodes[[first_var]]$var_category <- categories[IndepVariable == tree_var_list[first_var]]$Category
  Tree$Nodes[[first_var]]$num_simulations <- 0
  Tree$Nodes[[first_var]]$num_success <- 0
  Tree$Nodes[[first_var]]$weight <- 0
  
  Tree$Nodes[[first_var]]$Nodes <- list()
  
  for (second_var in ((first_var+1):(Num_vars-1))){
    Tree$Nodes[[first_var]]$Nodes[[second_var]] <- list()
    
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$var_name <- tree_var_list[second_var]
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$var_num <- second_var
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$var_category <- categories[IndepVariable == 
                                                                             tree_var_list[second_var]]$Category
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$parent_var_name <- tree_var_list[first_var]
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$parent_var_num <- first_var
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$parent_var_category <- categories[IndepVariable == 
                                                                                    tree_var_list[first_var]]$Category
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations <- 0
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_success <- 0
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$weight <- 0
    
    Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes <- list()
    
    for (third_var in ((second_var+1):(Num_vars))){
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]] <- list()
      
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_name <- tree_var_list[third_var]
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_num <- third_var
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_category <- categories[IndepVariable == 
                                                                                                  tree_var_list[third_var]]$Category
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$parent_var_name <- c(tree_var_list[first_var],
                                                                                          tree_var_list[second_var])
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$parent_var_num <- c(first_var, second_var)
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$parent_var_category <- c(categories[IndepVariable == 
                                                                                                           tree_var_list[first_var]]$Category,
                                                                                              categories[IndepVariable == 
                                                                                                           tree_var_list[second_var]]$Category)
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_simulations <- 0
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_success <- 0
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$weight <- 0
      
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$MAPE <- NA
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$R_sq <- NA
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$peak <- NA
      Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$VIF <- c()
    }
    
    
  }
  
}


#########################Training#################

for (first_var in 1:length(Tree$Nodes)){
  for (first_var_sim in 1:20){
    for(second_var in (first_var+1):length(Tree$Nodes[[first_var]]$Nodes)){
      for (second_var_sim in 1:20){
        for(third_var in (second_var+1):length(Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes)){
          for (third_var_sim in 1:20){
            Tree$num_simulations <- Tree$num_simulations + 1
            Tree$Nodes[[first_var]]$num_simulations <- Tree$Nodes[[first_var]]$num_simulations + 1
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations + 1
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_simulations <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_simulations + 1
            
            indep_Vars <- c(Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$parent_var_name,
                            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_name)
            curr_train_dat <- sample(train_data[, c('LOGODD_PD', indep_Vars),
                                                with=FALSE], floor(0.6 * nrow(train_data)))
            
            curr_model <- summary(lm(LOGODD_PD ~ ., data=curr_train_dat))
            
            curr_train_prediction <- data.matrix(curr_train_dat[, indep_Vars, with=FALSE]) %*% matrix(data=curr_model$coefficients[,1][2:4], nrow=3, ncol=1) + curr_model$coefficients[1, 1]
            
            
            curr_trajectory_dat <- trajectory_data[, indep_Vars, with=FALSE]
            
            curr_trajectory_prediction <- data.matrix(curr_trajectory_dat) %*% matrix(data=curr_model$coefficients[,1][2:4], nrow=3, ncol=1) + curr_model$coefficients[1, 1]
            
            curr_pd_prediction <- 1/(1 + exp(-curr_trajectory_prediction))
            
            curr_vif_list <- vif(curr_train_dat[, indep_Vars, with=FALSE])$VIF
            
            curr_mape <- GetMAPE(curr_train_prediction$LOGODD_PD, curr_train_prediction[,1])
            curr_rsq <- curr_model$r.squared
            curr_peak <- max(curr_pd_prediction)
            
            curr_categories <- c(Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$parent_var_category,
                                 Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_category)
            
            category_match_weight <- 0.5
            if (intersect(curr_categories, important_categories) > 1){
              category_match_weigth <- 1
            }
            
            vif_close <- FALSE
            if(curr_vif_list[1]/curr_vif_list[2] >= 0.8 && curr_vif_list[1]/curr_vif_list[2] <= 1.25){
              if(curr_vif_list[2]/curr_vif_list[3] >= 0.8 && curr_vif_list[2]/curr_vif_list[3] <= 1.25){
                vif_close <- TRUE
              }
            }
            
            
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$MAPE <- curr_mape
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$R_sq <- curr_rsq
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$peak <- curr_peak
            Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$VIF <- curr_vif_list
            
            if(curr_mape >= 0.8 * target_MAPE && curr_mape <= 1.2 * target_MAPE){
              if(curr_rsq >=0.8 * target_Rsq && curr_rsq <= 1.2 * target_Rsq){
                if(curr_peak <= 1.2 * target_peak){
                  if(!any(curr_vif_list >= target_VIF) && vif_close){
                    Tree$num_success <- Tree$num_success + category_match_weight
                    Tree$Nodes[[first_var]]$num_success <- Tree$Nodes[[first_var]]$num_success + category_match_weight
                    Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_success <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_success + category_match_weight
                    Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_success <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_success + category_match_weight
                    
                    Tree$Nodes[[first_var]]$weight <- Tree$Nodes[[first_var]]$num_success/Tree$Nodes[[first_var]]$num_simulations + 2 * sqrt(log(Tree$num_simulations)/Tree$Nodes[[first_var]]$num_simulations)
                    Tree$Nodes[[first_var]]$Nodes[[second_var]]$weight <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_success/Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations + 2 * sqrt(log(Tree$Nodes[[first_var]]$num_simulations)/Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations)
                    Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$weight <- Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_success/Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_simulations + 2 * sqrt(log(Tree$Nodes[[first_var]]$Nodes[[second_var]]$num_simulations)/Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$num_simulations)
                  }
                }
              }
            }
            
            
          }
        }
      }
    }
  }
}




#######################Searching Best Model#########################

model_table <- NULL

for (first_var in 1:length(Tree$Nodes)){
  for(second_var in (first_var+1):length(Tree$Nodes[[first_var]]$Nodes)){
    for(third_var in (second_var+1):length(Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes)){
      curr_score <- Tree$Nodes[[first_var]]$weight + Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$weight
      
      curr_model_dt <- data.table(INDEPVAR1 = Tree$Nodes[[first_var]]$var_name,
                                  INDEPVAR2 = Tree$Nodes[[first_var]]$Nodes[[second_var]]$var_name,
                                  INDEPVAR3 = Tree$Nodes[[first_var]]$Nodes[[second_var]]$Nodes[[third_var]]$var_name,
                                  SCORE = curr_score)
      model_table <- rbind.data.frame(model_table, curr_model_dt)
    }
  }
}

model_table <- model_table[order(SCORE)]