library(data.table)
library(depmixS4)

data_full <- read.csv('/Users/adititiwari/Downloads/hmm_credit_Card_data.csv', header=T, stringsAsFactors = FALSE)
data_full[is.na(data_full)] <- 0
setDT(data_full)

##Converting data to depmix format, 36 rows for each customer########
FeaturesToRows <- function(dt){
    res <- NULL
    for (i in 0:35){
        curr_res <- dt[, c('SALARY' , 'Existing_Bal', 'AGE',
                           paste0(c('BAD', 'BAL', 'DELQ', 'SPEND',
                                   'ACTIVE', 'WO', 'LMT', 'NII',
                                   'NFI', 'REV', 'CLOSED', 'WO_BAL', 'MON'),
                                 i)), with=FALSE]
        colnames(curr_res) <- c('SALARY' , 'Existing_Bal', 'AGE',
                                'BAD', 'BAL', 'DELQ', 'SPEND',
                                'ACTIVE', 'WO', 'LMT', 'NII',
                                'NFI', 'REV', 'CLOSED', 'WO_BAL', 'MON')
        res <- rbind.data.frame(res, curr_res)
    }
    return(res)
}

data_hmm <- data_full[, FeaturesToRows(.SD), .(customer_id)]


###############training HMM and finding best model#########
#####Emissions : REV, LMT######
#####Covariates : SALARY, AGE, Existing_Bal##########

curr_seed <- 123
set.seed(curr_seed)
for (n_states in 2:5){
    
    train_mod_hmm <- depmix(list(REV~1, LMT~1),
                            data=data_hmm,
                            nstates = n_states,
                            transition = ~SALARY + AGE + Existing_Bal,
                            prior = ~1,
                            family = list(gaussian(), gaussian()),
                            ntimes = data_hmm[, .N, .(customer_id)]$N)
    
    conv <- FALSE
    while(!conv){
        tryCatch({
            curr_mod <- fit(train_mod_hmm, emc = em.control(rand=TRUE))
            conv <- TRUE
        }, error=function(error){
            curr_seed <<- curr_seed + 324
            print(curr_seed)
            set.seed(curr_seed)
            conv <- FALSE
        })
    }
    
    
    curr_BIC <- BIC(curr_mod)
    print(paste('n_st :', n_states, 'BIC :', curr_BIC))
    if(n_states==2){
        nstates_final <- 2
        min_BIC <- curr_BIC
        model_hmm <- curr_mod
    }else if (curr_BIC < min_BIC){
        nstates_final <- n_states
        min_BIC <- curr_BIC
        model_hmm <- curr_mod
    }
}
    

#######Getting Parameters of HMM Model#############
summary(model_hmm)
n_covariates <- 3
num_emissions <- 2

final_pars <- getpars(model_hmm)

init_state_prob <- final_pars[1:nstates_final]


trans_coeff <- list()
curr_par_index <- nstates_final
for(i in 1:nstates_final){
   curr_trans_coeff  <- matrix(data=0, nrow = n_covariates+1, ncol = nstates_final)
   for(j in 1:(n_covariates+1)){
       curr_trans_coeff[j, ] <- final_pars[(curr_par_index + 1) : (curr_par_index + nstates_final)]
       curr_par_index <- curr_par_index + nstates_final
   }
   
   trans_coeff[[i]] <- curr_trans_coeff
}


emission_pars <- list()
for (i in 1:nstates_final){
    emission_pars[[i]] <- final_pars[(curr_par_index+1):(curr_par_index + 2*num_emissions)]
    curr_par_index <- curr_par_index + 2 * num_emissions
}


###########Doing Prediction for Next Month#########

emission_list <- c('REV', 'LMT')
covar_list <- c('SALARY', 'AGE', 'Existing_Bal')

GetEmisPDF <- function(x, mean, sd){
    res <- ifelse(sd==0, 0,
           exp(-((x - mean)^2)/(2*sd^2))/(sqrt(2*pi)*sd))
    return(res)
    }

GetTransitionMatrix <- function (curr_covar){
    trans_mat_curr <- matrix(data = 0, nrow= nstates_final,
                             ncol = nstates_final)
    
    for (k in 1:nstates_final){
        
        curr_sum <- t(trans_coeff[[k]]) %*% curr_covar
        curr_trans <- c()
        for (l in 1:nstates_final){
            temp_curr_sum <- curr_sum - curr_sum[l,1]
            curr_trans <- c(curr_trans, 1/sum(exp(temp_curr_sum)))
        }
        trans_mat_curr[k,] <- curr_trans
    }
    return(trans_mat_curr)
}

GetFuturePred <- function(dt){
    curr_dt <- copy(dt)
    
    init_covar <- as.numeric(curr_dt[1, covar_list, with=FALSE])
    init_emission <- as.numeric(curr_dt[1, emission_list, with=FALSE])
    alpha1 <- c()
    px1 <- c()
    
    for(j in 1:nstates_final){
        emis_mean <- emission_pars[[j]][seq(1, num_emissions*2, by =2)]
        emis_sd <- emission_pars[[j]][seq(2, num_emissions*2, by =2)]
        
        px <- mapply(GetEmisPDF,
                     init_emission, emis_mean, emis_sd)
        
        
        px1 <- c(px1, prod(px))
    }
    
    alpha1 <- init_state_prob * px1
    
    alpha_prev <- matrix(data=alpha1, nrow=nstates_final, ncol=1)
    for (i in 2:nrow(curr_dt)){
        
        curr_covar <- as.numeric(curr_dt[i-1, covar_list, with=FALSE])
        
        curr_covar <- matrix(data= c(1, curr_covar), nrow = (n_covariates+1), ncol = 1)
        
        
        curr_emission <- as.numeric(curr_dt[i, emission_list, with=FALSE])
        
        px_curr <- c()
        for(j in 1:nstates_final){
            emis_mean <- emission_pars[[j]][seq(1, num_emissions*2, by =2)]
            emis_sd <- emission_pars[[j]][seq(2, num_emissions*2, by =2)]
            
            px <- mapply(GetEmisPDF,
                         curr_emission, emis_mean, emis_sd)
            
            
            px_curr <- c(px_curr, prod(px))
        }
        
        px_curr <- matrix(data=px_curr, nrow = length(px_curr), ncol = 1)
        
        trans_mat_curr <- GetTransitionMatrix(curr_covar)
        
        curr_prod_alpha <- t(trans_mat_curr) %*% alpha_prev
        
        alpha_curr <- curr_prod_alpha * px_curr
        
        alpha_prev <- alpha_curr
    }
    
    prob_final_state <- alpha_curr /sum(alpha_curr)
    
    final_covar <- as.numeric(curr_dt[nrow(curr_dt), covar_list, with=FALSE])
    
    final_covar <- matrix(data= c(1, final_covar), nrow = (n_covariates+1), ncol = 1)
    
    trans_mat_final <- GetTransitionMatrix(final_covar)
    
    prob_next_state <- t(trans_mat_final) %*% prob_final_state
    
    
    emis_next <- matrix(data = 0, nrow = 1, ncol = num_emissions)
    for(m in 1:nstates_final){
        emis_mean <- emission_pars[[m]][seq(1, num_emissions*2, by =2)]
        
        emis_pred <- matrix(data = emis_mean * prob_next_state[m, 1],
                             nrow = 1, ncol = num_emissions)
        
        emis_next <- emis_next + emis_pred
    }
    
    emis_next <- as.data.frame(emis_next)
    setDT(emis_next)
    colnames(emis_next) <- emission_list
    
    return(emis_next)
}


data_hmm[, GetFuturePred(.SD), .(customer_id)]

data_hmm[MON == 35, emission_list, with=FALSE]
