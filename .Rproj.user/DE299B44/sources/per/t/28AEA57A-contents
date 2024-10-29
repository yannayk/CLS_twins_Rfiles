##########################################
### PROBABALISTIC SENSITIVITY ANALYSIS ###
##########################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(dampack)

set.seed(12345)

# Strategy_names
strat_names <- c("No screen", "One-step screen", "Two-step screen")

# Number of model iterations ("runs")
n_runs <- 1000

# Generating random values from distributions and storing in a data frame
df_psa_input <- data.frame(v_p_cl25_t1. = rbeta(n_runs, 32, 696),
                           v_p_cl15_t1. = rbeta(n_runs, 15, 17),
                           v_p_cl25_t2. = rbeta(n_runs, 53, 643),
                           v_p_cl15_t2. = rbeta(n_runs, 20, 33),
                           v_p_clN_28_t2. = rbeta(n_runs, 7, 642),
                           v_p_clN_32_t2. = rbeta(n_runs, 17, 625),
                           v_p_clN_34_t2. = rbeta(n_runs, 39, 586),
                           v_p_clN_37_t2. = rbeta(n_runs, 336, 250),
                           v_p_cl25_28_t1. = rbeta(n_runs, 2, 15),
                           v_p_cl25_32_t1. = rbeta(n_runs, 2, 13),
                           v_p_cl25_34_t1. = rbeta(n_runs, 1, 12),
                           v_p_cl25_37_t1. = rbeta(n_runs, 6, 6),
                           v_p_cl25_28_t2. = rbeta(n_runs, 4, 42),
                           v_p_cl25_32_t2. = rbeta(n_runs, 5, 37),
                           v_p_cl25_34_t2. = rbeta(n_runs, 5, 32),
                           v_p_cl25_37_t2. = rbeta(n_runs, 18, 14),
                           v_p_cl15_28_t1. = rbeta(n_runs, 8, 7),
                           v_p_cl15_32_t1. = rbeta(n_runs, 1, 7),
                           v_p_cl15_34_t1. = rbeta(n_runs, 0, 6),
                           v_p_cl15_37_t1. = rbeta(n_runs, 3, 6),
                           v_p_cl15_28_t2. = rbeta(n_runs, 10, 23),
                           v_p_cl15_32_t2. = rbeta(n_runs, 7, 16),
                           v_p_cl15_34_t2. = rbeta(n_runs, 1, 15),
                           v_p_cl15_37_t2. = rbeta(n_runs, 8, 7),
                           v_rr_p_28. = rlnorm(n_runs, log(0.53), 0.40),
                           v_rr_p_32. = rlnorm(n_runs, log(0.71), 0.37),
                           v_rr_p_34. = rlnorm(n_runs, log(0.64), 0.37),
                           v_rr_p_37. = rlnorm(n_runs, log(1.02), 0.17),
                           v_rr_cp_28. = rlnorm(n_runs, log(0.56), 0.39),
                           v_rr_cp_32. = rlnorm(n_runs, log(0.78), 0.37),
                           v_rr_cp_34. = rlnorm(n_runs, log(0.30), 0.53),
                           v_rr_cp_37. = rlnorm(n_runs, log(1.00), 0.26),
                           v_rr_c_28. = rlnorm(n_runs, log(0.56), 0.39),
                           v_rr_c_32. = rlnorm(n_runs, log(0.78), 0.37),
                           v_rr_c_34. = rlnorm(n_runs, log(0.30), 0.53),
                           v_rr_c_37. = rlnorm(n_runs, log(1.00), 0.26),
                           
                           # STILL BIRTH PROBABILITIES
                           v_p_sb_28. = rbeta(n_runs, 20, 26665),
                           v_p_sb_32. = rbeta(n_runs, 78, 51680),
                           v_p_sb_34. = rbeta(n_runs, 49, 24129),
                           v_p_sb_37. = rbeta(n_runs, 51, 45073),
                           v_p_sb_term. = rbeta(n_runs, 58, 10151),
                           
                           # ANNUAL MORTALITIES
                           v_hr_AD_28_01. = rlnorm(n_runs, log(236.32), 0.028),
                           v_hr_AD_32_01. = rlnorm(n_runs, log(32.1), 0.026),
                           v_hr_AD_34_01. = rlnorm(n_runs, log(32.1), 0.026),
                           v_hr_AD_37_01. = rlnorm(n_runs, log(6.75), 0.028),
                           v_hr_AD_28_19. = rlnorm(n_runs, log(4.52), 0.205),
                           v_hr_AD_32_19. = rlnorm(n_runs, log(3.26), 0.081),
                           v_hr_AD_34_19. = rlnorm(n_runs, log(3.26), 0.081),
                           v_hr_AD_37_19. = rlnorm(n_runs, log(1.9), 0.053),
                           v_hr_AD_28_1019. = rlnorm(n_runs, log(1.89), 0.333),
                           v_hr_AD_32_1019. = rlnorm(n_runs, log(1.91), 0.101),
                           v_hr_AD_34_1019. = rlnorm(n_runs, log(1.91), 0.101),
                           v_hr_AD_37_1019. = rlnorm(n_runs, log(1.37), 0.063),
                           v_hr_AD_28_2029. = rlnorm(n_runs, log(2.12), 0.269),
                           v_hr_AD_32_2029. = rlnorm(n_runs, log(1.42), 0.092),
                           v_hr_AD_34_2029. = rlnorm(n_runs, log(1.42), 0.092),
                           v_hr_AD_37_2029. = rlnorm(n_runs, log(1.35), 0.047),
                           v_hr_AD_28_3045. = rlnorm(n_runs, log(2.04), 0.409),
                           v_hr_AD_32_3045. = rlnorm(n_runs, log(1.48), 0.119),
                           v_hr_AD_34_3045. = rlnorm(n_runs, log(1.48), 0.119),
                           v_hr_AD_37_3045. = rlnorm(n_runs, log(1.22), 0.067),
                           
                           # NICU (INITIAL INPATIENT) COSTS
                           v_c_n_28. = rgamma(n_runs, shape = gamma_params(121650, 2211)$shape, scale = gamma_params(121650, 2211)$scale),
                           v_c_n_32. = rgamma(n_runs, shape = gamma_params(42678, 779)$shape, scale = gamma_params(42678, 779)$scale),
                           v_c_n_34. = rgamma(n_runs, shape = gamma_params(28034, 553)$shape, scale = gamma_params(28034, 553)$scale),
                           v_c_n_37. = rgamma(n_runs, shape = gamma_params(17243, 413)$shape, scale = gamma_params(17243, 413)$scale),
                           v_c_n_term. = rgamma(n_runs, shape = gamma_params(17243, 413)$shape, scale = gamma_params(17243, 413)$scale),
                           v_p_adm_37. = rbeta(n_runs, 10280, 12016),
                           v_p_adm_term. = rbeta(n_runs, 19270, 244226),
                           
                           # ANNUAL HEALTH COSTS
                           v_in_28_0. = rgamma(n_runs, shape = gamma_params(in_28_0[1], in_28_0[2])$shape, scale = gamma_params(in_28_0[1], in_28_0[2])$scale),
                           v_in_28_1. = rgamma(n_runs, shape = gamma_params(in_28_1[1], in_28_1[2])$shape, scale = gamma_params(in_28_1[1], in_28_1[2])$scale),
                           v_in_28_2. = rgamma(n_runs, shape = gamma_params(in_28_2[1], in_28_2[2])$shape, scale = gamma_params(in_28_2[1], in_28_2[2])$scale),
                           v_in_28_3. = rgamma(n_runs, shape = gamma_params(in_28_3[1], in_28_3[2])$shape, scale = gamma_params(in_28_3[1], in_28_3[2])$scale),
                           v_in_28_4. = rgamma(n_runs, shape = gamma_params(in_28_4[1], in_28_4[2])$shape, scale = gamma_params(in_28_4[1], in_28_4[2])$scale),
                           v_in_28_5. = rgamma(n_runs, shape = gamma_params(in_28_5[1], in_28_5[2])$shape, scale = gamma_params(in_28_5[1], in_28_5[2])$scale),
                           v_in_28_6. = rgamma(n_runs, shape = gamma_params(in_28_6[1], in_28_6[2])$shape, scale = gamma_params(in_28_6[1], in_28_6[2])$scale),
                           v_in_28_7. = rgamma(n_runs, shape = gamma_params(in_28_7[1], in_28_7[2])$shape, scale = gamma_params(in_28_7[1], in_28_7[2])$scale),
                           v_in_28_8. = rgamma(n_runs, shape = gamma_params(in_28_8[1], in_28_8[2])$shape, scale = gamma_params(in_28_8[1], in_28_8[2])$scale),
                           v_in_28_9. = rgamma(n_runs, shape = gamma_params(in_28_9[1], in_28_9[2])$shape, scale = gamma_params(in_28_9[1], in_28_9[2])$scale),
                           v_in_32_0. = rgamma(n_runs, shape = gamma_params(in_32_0[1], in_32_0[2])$shape, scale = gamma_params(in_32_0[1], in_32_0[2])$scale),
                           v_in_32_1. = rgamma(n_runs, shape = gamma_params(in_32_1[1], in_32_1[2])$shape, scale = gamma_params(in_32_1[1], in_32_1[2])$scale),
                           v_in_32_2. = rgamma(n_runs, shape = gamma_params(in_32_2[1], in_32_2[2])$shape, scale = gamma_params(in_32_2[1], in_32_2[2])$scale),
                           v_in_32_3. = rgamma(n_runs, shape = gamma_params(in_32_3[1], in_32_3[2])$shape, scale = gamma_params(in_32_3[1], in_32_3[2])$scale),
                           v_in_32_4. = rgamma(n_runs, shape = gamma_params(in_32_4[1], in_32_4[2])$shape, scale = gamma_params(in_32_4[1], in_32_4[2])$scale),
                           v_in_32_5. = rgamma(n_runs, shape = gamma_params(in_32_5[1], in_32_5[2])$shape, scale = gamma_params(in_32_5[1], in_32_5[2])$scale),
                           v_in_32_6. = rgamma(n_runs, shape = gamma_params(in_32_6[1], in_32_6[2])$shape, scale = gamma_params(in_32_6[1], in_32_6[2])$scale),
                           v_in_32_7. = rgamma(n_runs, shape = gamma_params(in_32_7[1], in_32_7[2])$shape, scale = gamma_params(in_32_7[1], in_32_7[2])$scale),
                           v_in_32_8. = rgamma(n_runs, shape = gamma_params(in_32_8[1], in_32_8[2])$shape, scale = gamma_params(in_32_8[1], in_32_8[2])$scale),
                           v_in_32_9. = rgamma(n_runs, shape = gamma_params(in_32_9[1], in_32_9[2])$shape, scale = gamma_params(in_32_9[1], in_32_9[2])$scale),
                           v_in_36_0. = rgamma(n_runs, shape = gamma_params(in_36_0[1], in_36_0[2])$shape, scale = gamma_params(in_36_0[1], in_36_0[2])$scale),
                           v_in_36_1. = rgamma(n_runs, shape = gamma_params(in_36_1[1], in_36_1[2])$shape, scale = gamma_params(in_36_1[1], in_36_1[2])$scale),
                           v_in_36_2. = rgamma(n_runs, shape = gamma_params(in_36_2[1], in_36_2[2])$shape, scale = gamma_params(in_36_2[1], in_36_2[2])$scale),
                           v_in_36_3. = rgamma(n_runs, shape = gamma_params(in_36_3[1], in_36_3[2])$shape, scale = gamma_params(in_36_3[1], in_36_3[2])$scale),
                           v_in_36_4. = rgamma(n_runs, shape = gamma_params(in_36_4[1], in_36_4[2])$shape, scale = gamma_params(in_36_4[1], in_36_4[2])$scale),
                           v_in_36_5. = rgamma(n_runs, shape = gamma_params(in_36_5[1], in_36_5[2])$shape, scale = gamma_params(in_36_5[1], in_36_5[2])$scale),
                           v_in_36_6. = rgamma(n_runs, shape = gamma_params(in_36_6[1], in_36_6[2])$shape, scale = gamma_params(in_36_6[1], in_36_6[2])$scale),
                           v_in_36_7. = rgamma(n_runs, shape = gamma_params(in_36_7[1], in_36_7[2])$shape, scale = gamma_params(in_36_7[1], in_36_7[2])$scale),
                           v_in_36_8. = rgamma(n_runs, shape = gamma_params(in_36_8[1], in_36_8[2])$shape, scale = gamma_params(in_36_8[1], in_36_8[2])$scale),
                           v_in_36_9. = rgamma(n_runs, shape = gamma_params(in_36_9[1], in_36_9[2])$shape, scale = gamma_params(in_36_9[1], in_36_9[2])$scale),
                           v_out_28_0. = rgamma(n_runs, shape = gamma_params(out_28_0[1], out_28_0[2])$shape, scale = gamma_params(out_28_0[1], out_28_0[2])$scale),
                           v_out_28_1. = rgamma(n_runs, shape = gamma_params(out_28_1[1], out_28_1[2])$shape, scale = gamma_params(out_28_1[1], out_28_1[2])$scale),
                           v_out_28_2. = rgamma(n_runs, shape = gamma_params(out_28_2[1], out_28_2[2])$shape, scale = gamma_params(out_28_2[1], out_28_2[2])$scale),
                           v_out_28_3. = rgamma(n_runs, shape = gamma_params(out_28_3[1], out_28_3[2])$shape, scale = gamma_params(out_28_3[1], out_28_3[2])$scale),
                           v_out_28_4. = rgamma(n_runs, shape = gamma_params(out_28_4[1], out_28_4[2])$shape, scale = gamma_params(out_28_4[1], out_28_4[2])$scale),
                           v_out_28_5. = rgamma(n_runs, shape = gamma_params(out_28_5[1], out_28_5[2])$shape, scale = gamma_params(out_28_5[1], out_28_5[2])$scale),
                           v_out_28_6. = rgamma(n_runs, shape = gamma_params(out_28_6[1], out_28_6[2])$shape, scale = gamma_params(out_28_6[1], out_28_6[2])$scale),
                           v_out_28_7. = rgamma(n_runs, shape = gamma_params(out_28_7[1], out_28_7[2])$shape, scale = gamma_params(out_28_7[1], out_28_7[2])$scale),
                           v_out_28_8. = rgamma(n_runs, shape = gamma_params(out_28_8[1], out_28_8[2])$shape, scale = gamma_params(out_28_8[1], out_28_8[2])$scale),
                           v_out_28_9. = rgamma(n_runs, shape = gamma_params(out_28_9[1], out_28_9[2])$shape, scale = gamma_params(out_28_9[1], out_28_9[2])$scale),
                           v_out_32_0. = rgamma(n_runs, shape = gamma_params(out_32_0[1], out_32_0[2])$shape, scale = gamma_params(out_32_0[1], out_32_0[2])$scale),
                           v_out_32_1. = rgamma(n_runs, shape = gamma_params(out_32_1[1], out_32_1[2])$shape, scale = gamma_params(out_32_1[1], out_32_1[2])$scale),
                           v_out_32_2. = rgamma(n_runs, shape = gamma_params(out_32_2[1], out_32_2[2])$shape, scale = gamma_params(out_32_2[1], out_32_2[2])$scale),
                           v_out_32_3. = rgamma(n_runs, shape = gamma_params(out_32_3[1], out_32_3[2])$shape, scale = gamma_params(out_32_3[1], out_32_3[2])$scale),
                           v_out_32_4. = rgamma(n_runs, shape = gamma_params(out_32_4[1], out_32_4[2])$shape, scale = gamma_params(out_32_4[1], out_32_4[2])$scale),
                           v_out_32_5. = rgamma(n_runs, shape = gamma_params(out_32_5[1], out_32_5[2])$shape, scale = gamma_params(out_32_5[1], out_32_5[2])$scale),
                           v_out_32_6. = rgamma(n_runs, shape = gamma_params(out_32_6[1], out_32_6[2])$shape, scale = gamma_params(out_32_6[1], out_32_6[2])$scale),
                           v_out_32_7. = rgamma(n_runs, shape = gamma_params(out_32_7[1], out_32_7[2])$shape, scale = gamma_params(out_32_7[1], out_32_7[2])$scale),
                           v_out_32_8. = rgamma(n_runs, shape = gamma_params(out_32_8[1], out_32_8[2])$shape, scale = gamma_params(out_32_8[1], out_32_8[2])$scale),
                           v_out_32_9. = rgamma(n_runs, shape = gamma_params(out_32_9[1], out_32_9[2])$shape, scale = gamma_params(out_32_9[1], out_32_9[2])$scale),
                           v_out_36_0. = rgamma(n_runs, shape = gamma_params(out_36_0[1], out_36_0[2])$shape, scale = gamma_params(out_36_0[1], out_36_0[2])$scale),
                           v_out_36_1. = rgamma(n_runs, shape = gamma_params(out_36_1[1], out_36_1[2])$shape, scale = gamma_params(out_36_1[1], out_36_1[2])$scale),
                           v_out_36_2. = rgamma(n_runs, shape = gamma_params(out_36_2[1], out_36_2[2])$shape, scale = gamma_params(out_36_2[1], out_36_2[2])$scale),
                           v_out_36_3. = rgamma(n_runs, shape = gamma_params(out_36_3[1], out_36_3[2])$shape, scale = gamma_params(out_36_3[1], out_36_3[2])$scale),
                           v_out_36_4. = rgamma(n_runs, shape = gamma_params(out_36_4[1], out_36_4[2])$shape, scale = gamma_params(out_36_4[1], out_36_4[2])$scale),
                           v_out_36_5. = rgamma(n_runs, shape = gamma_params(out_36_5[1], out_36_5[2])$shape, scale = gamma_params(out_36_5[1], out_36_5[2])$scale),
                           v_out_36_6. = rgamma(n_runs, shape = gamma_params(out_36_6[1], out_36_6[2])$shape, scale = gamma_params(out_36_6[1], out_36_6[2])$scale),
                           v_out_36_7. = rgamma(n_runs, shape = gamma_params(out_36_7[1], out_36_7[2])$shape, scale = gamma_params(out_36_7[1], out_36_7[2])$scale),
                           v_out_36_8. = rgamma(n_runs, shape = gamma_params(out_36_8[1], out_36_8[2])$shape, scale = gamma_params(out_36_8[1], out_36_8[2])$scale),
                           v_out_36_9. = rgamma(n_runs, shape = gamma_params(out_36_9[1], out_36_9[2])$shape, scale = gamma_params(out_36_9[1], out_36_9[2])$scale),
                        
                           # ANNUAL QUALY CALCULATION
                           v_term_init_28.   = rbeta(n_runs, beta_params(0.871, 0.023)$alpha, beta_params(0.871, 0.023)$beta),
                           v_term_init_32.   = rbeta(n_runs, beta_params(0.871, 0.023)$alpha, beta_params(0.871, 0.023)$beta),
                           v_term_init_34.   = rbeta(n_runs, beta_params(0.871, 0.023)$alpha, beta_params(0.871, 0.023)$beta),
                           v_term_init_37.   = rbeta(n_runs, beta_params(0.871, 0.023)$alpha, beta_params(0.871, 0.023)$beta),
                           v_term_init_term. = rbeta(n_runs, beta_params(0.871, 0.023)$alpha, beta_params(0.871, 0.023)$beta),
                           v_term_change_28.   = rbeta(n_runs, beta_params(0.211, 0.044)$alpha, beta_params(0.211, 0.044)$beta) * -1,
                           v_term_change_32.   = rbeta(n_runs, beta_params(0.211, 0.044)$alpha, beta_params(0.211, 0.044)$beta) * -1,
                           v_term_change_34.   = rbeta(n_runs, beta_params(0.211, 0.044)$alpha, beta_params(0.211, 0.044)$beta) * -1,
                           v_term_change_37.   = rbeta(n_runs, beta_params(0.211, 0.044)$alpha, beta_params(0.211, 0.044)$beta) * -1,
                           v_term_change_term. = rbeta(n_runs, beta_params(0.211, 0.044)$alpha, beta_params(0.211, 0.044)$beta) * -1,
                           v_pt_nsi_i_dif. = rnorm(n_runs, -0.264, 0.048),
                           v_pt_i_dif_28. = rnorm(n_runs, -0.092, 0.033),
                           v_p_nsi_28. = rbeta(n_runs, 406, 504),
                           v_p_nsi_32. = rbeta(n_runs, 142, 370),
                           v_p_nsi_34. = rbeta(n_runs, 4, 83),
                           v_p_nsi_37. = rbeta(n_runs, 40, 511),
                           v_p_nsi_term. = rbeta(n_runs, 19, 746))

# Initiate an empty list (to be populated by )
l_psa <- vector(mode = "list")

# Populate the list using variable values from df_psa_input
# Each item represents output for a "run" of the model
for (i in 1: n_runs){
  x <- CLS_model(twin_factor. = twin_factor,
                 v_strats.    = v_strats,
                 n_strats.    = n_strats,
                 v_GA_cat.    = v_GA_cat,
                 n_GA_cat.    = n_GA_cat,
                 n_scans.     = n_scans, 
                 prog_start.  = prog_start,
                 prog_stop.   = prog_stop, 
                 wks.         = wks,
                 
                 # TREE PROBABILITIES
                 p_cl25_t1.    = df_psa_input$v_p_cl25_t1.[i],
                 p_cl15_t1.    = df_psa_input$v_p_cl15_t1.[i],
                 p_cl25_t2.    = df_psa_input$v_p_cl25_t2.[i],
                 p_cl15_t2.    = df_psa_input$v_p_cl15_t2.[i],
                 p_clN_28_t2.  = df_psa_input$v_p_clN_28_t2.[i],
                 p_clN_32_t2.  = df_psa_input$v_p_clN_32_t2.[i],
                 p_clN_34_t2.  = df_psa_input$v_p_clN_34_t2.[i],
                 p_clN_37_t2.  = df_psa_input$v_p_clN_37_t2.[i],
                 p_cl25_28_t1. = df_psa_input$v_p_cl25_28_t1.[i],
                 p_cl25_32_t1. = df_psa_input$v_p_cl25_32_t1.[i],
                 p_cl25_34_t1. = df_psa_input$v_p_cl25_34_t1.[i],
                 p_cl25_37_t1. = df_psa_input$v_p_cl25_37_t1.[i],
                 p_cl25_28_t2. = df_psa_input$v_p_cl25_28_t2.[i],
                 p_cl25_32_t2. = df_psa_input$v_p_cl25_32_t2.[i],
                 p_cl25_34_t2. = df_psa_input$v_p_cl25_34_t2.[i],
                 p_cl25_37_t2. = df_psa_input$v_p_cl25_37_t2.[i],
                 p_cl15_28_t1. = df_psa_input$v_p_cl15_28_t1.[i],
                 p_cl15_32_t1. = df_psa_input$v_p_cl15_32_t1.[i],
                 p_cl15_34_t1. = df_psa_input$v_p_cl15_34_t1.[i],
                 p_cl15_37_t1. = df_psa_input$v_p_cl15_37_t1.[i],
                 p_cl15_28_t2. = df_psa_input$v_p_cl15_28_t2.[i],
                 p_cl15_32_t2. = df_psa_input$v_p_cl15_32_t2.[i],
                 p_cl15_34_t2. = df_psa_input$v_p_cl15_34_t2.[i],
                 p_cl15_37_t2. = df_psa_input$v_p_cl15_37_t2.[i],
                 rr_p_28.      = df_psa_input$v_rr_p_28.[i],
                 rr_p_32.      = df_psa_input$v_rr_p_32.[i],
                 rr_p_34.      = df_psa_input$v_rr_p_34.[i],
                 rr_p_37.      = df_psa_input$v_rr_p_37.[i],
                 rr_cp_28.     = df_psa_input$v_rr_cp_28.[i],
                 rr_cp_32.     = df_psa_input$v_rr_cp_32.[i],
                 rr_cp_34.     = df_psa_input$v_rr_cp_34.[i],
                 rr_cp_37.     = df_psa_input$v_rr_cp_37.[i],
                 rr_c_28.      = df_psa_input$v_rr_c_28.[i],
                 rr_c_32.      = df_psa_input$v_rr_c_32.[i],
                 rr_c_34.      = df_psa_input$v_rr_c_34.[i],
                 rr_c_37.      = df_psa_input$v_rr_c_37.[i],
                 p_prog.       = p_prog, # 1      
                 p_pY_cerc.    = p_pY_cerc, # 1
                 p_pN_cerc.    = p_pN_cerc, # 0 
                 
                 # TREE-RELATED COSTS
                 c_s. = c_s, 
                 c_c. = c_c,
                 c_p. = c_p,
                 c_del_28.   = c_del_28,
                 c_del_32.   = c_del_32,
                 c_del_34.   = c_del_34,
                 c_del_37.   = c_del_37,
                 c_del_term. = c_del_term,
                 
                 # MARKOV SETUP
                 cycle_length.  = cycle_length,         
                 n_age_init.     = n_age_init,
                 n_age_max.      = n_age_max,
                 v_names_states. = v_names_states,
                 n_states.       = n_states,
                 
                 # STILL BIRTH PROBABILITIES
                 p_sb_28.    = df_psa_input$v_p_sb_28.[i],  
                 p_sb_32.    = df_psa_input$v_p_sb_32.[i],  
                 p_sb_34.    = df_psa_input$v_p_sb_34.[i],  
                 p_sb_37.    = df_psa_input$v_p_sb_37.[i],  
                 p_sb_term.  = df_psa_input$v_p_sb_term.[i],
    
                 # ANNUAL MORTALITIES
                 v_r_mort.        = v_r_mort, 
                 hr_AD_28_01.     = df_psa_input$v_hr_AD_28_01.[i],
                 hr_AD_32_01.     = df_psa_input$v_hr_AD_32_01.[i],
                 hr_AD_34_01.     = df_psa_input$v_hr_AD_34_01.[i],
                 hr_AD_37_01.     = df_psa_input$v_hr_AD_37_01.[i],
                 hr_AD_term_01.   = hr_AD_term_01,
                 hr_AD_28_19.     = df_psa_input$v_hr_AD_28_19.[i],  
                 hr_AD_32_19.     = df_psa_input$v_hr_AD_32_19.[i],  
                 hr_AD_34_19.     = df_psa_input$v_hr_AD_34_19.[i],  
                 hr_AD_37_19.     = df_psa_input$v_hr_AD_37_19.[i],  
                 hr_AD_term_19.   = hr_AD_term_19,
                 hr_AD_28_1019.   = df_psa_input$v_hr_AD_28_1019.[i],
                 hr_AD_32_1019.   = df_psa_input$v_hr_AD_32_1019.[i],
                 hr_AD_34_1019.   = df_psa_input$v_hr_AD_34_1019.[i],
                 hr_AD_37_1019.   = df_psa_input$v_hr_AD_37_1019.[i],
                 hr_AD_term_1019. = hr_AD_term_1019,
                 hr_AD_28_2029.   = df_psa_input$v_hr_AD_28_2029.[i],
                 hr_AD_32_2029.   = df_psa_input$v_hr_AD_32_2029.[i],
                 hr_AD_34_2029.   = df_psa_input$v_hr_AD_34_2029.[i],
                 hr_AD_37_2029.   = df_psa_input$v_hr_AD_37_2029.[i],
                 hr_AD_term_2029. = hr_AD_term_2029,
                 hr_AD_28_3045.   = df_psa_input$v_hr_AD_28_3045.[i],
                 hr_AD_32_3045.   = df_psa_input$v_hr_AD_32_3045.[i],
                 hr_AD_34_3045.   = df_psa_input$v_hr_AD_34_3045.[i],
                 hr_AD_37_3045.   = df_psa_input$v_hr_AD_37_3045.[i],
                 hr_AD_term_3045. = hr_AD_term_3045,
                 length_19.       = length_19,
                 length_1019.     = length_1019,
                 length_2029.     = length_2029,
                 length_3045.     = length_3045,
                 length_100.      = length_100, 
                 hr_final.        = hr_final, 
                 
                 # NICU (INITIAL INPATIENT) COSTS
                 c_n_28.     = df_psa_input$v_c_n_28.[i],  
                 c_n_32.     = df_psa_input$v_c_n_32.[i],  
                 c_n_34.     = df_psa_input$v_c_n_34.[i],  
                 c_n_37.     = df_psa_input$v_c_n_37.[i],  
                 c_n_term.   = df_psa_input$v_c_n_term.[i],
                 p_adm_28.   = p_adm_28,
                 p_adm_32.   = p_adm_32,
                 p_adm_34.   = p_adm_34,
                 p_adm_37.   = df_psa_input$v_p_adm_37.[i],
                 p_adm_term. = df_psa_input$v_p_adm_term.[i],
                 
                 # ANNUAL HEALTH COSTS
                 in_28_0.  = df_psa_input$v_in_28_0.[i],
                 in_28_1.  = df_psa_input$v_in_28_1.[i],
                 in_28_2.  = df_psa_input$v_in_28_2.[i],
                 in_28_3.  = df_psa_input$v_in_28_3.[i],
                 in_28_4.  = df_psa_input$v_in_28_4.[i],
                 in_28_5.  = df_psa_input$v_in_28_5.[i],
                 in_28_6.  = df_psa_input$v_in_28_6.[i],
                 in_28_7.  = df_psa_input$v_in_28_7.[i],
                 in_28_8.  = df_psa_input$v_in_28_8.[i],
                 in_28_9.  = df_psa_input$v_in_28_9.[i],
                 in_32_0.  = df_psa_input$v_in_32_0.[i],
                 in_32_1.  = df_psa_input$v_in_32_1.[i],
                 in_32_2.  = df_psa_input$v_in_32_2.[i],
                 in_32_3.  = df_psa_input$v_in_32_3.[i],
                 in_32_4.  = df_psa_input$v_in_32_4.[i],
                 in_32_5.  = df_psa_input$v_in_32_5.[i],
                 in_32_6.  = df_psa_input$v_in_32_6.[i],
                 in_32_7.  = df_psa_input$v_in_32_7.[i],
                 in_32_8.  = df_psa_input$v_in_32_8.[i],
                 in_32_9.  = df_psa_input$v_in_32_9.[i],
                 in_36_0.  = df_psa_input$v_in_36_0.[i],
                 in_36_1.  = df_psa_input$v_in_36_1.[i],
                 in_36_2.  = df_psa_input$v_in_36_2.[i],
                 in_36_3.  = df_psa_input$v_in_36_3.[i],
                 in_36_4.  = df_psa_input$v_in_36_4.[i],
                 in_36_5.  = df_psa_input$v_in_36_5.[i],
                 in_36_6.  = df_psa_input$v_in_36_6.[i],
                 in_36_7.  = df_psa_input$v_in_36_7.[i],
                 in_36_8.  = df_psa_input$v_in_36_8.[i],
                 in_36_9.  = df_psa_input$v_in_36_9.[i],
                 out_28_0. = df_psa_input$v_out_28_0.[i],
                 out_28_1. = df_psa_input$v_out_28_1.[i],
                 out_28_2. = df_psa_input$v_out_28_2.[i],
                 out_28_3. = df_psa_input$v_out_28_3.[i],
                 out_28_4. = df_psa_input$v_out_28_4.[i],
                 out_28_5. = df_psa_input$v_out_28_5.[i],
                 out_28_6. = df_psa_input$v_out_28_6.[i],
                 out_28_7. = df_psa_input$v_out_28_7.[i],
                 out_28_8. = df_psa_input$v_out_28_8.[i],
                 out_28_9. = df_psa_input$v_out_28_9.[i],
                 out_32_0. = df_psa_input$v_out_32_0.[i],
                 out_32_1. = df_psa_input$v_out_32_1.[i],
                 out_32_2. = df_psa_input$v_out_32_2.[i],
                 out_32_3. = df_psa_input$v_out_32_3.[i],
                 out_32_4. = df_psa_input$v_out_32_4.[i],
                 out_32_5. = df_psa_input$v_out_32_5.[i],
                 out_32_6. = df_psa_input$v_out_32_6.[i],
                 out_32_7. = df_psa_input$v_out_32_7.[i],
                 out_32_8. = df_psa_input$v_out_32_8.[i],
                 out_32_9. = df_psa_input$v_out_32_9.[i],
                 out_36_0. = df_psa_input$v_out_36_0.[i],
                 out_36_1. = df_psa_input$v_out_36_1.[i],
                 out_36_2. = df_psa_input$v_out_36_2.[i],
                 out_36_3. = df_psa_input$v_out_36_3.[i],
                 out_36_4. = df_psa_input$v_out_36_4.[i],
                 out_36_5. = df_psa_input$v_out_36_5.[i],
                 out_36_6. = df_psa_input$v_out_36_6.[i],
                 out_36_7. = df_psa_input$v_out_36_7.[i],
                 out_36_8. = df_psa_input$v_out_36_8.[i],
                 out_36_9. = df_psa_input$v_out_36_9.[i],
                 length_101. = length_101,
                 
                 # Death-related costs, utility
                 c_D. = c_D,
                 u_D. = u_D,
                 
                 # ANNUAL QUAY CALCULATION
                 term_init_28.      = df_psa_input$v_term_init_28.[i],
                 term_init_32.      = df_psa_input$v_term_init_32.[i],
                 term_init_34.      = df_psa_input$v_term_init_34.[i],
                 term_init_37.      = df_psa_input$v_term_init_37.[i],
                 term_init_term.    = df_psa_input$v_term_init_term.[i],
                 term_change_28.    = df_psa_input$v_term_change_28.[i],
                 term_change_32.    = df_psa_input$v_term_change_32.[i],
                 term_change_34.    = df_psa_input$v_term_change_34.[i],
                 term_change_37.    = df_psa_input$v_term_change_37.[i],
                 term_change_term.  = df_psa_input$v_term_change_term.[i],
                 pt_nsi_i_dif. = df_psa_input$v_pt_nsi_i_dif.[i],
                 pt_i_dif_28.  = df_psa_input$v_pt_i_dif_28.[i],
                 pt_i_dif_32.  = pt_i_dif_32,
                 pt_i_dif_34.  = pt_i_dif_34,
                 pt_i_dif_37.  = pt_i_dif_37,
                 p_nsi_28.     = df_psa_input$v_p_nsi_28.[i],
                 p_nsi_32.     = df_psa_input$v_p_nsi_32.[i],
                 p_nsi_34.     = df_psa_input$v_p_nsi_34.[i],
                 p_nsi_37.     = df_psa_input$v_p_nsi_37.[i],
                 p_nsi_term.   = df_psa_input$v_p_nsi_term.[i],
                 
                 # DISCOUNTING
                 d_u. = d_u,
                 d_c. = d_c)$df_all %>% 
    
    # Add column to represent the run
    mutate(run = rep(i, times = 3))
  
  l_psa[[i]] <- x
}

# Bind each "run" of the model into a single df
df_psa <- do.call("rbind", l_psa)

# Dataframe for ICERS
df_icer <- df_psa %>%
  select(run, strategy, costs, QALYs) %>%
  pivot_wider(id_cols = run, names_from = strategy, values_from = c(costs, QALYs)) %>%
  mutate(QALYs_s2s1 = QALYs_s2 - QALYs_s1) %>%
  mutate(costs_s2s1 = costs_s2 - costs_s1) %>%
  mutate(QALYs_s3s1 = QALYs_s3 - QALYs_s1) %>%
  mutate(costs_s3s1 = costs_s3 - costs_s1) 

# Modify df for plotting
df_icer_long <- df_icer %>%
  select(QALYs_s2s1, costs_s2s1, QALYs_s3s1, costs_s3s1) %>%
  pivot_longer(cols = everything(), 
               names_to = c(".value", "comparison"), 
               names_sep = "_") %>%
  mutate(icer = costs/QALYs)

summary_icer <- df_icer_long %>%
  group_by(comparison) %>%
  summarize(mean_costs = mean(costs),
            p0025_costs = quantile(costs, 0.025),
            p9750_costs = quantile(costs, 0.975),
            mean_QALYs = mean(QALYs),
            p0025_QALYs = quantile(QALYs, 0.025),
            p9750_QALYs = quantile(QALYs, 0.975),
            mean_icer = mean(icer),
            p0025_icer = quantile(icer, 0.025),
            p9750_icer = quantile(icer, 0.975))

# Plot of ICERs
psa_plot_icer <- df_icer_long %>%
  ggplot(aes(x = QALYs, y = costs)) + 
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  facet_wrap(vars(comparison), 
             labeller = as_labeller(c("s2s1" = "One-step screen vs. No screen", 
                                      "s3s1" = "Two-step screen vs. No screen"))) +
  geom_point(shape = 1) +
  labs(x = "Incremental effect (QALYs)",
       y = "Incremental costs (CAD$)") + 
  theme_bw()

psa_plot_icer

#ggsave(plot = psa_plot_icer, filename = "psa_plot_icer.pdf", path = "Results/", width = 9, height = 5)

## Create df for costs and QALYs for dampack to use ## 

# Costs
df_costs <- df_psa %>%
  select(run, strategy, costs) %>%
  pivot_wider(id_cols = run, names_from = strategy, values_from = costs) %>%
  select(-run)

# Effectiveness (QALYs)
df_QALYs <- df_psa %>%
  select(run, strategy, QALYs) %>%
  pivot_wider(id_cols = run, names_from = strategy, values_from = QALYs) %>%
  select(-run)

# PSA object for dampack 
psa_obj <- make_psa_obj(cost = df_costs, effectiveness = df_QALYs, strategies = v_strats)
psa_obj_eff <- psa_obj$effectiveness # check

## INCREMENTAL COST-EFFECTIVENESS TABLE

# Calculate the mean costs and QALYs for each strategy
mean_costs <- apply(df_costs, MARGIN = 2, FUN = mean)
mean_QALYs <- apply(df_QALYs, MARGIN = 2, FUN = mean)

# ICER table/plot (dampack)
icers <- calculate_icers(cost = mean_costs, 
                         effect = mean_QALYs, 
                         strategies = v_strats)

plot(icers)

## Variable (OUTCOMES) definition
var_def <- data.frame(variable = c("c_cerc", "c_del", "c_neon", "c_preg_total", "c_progest", "c_us", "cerclage", "CL15", "CL25", "costs", "LE", "progest", "ptb28", "ptb32", "ptb34", "ptb37", "term", "QALYs", "stillbirths"),
                      defn = c("cerclage cost", "delivery cost", "NICU cost", "total pregnancy cost", "progesterone cost", "ultrasound cost", "cerclage use", "CL <=15mm", "CL <=25mm", "total costs", "life expectancy", "progesterone use", "delivery <=28wks", "delivery <=32wks", "delivery <=34wks", "delivery <=37wks", "delivery >37wks", "QALYs", "stillbirth"))

## Summary table
psa_sum <- df_psa %>%
  group_by(strategy) %>%
  # Summarize for each variable: mean, p5, p95
  summarize(across(.cols = -run,
                   list(mean = ~mean(.), 
                        p25 = ~quantile(., 0.025), 
                        p975 = ~quantile(., 0.975)),
                   .names = "{col}X{fn}"))

#write.csv(x = psa_sum, file = 'Results/psa_summary.csv', quote= F)

psa_sum_long <- psa_sum %>%
  pivot_longer(
    cols = -strategy,
    names_to = c("variable", ".value"),
    names_sep = "X") %>%
  # Add definitions of the variable names
  left_join(var_def)

#write.csv(x = psa_sum_long, file = 'Results/psa_summary_long.csv', quote= F)

## Summary graph 
psa_sum_plot <- ggplot(data = psa_sum_long, aes(x = strategy, y = mean, ymin = p25, ymax = p975)) + 
  geom_pointrange(aes(col = strategy)) +
  facet_wrap(~defn, scales = "free_y") +
  scale_color_discrete(labels = strat_names) +
  theme_bw() + 
  theme(legend.position = c(0.92,0.075))

psa_sum_plot

#ggsave(plot = psa_sum_plot, filename = "psa_sum_plot.pdf", path = "Results/", width = 9, height = 5)

## Plot: Cost vs. QALYs (dampack)
psa_plot <- plot(psa_obj) + 
  labs(x = "Effectiveness (QALYs)") +
  scale_fill_discrete(labels = strat_names) +
  guides(color = "none")

psa_plot

#ggsave(plot = psa_plot, filename = "psa_plot.pdf", path = "Results/", width = 8, height = 5)
 
## CEAC (dampack)

# Willingness to pay threshold
v_wtp <- seq(from = 0, to = 250000, by = 10000)

ceac_obj <- ceac(wtp = v_wtp, psa = psa_obj)
ceac_plot <- plot(ceac_obj, ylim = c(0, 1.0)) +
  scale_color_discrete(labels = strat_names)

ceac_plot

#ggsave(plot = ceac_plot, filename = "ceac_plot.pdf", path = "Results/", height = 5, width = 8)

ceac_obj
summary(ceac_obj)

## Expected loss curves (ELCs) and expected value of perfect information (EVPI).
exp_loss <- calc_exp_loss(psa_obj, v_wtp)

# can use head(), summary(), print(), etc.
head(exp_loss)

# plot an expected loss curve (ELC)
elc_plot <- plot(exp_loss, log_y = FALSE, xlim = c(10, 250)) +
  scale_color_discrete(labels = strat_names) 

elc_plot

#ggsave(plot = elc_plot, filename = "elc_plot.pdf", path = "Results/")

#######################
## TROUBLE-SHOOTING ###
#######################

# Understanding variability

## REGRESSION COSTS ###
df_lm_costs <- data.frame(y = df_costs$s3, df_psa_input)
test_model_costs <- lm(y ~ ., data = df_lm_costs)
(anova_costs <- anova(test_model_costs))

#write.csv(x = anova_costs, file = 'Results/anova_costs.csv', quote= F)

## REGRESSION QALYs ###
df_lm_qaly <- data.frame (y = df_QALYs$s3, df_psa_input)
test_model_QALY <- lm(y ~ ., data = df_lm_qaly)
(anova_qaly <- anova(test_model_QALY))

#write.csv(x = anova_qaly, file = 'Results/anova_qaly.csv', quote= F)

# Identifying outcomes 
low_utilities <- which(df_QALYs$s3 < 30)
View(df_psa_input[low_utilities,])

hist(df_psa_input$v_term_init.)

## END ##