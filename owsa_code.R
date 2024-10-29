####################################
### One-way sensitivity analysis ###
####################################

###
### OWSA FUNCTION ### 
###

## The function does three things:
# 1. Creates a vector of values for a given variable min and max
# 2. Runs the model across the range of input values for the variable
# 3. Generates a single dataframe that contains the model outputs across input variable values

## Returns: dataframe with model outputs for 3 strategies across values of the variable

owsa_fun <- function(min.,
                     max.,
                     var_name.,
                     values_length. = 50) {
  
  # Create a vector containing the range of variable values
  values <- seq(from = min., to = max., length.out = values_length.)
  
  # Add a "." to the variable name to serve as an argument for the CLS_model
  arg_name <- paste0(var_name., ".")
  
  # Creates a list, where each item is a value from values with the same name (var_name)
  args_list <- lapply(values, function(value) setNames(list(value), arg_name))
  
  # Run the CLS model for each value in args_list, to create a list of outputs (model_out)
  # This code applies an anonymous function, where CLS is later called using do.call()
  # Take only the 4th row (df_all)
  model_out <- mapply(function(arg) {
    do.call(CLS_model, arg)
  }, args_list, SIMPLIFY = TRUE)[4,]
  
  # For each data frame in the list model_out, add variable value and name column 
  for(i in seq_along(model_out)) {
    model_out[[i]]$var_value <- rep(values[i], nrow(model_out[[i]]))
    model_out[[i]]$var_name <- rep(var_name., nrow(model_out[[i]]))
  }
  
  # Combine the data frames from model_out into a single long list
  combined_df <- do.call(rbind, model_out)
  
  return(combined_df)

}

### 
### DATA FRAME: Parameter names, definitions, and base values
###

# OWSA parameter names
v_params <- c("twin_factor", "p_cl25_t1", "p_cl15_t1", "p_cl25_t2", "p_cl15_t2", "p_prog", "p_pY_cerc", "p_pN_cerc", "p_clN_28_t2", "p_clN_32_t2", "p_clN_34_t2", "p_clN_37_t2", "p_cl25_28_t1", "p_cl25_32_t1", "p_cl25_34_t1", "p_cl25_37_t1", "p_cl25_28_t2", "p_cl25_32_t2", "p_cl25_34_t2", "p_cl25_37_t2", "p_cl15_28_t1", "p_cl15_32_t1", "p_cl15_34_t1", "p_cl15_37_t1", "p_cl15_28_t2", "p_cl15_32_t2", "p_cl15_34_t2", "p_cl15_37_t2", "rr_p_28", "rr_p_32", "rr_p_34", "rr_p_37", "rr_c_28", "rr_c_32", "rr_c_34", "rr_c_37", "rr_cp_28", "rr_cp_32", "rr_cp_34", "rr_cp_37", "p_sb_28", "p_sb_32", "p_sb_34", "p_sb_37", "p_sb_term", "hr_AD_28_01", "hr_AD_32_01", "hr_AD_34_01", "hr_AD_37_01", "hr_AD_28_19", "hr_AD_32_19", "hr_AD_34_19", "hr_AD_37_19", "hr_AD_28_1019", "hr_AD_32_1019", "hr_AD_34_1019", "hr_AD_37_1019", "hr_AD_28_2029", "hr_AD_32_2029", "hr_AD_34_2029", "hr_AD_37_2029", "hr_AD_28_3045", "hr_AD_32_3045", "hr_AD_34_3045", "hr_AD_37_3045", "c_s", "c_c", "c_p", "c_del_28", "c_del_32", "c_del_34", "c_del_37", "c_del_term", "c_n_28", "c_n_32", "c_n_34", "c_n_37", "c_n_term", "p_adm_37", "p_adm_term", "term_init_28", "term_init_32", "term_init_34", "term_init_37", "term_init_term", "term_change_28", "term_change_32", "term_change_34", "term_change_37", "term_change_term", "pt_nsi_i_dif", "pt_i_dif_28", "pt_i_dif_32", "pt_i_dif_34", "p_nsi_28", "p_nsi_32", "p_nsi_34", "p_nsi_37", "p_nsi_term", "d_c", "d_u")

# Convert v_params to objects instead of strings and combine with the strings
df_base_values <- data.frame(var_name = v_params,
                            base_value = I(mget(v_params, envir = .GlobalEnv))) %>%
  mutate(base_value = as.numeric(base_value))

v_defn <- c("Twin multiplier", "Screen one CL <=25", "Screen one CL <=15", "Screen two CL <=25", "Screen two CL <=15", "Prog. uptake CL <=25", "Cerclage uptake CL <=15 given prog.", "Cerclage uptake CL <=15 given NO prog.", "Delivery <28, CL >25 screen 2", "Delivery 28-32, CL >25 screen 2", "Delivery 32-34, CL >25 screen 2", "Delivery 34-37, CL >25 screen 2", "Delivery <28, CL 15-25 screen 1", "Delivery 28-32, CL 15-25 screen 1", "Delivery 32-34, CL 15-25 screen 1", "Delivery 34-37, CL 15-25 screen 1", "Delivery <28, CL 15-25 screen 2", "Delivery 28-32, CL 15-25 screen 2", "Delivery 32-34, CL 15-25 screen 2", "Delivery 34-37, CL 15-25 screen 2", "Delivery <28, CL <15 screen 1", "Delivery 28-32, CL <15 screen 1", "Delivery 32-34, CL <15 screen 1", "Delivery 34-37, CL <15 screen 1", "Delivery <28, CL <15 screen 2", "Delivery 28-32, CL <15 screen 2", "Delivery 32-34, CL <15 screen 2", "Delivery 34-37, CL <15 screen 2", "Prog. effect <28", "Prog. effect 28-32", "Prog. effect 32-34", "Prog. effect 34-37", "Cerc. effect <28", "Cerc. effect 28-32", "Cerc. effect 32-34", "Cerc. effect 34-37", "C+P effect <28", "C+P effect 28-32", "C+P effect 32-34", "C+P effect 34-37", "Stillbirth <28", "Stillbirth 28-32", "Stillbirth 32-34", "Stillbirth 34-37", "Stillbirth term", "HR mort. 0-1 <28", "HR mort. 0-1 28-32", "HR mort. 0-1 32-34", "HR mort. 0-1 34-37", "HR mort. 1-9 <28", "HR mort. 1-9 28-32", "HR mort. 1-9 32-34", "HR mort. 1-9 34-37", "HR mort. 10-19 <28", "HR mort. 10-19 28-32", "HR mort. 10-19 32-34", "HR mort. 10-19 34-37", "HR mort. 20-29 <28", "HR mort. 20-29 28-32", "HR mort. 20-29 32-34", "HR mort. 20-29 34-37", "HR mort. 30-45 <28", "HR mort. 30-45 28-32", "HR mort. 30-45 32-34", "HR mort. 30-45 34-37", "Scan cost", "Cerclage cost", "Progesterone cost", "Delivery cost <28", "Delivery cost 28-32", "Delivery cost 32-34", "Delivery cost 34-37", "Delivery cost term", "NICU cost <28", "NICU cost 28-32", "NICU cost 32-34", "NICU cost 34-37", "NICU cost term", "Admission 34-37", "Admission term", "HUI3 at birth (<28)", "HUI3 at birth (28-32)", "HUI3 at birth (32-34)", "HUI3 at birth (34-37)", "HUI3 at birth (term)", "HUI3 change (<28)", "HUI3 change (28-32)", "HUI3 change (32-34)", "HUI3 change (34-37)", "HUI3 change (term)", "Disutility preterm + NSI", "Disutility < 28", "Disutility 28-32", "Disutility 32-34", "NSI <28", "NSI 28-32", "NSI 32-34", "NSI 34-37", "NSI term", "Discount_costs", "Discount_utility")

df_params <- data.frame(var_name = v_params,
                        var_def = v_defn) %>%
  left_join(df_base_values)

length(df_params$var_def)

###
### GENERATING OWSA OUTCOMES ###
###

## OWSA for each variable
owsa_twin_factor      <- owsa_fun(min. = 1, max. = 2, var_name. = "twin_factor")
owsa_p_cl25_t1        <- owsa_fun(min. = 0.03, max. = 0.06, var_name. = "p_cl25_t1")
owsa_p_cl15_t1        <- owsa_fun(0.31, 0.64, var_name. = "p_cl15_t1")
owsa_p_cl25_t2        <- owsa_fun(0.06, 0.1, var_name. = "p_cl25_t2")
owsa_p_cl15_t2        <- owsa_fun(0.26, 0.51, var_name. = "p_cl15_t2")
owsa_p_prog           <- owsa_fun(0.5, 1, var_name. = "p_prog")
owsa_p_pY_cerc        <- owsa_fun(0, 1, var_name. = "p_pY_cerc")
owsa_p_pN_cerc        <- owsa_fun(0, 0.5, var_name. = "p_pN_cerc")
owsa_p_clN_28_t2      <- owsa_fun(0.01, 0.02, var_name. = "p_clN_28_t2")
owsa_p_clN_32_t2      <- owsa_fun(0.02, 0.04, var_name. = "p_clN_32_t2")
owsa_p_clN_34_t2      <- owsa_fun(0.05, 0.08, var_name. = "p_clN_34_t2")
owsa_p_clN_37_t2      <- owsa_fun(0.53, 0.61, var_name. = "p_clN_37_t2")
owsa_p_cl25_28_t1     <- owsa_fun(0.03, 0.34, var_name. = "p_cl25_28_t1")
owsa_p_cl25_32_t1     <- owsa_fun(0.04, 0.38, var_name. = "p_cl25_32_t1")
owsa_p_cl25_34_t1     <- owsa_fun(0.01, 0.33, var_name. = "p_cl25_34_t1")
owsa_p_cl25_37_t1     <- owsa_fun(0.25, 0.75, var_name. = "p_cl25_37_t1")
owsa_p_cl25_28_t2     <- owsa_fun(0.03, 0.2, var_name. = "p_cl25_28_t2")
owsa_p_cl25_32_t2     <- owsa_fun(0.05, 0.25, var_name. = "p_cl25_32_t2")
owsa_p_cl25_34_t2     <- owsa_fun(0.06, 0.28, var_name. = "p_cl25_34_t2")
owsa_p_cl25_37_t2     <- owsa_fun(0.39, 0.72, var_name. = "p_cl25_37_t2")
owsa_p_cl15_28_t1     <- owsa_fun(0.3, 0.75, var_name. = "p_cl15_28_t1")
owsa_p_cl15_32_t1     <- owsa_fun(0.03, 0.51, var_name. = "p_cl15_32_t1")
owsa_p_cl15_34_t1     <- owsa_fun(0, 0.39, var_name. = "p_cl15_34_t1")
owsa_p_cl15_37_t1     <- owsa_fun(0.19, 0.81, var_name. = "p_cl15_37_t1")
owsa_p_cl15_28_t2     <- owsa_fun(0.17, 0.47, var_name. = "p_cl15_28_t2")
owsa_p_cl15_32_t2     <- owsa_fun(0.16, 0.51, var_name. = "p_cl15_32_t2")
owsa_p_cl15_34_t2     <- owsa_fun(0.01, 0.28, var_name. = "p_cl15_34_t2")
owsa_p_cl15_37_t2     <- owsa_fun(0.3, 0.75, var_name. = "p_cl15_37_t2")
owsa_rr_p_28          <- owsa_fun(0.24, 1.16, var_name. = "rr_p_28")
owsa_rr_p_32          <- owsa_fun(0.34, 1.47, var_name. = "rr_p_32")
owsa_rr_p_34          <- owsa_fun(0.31, 1.33, var_name. = "rr_p_34")
owsa_rr_p_37          <- owsa_fun(0.72, 1.43, var_name. = "rr_p_37")
owsa_rr_c_28          <- owsa_fun(0.26, 1.21, var_name. = "rr_c_28")
owsa_rr_c_32          <- owsa_fun(0.38, 1.6, var_name. = "rr_c_32")
owsa_rr_c_34          <- owsa_fun(0.11, 0.84, var_name. = "rr_c_34")
owsa_rr_c_37          <- owsa_fun(0.6, 1.67, var_name. = "rr_c_37")
owsa_rr_cp_28         <- owsa_fun(0.26, 1.21, var_name. = "rr_cp_28")
owsa_rr_cp_32         <- owsa_fun(0.38, 1.6, var_name. = "rr_cp_32")
owsa_rr_cp_34         <- owsa_fun(0.11, 0.84, var_name. = "rr_cp_34")
owsa_rr_cp_37         <- owsa_fun(0.6, 1.67, var_name. = "rr_cp_37")
owsa_p_sb_28          <- owsa_fun(0.0004, 0.0011, var_name. = "p_sb_28")
owsa_p_sb_32          <- owsa_fun(0.0012, 0.0018, var_name. = "p_sb_32")
owsa_p_sb_34          <- owsa_fun(0.0015, 0.0026, var_name. = "p_sb_34")
owsa_p_sb_37          <- owsa_fun(0.0008, 0.0014, var_name. = "p_sb_37")
owsa_p_sb_term        <- owsa_fun(0.0042, 0.0071, var_name. = "p_sb_term")
owsa_hr_AD_28_01      <- owsa_fun(223.81, 249.54, var_name. = "hr_AD_28_01")
owsa_hr_AD_32_01      <- owsa_fun(30.49, 33.79, var_name. = "hr_AD_32_01")
owsa_hr_AD_34_01      <- owsa_fun(30.49, 33.79, var_name. = "hr_AD_34_01")
owsa_hr_AD_37_01      <- owsa_fun(6.39, 7.13, var_name. = "hr_AD_37_01")
owsa_hr_AD_28_19      <- owsa_fun(3.02, 6.75, var_name. = "hr_AD_28_19")
owsa_hr_AD_32_19      <- owsa_fun(2.78, 3.82, var_name. = "hr_AD_32_19")
owsa_hr_AD_34_19      <- owsa_fun(2.78, 3.82, var_name. = "hr_AD_34_19")
owsa_hr_AD_37_19      <- owsa_fun(1.7, 2.11, var_name. = "hr_AD_37_19")
owsa_hr_AD_28_1019    <- owsa_fun(0.98, 3.63, var_name. = "hr_AD_28_1019")
owsa_hr_AD_32_1019    <- owsa_fun(1.56, 2.33, var_name. = "hr_AD_32_1019")
owsa_hr_AD_34_1019    <- owsa_fun(1.56, 2.33, var_name. = "hr_AD_34_1019")
owsa_hr_AD_37_1019    <- owsa_fun(1.21, 1.55, var_name. = "hr_AD_37_1019")
owsa_hr_AD_28_2029    <- owsa_fun(1.26, 3.59, var_name. = "hr_AD_28_2029")
owsa_hr_AD_32_2029    <- owsa_fun(1.19, 1.7, var_name. = "hr_AD_32_2029")
owsa_hr_AD_34_2029    <- owsa_fun(1.19, 1.7, var_name. = "hr_AD_34_2029")
owsa_hr_AD_37_2029    <- owsa_fun(1.23, 1.48, var_name. = "hr_AD_37_2029")
owsa_hr_AD_28_3045    <- owsa_fun(0.92, 4.55, var_name. = "hr_AD_28_3045")
owsa_hr_AD_32_3045    <- owsa_fun(1.17, 1.87, var_name. = "hr_AD_32_3045")
owsa_hr_AD_34_3045    <- owsa_fun(1.17, 1.87, var_name. = "hr_AD_34_3045")
owsa_hr_AD_37_3045    <- owsa_fun(1.07, 1.39, var_name. = "hr_AD_37_3045")
owsa_c_s              <- owsa_fun(50, 566, var_name. = "c_s")
owsa_c_c              <- owsa_fun(2400, 4374, var_name. = "c_c")
owsa_c_p              <- owsa_fun(15, 25, var_name. = "c_p")
owsa_c_del_28         <- owsa_fun(2596, 12677, var_name. = "c_del_28")
owsa_c_del_32         <- owsa_fun(2596, 12677, var_name. = "c_del_32")
owsa_c_del_34         <- owsa_fun(2596, 12677, var_name. = "c_del_34")
owsa_c_del_37         <- owsa_fun(2596, 12677, var_name. = "c_del_37")
owsa_c_del_term       <- owsa_fun(2596, 12677, var_name. = "c_del_term")
owsa_c_n_28           <- owsa_fun(2781, 633131, var_name. = "c_n_28")
owsa_c_n_32           <- owsa_fun(1949, 593043, var_name. = "c_n_32")
owsa_c_n_34           <- owsa_fun(1506, 351495, var_name. = "c_n_34")
owsa_c_n_37           <- owsa_fun(1506, 377443, var_name. = "c_n_37")
owsa_c_n_term         <- owsa_fun(1506, 377443, var_name. = "c_n_term")
owsa_p_adm_37         <- owsa_fun(0.25, 1.0, var_name. = "p_adm_37")
owsa_p_adm_term       <- owsa_fun(0.07, 0.09, var_name. = "p_adm_term")
owsa_term_init_28     <- owsa_fun(0.826, 0.916, var_name. = "term_init_28")
owsa_term_init_32     <- owsa_fun(0.826, 0.916, var_name. = "term_init_32")
owsa_term_init_34     <- owsa_fun(0.826, 0.916, var_name. = "term_init_34")
owsa_term_init_37     <- owsa_fun(0.826, 0.916, var_name. = "term_init_37")
owsa_term_init_term   <- owsa_fun(0.826, 0.916, var_name. = "term_init_term")
owsa_term_change_28   <- owsa_fun(-0.297, -0.125, var_name. = "term_change_28")
owsa_term_change_32   <- owsa_fun(-0.297, -0.125, var_name. = "term_change_32")
owsa_term_change_34   <- owsa_fun(-0.297, -0.125, var_name. = "term_change_34")
owsa_term_change_37   <- owsa_fun(-0.297, -0.125, var_name. = "term_change_37")
owsa_term_change_term <- owsa_fun(-0.297, -0.125, var_name. = "term_change_term")
owsa_pt_nsi_i_dif     <- owsa_fun(-0.358, -0.17, var_name. = "pt_nsi_i_dif")
owsa_pt_i_dif_28      <- owsa_fun(-0.157, -0.027, var_name. = "pt_i_dif_28")
owsa_pt_i_dif_32      <- owsa_fun(-0.100, -0.000, var_name. = "pt_i_dif_32")
owsa_pt_i_dif_34      <- owsa_fun(-0.100, -0.000, var_name. = "pt_i_dif_34")
owsa_p_nsi_28         <- owsa_fun(0.41, 0.48, var_name. = "p_nsi_28")
owsa_p_nsi_32         <- owsa_fun(0.24, 0.32, var_name. = "p_nsi_32")
owsa_p_nsi_34         <- owsa_fun(0, 0.09, var_name. = "p_nsi_34")
owsa_p_nsi_37         <- owsa_fun(0.05, 0.09, var_name. = "p_nsi_37")
owsa_p_nsi_term       <- owsa_fun(0.01, 0.04, var_name. = "p_nsi_term")
owsa_d_c              <- owsa_fun(0.0, 0.03, var_name. = "d_c")
owsa_d_u              <- owsa_fun(0.0, 0.03, var_name. = "d_u")

# Combine the owsa dataframes
owsa_df <- rbind(owsa_twin_factor, owsa_p_cl25_t1, owsa_p_cl15_t1, owsa_p_cl25_t2, owsa_p_cl15_t2, owsa_p_prog, owsa_p_pY_cerc, owsa_p_pN_cerc, owsa_p_clN_28_t2, owsa_p_clN_32_t2, owsa_p_clN_34_t2, owsa_p_clN_37_t2, owsa_p_cl25_28_t1, owsa_p_cl25_32_t1, owsa_p_cl25_34_t1, owsa_p_cl25_37_t1, owsa_p_cl25_28_t2, owsa_p_cl25_32_t2, owsa_p_cl25_34_t2, owsa_p_cl25_37_t2, owsa_p_cl15_28_t1, owsa_p_cl15_32_t1, owsa_p_cl15_34_t1, owsa_p_cl15_37_t1, owsa_p_cl15_28_t2, owsa_p_cl15_32_t2, owsa_p_cl15_34_t2, owsa_p_cl15_37_t2, owsa_rr_p_28, owsa_rr_p_32, owsa_rr_p_34, owsa_rr_p_37, owsa_rr_c_28, owsa_rr_c_32, owsa_rr_c_34, owsa_rr_c_37, owsa_rr_cp_28, owsa_rr_cp_32, owsa_rr_cp_34, owsa_rr_cp_37, owsa_p_sb_28, owsa_p_sb_32, owsa_p_sb_34, owsa_p_sb_37, owsa_p_sb_term, owsa_hr_AD_28_01, owsa_hr_AD_32_01, owsa_hr_AD_34_01, owsa_hr_AD_37_01, owsa_hr_AD_28_19, owsa_hr_AD_32_19, owsa_hr_AD_34_19, owsa_hr_AD_37_19, owsa_hr_AD_28_1019, owsa_hr_AD_32_1019, owsa_hr_AD_34_1019, owsa_hr_AD_37_1019, owsa_hr_AD_28_2029, owsa_hr_AD_32_2029, owsa_hr_AD_34_2029, owsa_hr_AD_37_2029, owsa_hr_AD_28_3045, owsa_hr_AD_32_3045, owsa_hr_AD_34_3045, owsa_hr_AD_37_3045, owsa_c_s, owsa_c_c, owsa_c_p, owsa_c_del_28, owsa_c_del_32, owsa_c_del_34, owsa_c_del_37, owsa_c_del_term, owsa_c_n_28, owsa_c_n_32, owsa_c_n_34, owsa_c_n_37, owsa_c_n_term, owsa_p_adm_37, owsa_p_adm_term, owsa_term_init_28, owsa_term_init_32, owsa_term_init_34, owsa_term_init_37, owsa_term_init_term, owsa_term_change_28, owsa_term_change_32, owsa_term_change_34, owsa_term_change_37, owsa_term_change_term, owsa_pt_nsi_i_dif, owsa_pt_i_dif_28, owsa_pt_i_dif_32, owsa_pt_i_dif_34, owsa_p_nsi_28, owsa_p_nsi_32, owsa_p_nsi_34, owsa_p_nsi_37, owsa_p_nsi_term, owsa_d_c, owsa_d_u) %>%
  # Add variable definitions and base case values
  left_join(df_params)

###
### STRATEGY WINNER GENERATION (costs, QALYs, ptb...)
###

library(dplyr)

# Costs 
owsa_costs <- owsa_df %>%
  # Find the "winning" strategy for a given variable value
  group_by(var_name, var_value) %>%
  summarize(win_strat = strategy[which.min(costs)], .groups = "drop") %>%
  ungroup() %>%
  # Create a standardized value to allow aggregate plotting
  group_by(var_name) %>%
  arrange(var_value) %>%
  # As many rows as the values_length. in the function
  # Subtracting 1 to get 0.00 for the first value
  mutate(value_num = row_number() - 1) %>% 
  # Proportions, for easier interpretation
  mutate(strd_value = value_num/max(value_num)) %>%
  ungroup() %>%
  left_join(df_params) %>%
  # Standardize the base value based on the existing standardization
  group_by(var_name) %>%
  mutate(base_strd = predict(lm(strd_value ~ var_value), 
                             newdata = data.frame(var_value = unique(base_value))))

# QALYs
owsa_QALYs <- owsa_df %>%
  # Find the "winning" strategy for a given variable value
  group_by(var_name, var_value) %>%
  summarize(win_strat = strategy[which.max(QALYs)], .groups = "drop") %>%
  ungroup() %>%
  # Create a standardized value to allow aggregate plotting
  group_by(var_name) %>%
  arrange(var_value) %>%
  # As many rows as the values_length. in the function
  # Subtracting 1 to get 0.00 for the first value
  mutate(value_num = row_number() - 1) %>% 
  # Proportions, for easier interpretation
  mutate(strd_value = value_num/max(value_num)) %>%
  ungroup() %>%
  left_join(df_params) %>%
  # Standardize the base value based on the existing standardization
  group_by(var_name) %>%
  mutate(base_strd = predict(lm(strd_value ~ var_value), 
                             newdata = data.frame(var_value = unique(base_value))))

# ptb28
owsa_ptb28 <- owsa_df %>%
  # Find the "winning" strategy for a given variable value
  group_by(var_name, var_value) %>%
  summarize(win_strat = strategy[which.min(ptb28)], .groups = "drop") %>%
  ungroup() %>%
  # Create a standardized value to allow aggregate plotting
  group_by(var_name) %>%
  arrange(var_value) %>%
  # As many rows as the values_length. in the function
  # Subtracting 1 to get 0.00 for the first value
  mutate(value_num = row_number() - 1) %>% 
  # Proportions, for easier interpretation
  mutate(strd_value = value_num/max(value_num)) %>%
  ungroup() %>%
  left_join(df_params) %>%
  # Standardize the base value based on the existing standardization
  group_by(var_name) %>%
  mutate(base_strd = predict(lm(strd_value ~ var_value), 
                             newdata = data.frame(var_value = unique(base_value))))

# ptb34
owsa_ptb34 <- owsa_df %>%
  # Find the "winning" strategy for a given variable value
  group_by(var_name, var_value) %>%
  summarize(win_strat = strategy[which.min(ptb34)], .groups = "drop") %>%
  ungroup() %>%
  # Create a standardized value to allow aggregate plotting
  group_by(var_name) %>%
  arrange(var_value) %>%
  # As many rows as the values_length. in the function
  # Subtracting 1 to get 0.00 for the first value
  mutate(value_num = row_number() - 1) %>% 
  # Proportions, for easier interpretation
  mutate(strd_value = value_num/max(value_num)) %>%
  ungroup() %>%
  left_join(df_params) %>%
  # Standardize the base value based on the existing standardization
  group_by(var_name) %>%
  mutate(base_strd = predict(lm(strd_value ~ var_value), 
                             newdata = data.frame(var_value = unique(base_value))))

###
### VISUALIZING THE RESULTS
###

library(ggplot2)

# Strategy_names and colours
strat_names <- c(s1 = "No screen", s2 = "One-step screen", s3 = "Two-step screen")
v_strat_colors <- c(s1 = "#F8766D", s2 = "#00BA38", s3 = "#619CFF")

## FACET WRAP (single outcome)
owsa_facet_costs <- owsa_df %>%
  ggplot(aes(x = var_value, y = costs, color = strategy)) +
  geom_line() +
  scale_color_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  geom_vline(data = df_params, aes(xintercept = base_value), alpha = 0.7, linetype = "dotted") +
  facet_wrap(~var_name, scales = "free") +
  labs(title = "OWSA: Costs",
       subtitle = "Dotted vertical lines = base case") +
  theme_bw()

owsa_facet_costs

owsa_facet_QALYs <- owsa_df %>%
  ggplot(aes(x = var_value, y = QALYs, color = strategy)) +
  geom_line() +
  scale_color_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  geom_vline(data = df_params, aes(xintercept = base_value), alpha = 0.7, linetype = "dotted") +
  facet_wrap(~var_name, scales = "free") +
  labs(title = "OWSA: QALYs",
       subtitle = "Dotted vertical lines = base case") +
  theme_bw()

owsa_facet_QALYs

## WINNING STRAT PLOTS

# Costs
owsa_costs_plot <- ggplot(data = owsa_costs, aes(x = strd_value, y = var_def, fill = win_strat)) +
  geom_tile(color = "black") +
  geom_point(aes(x = base_strd, y = var_def), shape = 3, color = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  labs(title = "(A) Costs",
       subtitle = "+ = base case value",
       y = "Model parameter",
       x = "Standardized parameter value") +
  theme_bw()

owsa_costs_plot

#ggsave(plot = owsa_costs_plot, filename = "owsa_costs.pdf", path = "Results/", width = 10, height = 13)

# QALYs
owsa_QALYs_plot <- ggplot(data = owsa_QALYs, aes(x = strd_value, y = var_def, fill = win_strat)) +
  geom_tile(color = "black") +
  geom_point(aes(x = base_strd, y = var_def), shape = 3, color = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  labs(title = "(B) QALYs",
       subtitle = "+ = base case value",
       y = "Model parameter",
       x = "Standardized parameter value") +
  theme_bw()

owsa_QALYs_plot

#ggsave(plot = owsa_QALYs_plot, filename = "owsa_QALYs.pdf", path = "Results/", width = 10, height = 13)

# ptb28
owsa_ptb28_plot <- ggplot(data = owsa_ptb28, aes(x = strd_value, y = var_def, fill = win_strat)) +
  geom_tile(color = "black") +
  geom_point(aes(x = base_strd, y = var_def), shape = 3, color = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  labs(title = "(C) Pre-term birth <= 28+0 weeks",
       subtitle = "+ = base case value",
       y = "Model parameter",
       x = "Standardized parameter value") +
  theme_bw()

owsa_ptb28_plot

#ggsave(plot = owsa_ptb28_plot, filename = "owsa_ptb28.pdf", path = "Results/", width = 10, height = 13)

# ptb34
owsa_ptb34_plot <- ggplot(data = owsa_ptb34, aes(x = strd_value, y = var_def, fill = win_strat)) +
  geom_tile(color = "black") +
  geom_point(aes(x = base_strd, y = var_def), shape = 3, color = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = v_strat_colors, name = "Dominant strategy", labels = strat_names) +
  labs(title = "(C) Pre-term birth <= 34+0 weeks",
       subtitle = "+ = base case value",
       y = "Model parameter",
       x = "Standardized parameter value") +
  theme_bw()

owsa_ptb34_plot

#ggsave(plot = owsa_ptb34_plot, filename = "owsa_ptb34.pdf", path = "Results/", width = 10, height = 13)

## END ##