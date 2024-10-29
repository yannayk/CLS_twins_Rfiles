####################################
### TW0-WAY SENSITIVITY ANALYSIS ###
####################################

library(dplyr)
library(ggplot2)
library(rlang)
library(ggpubr)

# Strategy_names and colours
strat_names <- c(s1 = "No screen", s2 = "One-step screen", s3 = "Two-step screen")
v_strat_colors <- c(s1 = "#F8766D", s2 = "#00BA38", s3 = "#619CFF")

## TWO WAY SENSITIVITY ANALYSIS FUNCTION
# Define the two variables and their clinical ranges
# Outputs:
  # 1. Dataframe containing the model output for each parameter value combination
  # 2. Dataframes defining the "winning" strategies for a given outcome measure
  # 3. A twsa plot for visualizing the results

twsa_fun <- function(var_nameA.,
                     base_caseA.,
                     minA.,
                     maxA.,
                     var_nameB.,
                     base_caseB.,
                     minB.,
                     maxB.,
                     values_length. = 50,
                     v_strat_colors. = v_strat_colors,
                     strat_names. = strat_names) {
  
  # Add a "." to the variable name to serve as an argument for the CLS_model
  arg_nameA <- paste0(var_nameA., ".")
  arg_nameB <- paste0(var_nameB., ".")
  
  # Create ranges for the two variables
  range_A <- seq(from = minA., to = maxA., length.out = values_length.)
  range_B <- seq(from = minB., to = maxB., length.out = values_length.)
  
  # Combine the ranges into a dataframe of all combinations of values
  df_ranges <- expand.grid(range_A = range_A, range_B = range_B)
  
  # Rename columns based on the provided variable names (dynamically)
  df_ranges <- df_ranges %>%
    rename(!!sym(var_nameA.) := range_A,
           !!sym(var_nameB.) := range_B)
  
  # Create list providing names and values from df_ranges
  args_list <- lapply(1:nrow(df_ranges), function(i) setNames(as.list(df_ranges[i,]), names(df_ranges)))
  
  # Run the CLS_model for each list item in args_list
  # Extract only list value 4 (the df_all)
  model_out <- lapply(args_list, function(arg) {
    do.call(CLS_model, arg)[[4]]
  })
  
  # For each data frame in the list model_out, add parameter values for reference 
  # Column names are defined dynamically
  for(i in seq_along(model_out)) {
    model_out[[i]] <- model_out[[i]] %>%
      mutate(!!var_nameA. := as.numeric(rep(args_list[[i]][[1]], nrow(model_out[[i]]))),
             !!var_nameB. := as.numeric(rep(args_list[[i]][[2]], nrow(model_out[[i]]))))
  }
  
  # Combine the data frames from model_out into a single long df
  twsa_df <- bind_rows(model_out)
  
  # Determine the "winning" strategy for the main outcomes: costs, QALYs, ptb28, ptb34
  twsa_costs <- twsa_df %>%
    # Find the "winning" strategy for a given variable value
    group_by(across(all_of(c(var_nameA., var_nameB.)))) %>%
    summarize(win_strat = strategy[which.min(costs)], .groups = "drop") %>%
    ungroup()
  
  twsa_QALYs <- twsa_df %>%
    # Find the "winning" strategy for a given variable value
    group_by(across(all_of(c(var_nameA., var_nameB.)))) %>%
    summarize(win_strat = strategy[which.max(QALYs)], .groups = "drop") %>%
    ungroup()
  
  # Create plots: costs, QALYs, ptb28, ptb34
  
  plot_costs <- twsa_costs %>%
    ggplot(aes(x = .data[[var_nameA.]], y = .data[[var_nameB.]])) +
    geom_tile(aes(fill = win_strat), , color = "black") +
    geom_point(aes(x = base_caseA., y = base_caseB.), shape = 3, color = "white") +
    scale_fill_manual(values = v_strat_colors., name = "Dominant strategy", labels = strat_names.) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() + 
    theme(aspect.ratio = 1)
  
  plot_QALYs <- twsa_QALYs %>%
    ggplot(aes(x = .data[[var_nameA.]], y = .data[[var_nameB.]])) +
    geom_tile(aes(fill = win_strat), color = "black") +
    geom_point(aes(x = base_caseA., y = base_caseB.), shape = 3, color = "white") +
    scale_fill_manual(values = v_strat_colors., name = "Dominant strategy", labels = strat_names.) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() + 
    theme(aspect.ratio = 1)
    
  # Function output: dataframe of model results and twsa plot
  return(list(twsa_df = twsa_df,
              twsa_costs = twsa_costs, 
              twsa_QALYs = twsa_QALYs, 
              plot_costs = plot_costs,
              plot_QALYs = plot_QALYs
              )
         )
}

###
### RUNNING THE MODEL
###

# Progesterone effectiveness vs. progesterone cost

twsa_prog28 <- twsa_fun(var_nameA. = "rr_p_28", base_caseA. = rr_p_28, minA. = 0.24, maxA. = 1.15,
                        var_nameB. = "c_p",     base_caseB. = c_p,     minB. = 15 , maxB. = 25)$plot_costs

twsa_prog32 <- twsa_fun(var_nameA. = "rr_p_32", base_caseA. = rr_p_32, minA. = 0.34, maxA. = 1.47,
                        var_nameB. = "c_p",     base_caseB. = c_p,     minB. = 15, maxB. = 25)$plot_costs

twsa_prog34 <- twsa_fun(var_nameA. = "rr_p_34", base_caseA. = rr_p_34, minA. = 0.31, maxA. = 1.33,
                        var_nameB. = "c_p",     base_caseB. = c_p,     minB. = 15, maxB. = 25)$plot_costs

twsa_prog37 <- twsa_fun(var_nameA. = "rr_p_37", base_caseA. = rr_p_37, minA. = 0.72, maxA. = 1.43,
                        var_nameB. = "c_p",     base_caseB. = c_p,     minB. = 15, maxB. = 25)$plot_costs

twsa_prog <- ggarrange(twsa_prog28, twsa_prog32, twsa_prog34, twsa_prog37,
                       nrow = 1, common.legend = TRUE) %>%
  annotate_figure(bottom = text_grob("Progesterone effectiveness by GA category"),
                  left = text_grob("Cost of progesterone ($)", rot = 90))

twsa_prog

#ggsave(plot = twsa_prog, filename = "twsa_prog.pdf", path = "Results/", height = 3.5)

# Cerclage alone effectiveness vs. cerclage cost

twsa_cerc28 <- twsa_fun(var_nameA. = "rr_c_28", base_caseA. = rr_c_28, minA. = 0.26, maxA. = 1.16,
                        var_nameB. = "c_c",     base_caseB. = c_c,     minB. = 2400, maxB. = 4374)$plot_costs

twsa_cerc32 <- twsa_fun(var_nameA. = "rr_c_32", base_caseA. = rr_c_32, minA. = 0.38, maxA. = 1.6,
                        var_nameB. = "c_c",     base_caseB. = c_c,     minB. = 2400, maxB. = 4374)$plot_costs

twsa_cerc34 <- twsa_fun(var_nameA. = "rr_c_34", base_caseA. = rr_c_34, minA. = 0.11, maxA. = 0.84,
                        var_nameB. = "c_c",     base_caseB. = c_c,     minB. = 2400, maxB. = 4374)$plot_costs

twsa_cerc37 <- twsa_fun(var_nameA. = "rr_c_37", base_caseA. = rr_c_37, minA. = 0.6, maxA. = 1.67,
                        var_nameB. = "c_c",     base_caseB. = c_c,     minB. = 2400, maxB. = 4374)$plot_costs


twsa_cerc <- ggarrange(twsa_cerc28, twsa_cerc32, twsa_cerc34, twsa_cerc37,
                       nrow = 1, common.legend = TRUE) %>%
  annotate_figure(bottom = text_grob("Cerclage effectiveness by GA category"),
                  left = text_grob("Cost of cerclage ($)", rot = 90))

twsa_cerc

#ggsave(plot = twsa_cerc, filename = "twsa_cerc.pdf", path = "Results/", height = 3.5)

# Cerclage + progesterone effectiveness vs. cerclage cost

twsa_cercprog28 <- twsa_fun(var_nameA. = "rr_cp_28", base_caseA. = rr_cp_28, minA. = 0.26, maxA. = 1.21,
                            var_nameB. = "c_c",      base_caseB. = c_c,      minB. = 2400, maxB. = 4374)

View(twsa_cercprog28$twsa_costs)

twsa_cercprog32 <- twsa_fun(var_nameA. = "rr_cp_32", base_caseA. = rr_cp_32, minA. = 0.38, maxA. = 1.6,
                            var_nameB. = "c_c",      base_caseB. = c_c,      minB. = 2400, maxB. = 4374)$plot_costs

twsa_cercprog34 <- twsa_fun(var_nameA. = "rr_cp_34", base_caseA. = rr_cp_34, minA. = 0.11, maxA. = 0.84,
                            var_nameB. = "c_c",      base_caseB. = c_c,      minB. = 2400, maxB. = 4374)$plot_costs

twsa_cercprog37 <- twsa_fun(var_nameA. = "rr_cp_37", base_caseA. = rr_cp_37, minA. = 0.6, maxA. = 1.67,
                            var_nameB. = "c_c",      base_caseB. = c_c,      minB. = 2400, maxB. = 4374)$plot_costs

twsa_cercprog <- ggarrange(twsa_cercprog28$plot_costs, twsa_cercprog32, twsa_cercprog34, twsa_cercprog37,
                           nrow = 1, common.legend = TRUE) %>%
  annotate_figure(bottom = text_grob("Cerclage effectiveness by GA category"),
                  left = text_grob("Cost of cerclage ($)", rot = 90))

twsa_cercprog

#ggsave(plot = twsa_cercprog, filename = "twsa_cercprog.pdf", path = "Results/", height = 3.5)

## END ##