##########################################################
### CLS MODEL IN ONE FUNCTION FOR SENSITIVITY ANALYSIS ###
##########################################################

## Defaults are set to base case

CLS_model <- function(twin_factor. = twin_factor,      # TREE PARAMETERS START
                      v_strats. = v_strats,
                      n_strats. = n_strats,
                      v_GA_cat. = v_GA_cat,
                      n_GA_cat. = n_GA_cat,
                      n_scans. = n_scans,
                      prog_start. = prog_start,
                      prog_stop.  = prog_stop,
                      wks. = wks,
                      p_cl25_t1. = p_cl25_t1,
                      p_cl15_t1. = p_cl15_t1,
                      p_cl25_t2. = p_cl25_t2,
                      p_cl15_t2. = p_cl15_t2,
                      p_clN_28_t2. = p_clN_28_t2,
                      p_clN_32_t2. = p_clN_32_t2,
                      p_clN_34_t2. = p_clN_34_t2,
                      p_clN_37_t2. = p_clN_37_t2,
                      p_cl25_28_t1. = p_cl25_28_t1,
                      p_cl25_32_t1. = p_cl25_32_t1,
                      p_cl25_34_t1. = p_cl25_34_t1,
                      p_cl25_37_t1. = p_cl25_37_t1,
                      p_cl25_28_t2. = p_cl25_28_t2,
                      p_cl25_32_t2. = p_cl25_32_t2,
                      p_cl25_34_t2. = p_cl25_34_t2,
                      p_cl25_37_t2. = p_cl25_37_t2,
                      p_cl15_28_t1. = p_cl15_28_t1,
                      p_cl15_32_t1. = p_cl15_32_t1,
                      p_cl15_34_t1. = p_cl15_34_t1,
                      p_cl15_37_t1. = p_cl15_37_t1,
                      p_cl15_28_t2. = p_cl15_28_t2,
                      p_cl15_32_t2. = p_cl15_32_t2,
                      p_cl15_34_t2. = p_cl15_34_t2,
                      p_cl15_37_t2. = p_cl15_37_t2,
                      rr_p_28. = rr_p_28,
                      rr_p_32. = rr_p_32,
                      rr_p_34. = rr_p_34,
                      rr_p_37. = rr_p_37,
                      rr_cp_28. = rr_cp_28,
                      rr_cp_32. = rr_cp_32,
                      rr_cp_34. = rr_cp_34,
                      rr_cp_37. = rr_cp_37,
                      rr_c_28. = rr_c_28,
                      rr_c_32. = rr_c_32,
                      rr_c_34. = rr_c_34,
                      rr_c_37. = rr_c_37,
                      p_prog. = p_prog,       
                      p_pY_cerc. = p_pY_cerc,
                      p_pN_cerc. = p_pN_cerc,
                      c_s. = c_s,
                      c_c. = c_c,
                      c_p. = c_p,
                      c_del_28. = c_del_28,
                      c_del_32. = c_del_32,
                      c_del_34. = c_del_34,
                      c_del_37. = c_del_37,
                      c_del_term. = c_del_term,
                      cycle_length. = cycle_length,         # MARKOV + NICU COSTS PARAMETERS START
                      n_age_init. = n_age_init,
                      n_age_max. = n_age_max,
                      v_names_states. = v_names_states,
                      n_states. = n_states,
                      p_sb_28. = p_sb_28,
                      p_sb_32. = p_sb_32,
                      p_sb_34. = p_sb_34,
                      p_sb_37. = p_sb_37,
                      p_sb_term. = p_sb_term,
                      v_r_mort. = v_r_mort, # static in the PSA
                      hr_AD_28_01. = hr_AD_28_01,
                      hr_AD_32_01. = hr_AD_32_01,
                      hr_AD_34_01. = hr_AD_34_01,
                      hr_AD_37_01. = hr_AD_37_01,
                      hr_AD_term_01. = hr_AD_term_01,
                      hr_AD_28_19. = hr_AD_28_19,
                      hr_AD_32_19. = hr_AD_32_19,
                      hr_AD_34_19. = hr_AD_34_19,
                      hr_AD_37_19. = hr_AD_37_19,
                      hr_AD_term_19. = hr_AD_term_19,
                      hr_AD_28_1019. = hr_AD_28_1019,
                      hr_AD_32_1019. = hr_AD_32_1019,
                      hr_AD_34_1019. = hr_AD_34_1019,
                      hr_AD_37_1019. = hr_AD_37_1019,
                      hr_AD_term_1019. = hr_AD_term_1019,
                      hr_AD_28_2029. = hr_AD_28_2029,
                      hr_AD_32_2029. = hr_AD_32_2029,
                      hr_AD_34_2029. = hr_AD_34_2029,
                      hr_AD_37_2029. = hr_AD_37_2029,
                      hr_AD_term_2029. = hr_AD_term_2029,
                      hr_AD_28_3045. = hr_AD_28_3045,
                      hr_AD_32_3045. = hr_AD_32_3045,
                      hr_AD_34_3045. = hr_AD_34_3045,
                      hr_AD_37_3045. = hr_AD_37_3045,
                      hr_AD_term_3045. = hr_AD_term_3045,
                      length_19. = length_19,
                      length_1019. = length_1019,
                      length_2029. = length_2029,
                      length_3045. = length_3045,
                      length_100. = length_100, 
                      hr_final. = hr_final,
                      p_adm_28. = p_adm_28,
                      p_adm_32. = p_adm_32,
                      p_adm_34. = p_adm_34,
                      p_adm_37. = p_adm_37,
                      p_adm_term. = p_adm_term,
                      c_n_28. = c_n_28,
                      c_n_32. = c_n_32,
                      c_n_34. = c_n_34,
                      c_n_37. = c_n_37,
                      c_n_term. = c_n_term,
                      in_28_0. = in_28_0, 
                      in_28_1. = in_28_1,
                      in_28_2. = in_28_2,
                      in_28_3. = in_28_3,
                      in_28_4. = in_28_4,
                      in_28_5. = in_28_5,
                      in_28_6. = in_28_6,
                      in_28_7. = in_28_7,
                      in_28_8. = in_28_8,
                      in_28_9. = in_28_9,
                      in_32_0. = in_32_0,
                      in_32_1. = in_32_1,
                      in_32_2. = in_32_2,
                      in_32_3. = in_32_3,
                      in_32_4. = in_32_4,
                      in_32_5. = in_32_5,
                      in_32_6. = in_32_6,
                      in_32_7. = in_32_7,
                      in_32_8. = in_32_8,
                      in_32_9. = in_32_9,
                      in_36_0. = in_36_0,
                      in_36_1. = in_36_1,
                      in_36_2. = in_36_2,
                      in_36_3. = in_36_3,
                      in_36_4. = in_36_4,
                      in_36_5. = in_36_5,
                      in_36_6. = in_36_6,
                      in_36_7. = in_36_7,
                      in_36_8. = in_36_8,
                      in_36_9. = in_36_9,
                      out_28_0. = out_28_0,
                      out_28_1. = out_28_1,
                      out_28_2. = out_28_2,
                      out_28_3. = out_28_3,
                      out_28_4. = out_28_4,
                      out_28_5. = out_28_5,
                      out_28_6. = out_28_6,
                      out_28_7. = out_28_7,
                      out_28_8. = out_28_8,
                      out_28_9. = out_28_9,
                      out_32_0. = out_32_0,
                      out_32_1. = out_32_1,
                      out_32_2. = out_32_2,
                      out_32_3. = out_32_3,
                      out_32_4. = out_32_4,
                      out_32_5. = out_32_5,
                      out_32_6. = out_32_6,
                      out_32_7. = out_32_7,
                      out_32_8. = out_32_8,
                      out_32_9. = out_32_9,
                      out_36_0. = out_36_0,
                      out_36_1. = out_36_1,
                      out_36_2. = out_36_2,
                      out_36_3. = out_36_3,
                      out_36_4. = out_36_4,
                      out_36_5. = out_36_5,
                      out_36_6. = out_36_6,
                      out_36_7. = out_36_7,
                      out_36_8. = out_36_8,
                      out_36_9. = out_36_9,
                      length_101. = length_101,
                      c_D. = c_D,
                      u_D. = u_D,
                      term_init_28.   = term_init,
                      term_init_32.   = term_init, 
                      term_init_34.   = term_init, 
                      term_init_37.   = term_init, 
                      term_init_term. = term_init, 
                      term_change_28.   = term_change,
                      term_change_32.   = term_change,
                      term_change_34.   = term_change,
                      term_change_37.   = term_change,
                      term_change_term. = term_change,
                      pt_nsi_i_dif. = pt_nsi_i_dif,
                      pt_i_dif_28. = pt_i_dif_28,
                      pt_i_dif_32. = pt_i_dif_32,
                      pt_i_dif_34. = pt_i_dif_34,
                      pt_i_dif_37. = pt_i_dif_37,
                      p_nsi_28. = p_nsi_28,
                      p_nsi_32. = p_nsi_32,
                      p_nsi_34. = p_nsi_34,
                      p_nsi_37. = p_nsi_37,
                      p_nsi_term. = p_nsi_term,
                      d_u. = d_u,
                      d_c. = d_c){
  
  ### MODEL START ###
  
  # Vectors, lists, and calculations defined in the setup paramaters 
  n_cycles   <- (n_age_max. - n_age_init.)/cycle_length.
  v_c_n_GA   <- c(c_n_28., c_n_32., c_n_34., c_n_37., c_n_term.)
  v_c_del_GA <- c(c_del_28., c_del_32., c_del_34., c_del_37., c_del_term.)
  v_c_wks    <- c_p. * wks.
  v_p_sb_GA  <- c(p_sb_28., p_sb_32., p_sb_34., p_sb_37., p_sb_term.)
  v_adm_GA   <- c(p_adm_28., p_adm_32., p_adm_34., p_adm_37., p_adm_term.)
  
  ### TREE GA OUTPUTS ###
  
  # s1
  s1_t2_branch <- CL_subtree(p_cl25. = p_cl25_t2.,
                             p_cl15. = p_cl15_t2.,
                             e_cp_28. = 1,      
                             e_cp_32. = 1,
                             e_cp_34. = 1,
                             e_cp_37. = 1,
                             e_p_28. = 1,
                             e_p_32. = 1,
                             e_p_34. = 1,
                             e_p_37. = 1,
                             e_c_28. = 1,
                             e_c_32. = 1,
                             e_c_34. = 1,
                             e_c_37. = 1,
                             p_prog. = p_prog.,       
                             p_pY_cerc. = p_pY_cerc.,
                             p_pN_cerc. = p_pN_cerc.,
                             p_cl15_28. = p_cl15_28_t2., 
                             p_cl15_32. = p_cl15_32_t2.,
                             p_cl15_34. = p_cl15_34_t2.,
                             p_cl15_37. = p_cl15_37_t2.,
                             p_cl25_28. = p_cl25_28_t2., 
                             p_cl25_32. = p_cl25_32_t2.,
                             p_cl25_34. = p_cl25_34_t2.,
                             p_cl25_37. = p_cl25_37_t2.,
                             GA_sub. = GA_subtree(p_28. = p_clN_28_t2.,
                                                  p_32. = p_clN_32_t2.,
                                                  p_34. = p_clN_34_t2.,
                                                  p_37. = p_clN_37_t2.))
  
  s1_tree <- CL_subtree(p_cl25. = p_cl25_t1.,
                        p_cl15. = p_cl15_t1.,
                        e_cp_28. = 1,      
                        e_cp_32. = 1,
                        e_cp_34. = 1,
                        e_cp_37. = 1,
                        e_p_28. = 1,
                        e_p_32. = 1,
                        e_p_34. = 1,
                        e_p_37. = 1,
                        e_c_28. = 1,
                        e_c_32. = 1,
                        e_c_34. = 1,
                        e_c_37. = 1,
                        p_prog. = p_prog.,       
                        p_pY_cerc. = p_pY_cerc.,
                        p_pN_cerc. = p_pN_cerc.,
                        p_cl15_28. = p_cl15_28_t1., 
                        p_cl15_32. = p_cl15_32_t1.,
                        p_cl15_34. = p_cl15_34_t1.,
                        p_cl15_37. = p_cl15_37_t1.,
                        p_cl25_28. = p_cl25_28_t1., 
                        p_cl25_32. = p_cl25_32_t1.,
                        p_cl25_34. = p_cl25_34_t1.,
                        p_cl25_37. = p_cl25_37_t1.,
                        GA_sub. = s1_t2_branch$all)
  
  s1_GA <- s1_tree$all 
  
  # s2
  s2_t2_branch <- CL_subtree(p_cl25. = p_cl25_t2.,
                             p_cl15. = p_cl15_t2.,
                             e_cp_28. = 1,      
                             e_cp_32. = 1,
                             e_cp_34. = 1,
                             e_cp_37. = 1,
                             e_p_28. = 1,
                             e_p_32. = 1,
                             e_p_34. = 1,
                             e_p_37. = 1,
                             e_c_28. = 1,
                             e_c_32. = 1,
                             e_c_34. = 1,
                             e_c_37. = 1,
                             p_prog. = p_prog.,       
                             p_pY_cerc. = p_pY_cerc.,
                             p_pN_cerc. = p_pN_cerc.,
                             p_cl15_28. = p_cl15_28_t2., 
                             p_cl15_32. = p_cl15_32_t2.,
                             p_cl15_34. = p_cl15_34_t2.,
                             p_cl15_37. = p_cl15_37_t2.,
                             p_cl25_28. = p_cl25_28_t2., 
                             p_cl25_32. = p_cl25_32_t2.,
                             p_cl25_34. = p_cl25_34_t2.,
                             p_cl25_37. = p_cl25_37_t2.,
                             GA_sub. = GA_subtree(p_28. = p_clN_28_t2.,
                                                  p_32. = p_clN_32_t2.,
                                                  p_34. = p_clN_34_t2.,
                                                  p_37. = p_clN_37_t2.))
  
  s2_tree <- CL_subtree(p_cl25. = p_cl25_t1.,
                        p_cl15. = p_cl15_t1.,
                        e_cp_28. = rr_cp_28., 
                        e_cp_32. = rr_cp_32., 
                        e_cp_34. = rr_cp_34.,
                        e_cp_37. = rr_cp_37.,
                        e_p_28. = rr_p_28.,
                        e_p_32. = rr_p_32.,
                        e_p_34. = rr_p_34.,
                        e_p_37. = rr_p_37.,
                        e_c_28. = rr_c_28.,
                        e_c_32. = rr_c_32.,
                        e_c_34. = rr_c_34.,
                        e_c_37. = rr_c_37.,
                        p_prog. = p_prog.,       
                        p_pY_cerc. = p_pY_cerc.,
                        p_pN_cerc. = p_pN_cerc.,
                        p_cl15_28. = p_cl15_28_t1., 
                        p_cl15_32. = p_cl15_32_t1.,
                        p_cl15_34. = p_cl15_34_t1.,
                        p_cl15_37. = p_cl15_37_t1.,
                        p_cl25_28. = p_cl25_28_t1., 
                        p_cl25_32. = p_cl25_32_t1.,
                        p_cl25_34. = p_cl25_34_t1.,
                        p_cl25_37. = p_cl25_37_t1.,
                        GA_sub. = s2_t2_branch$all)
  
  s2_GA <- s2_tree$all
  
  # s3
  s3_t2_branch <- CL_subtree(p_cl25. = p_cl25_t2.,
                             p_cl15. = p_cl15_t2.,
                             e_cp_28. = rr_cp_28., 
                             e_cp_32. = rr_cp_32., 
                             e_cp_34. = rr_cp_34.,
                             e_cp_37. = rr_cp_37.,
                             e_p_28. = rr_p_28.,
                             e_p_32. = rr_p_32.,
                             e_p_34. = rr_p_34.,
                             e_p_37. = rr_p_37.,
                             e_c_28. = rr_c_28.,
                             e_c_32. = rr_c_32.,
                             e_c_34. = rr_c_34.,
                             e_c_37. = rr_c_37.,
                             p_prog. = p_prog.,       
                             p_pY_cerc. = p_pY_cerc.,
                             p_pN_cerc. = p_pN_cerc.,
                             p_cl15_28. = p_cl15_28_t2., 
                             p_cl15_32. = p_cl15_32_t2.,
                             p_cl15_34. = p_cl15_34_t2.,
                             p_cl15_37. = p_cl15_37_t2.,
                             p_cl25_28. = p_cl25_28_t2., 
                             p_cl25_32. = p_cl25_32_t2.,
                             p_cl25_34. = p_cl25_34_t2.,
                             p_cl25_37. = p_cl25_37_t2.,
                             GA_sub. = GA_subtree(p_28. = p_clN_28_t2.,
                                                  p_32. = p_clN_32_t2.,
                                                  p_34. = p_clN_34_t2.,
                                                  p_37. = p_clN_37_t2.))
  
  s3_tree <- CL_subtree(p_cl25. = p_cl25_t1,
                        p_cl15. = p_cl15_t1.,
                        e_cp_28. = rr_cp_28., 
                        e_cp_32. = rr_cp_32., 
                        e_cp_34. = rr_cp_34.,
                        e_cp_37. = rr_cp_37.,
                        e_p_28. = rr_p_28.,
                        e_p_32. = rr_p_32.,
                        e_p_34. = rr_p_34.,
                        e_p_37. = rr_p_37.,
                        e_c_28. = rr_c_28.,
                        e_c_32. = rr_c_32.,
                        e_c_34. = rr_c_34.,
                        e_c_37. = rr_c_37.,
                        p_prog. = p_prog.,       
                        p_pY_cerc. = p_pY_cerc.,
                        p_pN_cerc. = p_pN_cerc.,
                        p_cl15_28. = p_cl15_28_t1., 
                        p_cl15_32. = p_cl15_32_t1.,
                        p_cl15_34. = p_cl15_34_t1.,
                        p_cl15_37. = p_cl15_37_t1.,
                        p_cl25_28. = p_cl25_28_t1., 
                        p_cl25_32. = p_cl25_32_t1.,
                        p_cl25_34. = p_cl25_34_t1.,
                        p_cl25_37. = p_cl25_37_t1.,
                        GA_sub. = s3_t2_branch$all)
  
  s3_GA <- s3_tree$all
  
  ### TREE-RELATED COSTS ###
  
  ## Ultrasound 
  s1_c_s <- 0
  s2_c_s <- (p_cl25_t1.) * (c_s. * n_scans.) + ((1 - (p_cl25_t1.)) * c_s. * 1) 
  s3_c_s <- (p_cl25_t1. + ((1 - p_cl25_t1.) * (p_cl25_t2.))) * (c_s. * n_scans.) + ((1 - (p_cl25_t1. + ((1 - p_cl25_t1.) * (p_cl25_t2.)))) * c_s. * 2)
  # Vector of ultrasound costs 
  v_c_s <- c(s1_c_s, s2_c_s, s3_c_s)
  
  ## Progesterone 
  s2_p <- s2_tree$p
  s3_p <- s3_tree$p + ((1 - p_cl25_t1.) * s3_t2_branch$p) 
  s1_c_p <- 0
  s2_c_p <- sum(s2_p * v_c_wks) 
  s3_c_p <- sum(s3_p * v_c_wks)
  # Vector of progesterone costs
  v_c_p  <- c(s1_c_p, s2_c_p, s3_c_p)
  
  ##  Cerclage
  s2_cerc <- s2_tree$c
  s3_cerc <- s3_tree$c + ((1 - p_cl25_t1.) * s3_t2_branch$c) 
  s1_c_c <- 0
  s2_c_c <- s2_cerc * c_c.
  s3_c_c <- s3_cerc * c_c.
  # Vector of cerclage costs
  v_c_cerc <- c(s1_c_c, s2_c_c, s3_c_c)
  
  ## Delivery
  s1_c_del <- sum(s1_GA * v_c_del_GA)
  s2_c_del <- sum(s2_GA * v_c_del_GA)
  s3_c_del <- sum(s3_GA * v_c_del_GA)
  # Vector of delivery costs by strategy
  v_c_del <- c(s1_c_del, s2_c_del, s3_c_del)
  
  ### NEONATAL ADMISSION COSTS ###
  
  v_c_n_s1 <- s1_GA * (1 - v_p_sb_GA) * v_adm_GA * v_c_n_GA 
  v_c_n_s2 <- s2_GA * (1 - v_p_sb_GA) * v_adm_GA * v_c_n_GA 
  v_c_n_s3 <- s3_GA * (1 - v_p_sb_GA) * v_adm_GA * v_c_n_GA
  s1_c_n <- sum(v_c_n_s1)
  s2_c_n <- sum(v_c_n_s2)
  s3_c_n <- sum(v_c_n_s3)
  # Vector of neonatal costs
  v_n_c <- c(s1_c_n, s2_c_n, s3_c_n)
  
  ## Tree-related and neonatal admission costs 
  v_tree_c <- v_c_s + v_c_p + v_c_cerc + v_c_del
  
  df_tree_c <- data.frame(row.names = v_strats.,
                          c_us = v_c_s, 
                          c_progest = v_c_p, 
                          c_cerc = v_c_cerc, 
                          c_del = v_c_del, 
                          c_preg_total = v_tree_c,
                          c_neon = v_n_c * twin_factor.)
  
  ### MARKOV MODEL ###
  
  ## Initial states, GA cat
  v_p_sb <- v_p_sb_GA     # stillborn 
  v_p_a  <- 1 - v_p_sb_GA # live-born
  m_init_GA <- cbind(A = v_p_a, D = v_p_sb)
  rownames(m_init_GA) <- v_GA_cat
  
  # Annual mortality rate calculations
  
  # Hazard ratio vectors 
  v_hr_AD_28 <- c(hr_AD_28_01., 
                  rep(hr_AD_28_19., length_19.), 
                  rep(hr_AD_28_1019., length_1019.),
                  rep(hr_AD_28_2029., length_2029.), 
                  rep(hr_AD_28_3045., length_3045.),
                  seq(from = hr_AD_28_3045., to = hr_final., length.out = length_100.))
  
  v_hr_AD_32 <- c(hr_AD_32_01., 
                  rep(hr_AD_32_19., length_19.), 
                  rep(hr_AD_32_1019., length_1019.),
                  rep(hr_AD_32_2029., length_2029.), 
                  rep(hr_AD_32_3045., length_3045.),
                  seq(from = hr_AD_32_3045., to = hr_final., length.out = length_100.))
  
  v_hr_AD_34 <- c(hr_AD_34_01., 
                  rep(hr_AD_34_19., length_19.), 
                  rep(hr_AD_34_1019., length_1019.),
                  rep(hr_AD_34_2029., length_2029.), 
                  rep(hr_AD_34_3045., length_3045.),
                  seq(from = hr_AD_34_3045., to = hr_final., length.out = length_100.))
  
  v_hr_AD_37 <- c(hr_AD_37_01., 
                  rep(hr_AD_37_19., length_19.), 
                  rep(hr_AD_37_1019., length_1019.),
                  rep(hr_AD_37_2029., length_2029.), 
                  rep(hr_AD_37_3045., length_3045.),
                  seq(from = hr_AD_37_3045., to = hr_final., length.out = length_100.))
  
  v_hr_AD_term <- c(hr_AD_term_01., 
                    rep(hr_AD_term_19., length_19.), 
                    rep(hr_AD_term_1019., length_1019.),
                    rep(hr_AD_term_2029., length_2029.), 
                    rep(hr_AD_term_3045., length_3045.),
                    seq(from = hr_AD_term_3045., to = hr_final., length.out = length_100.))
  
  # Baseline mortality rate * hazard ratio vectors
  v_r_AD28 <- v_r_mort. * v_hr_AD_28
  v_r_AD32 <- v_r_mort. * v_hr_AD_32
  v_r_AD34 <- v_r_mort. * v_hr_AD_34
  v_r_AD37 <- v_r_mort. * v_hr_AD_37
  v_r_ADterm <- v_r_mort. * v_hr_AD_term
  
  # Conversion to probabilities
  v_p_AD28 <- 1 - exp(-v_r_AD28 * cycle_length.)
  v_p_AD32 <- 1 - exp(-v_r_AD32 * cycle_length.)
  v_p_AD34 <- 1 - exp(-v_r_AD34 * cycle_length.)
  v_p_AD37 <- 1 - exp(-v_r_AD37 * cycle_length.)
  v_p_ADterm <- 1 - exp(-v_r_ADterm * cycle_length.)
  
  # Add values to a list
  l_p_AD <- list(v_p_AD28, v_p_AD32, v_p_AD34, v_p_AD37, v_p_ADterm)
  
  ## Transition probability array
  a_P_GA <- vector(mode = "list", n_GA_cat.)
  
  for (i in 1: n_GA_cat){
    a_P_GA[[i]] <- trans_prob(n_states.       = n_states.,
                              n_cycles.       = n_cycles,
                              v_names_states. = v_names_states.,
                              v_p_AD. = l_p_AD[[i]])
  }
  
  a_P_28   <- a_P_GA[[1]]
  a_P_32   <- a_P_GA[[2]]
  a_P_34   <- a_P_GA[[3]] 
  a_P_37   <- a_P_GA[[4]]
  a_P_term <- a_P_GA[[5]]
  
  ## Markov chain, GA cat
  l_m_M_GA <- vector(mode = "list", n_GA_cat.)

  for (i in 1: n_GA_cat.){
    l_m_M_GA[[i]] <- markov_chain(GA_num. = i,
                                  a_P. = a_P_GA[[i]],
                                  n_cycles.       = n_cycles,
                                  n_states.       = n_states.,
                                  v_names_states. = v_names_states.,
                                  m_init.         = m_init_GA)
  }

  m_M_28   <- l_m_M_GA[[1]]
  m_M_32   <- l_m_M_GA[[2]]
  m_M_34   <- l_m_M_GA[[3]] 
  m_M_37   <- l_m_M_GA[[4]]
  m_M_term <- l_m_M_GA[[5]]
  
  ### MARKOV COSTS ###
  
  # Synthesizing costs from Johnston data
  
  # Average value at age 10 - to be used for 10-100 for all categories
  in_10 <- c(mean = mean(in_28_9.[1], in_32_9.[1], in_36_9.[1]))
  
  # Average value at age 10 - to be used for 10-100 for all categories
  out_10 <- c(mean = mean(out_28_9.[1], out_32_9.[1], out_36_9.[1]))
  
  ## Using the individual costs by year, combine to form vectors for Markov model
  # Vector of inpatient costs by GA
  # Note that in the PSA, the designation of "[1]" is redundant because the distribution providers 1 number. But this is kept here for functionality outside of the PSA.
  v_c_in_28 <- c(in_28_0.[1], in_28_1.[1], in_28_2.[1], in_28_3.[1], in_28_4.[1], in_28_5.[1], in_28_6.[1], in_28_7.[1], in_28_8.[1], in_28_9.[1], 
                 rep(in_10[1], length_101.))
  
  v_c_in_32 <- c(in_32_0.[1], in_32_1.[1], in_32_2.[1], in_32_3.[1], in_32_4.[1], in_32_5.[1], in_32_6.[1], in_32_7.[1], in_32_8.[1], in_32_9.[1], 
                 rep(in_10[1], length_101.))
  
  v_c_in_34 <- c(in_36_0.[1], in_36_1.[1], in_36_2.[1], in_36_3.[1], in_36_4.[1], in_36_5.[1], in_36_6.[1], in_36_7.[1], in_36_8.[1], in_36_9.[1], 
                 rep(in_10[1], length_101.))
  
  v_c_in_37 <- c(in_36_0.[1], in_36_1.[1], in_36_2.[1], in_36_3.[1], in_36_4.[1], in_36_5.[1], in_36_6.[1], in_36_7.[1], in_36_8.[1], in_36_9.[1], 
                 rep(in_10[1], length_101.))
  
  v_c_in_term <- c(in_36_0.[1], in_36_1.[1], in_36_2.[1], in_36_3.[1], in_36_4.[1], in_36_5.[1], in_36_6.[1], in_36_7.[1], in_36_8.[1], in_36_9.[1], 
                   rep(in_10[1], length_101.))
  
  # Vector of outpatient costs by GA
  v_c_out_28 <- c(out_28_0.[1], out_28_1.[1], out_28_2.[1], out_28_3.[1], out_28_4.[1], out_28_5.[1], out_28_6.[1], out_28_7.[1], out_28_8.[1], out_28_9.[1], 
                  rep(in_10[1], length_101.))
  
  v_c_out_32 <- c(out_32_0.[1], out_32_1.[1], out_32_2.[1], out_32_3.[1], out_32_4.[1], out_32_5.[1], out_32_6.[1], out_32_7.[1], out_32_8.[1], out_32_9.[1], 
                  rep(in_10[1], length_101.))
  
  v_c_out_34 <- c(out_36_0.[1], out_36_1.[1], out_36_2.[1], out_36_3.[1], out_36_4.[1], out_36_5.[1], out_36_6.[1], out_36_7.[1], out_36_8.[1], out_36_9.[1], 
                  rep(in_10[1], length_101.))
  
  v_c_out_37 <- c(out_36_0.[1], out_36_1.[1], out_36_2.[1], out_36_3.[1], out_36_4.[1], out_36_5.[1], out_36_6.[1], out_36_7.[1], out_36_8.[1], out_36_9.[1], 
                  rep(in_10[1], length_101.))
  
  v_c_out_term <- c(out_36_0.[1], out_36_1.[1], out_36_2.[1], out_36_3.[1], out_36_4.[1], out_36_5.[1], out_36_6.[1], out_36_7.[1], out_36_8.[1], out_36_9.[1], 
                    rep(in_10[1], length_101.))
  
  # Vector of total costs by GA
  v_c_28 <- v_c_in_28 + v_c_out_28
  v_c_32 <- v_c_in_32 + v_c_out_32
  v_c_34 <- v_c_in_34 + v_c_out_34
  v_c_37 <- v_c_in_37 + v_c_out_37
  v_c_term <- v_c_in_term + v_c_out_term
  
  # Calculating Markov costs
  
  c_M_28   <- markov_costs(m_M. = m_M_28, 
                           v_c_A. = v_c_28,
                           n_cycles. = n_cycles,
                           cycle_length. = cycle_length.,
                           n_states. = n_states.,
                           v_names_states. = v_names_states.,
                           d_c. = d_c.,
                           c_D. = c_D.)$total
  c_M_32   <- markov_costs(m_M. = m_M_32, 
                           v_c_A. = v_c_32,
                           n_cycles. = n_cycles,
                           cycle_length. = cycle_length.,
                           n_states. = n_states.,
                           v_names_states. = v_names_states.,
                           d_c. = d_c.,
                           c_D. = c_D.)$total
  c_M_34   <- markov_costs(m_M. = m_M_34, 
                           v_c_A. = v_c_34,
                           n_cycles. = n_cycles,
                           cycle_length. = cycle_length.,
                           n_states. = n_states.,
                           v_names_states. = v_names_states.,
                           d_c. = d_c.,
                           c_D. = c_D.)$total
  c_M_37   <- markov_costs(m_M. = m_M_37, 
                           v_c_A. = v_c_37,
                           n_cycles. = n_cycles,
                           cycle_length. = cycle_length.,
                           n_states. = n_states.,
                           v_names_states. = v_names_states.,
                           d_c. = d_c.,
                           c_D. = c_D.)$total
  c_M_term <- markov_costs(m_M. = m_M_term, 
                           v_c_A. = v_c_term,
                           n_cycles. = n_cycles,
                           cycle_length. = cycle_length.,
                           n_states. = n_states.,
                           v_names_states. = v_names_states.,
                           d_c. = d_c.,
                           c_D. = c_D.)$total
  
  # Vector of costs by GA
  v_c_M_GA <- c(c_M_28, c_M_32, c_M_34, c_M_37, c_M_term)

  c_s1 <- sum(v_c_M_GA * s1_GA)
  c_s2 <- sum(v_c_M_GA * s2_GA)
  c_s3 <- sum(v_c_M_GA * s3_GA)
  # Vector of strategy costs
  v_M_costs <- c(s1 = c_s1, s2 = c_s2, s3 = c_s3)
  
  ## MARKOV QALYs ###
  
  # Calculation of age-dependent HUI3 values for each GA category
  v_q_28 <- hui3_calc(term_init. = term_init_28.,
                      term_change. = term_change_28.,
                      pt_i_dif. = pt_i_dif_28,
                      pt_nsi_i_dif. = pt_nsi_i_dif.,
                      p_nsi_term. = p_nsi_term.,
                      p_nsi_GA. = p_nsi_28.)
  
  v_q_32 <- hui3_calc(term_init. = term_init_32.,
                      term_change. = term_change_32.,
                      pt_i_dif. = pt_i_dif_32.,
                      pt_nsi_i_dif. = pt_nsi_i_dif.,
                      p_nsi_term. = p_nsi_term.,
                      p_nsi_GA. = p_nsi_32.)
  
  v_q_34 <- hui3_calc(term_init. = term_init_34.,
                      term_change. = term_change_34.,
                      pt_i_dif. = pt_i_dif_34.,
                      pt_nsi_i_dif. = pt_nsi_i_dif.,
                      p_nsi_term. = p_nsi_term.,
                      p_nsi_GA. = p_nsi_34.)
  
  v_q_37 <- hui3_calc(term_init. = term_init_37.,
                      term_change. = term_change_37.,
                      pt_i_dif. = pt_i_dif_37.,
                      pt_nsi_i_dif. = pt_nsi_i_dif.,
                      p_nsi_term. = p_nsi_term.,
                      p_nsi_GA. = p_nsi_37.)
  
  v_q_term <- hui3_calc(term_init. = term_init_term.,
                        term_change. = term_change_term.,
                        pt_i_dif. = 0,
                        pt_nsi_i_dif. = 0,
                        p_nsi_term. = p_nsi_term.,
                        p_nsi_GA. = 0)

  # Markov chain for QALYs
  q_M_28   <- markov_qaly(m_M. = m_M_28, 
                          v_q. = v_q_28,
                          n_cycles. = n_cycles,
                          cycle_length. = cycle_length.,
                          n_states. = n_states.,
                          v_names_states. = v_names_states.,
                          d_u. = d_u.,
                          u_D. = u_D.)$total
  
  q_M_32   <- markov_qaly(m_M. = m_M_32, 
                          v_q. = v_q_32,
                          n_cycles. = n_cycles,
                          cycle_length. = cycle_length.,
                          n_states. = n_states.,
                          v_names_states. = v_names_states.,
                          d_u. = d_u.,
                          u_D. = u_D.)$total
  
  q_M_34   <- markov_qaly(m_M. = m_M_34, 
                          v_q. = v_q_34,
                          n_cycles. = n_cycles,
                          cycle_length. = cycle_length.,
                          n_states. = n_states.,
                          v_names_states. = v_names_states.,
                          d_u. = d_u.,
                          u_D. = u_D.)$total
  
  q_M_37   <- markov_qaly(m_M. = m_M_37, 
                          v_q. = v_q_37,
                          n_cycles. = n_cycles,
                          cycle_length. = cycle_length.,
                          n_states. = n_states.,
                          v_names_states. = v_names_states.,
                          d_u. = d_u.,
                          u_D. = u_D.)$total
  
  q_M_term <- markov_qaly(m_M. = m_M_term, 
                          v_q. = v_q_term,
                          n_cycles. = n_cycles,
                          cycle_length. = cycle_length.,
                          n_states. = n_states.,
                          v_names_states. = v_names_states.,
                          d_u. = d_u.,
                          u_D. = u_D.)$total
  
  # Vector of QALYs by GA
  v_q_M_GA <- c(q_M_28, q_M_32, q_M_34, q_M_37, q_M_term)
  
  q_s1 <- sum(v_q_M_GA * s1_GA) * twin_factor.
  q_s2 <- sum(v_q_M_GA * s2_GA) * twin_factor.
  q_s3 <- sum(v_q_M_GA * s3_GA) * twin_factor.
  # Vector of strategy QALYs
  v_M_qaly <- c(s1 = q_s1, s2 = q_s2, s3 = q_s3)
  
  ### CLINICAL OUTCOMES ###
  # CL <= 15
  cl15_s1 <- 0
  cl15_s2 <- s2_tree$vscx 
  cl15_s3 <- s3_tree$vscx + ((1 - p_cl25_t1) * s3_t2_branch$vscx)
  v_cl15 <- c(cl15_s1, cl15_s2, cl15_s3)
  
  # CL <=25
  cl25_s1 <- 0
  cl25_s2 <- s2_tree$scx
  cl25_s3 <- s3_tree$scx + ((1 - p_cl25_t1) * s3_t2_branch$scx)
  v_cl25 <- c(cl25_s1, cl25_s2, cl25_s3) + v_cl15
  
  ## Patients on progesterone (see Tree-related costs)
  v_p <- c(0, sum(s2_p), sum(s3_p))
  
  ## Patients having cerclage (see Tree-related costs)
  v_cerc <- c(0, s2_cerc, s3_cerc)
  
  # TERM (> 37 weeks) probablilty
  n_term_s1 <- s1_GA[5]
  n_term_s2 <- s2_GA[5]
  n_term_s3 <- s3_GA[5]
  
  # Probability preterm deliveries by strategy
  v_n_term <- c(n_term_s1, n_term_s2, n_term_s3)
  
  ## PRE-TERM (< 37 weeks) probability
  n_pret37_s1 <- sum(s1_GA[-5])
  n_pret37_s2 <- sum(s2_GA[-5])
  n_pret37_s3 <- sum(s3_GA[-5])
  
  # Probability preterm deliveries by strategy
  v_n_pret37 <- c(n_pret37_s1, n_pret37_s2, n_pret37_s3)
  
  ## PRE-TERM (< 34 weeks) probability
  n_pret34_s1 <- sum(s1_GA[1:3])
  n_pret34_s2 <- sum(s2_GA[1:3])
  n_pret34_s3 <- sum(s3_GA[1:3])
  
  # Probability of preterm deliveries by strategy
  v_n_pret34 <- c(n_pret34_s1, n_pret34_s2, n_pret34_s3)
  
  ## PRE-TERM (< 32 weeks)
  # Number of preterm deliveries per n pregnancies
  n_pret32_s1 <- sum(s1_GA[1:2])
  n_pret32_s2 <- sum(s2_GA[1:2])
  n_pret32_s3 <- sum(s3_GA[1:2])
  
  # Number of preterm deliveries by strategy
  v_n_pret32 <- c(n_pret32_s1, n_pret32_s2, n_pret32_s3)
  
  ## EXTREME PRE-TERM (<28 weeks)
  n_pret28_s1 <- s1_GA[1]
  n_pret28_s2 <- s2_GA[1]
  n_pret28_s3 <- s3_GA[1]
  
  # Number of preterm deliveries by strategy
  v_n_pret28 <- c(n_pret28_s1, n_pret28_s2, n_pret28_s3)
  
  ## STILLBIRTHS
  # Number of stillbirths by strategy per n pregnancies
  n_sb_s1 <- sum(v_p_sb_GA * s1_GA) * twin_factor.
  n_sb_s2 <- sum(v_p_sb_GA * s2_GA) * twin_factor.
  n_sb_s3 <- sum(v_p_sb_GA * s3_GA) * twin_factor.
  
  # Number of stillbirths by strategy vs. s1
  v_n_sb <- c(n_sb_s1, n_sb_s2, n_sb_s3)
  
  ## LIFE-EXPECTANCY (single child)
  # Calculate LE for each GA category
  le_28 <- life_exp(m. = m_M_28)
  le_32 <- life_exp(m. = m_M_32)
  le_34 <- life_exp(m. = m_M_34)
  le_37 <- life_exp(m. = m_M_37)
  le_term <- life_exp(m. = m_M_term)
  v_le <- c(le_28, le_32, le_34, le_37, le_term)
  
  # Calculate LE for each strategy
  le_s1 <- sum(v_le * s1_GA) * twin_factor.
  le_s2 <- sum(v_le * s2_GA) * twin_factor.
  le_s3 <- sum(v_le * s3_GA) * twin_factor.
  v_le_strat <- c(le_s1, le_s2, le_s3)
  
  # CLINICAL OUTCOMES SUMMARY TABLE
  df_clin <- data.frame("strategy" = v_strats,
                       "CL15" = v_cl15,
                       "CL25" = v_cl25,
                       "cerclage" = v_cerc,
                       "progest" = v_p,
                       "term" = v_n_term,
                       "ptb37" = v_n_pret37,
                       "ptb34" = v_n_pret34,
                       "ptb32" = v_n_pret32,
                       "ptb28" = v_n_pret28, 
                       "stillbirths" = v_n_sb,
                       "LE" = v_le_strat) # For one twin
  
  ### CEA OUTPUT ###
  
  # Total costs
  v_total_costs <- (v_tree_c + (v_n_c * twin_factor.) + (v_M_costs * twin_factor.))
  
  # Results as dataframe
  df_CEA <- data.frame(strategy = v_strats.,
                       costs = v_total_costs,
                       QALYs = v_M_qaly)
  
  df_all <- cbind(df_CEA, df_clin[, -1], df_tree_c)
  
  rownames(df) <- NULL
  
  ### FUNCTION OUTPUT ###
  return(list(df_tree_c = df_tree_c,
              df_clin = df_clin,
              df_CEA = df_CEA,
              df_all = df_all))
}

# BASE CASE TEST
CLS_model()$df_all
