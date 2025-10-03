# Dependants of the various datXXX for FV
inputListFVProd <- c("seed", "n_lots", "size_lot", "unit_size", "beta_alpha", "beta_beta",
                     "prop_var_inter", "log_mean_c", "log_sd_c", "poisson")
inputListFVBlanch <- c(inputListFVProd, "temp_blanch", "time_blanch")
inputListFVPart <- c(inputListFVBlanch, "pcc", "n_equip","bccfv")
inputListFVTest <- c(inputListFVPart, "n_tested", "g_tested","m_tested","c_tested", "p_lot_tested", "se", "g_tested_enumeration")
inputListFVPort <- c(inputListFVTest, "serving_size_port", "b_port")
inputListFVDefrost <- c(inputListFVPort, "temp_defrost", "time_defrost","mpd","p_defrost")
inputListFVCook  <- c(inputListFVDefrost, "p_cooked", "min_cook","mode_cook","max_cook")
inputListFVRisk  <- c(inputListFVCook, "Model", "PopulationJEMRA","PopulationPouillot","PopulationEFSA",
                      "PopulationEFSALV","PopulationEFSAV","PopulationEFSAMV","PopulationFritsch")

# Dependants of the various datXXX for SF
inputListSFProd <- c("seed", "n_lots", "size_lot", "unit_size_prod", "beta_alpha", "beta_beta",
                     "prop_var_inter", "log_mean_c", "log_sd_c", "poisson")
inputListSFPrefill <- c(inputListSFProd, "mpd", "temp_min", "temp_mode", "temp_max", "time_min", "time_mode",
                        "time_max")
inputListSFFill <- c(inputListSFPrefill, "w_slices_f", "init_slicer_f")
inputListSFHold <- c(inputListSFFill, "temp_min_hold", "temp_mode_hold", "temp_max_hold", 
                     "time_min_hold", "time_mode_hold", "time_max_hold")

inputListSFBrineSalt <- c(inputListSFHold, "p_brine", "pcc_brine", "vol_inj_min", 
                          "vol_inj_mode", "vol_inj_max", "conc_brine_min","conc_brine_mode",
                          "conc_brine_max","pcc_smearing","n_surface")

inputListSFSmoke <- c(inputListSFBrineSalt, "r_brine_mean", "r_brine_sd", "r_drysalt_mean", "r_drysalt_sd")

inputListSFSlice <- c(inputListSFSmoke, "w_slices_s", "init_slicer_s")

inputListSFPack <- c(inputListSFSmoke, "slices_per_pack")

inputListSFRTE <- c(inputListSFPack,  "aw_min_SF", "aw_mode_SF","aw_max_SF",
                    "NaCl_min_SF","NaCl_mode_SF","NaCl_max_SF",
                    "P_min_SF", "P_mode_SF", "P_max_SF", 
                    "pH_min_SF", "pH_mode_SF", "pH_max_SF", 
                    "CO2equilibrium_min_SF", "CO2equilibrium_mode_SF",  "CO2equilibrium_max_SF",
                    "NIT_min_SF", "NIT_mode_SF", "NIT_max_SF", 
                    "aaWph_min_SF","aaWph_mode_SF", "aaWph_max_SF", 
                    "baWph_min_SF", "baWph_mode_SF", "baWph_max_SF", 
                    "caWph_min_SF", "caWph_mode_SF", "caWph_max_SF", 
                    "daWph_min_SF", "daWph_mode_SF", "daWph_max_SF", 
                    "laWph_min_SF", "laWph_mode_SF", "laWph_max_SF", 
                    "saWph_min_SF", "saWph_mode_SF", "saWph_max_SF")

inputListSFColdChain <- c(inputListSFRTE,  "temp_min_cc", "temp_mode_cc","temp_max_cc",
                          "time_min_cc","time_mode_cc","time_max_cc",
                          "Variability_cc", "cor_time_temp", 
                          "N0_LAB_min", "N0_LAB_mode", "N0_LAB_max", 
                          "MPD_LAB_min", "MPD_LAB_mode",  "MPD_LAB_max",
                          "MPD_Lm_min", "MPD_Lm_mode", "MPD_Lm_max")

inputListSFHome <- c(inputListSFColdChain, "temp_min_h", "temp_mode_h", "temp_max_h",
                     "time_min_h", "time_mode_h", "time_max_h", 
                     "Variability_h", "cor_time_temp_h")

inputListSFPort <- c(inputListSFHome, "serving_size", "b_port_sf")

inputListSFRisk <- c(inputListSFHome, "Model", "PopulationJEMRA","PopulationPouillot","PopulationEFSA",
                     "PopulationEFSALV","PopulationEFSAV","PopulationEFSAMV","PopulationFritsch")


# Dependants of the various datXXX for Cantaloupe

inputListCaProd <-  c("seed", "n_lots", "size_lot", "canta_weight", "p_soil", "f_manure",
                      "p_manure", "f_irrig_raining", "p_irrig_raining", "c_soil_log_min",
                      "c_soil_log_mode", "c_soil_log_max", "q_soil_min", "q_soil_mode",
                      "q_soil_max", "p_foil", "r_foil", "p_irrig",
                      "c_irrig_log_min", "c_irrig_log_max", "p_water_gain_min", "p_water_gain_max")

inputListCAHarvest <- c(inputListCaProd, "prob_cch", "n_plas")

inputListCABrush <- c(inputListCAHarvest, "log_dec_brush")

inputListCAStoring <-  c(inputListCABrush, "p_cooled", "time_sto")

inputListCAWashing <- c(inputListCAStoring, "prob_ccw", 
                        "log_water_min", "log_water_mode","log_water_max",
                        "p_water_gain", "b_water")

inputListCADicing <- c(inputListCAWashing, "canta_rind_free", "size_sublot")


inputListCAPartitioning <- c(inputListCADicing, "prob_cc_dice", 
                             "n_dicer", "b_canta","unit_size_dic")

inputListCATesting <- c(inputListCAPartitioning, "n_tested", 
                        "g_tested", "m_tested","c_tested", "p_lot_tested", "se", "g_tested_enum")

inputListCATransport <- c(inputListCATesting, 
                          "temp_min", "temp_mod", "temp_max",
                          "time_min", "time_mod", "time_max")

inputListCARTEStorage <- c(inputListCATransport, 
                           "temp_min_rte", "temp_mod_rte", "temp_max_rte",
                           "time_min_rte", "time_mod_rte", "time_max_rte")

inputListCATransportCon <- c(inputListCARTEStorage, 
                             "temp_min_con", "temp_mod_con", "temp_max_con",
                             "time_shape", "time_scale")

inputListCAStoredDices <- c(inputListCATransportCon, 
                            "temp_min_h", "temp_mode_h", "temp_max_h",
                            "time_min_h", "time_mode_h", "time_max_h")

inputListCARisk <- c(inputListCAStoredDices, 
                     "Model", "PopulationJEMRA","PopulationPouillot","PopulationEFSA",
                     "PopulationEFSALV","PopulationEFSAV","PopulationEFSAMV","PopulationFritsch")

