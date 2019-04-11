#V3.24U
#_data_and_control_files: GOAPcod2016_116e.dat // GOA16.6.1.ctl
#_SS-V3.24U-safe;_08/29/2014;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.2_Win64
1  #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
#_Cond 0  #  N recruitment designs goes here if N_GP*nseas*area>1
#_Cond 0  #  placeholder for recruitment interaction request
#_Cond 1 1 1  # example recruitment design element for GP=1, seas=1, area=1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
5 #_Nblock_Patterns
 2 4 1 1 1 #_blocks_per_pattern 
# begin and end years of blocks
 1996 2005 2006 2018
 1990 2004 2005 2006 2007 2016 2017 2018
 2013 2018
 2015 2016
 1976 1977
#
0.5 #_fracfemale 
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented
0.5 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
2 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
2 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP
#_placeholder for empirical age- or length- maturity by growth pattern (female only)
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
 0.1 1 0.4377930 -0.81 3 0.1 5 0 0 0 0 0 4 2 # NatM_p_1_Fem_GP_1
 0 50 4.76758 6.1252 -1 99 1 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 70 130 96.162 116.541 -1 0.01 1 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0 1 0.170434 0.1352 -1 99 1 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0 10 2.23217 0 -1 0 2 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0 20 9.36965 0 -1 0 2 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
 -99 99 5.63096e-006 0 -1 0 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem
 -99 99 3.1306 0 -1 0 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem
 -99 99 4.3499 0 -1 0 -1 0 0 0 0 0 0 0 # Mat50%_Fem
 -99 99 -1.9632 0 -1 0 -1 0 0 0 0 0 0 0 # Mat_slope_Fem
 -99 99 1 0 -1 0 -1 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -99 99 0 0 -1 0 -1 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
 -99 99 0 0 -1 0 -1 0 0 0 0 0 0 0 # RecrDist_GP_1
 -99 99 0 0 -1 0 -1 0 0 0 0 0 0 0 # RecrDist_Area_1
 -99 99 0 0 -1 0 -1 0 0 0 0 0 0 0 # RecrDist_Seas_1
 -99 99 1 0 -1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 0 99 -2 #_placeholder when no MG-environ parameters
#
1 #_Cond 0  #custom_MG-block_setup (0/1)
0.1 2 0.4377930 -0.81 3 0.1 1
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Cond -4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
 10 20 12.4331 0 -1 0 1 # SR_LN(R0)
 0 1 1 1 -1 0 -1 # SR_BH_steep
 0 10 0.44 0.44 -1 0 -4 # SR_sigmaR
 -10 10 0 0 -1 0 0 # SR_envlink
 -5 5 0 0 -1 0 3 # SR_R1_offset
 -99 99 0 0 -1 0 -1 # SR_autocorr
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
2 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1978 # first year of main recr_devs; early devs can preceed this era
2014 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
-16 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 2 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1964.4 #_last_early_yr_nobias_adj_in_MPD
 1979.7 #_first_yr_fullbias_adj_in_MPD
 2012.9 #_last_yr_fullbias_adj_in_MPD
 2017 #_first_recent_yr_nobias_adj_in_MPD
 0.9089 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -5 #min rec_dev
 5 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#DisplayOnly -0.0442715 # Early_InitAge_12
#DisplayOnly -0.0621243 # Early_InitAge_11
#DisplayOnly -0.0859569 # Early_InitAge_10
#DisplayOnly -0.116971 # Early_InitAge_9
#DisplayOnly -0.155889 # Early_InitAge_8
#DisplayOnly -0.201341 # Early_InitAge_7
#DisplayOnly -0.242393 # Early_InitAge_6
#DisplayOnly -0.261126 # Early_InitAge_5
#DisplayOnly -0.279027 # Early_InitAge_4
#DisplayOnly -0.394419 # Early_InitAge_3
#DisplayOnly -0.464298 # Early_InitAge_2
#DisplayOnly -0.274565 # Early_InitAge_1
#DisplayOnly -0.260691 # Early_RecrDev_1977
#DisplayOnly -0.489019 # Main_RecrDev_1978
#DisplayOnly -0.413522 # Main_RecrDev_1979
#DisplayOnly -0.219005 # Main_RecrDev_1980
#DisplayOnly -0.0213668 # Main_RecrDev_1981
#DisplayOnly 0.128388 # Main_RecrDev_1982
#DisplayOnly 0.110782 # Main_RecrDev_1983
#DisplayOnly 0.441535 # Main_RecrDev_1984
#DisplayOnly 0.499911 # Main_RecrDev_1985
#DisplayOnly 0.211893 # Main_RecrDev_1986
#DisplayOnly 0.412851 # Main_RecrDev_1987
#DisplayOnly 0.25967 # Main_RecrDev_1988
#DisplayOnly 0.484372 # Main_RecrDev_1989
#DisplayOnly 0.394515 # Main_RecrDev_1990
#DisplayOnly 0.108969 # Main_RecrDev_1991
#DisplayOnly 0.0296207 # Main_RecrDev_1992
#DisplayOnly -0.0315141 # Main_RecrDev_1993
#DisplayOnly 0.0764248 # Main_RecrDev_1994
#DisplayOnly 0.0995837 # Main_RecrDev_1995
#DisplayOnly -0.375317 # Main_RecrDev_1996
#DisplayOnly -0.341073 # Main_RecrDev_1997
#DisplayOnly -0.104493 # Main_RecrDev_1998
#DisplayOnly 0.186393 # Main_RecrDev_1999
#DisplayOnly 0.019572 # Main_RecrDev_2000
#DisplayOnly -0.448536 # Main_RecrDev_2001
#DisplayOnly -0.528348 # Main_RecrDev_2002
#DisplayOnly -0.505213 # Main_RecrDev_2003
#DisplayOnly -0.000184582 # Main_RecrDev_2004
#DisplayOnly 0.466282 # Main_RecrDev_2005
#DisplayOnly 0.501264 # Main_RecrDev_2006
#DisplayOnly 0.259704 # Main_RecrDev_2007
#DisplayOnly 0.441533 # Main_RecrDev_2008
#DisplayOnly -0.161696 # Main_RecrDev_2009
#DisplayOnly 0.131451 # Main_RecrDev_2010
#DisplayOnly 0.612327 # Main_RecrDev_2011
#DisplayOnly 0.865579 # Main_RecrDev_2012
#DisplayOnly 0.185403 # Main_RecrDev_2013
#DisplayOnly -0.2649 # Late_RecrDev_2014
#DisplayOnly -0.037132 # Late_RecrDev_2015
#DisplayOnly 0 # Late_RecrDev_2016
#DisplayOnly 0 # ForeRecr_2017
#DisplayOnly 0 # ForeRecr_2018
#DisplayOnly 0 # ForeRecr_2019
#DisplayOnly 0 # Impl_err_2017
#DisplayOnly 0 # Impl_err_2018
#DisplayOnly 0 # Impl_err_2019
#
#Fishing Mortality info 
0.3489 # F ballpark for annual F (=Z-M) for specified year
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
5 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
5  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 0.1 0.00282562 0 -1 0 1 # InitF_1FshTrawl
 0 0.1 0.0083726 0 -1 0 1 # InitF_2FshLL
 0 1 0 0 -1 0 -1 # InitF_3FshPot
#
#_Q_setup
 # Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
 0 0 0 0 # 1 FshTrawl
 0 0 0 0 # 2 FshLL
 0 0 0 0 # 3 FshPot
 0 0 0 2 # 4 Srv
 0 4 0 2 # 5 LLSrv
 0 0 0 0
 0 0 0 0
#
#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any);Qunits_are_ln(q)
# LO HI INIT PRIOR PR_type SD PHASE
 -10 10 0 0 -1 99 5 # LnQ_base_4_Srv
  -10 10 0 0 -1 99 5 # LnQ_base_4_Srv
   -10 10 0 0 -1 99 5 # LnQ_base_4_Srv
  
#
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
 24 0 0 0 # 1 FshTrawl
 24 0 0 0 # 2 FshLL
 24 0 0 0 # 3 FshPot
 24 0 0 0 # 4 Srv
 24 0 0 0 # 5 LLSrv
 15 0 0 4 # 5 LLSrv
 15 0 0 4 # 5 LLSrv
 
#
#_age_selex_types
#_Pattern ___ Male Special
 10 0 0 0 # 1 FshTrawl
 10 0 0 0 # 2 FshLL
 10 0 0 0 # 3 FshPot
 10 0 0 0 # 4 Srv
 10 0 0 0 # 5 LLSrv
 10 0 0 0 # 5 LLSrv
 10 0 0 0 # 5 LLSrv
 
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn

 10 90 59.3747 50 -1 0 1 0 1 1977 1989 0.2 2 2 # SizeSel_1P_1_FshTrawl
 -20 10 0.204107 0 -1 0 2 0 0 1977 1989 0.2 2 2 # SizeSel_1P_2_FshTrawl
 -10 10 5.12235 0 -1 0 2 0 1 1977 1989 0.2 2 2 # SizeSel_1P_3_FshTrawl
 0 10 0.852218 10 -1 0 2 0 1 1977 1989 0.2 2 2 # SizeSel_1P_4_FshTrawl
 -1000 2.71828 -999 -10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_1P_5_FshTrawl
 -10 10 10 10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_1P_6_FshTrawl
 10 90 67.0461 50 -1 0 1 0 1 1978 1989 0.2 2 2 # SizeSel_2P_1_FshLL
 -20 10 -5.05784 0 -1 0 2 0 0 1978 1989 0.2 2 2 # SizeSel_2P_2_FshLL
 -10 10 5.12967 0 -1 0 2 0 1 1978 1989 0.2 2 2 # SizeSel_2P_3_FshLL
 0 10 10 10 -1 0 -2 0 0 0 0 0 2 2 # SizeSel_2P_4_FshLL
 -1000 2.71828 -999 -10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_FshLL
 -10 10 10 10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_2P_6_FshLL
 10 90 72.1781 50 -1 0 1 0 0 0 0 0 3 2 # SizeSel_3P_1_FshPot
 -20 10 -11.3127 0 -1 0 2 0 0 0 0 0 3 2 # SizeSel_3P_2_FshPot
 -10 10 4.99455 0 -1 0 2 0 0 0 0 0 3 2 # SizeSel_3P_3_FshPot
 0 10 3.71421 10 -1 0 2 0 0 0 0 0 0 0 # SizeSel_3P_4_FshPot
 -1000 2.71828 -999 -10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_3P_5_FshPot
 -10 10 0.409467 10 -1 0 2 0 0 0 0 0 0 0 # SizeSel_3P_6_FshPot
 10 90 61.7001 50 -1 0 1 0 0 0 0 0 1 2 # SizeSel_4P_1_Srv
 -20 10 -10.9741 0 -1 0 2 0 0 0 0 0 1 2 # SizeSel_4P_2_Srv
 -10 10 5.64538 0 -1 0 2 0 0 0 0 0 1 2 # SizeSel_4P_3_Srv
 0 10 4.38648 10 -1 0 5 0 0 0 0 0 1 2 # SizeSel_4P_4_Srv
 -10 2.71828 -3.66472 -10 -1 0 2 0 0 0 0 0 1 2 # SizeSel_4P_5_Srv
 -10 10 -0.357061 10 -1 0 5 0 0 0 0 0 1 2 # SizeSel_4P_6_Srv
 10 90 66.4615 50 -1 0 1 0 0 0 0 0 0 0 # SizeSel_5P_1_LLSrv
 -20 10 -12.4358 0 -1 0 2 0 0 0 0 0 0 0 # SizeSel_5P_2_LLSrv
 -10 10 4.68044 0 -1 0 2 0 0 0 0 0 0 0 # SizeSel_5P_3_LLSrv
 0 10 4.62356 10 -1 0 2 0 0 0 0 0 0 0 # SizeSel_5P_4_LLSrv
 -1000 2.71828 -999 -10 -1 0 -2 0 0 0 0 0 0 0 # SizeSel_5P_5_LLSrv
 -10 10 -0.871109 10 -1 0 2 0 0 0 0 0 0 0 # SizeSel_5P_6_LLSrv
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
1 #_custom_sel-blk_setup (0/1) 
 10 90 86.699 50 -1 0 1 # SizeSel_1P_1_FshTrawl_BLK2repl_1990
 10 90 81.3462 50 -1 0 1 # SizeSel_1P_1_FshTrawl_BLK2repl_2013
 10 90 76.8778 50 -1 0 1 # SizeSel_1P_1_FshTrawl_BLK2repl_2017
 10 90 76.8778 50 -1 0 1 # SizeSel_1P_1_FshTrawl_BLK2repl_2017
 #10 90 76.8778 50 -1 0 1 # SizeSel_1P_1_FshTrawl_BLK2repl_2017

-20 10 -0.576415 0 -1 0 2 # SizeSel_1P_2_FshTrawl_BLK2repl_1990
 -20 10 -5.02371 0 -1 0 2 # SizeSel_1P_2_FshTrawl_BLK2repl_2013
 -20 10 -5.01959 0 -1 0 2 # SizeSel_1P_2_FshTrawl_BLK2repl_2017
 -20 10 -5.01959 0 -1 0 2 # SizeSel_1P_2_FshTrawl_BLK2repl_2017
 #-20 10 -5.01959 0 -1 0 2 # SizeSel_1P_2_FshTrawl_BLK2repl_2017

-10 10 6.25099 0 -1 0 2 # SizeSel_1P_3_FshTrawl_BLK2repl_1990
 -10 10 6.23033 0 -1 0 2 # SizeSel_1P_3_FshTrawl_BLK2repl_2013
 -10 10 5.65171 0 -1 0 2 # SizeSel_1P_3_FshTrawl_BLK2repl_2017
 -10 10 5.65171 0 -1 0 2 # SizeSel_1P_3_FshTrawl_BLK2repl_2017
 #-10 10 5.65171 0 -1 0 2 # SizeSel_1P_3_FshTrawl_BLK2repl_2017

0 10 5.18363 10 -1 0 2 # SizeSel_1P_4_FshTrawl_BLK2repl_1990
 0 10 4.97474 10 -1 0 2 # SizeSel_1P_4_FshTrawl_BLK2repl_2013
 0 10 4.95914 10 -1 0 2 # SizeSel_1P_4_FshTrawl_BLK2repl_2017
 #0 10 4.95914 10 -1 0 2 # SizeSel_1P_4_FshTrawl_BLK2repl_2017
 0 10 4.95914 10 -1 0 2 # SizeSel_1P_4_FshTrawl_BLK2repl_2017

10 90 72.9481 50 -1 0 1 # SizeSel_2P_1_FshLL_BLK2repl_1990
 10 90 70.1696 50 -1 0 1 # SizeSel_2P_1_FshLL_BLK2repl_2013
 10 90 69.1852 50 -1 0 1 # SizeSel_2P_1_FshLL_BLK2repl_2017
 10 90 69.1852 50 -1 0 1 # SizeSel_2P_1_FshLL_BLK2repl_2017
 #10 90 69.1852 50 -1 0 1 # SizeSel_2P_1_FshLL_BLK2repl_2017


-20 10 -0.570224 0 -1 0 2 # SizeSel_2P_2_FshLL_BLK2repl_1990
 -20 10 -5.01129 0 -1 0 2 # SizeSel_2P_2_FshLL_BLK2repl_2013
 -20 10 -4.99529 0 -1 0 2 # SizeSel_2P_2_FshLL_BLK2repl_2017
 -20 10 -4.99529 0 -1 0 2 # SizeSel_2P_2_FshLL_BLK2repl_2017
 #-20 10 -4.99529 0 -1 0 2 # SizeSel_2P_2_FshLL_BLK2repl_2017

-10 10 5.33624 0 -1 0 2 # SizeSel_2P_3_FshLL_BLK2repl_1990
 -10 10 5.21787 0 -1 0 2 # SizeSel_2P_3_FshLL_BLK2repl_2013
 -10 10 5.5199 0 -1 0 2 # SizeSel_2P_3_FshLL_BLK2repl_2017
  -10 10 5.5199 0 -1 0 2 # SizeSel_2P_3_FshLL_BLK2repl_2017
 # -10 10 5.5199 0 -1 0 2 # SizeSel_2P_3_FshLL_BLK2repl_2017

0 10 10 10 -1 0 -2 # SizeSel_2P_4_FshLL_BLK2repl_1990
 0 10 10 10 -1 0 -2 # SizeSel_2P_4_FshLL_BLK2repl_2013
 0 10 10 10 -1 0 -2 # SizeSel_2P_4_FshLL_BLK2repl_2017
 0 10 10 10 -1 0 -2 # SizeSel_2P_4_FshLL_BLK2repl_2017
# 0 10 10 10 -1 0 -2 # SizeSel_2P_4_FshLL_BLK2repl_2017

10 90 63.9424 50 -1 0 1 # SizeSel_3P_1_FshPot_BLK3repl_2013
 -20 10 -2.18352 0 -1 0 2 # SizeSel_3P_2_FshPot_BLK3repl_2013
 -10 10 4.66321 0 -1 0 2 # SizeSel_3P_3_FshPot_BLK3repl_2013

10 90 63.1194 50 -1 0 1 # SizeSel_4P_1_Srv_BLK1repl_1996
 10 90 52.2393 50 -1 0 1 # SizeSel_4P_1_Srv_BLK1repl_2006
 
 -20 10 -5.0067 0 -1 0 2 # SizeSel_4P_2_Srv_BLK1repl_1996
 -20 10 -3.00654 0 -1 0 2 # SizeSel_4P_2_Srv_BLK1repl_2006

-10 10 5.59976 0 -1 0 2 # SizeSel_4P_3_Srv_BLK1repl_1996
 -10 10 4.78607 0 -1 0 2 # SizeSel_4P_3_Srv_BLK1repl_2006
 0 10 4.21454 10 -1 0 5 # SizeSel_4P_4_Srv_BLK1repl_1996
 0 10 4.6981 10 -1 0 5 # SizeSel_4P_4_Srv_BLK1repl_2006
 -10 2.71828 -2.6735 -10 -1 0 2 # SizeSel_4P_5_Srv_BLK1repl_1996
 -10 2.71828 -2.33057 -10 -1 0 2 # SizeSel_4P_5_Srv_BLK1repl_2006
 0 10 10 10 -1 0 -5 # SizeSel_4P_6_Srv_BLK1repl_1996
 0 10 10 10 -1 0 -5 # SizeSel_4P_6_Srv_BLK1repl_2006

#_Cond No selex parm trends 
# -0.0122656 # SizeSel_1P_1_FshTrawl_DEVmult_1978
# -0.000925713 # SizeSel_1P_1_FshTrawl_DEVmult_1979
# -0.0153054 # SizeSel_1P_1_FshTrawl_DEVmult_1980
# -0.0200355 # SizeSel_1P_1_FshTrawl_DEVmult_1981
# -0.0213287 # SizeSel_1P_1_FshTrawl_DEVmult_1982
# 0.00658896 # SizeSel_1P_1_FshTrawl_DEVmult_1983
# -0.0235733 # SizeSel_1P_1_FshTrawl_DEVmult_1984
# 0.0592499 # SizeSel_1P_1_FshTrawl_DEVmult_1985
# -0.00599946 # SizeSel_1P_1_FshTrawl_DEVmult_1986
# 0.0215179 # SizeSel_1P_1_FshTrawl_DEVmult_1987
# 0.0320129 # SizeSel_1P_1_FshTrawl_DEVmult_1988
# 0.0327435 # SizeSel_1P_1_FshTrawl_DEVmult_1989
# 2.99137e-010 # SizeSel_1P_2_FshTrawl_DEVmult_1978
# 3.21583e-010 # SizeSel_1P_2_FshTrawl_DEVmult_1979
# 3.9239e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1980
# 8.14836e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1981
# -2.66118e-009 # SizeSel_1P_2_FshTrawl_DEVmult_1982
# -1.2264e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1983
# -1.69231e-007 # SizeSel_1P_2_FshTrawl_DEVmult_1984
# -9.3974e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1985
# -1.64413e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1986
# 3.23561e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1987
# -1.77067e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1988
# -1.1583e-008 # SizeSel_1P_2_FshTrawl_DEVmult_1989
# 0.00967119 # SizeSel_1P_3_FshTrawl_DEVmult_1978
# 0.000419343 # SizeSel_1P_3_FshTrawl_DEVmult_1979
# 0.00691903 # SizeSel_1P_3_FshTrawl_DEVmult_1980
# 0.00476752 # SizeSel_1P_3_FshTrawl_DEVmult_1981
# 0.017949 # SizeSel_1P_3_FshTrawl_DEVmult_1982
# -0.0170568 # SizeSel_1P_3_FshTrawl_DEVmult_1983
# -0.0371975 # SizeSel_1P_3_FshTrawl_DEVmult_1984
# -0.00538534 # SizeSel_1P_3_FshTrawl_DEVmult_1985
# 0.0192707 # SizeSel_1P_3_FshTrawl_DEVmult_1986
# -0.0105793 # SizeSel_1P_3_FshTrawl_DEVmult_1987
# -0.0176044 # SizeSel_1P_3_FshTrawl_DEVmult_1988
# 0.000362452 # SizeSel_1P_3_FshTrawl_DEVmult_1989
# -2.05155e-008 # SizeSel_1P_4_FshTrawl_DEVmult_1978
# -1.52153e-008 # SizeSel_1P_4_FshTrawl_DEVmult_1979
# -8.13462e-008 # SizeSel_1P_4_FshTrawl_DEVmult_1980
# -2.33958e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1981
# -1.97242e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1982
# 1.98516e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1983
# -1.14317e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1984
# -2.05063e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1985
# 1.43106e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1986
# 1.99654e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1987
# 2.7443e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1988
# 1.57649e-007 # SizeSel_1P_4_FshTrawl_DEVmult_1989
# -0.009179 # SizeSel_2P_1_FshLL_DEVmult_1979
# -0.0415133 # SizeSel_2P_1_FshLL_DEVmult_1980
# -0.0772128 # SizeSel_2P_1_FshLL_DEVmult_1981
# -0.0519743 # SizeSel_2P_1_FshLL_DEVmult_1982
# -0.0254415 # SizeSel_2P_1_FshLL_DEVmult_1983
# 0.0305534 # SizeSel_2P_1_FshLL_DEVmult_1984
# 0.105958 # SizeSel_2P_1_FshLL_DEVmult_1985
# 0.118346 # SizeSel_2P_1_FshLL_DEVmult_1986
# 0.0282133 # SizeSel_2P_1_FshLL_DEVmult_1987
# 0.0142708 # SizeSel_2P_1_FshLL_DEVmult_1988
# 0.0102042 # SizeSel_2P_1_FshLL_DEVmult_1989
# 1.72734e-008 # SizeSel_2P_2_FshLL_DEVmult_1979
# -1.42177e-006 # SizeSel_2P_2_FshLL_DEVmult_1980
# 9.95657e-006 # SizeSel_2P_2_FshLL_DEVmult_1981
# 9.97257e-006 # SizeSel_2P_2_FshLL_DEVmult_1982
# 2.00438e-006 # SizeSel_2P_2_FshLL_DEVmult_1983
# -4.42911e-006 # SizeSel_2P_2_FshLL_DEVmult_1984
# -6.42492e-006 # SizeSel_2P_2_FshLL_DEVmult_1985
# -5.54239e-006 # SizeSel_2P_2_FshLL_DEVmult_1986
# -1.86114e-006 # SizeSel_2P_2_FshLL_DEVmult_1987
# -1.04874e-006 # SizeSel_2P_2_FshLL_DEVmult_1988
# -9.59243e-007 # SizeSel_2P_2_FshLL_DEVmult_1989
# 0.018198 # SizeSel_2P_3_FshLL_DEVmult_1979
# 0.0369881 # SizeSel_2P_3_FshLL_DEVmult_1980
# -0.0212028 # SizeSel_2P_3_FshLL_DEVmult_1981
# -0.0150739 # SizeSel_2P_3_FshLL_DEVmult_1982
# -0.0256157 # SizeSel_2P_3_FshLL_DEVmult_1983
# -0.00819693 # SizeSel_2P_3_FshLL_DEVmult_1984
# 0.0113417 # SizeSel_2P_3_FshLL_DEVmult_1985
# -0.00366508 # SizeSel_2P_3_FshLL_DEVmult_1986
# -0.0118405 # SizeSel_2P_3_FshLL_DEVmult_1987
# -0.0108623 # SizeSel_2P_3_FshLL_DEVmult_1988
# -0.0028436 # SizeSel_2P_3_FshLL_DEVmult_1989
# -0.00298108 # SizeSel_2P_4_FshLL_DEVmult_1979
# 0.00300771 # SizeSel_2P_4_FshLL_DEVmult_1980
# -0.0123917 # SizeSel_2P_4_FshLL_DEVmult_1981
# -0.0151222 # SizeSel_2P_4_FshLL_DEVmult_1982
# -0.00463731 # SizeSel_2P_4_FshLL_DEVmult_1983
# 0.0108451 # SizeSel_2P_4_FshLL_DEVmult_1984
# 0.0139542 # SizeSel_2P_4_FshLL_DEVmult_1985
# 0.0150145 # SizeSel_2P_4_FshLL_DEVmult_1986
# 0.00446733 # SizeSel_2P_4_FshLL_DEVmult_1987
# 0.00296544 # SizeSel_2P_4_FshLL_DEVmult_1988
# 0.00305757 # SizeSel_2P_4_FshLL_DEVmult_1989
# 4.21145e-005 # SizeSel_2P_6_FshLL_DEVmult_1979
# 1.04578e-006 # SizeSel_2P_6_FshLL_DEVmult_1980
# 3.36101e-005 # SizeSel_2P_6_FshLL_DEVmult_1981
# 9.15794e-005 # SizeSel_2P_6_FshLL_DEVmult_1982
# 2.34492e-005 # SizeSel_2P_6_FshLL_DEVmult_1983
# -6.40486e-005 # SizeSel_2P_6_FshLL_DEVmult_1984
# -7.91931e-005 # SizeSel_2P_6_FshLL_DEVmult_1985
# -9.4609e-005 # SizeSel_2P_6_FshLL_DEVmult_1986
# -3.29942e-005 # SizeSel_2P_6_FshLL_DEVmult_1987
# -2.24007e-005 # SizeSel_2P_6_FshLL_DEVmult_1988
# -2.43669e-005 # SizeSel_2P_6_FshLL_DEVmult_1989
3 #_selparmdev-phase
1 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 4 5 6 7
  0 0 0 0 0 0 0 #_add_to_survey_CV
  0 0 0 0 0 0 0 #_add_to_discard_stddev
  0 0 0 0 0 0 0 #_add_to_bodywt_CV
  1 1 1 1 1 1 1 #_mult_by_lencomp_N
  1 1 1 1 1 1 1 #_mult_by_agecomp_N
  1 1 1 1 1 1 1 #_mult_by_size-at-age_N
#
1 #_maxlambdaphase
1 #_sd_offset
#
0 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet/survey  phase  value  sizefreq_method
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  1 #_CPUE/survey:_4
#  1 #_CPUE/survey:_5
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  1 #_lencomp:_5
#  0 #_agecomp:_1
#  0 #_agecomp:_2
#  0 #_agecomp:_3
#  1 #_agecomp:_4
#  0 #_agecomp:_5
#  1 #_init_equ_catch
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

