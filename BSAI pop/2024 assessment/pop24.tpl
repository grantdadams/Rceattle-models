///////////////////////////////////////////////////////////////////
// BSAI POP Model  
//	Has asymptotic fishery selectivity curve
//   
//
// Naming Conventions:
//
//  GENERAL:
//    styr, endyr begining year and ending year of model (catch data available)
//    nages       number of age groups considered
//    nyrs_       number of observations available to specific data set
//
//  DATA SPECIFIC:
//    catch_bio   Observed fishery catch biomass
//    nyrs_srv2   Number of years of slope survey index value (annual)
//    obs_srv2    Observed trawl slope survey index value (annual)
//    nyrs_srv3   Number of years of AI trawl survey index value (annual)
//    obs_srv3   Observed trawl AI trawl survey index value (annual)
//
//    oac_fish    Observed age comp from the fishery
//    oac_srv2 	  Observed age comp from the trawl survey
//
//    pred_catch  Predicted catch from the fishery
//    pred_srv2   Predicted index of the trawl survey
//
//    eac_fish    Expected age comp from fishery data
//    eac_srv2	  Expected age comp from trawl survey.
//
//    sel_fish    selectivity for fishery                
//    sel_srv2    selectivity for the trawl survey
//
//////////////////////////////////////////////////////////////////////////////
DATA_SECTION //------------------------------------------------------------------------------------------------
  init_int styr				// start year for model
  init_int styr_fish			// start year for the fishery
  init_int endyr			// end year for model and fishery
  init_int yrs_r      // number of years to peel for retrospective run
  init_int nages      // number of ages modeled
  init_int nages_dat // number of ages in data, can be less than the modeled ages
  init_ivector ages(1,nages)    // vector of ages (for model)
  init_ivector ages_dat(1,nages_dat)  // vector of ages (for data) 
  init_int nselages			// number of ages with estimated selectivities
  init_int rec_age			// recruitment age
  init_int nlen				// number of length groups
  init_ivector lengths(1,nlen) 		// vector of lengths
  init_int nyrs_fish			// number of years in the fishery
  init_ivector yrs_fish(1,nyrs_fish)    // vector of the index years in the fishery
  init_vector catch_bio(styr_fish,endyr)     // observed catch biomass
  init_int nsrv                                          // number of surveys
  init_adstring srvnameread;                             // string with names of surveys, separated by "%"
  init_ivector nyrs_srv_bio(1,nsrv)                      // vector of number of years for survey biomass esitmates 
  init_imatrix yrs_srv_bio(1,nsrv,1,nyrs_srv_bio)        // years of the biomass estimates for the survey(s) (ragged array)
  init_matrix obs_srv_bio(1,nsrv,1,nyrs_srv_bio)         // observed survey biomass estimates (ragged array)
  init_matrix obs_srv_bio_sd(1,nsrv,1,nyrs_srv_bio)      // observed survey biomass standard deviations (ragged array)
  init_matrix obs_srv_bio_lower(1,nsrv,1,nyrs_srv_bio)   // survey biomass -2SD(ragged array)
  init_matrix obs_srv_bio_upper(1,nsrv,1,nyrs_srv_bio)   // survey biomass +2SD (ragged array)

  init_ivector nyrs_srv_abun(1,nsrv)                      // vector of number of years for survey abundance estimates
  init_imatrix yrs_srv_abun(1,nsrv,1,nyrs_srv_abun)        // years of the abundance estimates for the survey(s) (ragged array)
  init_matrix obs_srv_abun(1,nsrv,1,nyrs_srv_abun)         // observed survey abundance estimates (ragged array)
  init_matrix obs_srv_abun_sd(1,nsrv,1,nyrs_srv_abun)      // observed survey abundance standard deviations (ragged array)
  init_matrix obs_srv_abun_lower(1,nsrv,1,nyrs_srv_abun)   // survey abundance -2SD(ragged array)
  init_matrix obs_srv_abun_upper(1,nsrv,1,nyrs_srv_abun)   // survey abundance +2SD (ragged array)
  
  init_matrix unbiasedages(1,nages,1,nages) // transition age error matrix for unbiased ages
  init_matrix translen(1,nlen,1,nages)     // transition matrix from ages to lengths
  init_int nyrs_fish_unbiased_ac      // number of years with  unbiased fishery age comps
  init_ivector yrs_fish_unbiased_ac(1,nyrs_fish_unbiased_ac)  // vector of index years with unbiased fishery age comps
  init_matrix oac_fish_unbiased(1,nyrs_fish_unbiased_ac,1,nages_dat)  // observed unbiased fishery age comps
  init_int nyrs_fish_lc         // number of years with fishery length comps
  init_ivector yrs_fish_lc(1,nyrs_fish_lc)    // vector of index years with fishery length comps
  init_matrix olc_fish(1,nyrs_fish_lc,1,nlen)     // observed fishery length comps
  
  init_ivector nyrs_srv_ac(1,nsrv)                           // vector of number of years with age comps, per survey
  init_imatrix yrs_srv_ac(1,nsrv,1,nyrs_srv_ac)              // years with age comp estimates from survey(s) (ragged array)
  init_3darray obs_srv_ac(1,nsrv,1,nyrs_srv_ac,1,nages_dat)  // observed age comps from the survey(s) (ragged array)
  init_ivector nyrs_srv_lc(1,nsrv)                           // vector of number of years with length comps, per survey
  init_imatrix yrs_srv_lc(1,nsrv,1,nyrs_srv_lc)              // years with length comp estimates from survey(s) (ragged array)
  init_3darray obs_srv_lc(1,nsrv,1,nyrs_srv_lc,1,nlen)       // observed length comps from the survey(s) (ragged array)
  !! cout << obs_srv_lc << endl;

  init_vector wt_pop(1,nages)   // population weight at age
  init_vector wt_fsh(1,nages)   // fishery weights at age
  vector wt_pop_bin(1,nages_dat)    // rescale weights to match the data plus group
  vector wt_fsh_bin(1,nages_dat)    // rescale weights to match the data plus group
  init_number spawn_mo          // spawning month
  init_int fyear_ac_option	// first year age comp option
  init_number historic_catch	// historic catch level
  init_number M_mult_eq_styr  // M multiplier for startyear equilibrium numbers at age      
  init_int sr_type		// option for S-R curve (avg or B-H)
  init_int fixedrec_yrs		// the number of years from the endyr in which we fix the recruitments
  init_number sigr 		// the sd of the recruitments 
  //init_ivector switch_prior_q(1,nsrv)  // switch for use of the prior on q (1= yes, otherwise no) 
  //init_vector priormean_q(1,nsrv)	    // prior mean of trawl survey q
  //init_vector priorcv_q(1,nsrv) 	// prior cv of trawl survey q   
  //init_vector prop_bio_ai(styr,endyr)  // the proportion of survey biomass in the AI survey area  
  
  //init_matrix prop_srv(1,nsrv,styr,endyr)   // the proportion of biomass covered by each of the surveys
  //!! cout << prop_srv << endl;
  
  
  
  
  // +++++  Fishery selectivity ++++++
  init_int    fsh_sel_option  // option for selectivity functional form (1=logistic, 2=double logistic, 3= bicubic spline)
  init_int    fsh_sel_styr    // the start year for fitting selecitvity
  int         fsh_sel_endyr   // end year for estimating selectivity
  init_int    fsh_sel_fixedyrs    //the number of years from the endyr_r in which we fix the recruitments
  init_int    nbins_fsh_sel       // the number of bins for fishery selectivity
  init_ivector binstart_fsh_sel(1,nbins_fsh_sel)  // the start year for each bin
  init_number fsh_sigma_aslp    // sigma for selectivity acsending slope deviations
  init_number fsh_sigma_a50     // sigma for selectivity a50 deviations
  init_number fsh_sigma_dslp    // sigma for selectivity decsending slope deviations
  init_number fsh_sigma_d50     // sigma for selectivity d50 deviations
  init_int    fsh_n_yr_nodes      // number of year nodes for bicubic spline selectivity
  init_int    fsh_n_age_nodes     // number of age nodes for bicubic spline selectivity
  int         fsh_isel_npar       // generalized number of nodes for sel_par
  int         fsh_jsel_npar       // generalized number of nodes for sep_par

  // +++++  Survey selectivity ++++++
  init_ivector  srv_sel_option(1,nsrv)      // option for survey selectivity (vector, by survey)
  init_int      srv_sel_styr        // the start year for fitting survey selecitvity (start of model)
  int           srv_sel_endyr       // end year for estimating survey selectivity
  init_int      srv_sel_fixedyrs    // the number of years from the endyr_r in which we fix the survey selectivities
  init_ivector nbins_srv_sel(1,nsrv)       // the number of bins for survey selectivity
  init_imatrix binstart_srv_sel(1,nsrv,1,nbins_srv_sel)  // the start year for each survey selectivity bin (ragged array). 
  init_vector srv_sigma_aslp(1,nsrv)    // sigma for survey logistic selectivity slope deviations
  init_vector srv_sigma_a50(1,nsrv)     // sigma for survey logistic selectivity a50 deviations
  init_vector srv_sigma_mu(1,nsrv)      // sigma for survey double normal selectivity mu deviations
  init_vector srv_sigma_dist(1,nsrv)    // sigma for survey double normal selectivity dist deviations
  init_vector srv_sigma_sig1(1,nsrv)    // sigma for survey double normal selectivity sig1 deviations
  init_vector srv_sigma_sig2(1,nsrv)    // sigma for survey double normal selectivity sig2 deviations

  init_ivector    srv_n_yr_nodes(1,nsrv)      // number of year nodes for survey bicubic spline selectivity, by survey
  init_ivector    srv_n_age_nodes(1,nsrv)     // number of age nodes for bicubic spline survey selectivity, by survey
  ivector         srv_isel_npar(1,nsrv)       // generalized number of nodes for survey sel_par
  ivector         srv_jsel_npar(1,nsrv)       // generalized number of nodes for survey sep_par
  
  //+++++++  things for time-varying survey catchability
  init_ivector nbins_q(1,nsrv)  // number of bins, by survey
  init_imatrix binstart_q(1,nsrv,1,nbins_q)  // start year of the bins, by survey (ragged array). first bin start with first year of model
  init_ivector switch_prior_q(1,nsrv)
  //init_int     switch_prop_srv               // switch for using prop_srv (don't use of no priors on any survey q)
  init_matrix  priormean_q(1,nsrv,1,nbins_q) // prior mean of q   
  init_matrix  priorcv_q(1,nsrv,1,nbins_q) // prior mean of q  
  

  // +++++ Natural mortality, time-varying control parameters, and prior mean and CV
  init_int    nbins_M       // the number of bins for fishery selectivity
  init_ivector binstart_M(1,nbins_M)  // the start year for each bin
  init_int     switch_prior_M         // switch for use of the prior on q (1= yes, otherwise no) 
  init_vector priormean_M(1,nbins_M) // prior mean of M
  init_vector priorcv_M(1,nbins_M)   // prior cv of M
  init_int     switch_M_devs_pen         // switch for use of the prior on q (1= yes, otherwise no)
  init_number sigma_M_devs           // sigma for the M_devs penalty, if prior is not used

  // ++++++  bins for mean_log_rec    ++++++++
  init_int   nbins_mean_log_rec
  init_ivector binstart_mean_log_rec(1,nbins_mean_log_rec)  // the start year for each bin


  //  maturity data
  init_number nages_mat_ogive           // number of ages for maturity ogive
  init_vector matages_ogive(1,nages_mat_ogive) // maturity ages for ogive
  

  init_int  mat_input_switch  // switch for reading in maturity vector
  init_matrix mat_input(1,mat_input_switch,1,nages) // read in matutity curve if not estimated    

  init_int    nmat_datasets       // number of maturity datasets
  init_ivector nages_mat(1,nmat_datasets)  // number of maturity ages for each data set 
  init_imatrix ages_mat(1,nmat_datasets,1,nages_mat) // maturity ages for each data set
  init_imatrix n_mat(1,nmat_datasets,1,nages_mat)  // fish measured for for each data set, by age
  init_imatrix y_mat(1,nmat_datasets,1,nages_mat)  // mature fish for for each data set, by age  
  init_vector mat_lambda(1,nages_mat_ogive)  // weights for fitting the maturity data 
  !! cout << mat_lambda << endl; 


  // read in previous assessment biomass for comparison plot
  //init_vector biomass2018(1960,2018)
  //!! cout << biomass2018 << endl; 

  //int    nbins                  // number of bins for fishery selectivity parameters
  //int    tmpnbins               // round the number of bins
  //!! cout << " olc_fish is " << olc_fish << endl;
   

  
  number spmo_frac		// spawning month as fraction of the year
  int styr_rec			// start year for recruits
  int styr_rec_dev  // start year for which we have recruitment deviations (some may be fixed)
  int lastyr_rec		// last year for which we estimate the recruitment
  int i                         // index for years 
  int j				// index for ages
  int k
  int s             // index for surveys
  int l
  int f
  int bincount       // for looping through the number of bins                         
  int styr_fut			// Start year for projections
  int endyr_fut                 // End year for projections
  int num_proj_Fs 		// Number of Fs to evaluate in the future
  
  matrix cv_bio_srv(1,nsrv,1,nyrs_srv_bio)    // vector of CVs for the survey biomass
  matrix cv_abun_srv(1,nsrv,1,nyrs_srv_abun)    // vector of CVs for the survey biomass

  

  int phase_fydev              // phase for first year deviations
  int sr_phase                 // phase for estiamtion of rzero and steepness
  int mat_phase                // phase for maturity estimation
 
  matrix tmp(1,nages,1,nages)   // tmp matrix for rescaling the age error matrix
  matrix tmp2(1,nages,1,nlen)   // tmp matrix for rescaling the transition matrix

  imatrix psrvname(1,nsrv,1,2)  // dimensions of strings for the survey names
  vector ages_dat_mid(1,nages_dat)  // midpoint of the age bins, for Francis
  vector lengths_mid(1,nlen)        // midpoint of the length bins, for Francis
  !! ages_dat_mid = dvector(ages_dat) + 0.5;
  !! lengths_mid = dvector(lengths)+ 0.5; 




  


// things for getting the survey selectivity a10 (as modified by M)
  int firstage;              // the first age that exceeds 10% survey selection
  int excludeage;            // exclude ages at or below this

  int endyr_r   // end year for retrospective run
  
  int comp_yr_count  // for counting the number of years to be used for compositon data in retrospective run
  int surv_yr_count  // for counting the number of years to be used for survey biomass indices
  int nyrs_fish_unbiased_ac_r  // number of years to use for retrospecitve run  
  int nyrs_fish_lc_r  // number of years to use for retrospecitve run
  vector nyrs_srv_bio_abun_r(1,nsrv)  // number of years of the survey biomass/abundance indices to use for retrospecitve run 
 
  ivector nyrs_srv_ac_r(1,nsrv)      // number of years of the survey age comps to use for retrospecitve run
  ivector num_srv_ac_resid_r(1,nsrv) // number of residuals of the survey age comps to use for retrospecitve run      
  ivector nyrs_srv_lc_r(1,nsrv)      // number of years of the survey length comps to use for retrospecitve run
  ivector num_srv_lc_resid_r(1,nsrv) // number of residuals of the survey length comps to use for retrospecitve run

 LOCAL_CALCS                               // Get the names of the surveys
  adstring_array CRLF;   // blank to terminate lines
  CRLF+="";
  
  for(f=1;f<=nsrv;f++) {psrvname(f,1)=1; psrvname(f,2)=1;}    // set whole array to equal 1 in case not enough names are read
  f=1;
    for(i=1;i<=strlen(srvnameread);i++)
      if(adstring(srvnameread(i))==adstring("%"))
        {psrvname(f,2)=i-1; f+=1;  psrvname(f,1)=i+1;}
    psrvname(nsrv,2)=strlen(srvnameread);
  for(f=1;f<=nsrv;f++)
  {
    srvname+=srvnameread(psrvname(f,1),psrvname(f,2))+CRLF(1);
  }
  cout<<" surveynames are "<<srvname<<endl;
 END_CALCS
 
  
 
 LOCAL_CALCS
  // calculate endyr_r and sel_endyr for the fishery and surveys
  endyr_r = endyr - yrs_r;
  fsh_sel_endyr = endyr_r - fsh_sel_fixedyrs;
  srv_sel_endyr = endyr_r - srv_sel_fixedyrs;

  // count the number of years of age comps to be used in the retrospective run
  comp_yr_count = 0;
  for (i=1;i<=nyrs_fish_unbiased_ac;i++)
      if (yrs_fish_unbiased_ac(i) <= endyr_r)  comp_yr_count++;  
  nyrs_fish_unbiased_ac_r = comp_yr_count;

  comp_yr_count = 0;
  for (i=1;i<=nyrs_fish_lc;i++)
      if (yrs_fish_lc(i) <= endyr_r)  comp_yr_count++;  
  nyrs_fish_lc_r = comp_yr_count;

  // count the number of years of survey indices to be used in the retrospective run
  for (s=1;s<=nsrv;s++)
    {
      surv_yr_count = 0;
      for (i=1;i<=nyrs_srv_bio(s);i++)
          if (yrs_srv_bio(s,i) <= endyr_r)  surv_yr_count++;
      nyrs_srv_bio_abun_r(s) = surv_yr_count;
      
      comp_yr_count = 0;
      for (i=1;i<=nyrs_srv_ac(s);i++)
          if (yrs_srv_ac(s,i) <= endyr_r)  comp_yr_count++;
      nyrs_srv_ac_r(s) = comp_yr_count;
      num_srv_ac_resid_r(s) = nyrs_srv_ac_r(s)*nages; 
      
      comp_yr_count = 0;
      for (i=1;i<=nyrs_srv_lc(s);i++)
          if (yrs_srv_lc(s,i) <= endyr_r)  comp_yr_count++;
      nyrs_srv_lc_r(s) = comp_yr_count;
      num_srv_lc_resid_r(s) = nyrs_srv_lc_r(s)*nlen; 

    } 
 END_CALCS 

   
  matrix rescaled_sel_fish(styr,endyr_r,1,nages_dat)  // rescaled fishery selectivity matrix
  vector rescaled_F(styr,endyr_r)                 // rescaled F values
  vector recent_fish_sel(1,nages_dat)               // the recent fish selectivity (for spr calcs)
  number recent_M                                   // the recent M (for spr calcs)
  number average_M                                  // the average M (for age 10 calcs)

  3darray srv_sel_ndat(1,nsrv,styr,endyr_r,1,nages_dat)  // survey selectivity, up to ages nages_dat 

  vector yrs_fish_unbiased_ac_r(1,nyrs_fish_unbiased_ac_r)  // years to use for retrospecitve run  
  vector yrs_fish_lc_r(1,nyrs_fish_lc_r)  // years to use for retrospecitve run
  matrix oac_fish_unbiased_r(1,nyrs_fish_unbiased_ac_r,1,nages_dat)  // observed unbiased fishery age comps, retrospective run
  matrix olc_fish_r(1,nyrs_fish_lc_r,1,nlen)     // observed fishery length comps, retrospective run
  matrix yrs_srv_ac_r(1,nsrv,1,nyrs_srv_ac_r)            // years to use for retrospective run, survey age comps
  3darray oac_srv_r(1,nsrv,1,nyrs_srv_ac_r,1,nages_dat)  // observed survey age comps, retrospective run
  matrix yrs_srv_lc_r(1,nsrv,1,nyrs_srv_lc_r)            // years to use for retrospective run, survey length comps
  3darray olc_srv_r(1,nsrv,1,nyrs_srv_lc_r,1,nlen)       // observed survey length comps, retrospective run


 LOCAL_CALCS
  // get the comp data and years for the retrospective run


  yrs_fish_unbiased_ac_r = yrs_fish_unbiased_ac(1,nyrs_fish_unbiased_ac_r);
  yrs_fish_lc_r = yrs_fish_lc(1,nyrs_fish_lc_r);

  for (i=1;i<=nsrv;i++){
    yrs_srv_ac_r(i) = yrs_srv_ac(i)(1,nyrs_srv_ac_r(i));
    yrs_srv_lc_r(i) = yrs_srv_lc(i)(1,nyrs_srv_lc_r(i));
  }
  

  for (i=1;i<=nyrs_fish_unbiased_ac_r;i++)
     oac_fish_unbiased_r(i) = oac_fish_unbiased(i); 
  for (i=1;i<=nyrs_fish_lc_r;i++)
      olc_fish_r(i) =  olc_fish(i);
  for (i=1;i<=nsrv;i++){
    for (j=1;j<=nyrs_srv_ac_r(i);j++){
        oac_srv_r(i,j) = obs_srv_ac(i,j);
    }
    for (j=1;j<=nyrs_srv_lc_r(i);j++){
        olc_srv_r(i,j) = obs_srv_lc(i,j);
    }
  }      

  // bin the maturity and weight to match data plus group
  wt_pop_bin(1,nages_dat-1) = wt_pop(1,nages_dat-1);
  wt_pop_bin(nages_dat) = mean(wt_pop(nages_dat,nages)); 
  wt_fsh_bin(1,nages_dat-1) = wt_fsh(1,nages_dat-1);
  wt_fsh_bin(nages_dat) = mean(wt_fsh(nages_dat,nages));
  fsh_jsel_npar = 0;
  fsh_isel_npar = 0; 
  spmo_frac = (spawn_mo-1)/12.;
  num_proj_Fs = 5;
  styr_fut=endyr_r+1;
  endyr_fut = styr_fut+10;
  // define styr_rec +++++++++++++++++++++++++++++++++++++++

  if (fyear_ac_option == 1) // first year rec are combined with other recruitments  
  {
   styr_rec = styr-nages+1;
   styr_rec_dev = styr-nages_dat+1;    // some cohorts share a recruitment deviation
   phase_fydev = -1;
  }
  else if (fyear_ac_option == 2) // first year recruitments are in equilibrium with historic catch
  { 
   styr_rec = styr;
   styr_rec_dev = styr;
   phase_fydev = -1;
  }
  else if (fyear_ac_option == 3) // first year recruitments are stochastic, but separate from other recs
  { 
   styr_rec = styr;
   styr_rec_dev = styr;
   phase_fydev = 3;
  }  
  lastyr_rec = endyr_r - fixedrec_yrs;   // define the last year for which we estimate recruitment
  
  if (sr_type==1) sr_phase=-1;
    else sr_phase = 2;
   cout << "sr_phase is " <<sr_phase <<  endl;
    
  if (mat_input_switch==1)  mat_phase = -1;
    else mat_phase = 5; 
  
   

   tmp = trans(unbiasedages);
   for (i=1;i<=nages;i++) tmp(i) = tmp(i)/sum(tmp(i));
   unbiasedages = trans(tmp);
   
  
   tmp2 = trans(translen);
   for (i=1;i<=nages;i++) tmp2(i) = tmp2(i)/sum(tmp2(i));
   translen = trans(tmp2);

  // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // calculate cv's for the survey
  
  for (s=1;s<=nsrv;s++)
  {
    cv_bio_srv(s) = elem_div(obs_srv_bio_sd(s),obs_srv_bio(s));
    cv_abun_srv(s) = elem_div(obs_srv_abun_sd(s),obs_srv_abun(s));
  }

  
  // compute the number of bins
  //tmpnbins = (endyr - styr +1)/fselbinsize;
  //if ((endyr - styr +1)/fselbinsize - tmpnbins < 1e-3 ) nbins=tmpnbins;
  //else nbins=tmpnbins+1;
  //cout << "look here "<<endl;
  //cout << endyr - styr +1 <<" "<<fselbinsize<<" "<< tmpnbins <<" " <<nbins<<endl;  

  // start to read from the control file
  ad_comm::change_datafile_name("pop24.ctl");
 END_CALCS
  init_int phase_selcoff
  init_ivector phase_srv_sel_param(1,nsrv)
  init_int phase_f_sel_param
  //init_ivector phase_s_sel_aslope(1,nsrv)
  //init_ivector phase_s_sel_a50(1,nsrv)
  //init_ivector phase_s_sel_mu(1,nsrv)
  //init_ivector phase_s_sel_dist(1,nsrv)
  //init_ivector phase_s_sel_sig1(1,nsrv)
  //init_ivector phase_s_sel_sig2(1,nsrv)
  init_ivector phase_s_sel_q(1,nsrv)
  init_int phase_proj
  init_int phase_historic_F
  //init_int phase_logM
  init_int phase_log_avg_M
  init_int phase_M_devs
  init_int phase_log_avg_fmort
  init_int phase_fmort_dev
  init_int phase_rec_dev
  init_int phase_log_rinit
  
  init_int comp_like_switch             // Switch for composition likelihood function (1 = multinomial, 2 = Dirichlet-Multinomial)
  init_ivector ph_D_M_sac(1,nsrv)       // Phase for Dirichlet-Multinomial theta parameter, survey age comps (if being used)
  init_ivector ph_D_M_slc(1,nsrv)       // Phase for Dirichlet-Multinomial theta parameter, survey length comps (if being used)
  init_ivector ph_D_M_fish(1,2)         // Phase for Dirichlet-Multinomial theta parameter, fishery age and length comps (if being used)
  init_int har_flag
  init_vector srv_bio_flag(1,nsrv)      // flag to fit biomass estimates from the surveys
  init_vector srv_abun_flag(1,nsrv)     // flag to fit abundance estimates from the surveys 
  init_vector srv_age_flag(1,nsrv)      // flag to fit the age comp data from the surveys
  init_vector srv_len_flag(1,nsrv)      // flag to fit the length comp data from the surveys 
  init_int fish_bio_flag                // flag to fit catch biomass  
  init_int fish_age_flag                // flag to fit the proportional catch age comp data from the surveys
  init_int fish_len_flag                // flag to fit the proportional length comp data from the surveys 

  init_vector stg1_fish_unbiased_ac_samp(1,nyrs_fish_unbiased_ac)
  init_vector nsamples_fish_unbiased_ac(1,nyrs_fish_unbiased_ac)

  init_vector stg1_fish_lc_samp(1,nyrs_fish_lc)
  init_vector nsamples_fish_lc(1,nyrs_fish_lc)

  init_matrix stg1_srv_ac_samp(1,nsrv,1,nyrs_srv_ac)   // ragged array of survey age sample sizes
  init_matrix nsamples_srv_ac(1,nsrv,1,nyrs_srv_ac)

  init_matrix stg1_srv_lc_samp(1,nsrv,1,nyrs_srv_lc)   // ragged array of survey length sample sizes
  init_matrix nsamples_srv_lc(1,nsrv,1,nyrs_srv_lc)

  init_vector lambda(1,6)
  !! cout << lambda << endl;

 LOCAL_CALCS 
  // start to read from the initial values file 
  ad_comm::change_datafile_name("initvalues.ctl");
 END_CALCS

  //init_number logM_start       // initial value for logM 
  init_vector log_q_srv_start(1,nsrv) //0.153074980052
  init_number log_avg_M_start         // initial value for log_avg_M
  init_number mat_beta1_start
  init_number mat_beta2_start
  init_number log_rinit_start
  init_number log_rzero_start
  init_number log_avg_fmort_start
  init_number mean_log_rec_start
  init_number sel_aslope_fish_start //1.63656109222
  init_number sel_dslope_fish_start //-1.63 //0.0
  init_vector sel_aslope_srv_start(1,nsrv)  //   1.01826089348
  init_number sel_a50_fish_start //7.32882926631
  init_number sel_d50_fish_start //24.903941
  init_vector sel_a50_srv_start(1,nsrv) //6.05353089120
  init_number steepness_start //1.0
  init_number historic_F_start //0.000
  init_vector theta_fish_start(1,2) //0.1
  init_vector theta_sac_start(1,nsrv) //0.1
  init_vector theta_slc_start(1,nsrv) //0.1
  init_int    jitter_seed   // seed for jittering the starting values
  init_number jitter_stddev // jittering standard deviation

  vector jitterstdnorm(1,100)         // vector of standard normal errors for jittering
  vector jittererr(1,100)             // vector of jitter errors (expected mean of zero)
  int err_count                       // for jittering
  
  

  
  
  // vectors for sample sizes for retrospective runs
  vector fish_unbiased_ac_samp_r(1,nyrs_fish_unbiased_ac_r)
  vector fish_lc_samp_r(1,nyrs_fish_lc_r)

   // matrices for sample sizes for retrospective runs
  matrix srv_ac_samp_r(1,nsrv,1,nyrs_srv_ac_r)
  matrix srv_lc_samp_r(1,nsrv,1,nyrs_srv_lc_r)


 LOCAL_CALCS 
  // start to read from the composition weight file
  // order is fac, flc, sac, slc
  ad_comm::change_datafile_name("compweights.ctl");
 END_CALCS
  init_vector compweights_fsh(1,2)
  init_vector compweights_sac(1,nsrv)
  init_vector compweights_slc(1,nsrv)

   

 LOCAL_CALCS 
    if (nyrs_fish_unbiased_ac_r>0)
        fish_unbiased_ac_samp_r = compweights_fsh(1)*stg1_fish_unbiased_ac_samp(1,nyrs_fish_unbiased_ac_r);
    if (nyrs_fish_lc_r>0)
    fish_lc_samp_r = compweights_fsh(2)*stg1_fish_lc_samp(1,nyrs_fish_lc_r);
    
    for (i=1;i<=nsrv;i++){
       if(nyrs_srv_ac_r(i)>0)
           srv_ac_samp_r(i) = compweights_sac(i)*stg1_srv_ac_samp(i)(1,nyrs_srv_ac_r(i));
       if(nyrs_srv_lc_r(i)>0)
           srv_lc_samp_r(i) = compweights_slc(i)*stg1_srv_lc_samp(i)(1,nyrs_srv_lc_r(i));
    }
 END_CALCS
   
   
 

  
  // fishery selectivity phases
  int phase_f_sel_ascend;
  int phase_f_sel_descend;
  int phase_f_sel_par;
  int phase_fsh_a50_devs;
  int phase_fsh_aslope_devs;
  int phase_fsh_d50_devs;
  int phase_fsh_dslope_devs;
  // survey selectivity phases
  ivector phase_srv_sel_logistic(1,nsrv);
  ivector phase_srv_sel_dblnorm(1,nsrv);
  ivector phase_srv_sel_par(1,nsrv);
  ivector phase_srv_a50_devs(1,nsrv);
  ivector phase_srv_aslope_devs(1,nsrv);
  ivector phase_srv_mu_devs(1,nsrv);
  ivector phase_srv_dist_devs(1,nsrv);
  ivector phase_srv_sig1_devs(1,nsrv);
  ivector phase_srv_sig2_devs(1,nsrv);

  //int phase_cpue;
  ivector binindex_fish_sel(fsh_sel_styr,fsh_sel_endyr);       // binindex for time-varying fish sel (if used)
  ivector binindex_M(styr,endyr_r)
  //ivector binindex_mean_log_rec(styr_rec_dev,endyr_r);                    // binindex for time-varying M (if used)
  imatrix binindex_srv_q(1,nsrv,styr_fish,endyr_r)     // binindex for time-varying q, by survey (if used)
  //ivector binstart(1,nbins);           // the start year for each time bin
  vector fsh_scal_yr_nodes(1,fsh_n_yr_nodes);  // the fishery yr nodes scaled from 0 to 1
  vector fsh_scal_age_nodes(1,fsh_n_age_nodes);  // the fishery age nodes scaled from 0 to 1
  matrix srv_scal_yr_nodes(1,nsrv,1,srv_n_yr_nodes);  // the survey yr nodes scaled from 0 to 1
  matrix srv_scal_age_nodes(1,nsrv,1,srv_n_age_nodes);  // the survey age nodes scaled from 0 to 1



  imatrix binindex_srv_sel(1,nsrv,srv_sel_styr,srv_sel_endyr);       // binindex for time-varying survey sel (if used)
  

 
 LOCAL_CALCS  // set phases for fishery and survey selectivity options, and time-varying deviations
  //  phase for the fishery ascending parameters for logistic
  //  phase for the fishery descending parameters for the logistic
  //  phase for the fishery age and year nodes for the bicubic spline
  //  phase for ebs catchabilty and selectivity (turn off if they are not being fit)
  
  // compute the years each bin starts
    //binstart(1) = styr;
    //for (i=2;i<=nbins;i++) 
    //    binstart(i) = binstart(i-1) + fselbinsize;

  if (fish_age_flag==0 & fish_len_flag==0)  // turn off fishery selectivity parameters if not fitting fishery comp data
     {
      phase_f_sel_param = -1; 
     }

  // compute binindex_fish_sel for time-varying selectivity
  
  // set fishery selectivity phases to -1, and later turn on the ones we need; 
  //   selectivity parameters
  phase_f_sel_ascend = -1;
  phase_f_sel_descend = -1;
  phase_f_sel_par = -1;
  //   fishery time-varying parameters for logistic and double logistic
  phase_fsh_a50_devs = -1;
  phase_fsh_aslope_devs = -1;
  phase_fsh_d50_devs = -1;
  phase_fsh_dslope_devs = -1;
  if (fsh_sel_option==1)  // logistic fishery selectivity 
  {
    phase_f_sel_ascend = phase_f_sel_param;
    if(nbins_fsh_sel > 1)
      {
        phase_fsh_a50_devs = 4;
        phase_fsh_aslope_devs = 4;
      }

  }
  else if (fsh_sel_option==2)  // double logistic fishery selectivity 
  {
    phase_f_sel_ascend = phase_f_sel_param;
    phase_f_sel_descend = phase_f_sel_param;
    if(nbins_fsh_sel > 1)
      {
        phase_fsh_a50_devs = 4;
        phase_fsh_aslope_devs = 4;
        phase_fsh_d50_devs = 4;
        phase_fsh_dslope_devs = 4;
      }

  }
  else if (fsh_sel_option==3)  // bicubic fishery selectivity 
  {
    phase_f_sel_par = phase_f_sel_param;
    fsh_scal_age_nodes.fill_seqadd(0,1./(fsh_n_age_nodes-1));
    fsh_scal_yr_nodes.fill_seqadd(0,1./(fsh_n_yr_nodes-1));
    fsh_isel_npar = fsh_n_yr_nodes;
    fsh_jsel_npar = fsh_n_age_nodes; 
  }

  else if (fsh_sel_option==4)  // time-varying cubic fishery selectivity 
  {
    phase_f_sel_par = phase_f_sel_param;
    fsh_isel_npar = fsh_n_age_nodes;
    fsh_jsel_npar = nbins_fsh_sel; 
  }

  for (i=1;i<=nsrv;i++)
  {
    if (srv_age_flag(i)==0 & srv_len_flag(i)==0)  // turn off survey selectivity parameters if not fitting survey comp data
     {
      phase_srv_sel_param(i) = -1; 
     }
  }   

  
  // set phases for survey selectivity options, and time-varying deviations
  // set survey selectivity phases to -1, and later turn on the ones we need; 
  //   selectivity parameters
  phase_srv_sel_logistic = -1;
  phase_srv_sel_dblnorm = -1;
  phase_srv_sel_par = -1;
  //   fishery time-varying parameters for logistic and double logistic
  phase_srv_a50_devs = -1;
  phase_srv_aslope_devs = -1;
  phase_srv_mu_devs = -1;
  phase_srv_dist_devs = -1;
  phase_srv_sig1_devs = -1;
  phase_srv_sig2_devs = -1;

  //  phase for catchabilty (turn off if they are not being fit)

  for (i=1;i<=nsrv;i++)   
   {
     if(srv_bio_flag(i)==0 && srv_abun_flag(i)==0)  // turn off survey catchability paramaters if not being used
       {
          phase_s_sel_q(i)=-1;
   }

   if (srv_sel_option(i)==1)  // logistic survey selectivity 
     {
       phase_srv_sel_logistic(i) = phase_srv_sel_param(i);
       if((nbins_srv_sel(i)) > 1)
       {
        phase_srv_a50_devs(i) = 4;
        phase_srv_aslope_devs(i) = 4;
       }
     }
   else if (srv_sel_option(i)==2)  // double normal survey selectivity 
    {
        phase_srv_sel_dblnorm(i)  = phase_srv_sel_param(i);
        if((nbins_srv_sel(i)) > 1)
        {
         phase_srv_mu_devs(i) = 4;
         phase_srv_dist_devs(i) = 4;
         phase_srv_sig1_devs(i) = 4;
         phase_srv_sig2_devs(i) = 4;
        }
    }
   else if (srv_sel_option(i)==3)  // bicubic survey selectivity 
    {
      phase_srv_sel_par(i) = phase_srv_sel_param(i);
      srv_scal_age_nodes(i).fill_seqadd(0,1./(srv_n_age_nodes(i)-1));
      srv_scal_yr_nodes(i).fill_seqadd(0,1./(srv_n_yr_nodes(i)-1));
      srv_isel_npar(i) = srv_n_yr_nodes(i);
      srv_jsel_npar(i) = srv_n_age_nodes(i); 
    }
  else if (srv_sel_option(i)==4)  // time-varying cubic fishery selectivity 
    {
      phase_srv_sel_par(i) = phase_srv_sel_param(i);
      srv_isel_npar(i) = srv_n_age_nodes(i);
      srv_jsel_npar(i) = nbins_srv_sel(i); 
    }


   }

  // set phases for Dirichlet-Multinomial theta 
  if(comp_like_switch!=2)   // set phases for all DM theta parameters to -1 if not using DM 
  {
    for (i=1;i<=nsrv;i++) 
     {
       ph_D_M_sac(i) = -1; ph_D_M_slc(i) = -1;
      }   
    ph_D_M_fish(1) = -1;
    ph_D_M_fish(2) = -1; 
  }
  else                      // Using DM, but set phases for specific data types to -1 if no data
  { 
    for (i=1;i<=nsrv;i++)
      {                       
        if (nyrs_srv_ac_r(i) <= 0) ph_D_M_sac(i) = -1;
        if (nyrs_srv_lc_r(i) <= 0) ph_D_M_slc(i) = -1;          
      }
    if (nyrs_fish_unbiased_ac_r <= 0 ) ph_D_M_fish(1) = -1;
    if (nyrs_fish_lc_r <= 0 ) ph_D_M_fish(2) = -1;  
  }

  // set phases for the M_devs to -1 if nbins not greater than 1
  if(nbins_M <= 1)
     phase_M_devs = -1;

 END_CALCS
 

INITIALIZATION_SECTION //-------------------------------------------------------------------------------------
  log_avg_M              log_avg_M_start
  //F40 0.044
  //F35 0.053
  //F30 0.063
  mat_beta1         mat_beta1_start
  mat_beta2         mat_beta2_start
  //mean_log_rec 3.97
  mean_log_rec      mean_log_rec_start  
  log_rinit         log_rinit_start
  log_rzero         log_rzero_start
  log_avg_fmort     log_avg_fmort_start
  sel_aslope_fish   sel_aslope_fish_start //1.63656109222
  sel_dslope_fish   sel_dslope_fish_start //-1.63 //0.0
  sel_aslope_srv    sel_aslope_srv_start  //     1.01826089348
  sel_a50_fish      sel_a50_fish_start // 7.32882926631
  sel_d50_fish      sel_d50_fish_start //24.903941
  sel_a50_srv       sel_a50_srv_start  //6.05353089120
  steepness         steepness_start //  1.0

  historic_F        historic_F_start //0.000
  fsh_sel_par           0
  log_q_srv         log_q_srv_start //0.153074980052
  theta_fish        theta_fish_start //0.1
  theta_sac         theta_sac_start //0.1
  theta_slc         theta_slc_start //0.1
  srv_a50_devs      0.0
  
  
  //sigr 0.8013
  //historic_F 0.000
  
  //log_q_srv 0.153074980052

  //theta_fish 0.1
  //theta_sac 0.1
  //theta_slc 0.1

PARAMETER_SECTION //-----------------------------------------------------------------------------------------


 // offset parameters
 vector offset(1,6)
 vector sac_offset(1,nsrv)  // offset for survey age comps, by survey
 vector slc_offset(1,nsrv)  // offset for survey length comps, by survey
 // selectivity parameters 
 //  First, the logistic curve parameters for the domestic and foreign fisheries(slope and 50% parameters)
 //init_number sel_aslope_fish(phase_logist_param)
 init_bounded_number sel_aslope_fish(0.1,3.0,phase_f_sel_ascend)
 init_number sel_dslope_fish(phase_f_sel_descend)
 //init_number sel_a50_fish(phase_f_sel_ascend)
 init_bounded_number sel_a50_fish(0.1,30.0,phase_f_sel_ascend)
 init_number sel_d50_fish(phase_f_sel_descend)
 // fishery selectivity matrix for bicubic and time-varying cubic selectivity 
 init_matrix fsh_sel_par(1,fsh_jsel_npar,1,fsh_isel_npar,phase_f_sel_par);
 // fishery selectivity matrix for bicubic and time-varying cubic selectivity 
 init_matrix_vector srv_sel_par(1,nsrv,1,srv_jsel_npar,1,srv_isel_npar,phase_srv_sel_par);


 //init_number sel_d50_fish(-1)

 // now the logistic curve parameters for the surveys
 init_bounded_number_vector sel_aslope_srv(1,nsrv,0.1,3.0,phase_srv_sel_logistic)
 init_bounded_number_vector sel_a50_srv(1,nsrv,0.1,30.0,phase_srv_sel_logistic)

 // parameters for the double normal
 init_bounded_number_vector sel_srv_mu(1,nsrv,0,50,phase_srv_sel_dblnorm)
 init_bounded_number_vector sel_srv_dist(1,nsrv,0,50,phase_srv_sel_dblnorm)
 init_bounded_number_vector sel_srv_sig1(1,nsrv,0.1,100,phase_srv_sel_dblnorm)
 init_bounded_number_vector sel_srv_sig2(1,nsrv,0.1,100,phase_srv_sel_dblnorm)
 
// deviations on fishery selectivity
 //init_bounded_dev_vector a50_devs(1,nbins,-10.,10.,4)
 //init_bounded_dev_vector aslope_devs(1,nbins,-10.,10.,4)
 //number slptmp
 //number a50tmp
 init_bounded_dev_vector fsh_a50_devs(1,nbins_fsh_sel,-10.,10.,phase_fsh_a50_devs)
 init_bounded_dev_vector fsh_aslope_devs(1,nbins_fsh_sel,-10.,10.,phase_fsh_aslope_devs)
 init_bounded_dev_vector fsh_d50_devs(1,nbins_fsh_sel,-10.,10.,phase_fsh_d50_devs)
 init_bounded_dev_vector fsh_dslope_devs(1,nbins_fsh_sel,-10.,10.,phase_fsh_dslope_devs)
 
 number fsh_a_slptmp
 number fsh_a50tmp
 number fsh_d_slptmp
 number fsh_d50tmp

 number srv_a_slptmp      // for survey logistic selectivity
 number srv_a50tmp        // for survey logistic selectivity
 number srv_mu_tmp        // for survey double normal 
 number srv_dist_tmp      // for survey double normal
 number srv_sig1_tmp      // for survey double normal 
 number srv_sig2_tmp      // for survey double normal



// deviations on survey selectivity, logistic selectivity
 init_bounded_vector_vector srv_a50_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_a50_devs) 
 init_bounded_vector_vector srv_aslope_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_aslope_devs)
// deviations on survey selectivity, double normal selectivity
 init_bounded_vector_vector sel_srv_mu_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_mu_devs) 
 init_bounded_vector_vector sel_srv_dist_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_dist_devs)
 init_bounded_vector_vector sel_srv_sig1_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_sig1_devs)
 init_bounded_vector_vector sel_srv_sig2_devs(1,nsrv,1,nbins_srv_sel,-10.,10.,phase_srv_sig2_devs)

 // Or we could estimate the selectivity coefficients directly (not used so far)
 //init_vector log_selcoffs_fish(1,nselages,phase_selcoff)
 //init_vector log_selcoffs_srv2(1,nselages,phase_selcoff)
 //number avgsel_fish;  // (averages are used in the likelihood function (if used))
 //number avgsel_srv2;

 // Next, the selectivity values (logged and unlogged) 
 matrix log_sel_fish(fsh_sel_styr,fsh_sel_endyr,1,nselages)
 matrix sel_fish(styr,endyr_r,1,nages)
 
 3darray log_sel_srv(1,nsrv,srv_sel_styr,srv_sel_endyr,1,nselages)     // log of the selectivities for all surveys
 3darray sel_srv(1,nsrv,styr,endyr_r,1,nages)         // unlogged selectivities for all surveys 

 
 
// The survival parameters
 //init_number logM(phase_logM)
 init_bounded_number log_avg_M(-7,0.7,phase_log_avg_M)   // the average M
 init_bounded_dev_vector M_devs(1,nbins_M,-0.5,0.5,phase_M_devs)  // the M devs
 //number M
 vector M(styr_rec,endyr_r)
 vector surv(styr_rec,endyr_r)
 init_number log_avg_fmort(1)
 init_bounded_vector fmort_dev(styr_fish,endyr_r,-10,10,phase_fmort_dev)
 number avg_fmort_dev
 matrix F(styr_rec,endyr_r,1,nages)
 matrix Z(styr_rec,endyr_r,1,nages)
 matrix S(styr_rec,endyr_r,1,nages)
 matrix mort(styr_rec,endyr_r,1,nages);  // The multiplier for the mean population size
 matrix spawner_S(styr_rec,endyr_r,1,nages)
 


 // The numbers at age parameters
 init_bounded_dev_vector rec_dev(styr_rec_dev,lastyr_rec,-10,10,phase_rec_dev) // recruitment deviations (styr to endyr)
 init_number mean_log_rec(1)			// mean recruitment
 init_bounded_number log_rinit(0,10,phase_log_rinit)                       // initial recruitment to start model
 matrix natage(styr_rec,endyr_r+1,1,nages)		// numbers at age
 matrix natage_bin(styr_rec,endyr_r+1,1,nages_dat)    // numbers at age
 vector natagetmp(1,nages)   // temporary numbers at age (prior to start year)
 init_bounded_vector fydev(2,nages_dat,-10,10,phase_fydev)    // deviations around eq numbers in first year (stage 3)
 
 //vector spawners(styr,endyr) 			// estimated biomass of female spawners
 //vector expbiom(styr,endyr) 			// estimated exploitable biomass
 sdreport_vector totbiom(styr_rec,endyr_r+1)		// total biomass of population
 //sdreport_number depletion			// change in totbiom over time
 //sdreport_number endbiom			// totbiom in final year
 init_number historic_F(phase_historic_F)	// historic F for computing first year age comps
   
// The parameters for evaluating the objective function
 objective_function_value obj_fun
 vector rec_like(1,3)
 vector srv_bio_like(1,nsrv)
 vector srv_abun_like(1,nsrv)
 //vector surv_like(1,2)
 number catch_like
 number fpen
 number hf_pen
 vector age_like(1,6)
 vector age_like_sac(1,nsrv)
 vector age_like_slc(1,nsrv)
 matrix fish_effn(1,2,1,40)
 matrix sac_effn(1,nsrv,1,40)
 matrix slc_effn(1,nsrv,1,40)
 matrix DM_fish_effn(1,2,1,40)     // Effective N for the fish comps (1= age, 2 = length)
 matrix DM_sac_effn(1,nsrv,1,40)   // Effective N for the survey age comps 
 matrix DM_slc_effn(1,nsrv,1,40)   // Effective N for the survey length comps

 number rec_rmse
 vector srv_bio_rmse(1,nsrv)
 vector srv_abun_rmse(1,nsrv)
 //vector rmse(1,3)  // residual mean square error for the surveys and recruitment
 vector sel_like(1,20)
 number sprpen
 vector prior_M(1,nbins_M)
 matrix prior_q(1,nsrv,1,nbins_q)
 number M_dev_pen    // penalty for the M_devs, if prior is not used for time-varying mortality

 vector survey_bio_sdnr(1,nsrv)
 vector survey_abun_sdnr(1,nsrv)
 vector fish_sdnr(1,2)
 vector sac_sdnr(1,nsrv)
 vector slc_sdnr(1,nsrv)
 vector fish_rmse(1,2)
 vector sac_rmse(1,nsrv)
 vector slc_rmse(1,nsrv)
 vector fish_mean_effn(1,2)
 vector sac_mean_effn(1,nsrv)
 vector slc_mean_effn(1,nsrv)
 vector fish_mean_samp_wts(1,2)
 vector sac_mean_samp_wts(1,nsrv)
 vector slc_mean_samp_wts(1,nsrv)  
 number mat_like

// The parameters for getting the predicted values for the CPUE index (srv1)
  //init_number logq_cpue(phase_cpue)			// q for cpue
  //number q_cpue
  //vector pred_cpue(1,nyrs_cpue)			// predicted cpue (survey 1)
  //init_number cv_cpue(-1)			// cv for cpue index  

// The parameters for getting the predicted values for the AI and EBS trawl survey index (srv3)
  matrix pred_srv_bio(1,nsrv,styr_fish,endyr_r)     // predicted survey biomass
  matrix pred_srv_abun(1,nsrv,styr_fish,endyr_r)     // predicted survey numbers
  init_bounded_vector_vector  log_q_srv(1,nsrv,1,nbins_q,-5,5,phase_s_sel_q)  // q for each of the surveys, by bins            
  matrix q_srv(1,nsrv,styr_fish,endyr_r)   
// The parameters for getting the predicted values for the catch total
  vector pred_catch(styr_fish,endyr_r)  
  number ehc		   			// the estimated historic catch
  matrix catage(styr_fish,endyr_r,1,nages)

// The parameters for getting the predicted age comps
  matrix eac_fish_unbiased_mod(styr_fish,endyr_r,1,nages)
  matrix eac_fish_unbiased_dat(styr_fish,endyr_r,1,nages_dat)
  matrix elc_fish(styr_fish,endyr_r,1,nlen)
  matrix elc_fish_tempages(styr_fish,endyr_r,1,nages)  // the true ages for the year in which we have length comps
  
  3darray eac_srv_mod(1,nsrv,styr,endyr_r,1,nages)       // predicted survey age comps
  3darray eac_srv_dat(1,nsrv,styr,endyr_r,1,nages_dat)   // predicted survey age comps
  3darray elc_srv(1,nsrv,styr,endyr_r,1,nlen)            // predicted survey length comps
  3darray elc_srv_tempages(1,nsrv,styr,endyr_r,1,nages)  // the true survey ages for the year in which we have length comps 

// The parameters for getting the SPR values
   number F40
   number F35
   number F30
   number SB0
   number SBF40
   number SBF35
   number SBF30
   //matrix Nspr(1,4,1,nages)
   //number phizero

// numbers for getting the normalized residuals
 matrix  survey_bio_nr(1,nsrv,1,nyrs_srv_bio)   //  survey biomass normalized residuals
 matrix  survey_abun_nr(1,nsrv,1,nyrs_srv_bio)   //  survey abundance normalized residuals 
 //vector  cpue_nr(1,nyrs_cpue)  //  cpue normalized residuals
 //vector  fbac_nr(1,nyrs_fish_biased_ac*nages)   // fishery biased age comps
      // the normalized residuals from the multinomial, by bin (method TA1.2 in Francis 2011)
 vector  fac_nr(1,nyrs_fish_unbiased_ac_r*nages)   // fishery age comps
 vector  flc_nr(1,nyrs_fish_lc_r*nlen) 		   // fishery length comps
 matrix  sac_nr(1,nsrv,1,num_srv_ac_resid_r)   
 matrix  slc_nr(1,nsrv,1,num_srv_lc_resid_r)

 // number for getting the pearson residuals for the comp data
 3darray sac_pearson(1,nsrv,1,nyrs_srv_ac_r,1,nages_dat)  
 3darray slc_pearson(1,nsrv,1,nyrs_srv_lc_r,1,nlen)           
 matrix  flc_pearson(1,nyrs_fish_lc_r,1,nlen)
 matrix fac_pearson(1,nyrs_fish_unbiased_ac_r,1,nages_dat)




 vector  fac_mcian_wgt(1,nyrs_fish_unbiased_ac_r)   // McAllister-Ianelli weights (method TA1.1 in Francis 2011)
 vector  flc_mcian_wgt(1,nyrs_fish_lc_r)   
 matrix  sac_mcian_wgt(1,nsrv,1,nyrs_srv_ac_r)
 matrix  slc_mcian_wgt(1,nsrv,1,nyrs_srv_lc_r)
 
 vector  fac_mcian_wgt_inv(1,nyrs_fish_unbiased_ac_r)   // inverse of McAllister-Ianelli weights for harmonic mean
 vector  flc_mcian_wgt_inv(1,nyrs_fish_lc_r)   
 matrix  sac_mcian_wgt_inv(1,nsrv,1,nyrs_srv_ac_r)
 matrix  slc_mcian_wgt_inv(1,nsrv,1,nyrs_srv_lc_r)
 

 vector fac_nr_fran(1,nyrs_fish_unbiased_ac_r)       // normalized residuals from the Francis method (method TA1.8 In Francis 2011)
 vector flc_nr_fran(1,nyrs_fish_lc_r)       
 matrix sac_nr_fran(1,nsrv,1,nyrs_srv_ac_r)
 matrix slc_nr_fran(1,nsrv,1,nyrs_srv_lc_r)
 
 
 vector  fac_resid(1,nyrs_fish_unbiased_ac_r*nages)   // fishery age comps
 vector  flc_resid(1,nyrs_fish_lc_r*nlen)       // fishery length comps
 matrix  sac_resid(1,nsrv,1,num_srv_ac_resid_r)  // survey age comps
 matrix  slc_resid(1,nsrv,1,num_srv_lc_resid_r)  // survey length comps

 vector  fac_pearson_vec(1,nyrs_fish_unbiased_ac_r*nages)   // fishery age comps, pearson resids, as vector
 vector  flc_pearson_vec(1,nyrs_fish_lc_r*nlen)             // fishery length comps, pearson resids, as vector 
 matrix  sac_pearson_vec(1,nsrv,1,num_srv_ac_resid_r)        // survey age comps, pearson resids, as vector
 matrix  slc_pearson_vec(1,nsrv,1,num_srv_lc_resid_r)       // survey length comps, pearson resids, as vector



// Parameters for doing the future projections
 //matrix nage_future(styr_fut,endyr_fut,1,nages)
 //number ftmp
 //number mean_recent_fs
 //matrix Z_future(styr_fut,endyr_fut,1,nages)
 //matrix F_future(styr_fut,endyr_fut,1,nages)
 //matrix S_future(styr_fut,endyr_fut,1,nages)
 //init_vector rec_dev_future(styr_fut,endyr_fut,phase_proj)
 //matrix catage_future(styr_fut,endyr_fut,1,nages)
 //sdreport_matrix catch_future(1,num_proj_Fs-1,styr_fut,endyr_fut)
 //sdreport_matrix biomass_future(1,num_proj_Fs,styr_fut,endyr_fut)
 //sdreport_matrix ssb_future(1,num_proj_Fs,styr_fut,endyr_fut)
 
// Stock recruitment params 
   init_number log_rzero(sr_phase)
   number bzero
   number rzero
   sdreport_vector sp_biom(styr_rec,endyr_r)
   vector exp_biom(styr,endyr_r)
   sdreport_vector est_rec(styr,lastyr_rec-rec_age)  // for SR fit, indexed to year class
   number alpha   
   number beta
   init_bounded_number steepness(0.2001,1.0,sr_phase)
   vector pred_rec(styr,lastyr_rec-rec_age)          // prediction from SR model, index to year class
   vector est_spb(styr,lastyr_rec-rec_age)
   vector chi(styr,lastyr_rec-rec_age)   // the squared difference between est and pred recruits
   //init_bounded_number sigr(0.1,2.0,2)  // the sigma parameter for the recruit likelihood
   number sumrecdev // the sum of the lognormal deviations for the recruitments
   vector SRec_spawn(1,20) // data for estimated SR curve
   vector SRec_rec(1,20) //  data for estimated SR curve
   vector xdum2(styr,endyr_r)  // for the cxx file

// vector for projection data file
   vector Mvec(1,nages)  // natural mortality, repeated for each age

// maturity estimation
   init_bounded_number mat_beta1(-10,2,mat_phase)   // beta0 and beta1 parameters for logistic regression of maturity curve
   init_bounded_number mat_beta2(0,2,mat_phase)
   vector mat_theta(1,nages_mat_ogive)   // theta parameter estimates for logistic regression of maturity curve (uses all  ages for which we have data)
   vector maturity(1,nages) // the maturity for the ages used in the population model
   vector maturity_bin(1,nages_dat) // maturity ogive, binned to match the data plus group
       
// updated compweights
   vector compweightsnew_ta12_fsh(1,2)     // weight by inverse of variance of normalized resid
   vector compweightsnew_ta11_fsh(1,2)     //  McAllister-Ianelli method
   vector compweightsnew_ta18_fsh(1,2)     // the Francis method

// updated survey age compweights   
   vector compweightsnew_ta12_sac(1,nsrv)     // weight by inverse of variance of normalized resid
   vector compweightsnew_ta11_sac(1,nsrv)     //  McAllister-Ianelli method
   vector compweightsnew_ta18_sac(1,nsrv)     // the Francis method

// updated survey length compweights   
   vector compweightsnew_ta12_slc(1,nsrv)     // weight by inverse of variance of normalized resid
   vector compweightsnew_ta11_slc(1,nsrv)     //  McAllister-Ianelli method
   vector compweightsnew_ta18_slc(1,nsrv)     // the Francis method

// Dirichlet-multinomial theta parameters
  init_bounded_number_vector  theta_sac(1,nsrv,0.0001,10,ph_D_M_sac)
  init_bounded_number_vector  theta_slc(1,nsrv,0.0001,10,ph_D_M_slc)
  init_bounded_number_vector  theta_fish(1,2,0.0001,10,ph_D_M_fish)
  
  


PRELIMINARY_CALCS_SECTION //-------------------------------------------------------------------------------
 // Compute the offsets for the multinomial distributions
  
  for ( i=1;i<=nyrs_fish_unbiased_ac_r; i++)  // fishery unbiased age comps
 {
  oac_fish_unbiased_r(i) = oac_fish_unbiased_r(i)/sum(oac_fish_unbiased_r(i)); // make sure age comps add to 1.0 for each year
  offset(1)-=fish_unbiased_ac_samp_r(i)*(oac_fish_unbiased_r(i))*log(1.e-13+oac_fish_unbiased_r(i)); //get the negative log like
 }
 for ( i=1;i<=nyrs_fish_lc_r; i++)  // fishery length comps
 {
  olc_fish_r(i) = olc_fish_r(i)/sum(olc_fish_r(i)); // make sure age comps add to 1.0 for each year
  offset(2)-=fish_lc_samp_r(i)*(olc_fish_r(i))*log(1.e-13+olc_fish_r(i)); //get the negative log like
 }
  
 for (i=1;i<=nsrv;i++){
  for (j=1;j<=nyrs_srv_ac_r(i);j++){
    oac_srv_r(i,j) =  oac_srv_r(i,j)/sum(oac_srv_r(i,j));  // make sure age comps add to 1.0 for each year
    sac_offset(i) -= srv_ac_samp_r(i,j)*(oac_srv_r(i,j))*log(1.e-13+oac_srv_r(i,j)); // get the negative log like
  }
  for (j=1;j<=nyrs_srv_lc_r(i);j++){
    olc_srv_r(i,j) =  olc_srv_r(i,j)/sum(olc_srv_r(i,j));  // make sure length comps add to 1.0 for each year
    slc_offset(i) -= srv_lc_samp_r(i,j)*(olc_srv_r(i,j))*log(1.e-13+olc_srv_r(i,j)); // get the negative log like            
  }
 }
  
 if (nbins_fsh_sel > 1)     // getting the bin_index for time varying fish sel, if needed
    {
           binindex_fish_sel = get_binindex(fsh_sel_styr,fsh_sel_endyr,binstart_fsh_sel,nbins_fsh_sel);
           //cout << binindex_fish_sel << endl;
    }
 if (nbins_M > 1)     // getting the bin_index for time varying M, if needed
    {
           binindex_M = get_binindex(styr,endyr_r,binstart_M,nbins_M);
           //cout << binindex_M << endl;
    }
 for (i=1;i<=nsrv;i++){
   if (nbins_q(i) > 1)     // getting the bin_index for time varying M, if needed
      {
           binindex_srv_q(i) = get_binindex(styr_fish,endyr_r,binstart_q(i),nbins_q(i));
           //cout << binindex_srv_q(i) << endl;
      }
  }
 for (i=1;i<=nsrv;i++)
 {
  if (nbins_srv_sel(i) > 1)     // getting the bin_index for time varying survey sel, if needed
    {
           binindex_srv_sel(i) = get_binindex(srv_sel_styr  ,srv_sel_endyr,binstart_srv_sel(i),nbins_srv_sel(i));
    }
  }


PROCEDURE_SECTION //-----------------------------------------------------------------------------------------
// example of using FUNCTION to structure the procedure section
    //cout << test2 << endl;
    //cout << " start of procedure" << endl;

    get_srv_q();
    //cout << " got survey q" << endl;
    
    get_maturity();
    //cout << " got mat" << endl;
    get_selectivity();
    //cout << " got sel" << endl;
    //cout << "current phase is " << current_phase() << endl;
    //cout << "srv_sel_par(1) is  " << endl;
    //cout << srv_sel_par(1) << endl;
    get_mortality();
    //cout << " got mort"  << endl;
    get_first_year();
    //cout <<" got f year" << endl;
    get_numbers_at_age();
    //cout <<" got natage" << endl;
    get_expected_values();
    //cout <<" got expected values" << endl;
    get_sr_inputs();
    //cout <<" got sr inputs" << endl;
    get_catch_at_age();
    //cout <<" got catage" << endl;
    get_age_comps();
    //cout <<" got age comps" << endl;
    get_binned();
    //cout << " get binned"  << endl;
    
    
    
    
   
//    if (current_phase()>6)
//      future_projections();

 //    if (current_phase()>=phase_f40){
 //      cout << " in procedure " << endl;
 //     get_Fspr();
 //   }
    evaluate_the_objective_function();
    
    
    //if(comp_like_switch==1)
    comp_metrics();
    //cout << " got comp metrics"  << endl;
    //get_age10();
    //cout << " get age10"  << endl;
    if(comp_like_switch==1)
      update_compweights();
    //cout << " get upfate comp things"  << endl;

   
     if (mceval_phase())
  {
    ofstream evalout("evalout_pop.prj", ios::app);
    evalout <<obj_fun<<" "<<log_avg_M<<" "<<q_srv(1,styr_fish)<<" "<<mean_log_rec<<" "<<totbiom<<" "<<sp_biom<<" "<<est_rec<<" "<<maturity<<endl;
  }

  //cout <<" obj_fun is " <<obj_fun <<  endl;
  //cout <<" mat_beta  is "<< mat_beta << endl;
  
 
FUNCTION ivector get_binindex(const int& start,const int& end,const ivector& binstart,const int& nbins)
 RETURN_ARRAYS_INCREMENT();
 
 int i;
 int count;
 ivector index(start,end);

 count = 1;
      for (i=start; i<=end; i++)
          {
            if (i == binstart(count)) 
            {
             index(i) = count; 
             if( count < nbins ) count++;   
            }
            if (i < end) index(i+1) = index(i);
          }

 RETURN_ARRAYS_DECREMENT();
 return index;

FUNCTION get_maturity   // get the maturity ogive
 if(mat_input_switch==1)
  { 
   maturity = mat_input(1);
  }
 else
   {  
   mat_theta = elem_div(mfexp(mat_beta1 + mat_beta2*matages_ogive),(1. + mfexp(mat_beta1 + mat_beta2*matages_ogive)));
   maturity = mat_theta(1,nages);
   maturity(nages) = 0.5*(maturity(nages) +  mfexp(mat_beta1 + mat_beta2*100)/(1. + mfexp(mat_beta1 + mat_beta2*100)));
   }
 
FUNCTION get_selectivity //---------------------------------------------------------------------------------
 // Calculate the logistic selectivity (only if being used)
 // First the AI logistic selectivity (only if being used)
 dvariable diff;  // for double normal
 dvariable diff2;  // for double normal
  for (s=1;s<=nsrv;s++)
  {
    log_sel_srv(s) = 0.0;                          // define log(sel) to zero if not estimating
 
    if (current_phase()>=phase_srv_sel_param(s))   // estimation of survey selectivity
    {
        if(srv_sel_option(s)==1)  // logistic survey selectivity
          {
            for (i=srv_sel_styr; i<=srv_sel_endyr; i++)   
              {
                if (nbins_srv_sel(s)==1) 
                  {
                    srv_a_slptmp = sel_aslope_srv(s);
                    srv_a50tmp = sel_a50_srv(s);
                  }
                else if (nbins_fsh_sel>1)
                  {
                    srv_a_slptmp = sel_aslope_srv(s)*exp(srv_aslope_devs(s)(binindex_srv_sel(s,i)));
                    srv_a50tmp = sel_a50_srv(s)*exp(srv_a50_devs(s)(binindex_srv_sel(s,i))); 
                  }    
                for (j=1;j<=nselages;j++)
                      {
                        log_sel_srv(s,i,j) = -1.*log((1.0+mfexp(-1.0*srv_a_slptmp*(ages(j)-srv_a50tmp))));
                      }
              }  // logistic selectivity, loop over years  
          }  // logistic survey selectivity loop  
        else if (srv_sel_option(s)==2)   // double normal survey selectivity
          {
            for (i=srv_sel_styr; i<=srv_sel_endyr; i++)   
              {
                if (nbins_srv_sel(s)==1) 
                  {
                   srv_mu_tmp = sel_srv_mu(s);
                   srv_dist_tmp = sel_srv_dist(s);
                   srv_sig1_tmp = sel_srv_sig1(s);
                   srv_sig2_tmp = sel_srv_sig2(s);  
                  }
                else if (nbins_srv_sel(s)>1) 
                  {
                   srv_mu_tmp = sel_srv_mu(s)*exp(sel_srv_mu_devs(s)(binindex_srv_sel(s,i)));
                   srv_dist_tmp = sel_srv_dist(s)*exp(sel_srv_dist_devs(s)(binindex_srv_sel(s,i)));
                   srv_sig1_tmp = sel_srv_sig1(s)*exp(sel_srv_sig1_devs(s)(binindex_srv_sel(s,i)));
                   srv_sig2_tmp = sel_srv_sig2(s)*exp(sel_srv_sig2_devs(s)(binindex_srv_sel(s,i)));  
                  }  
                    for (j=1;j<=nselages;j++)
                      {
                       diff = ages(j)-srv_mu_tmp;  
                       diff2 = ages(j)-(srv_mu_tmp+srv_dist_tmp); 

                       if(ages(j)<=srv_mu_tmp)
                         {
                           log_sel_srv(s,i,j)= log(mfexp(-(diff*diff)/(2*srv_sig1_tmp*srv_sig1_tmp)));
                         }
            
                       else if(ages(j)>=(srv_mu_tmp+srv_dist_tmp))
                        {
                          log_sel_srv(s,i,j) =log(mfexp(-(diff2*diff2)/(2*srv_sig2_tmp*srv_sig2_tmp)));
                        }
                       else
                        {
                          log_sel_srv(s,i,j) = 0.0; 
                        }  
                     }   // double normal selectivity, loop over ages 
                  }    // double normal selectivity, loop over years
              }     // double normal survey selectivity loop
        else if (srv_sel_option(s)==3)                              // bicubic survey selectivity
          {
             bicubic_spline(srv_scal_yr_nodes(s),srv_scal_age_nodes(s),srv_sel_par(s),log_sel_srv(s));          
          }
        else if (srv_sel_option(s)==4)                              // cubic survey selectivity, by bin
        {
         int count;
         count = 1;
           for (i=srv_sel_styr; i<=srv_sel_endyr; i++)
          {
            if (i == binstart_srv_sel(s,count))
            {
             log_sel_srv(s,i)=cubic_spline(srv_sel_par(s,count));
             if( count < nbins_srv_sel(s) ) count++;   
            }
            if (i < endyr_r) log_sel_srv(s,i+1) = log_sel_srv(s,i);
          }
        }  

    }     // check if current phase > phase_srv_sel_param(s) 
  }    // loop over surveys

 
 log_sel_fish = 0.0;                       // define log(sel) to zero if not estimating 
 if (current_phase()>=phase_f_sel_param)   // estimation of survey selectivity
 {   
  if (fsh_sel_option==1)
 { 
 for (i=fsh_sel_styr; i<=fsh_sel_endyr; i++)   
   {
     if (nbins_fsh_sel==1) 

     {
      for (j=1;j<=nselages;j++)
        {
          log_sel_fish(i,j) = -1.*log((1.0+mfexp(-1.0*sel_aslope_fish*(ages(j)-sel_a50_fish))));
        }
     }
    else if (nbins_fsh_sel>1)
    {
     fsh_a_slptmp = sel_aslope_fish*exp(fsh_aslope_devs(binindex_fish_sel(i)));
     fsh_a50tmp = sel_a50_fish*exp(fsh_a50_devs(binindex_fish_sel(i)));
     for (j=1;j<=nselages;j++)
       {
        log_sel_fish(i,j) = -1.*log((1.0+mfexp(-1.0*fsh_a_slptmp*(ages(j)-fsh_a50tmp))));
       }

    }
   }
 }
 else if (fsh_sel_option==2)
 {
  for (i=fsh_sel_styr; i<=fsh_sel_endyr; i++)  // Use the selectivity values for the foreign fishery through 1988 
  {
    if (nbins_fsh_sel==1) 
    {
     for (j=1;j<=nselages;j++)
       {
        log_sel_fish(i,j) = -1.*log(1.0+mfexp(-1.0*sel_aslope_fish*(ages(j)-sel_a50_fish)))
                 -1.*log(1.0+mfexp(-1.0*sel_dslope_fish*(ages(j)-sel_d50_fish)));
       }
    }
    else if (nbins_fsh_sel>1)
    {
     fsh_a_slptmp = sel_aslope_fish*exp(fsh_aslope_devs(binindex_fish_sel(i)));
     fsh_a50tmp = sel_a50_fish*exp(fsh_a50_devs(binindex_fish_sel(i)));
     fsh_d_slptmp = sel_dslope_fish*exp(fsh_dslope_devs(binindex_fish_sel(i)));
     fsh_d50tmp = sel_d50_fish*exp(fsh_d50_devs(binindex_fish_sel(i)));

     for (j=1;j<=nselages;j++)
       {
         log_sel_fish(i,j) = -1.*log(1.0+mfexp(-1.0*fsh_a_slptmp*(ages(j)-fsh_a50tmp)))
                   -1.*log(1.0+mfexp(-1.0*fsh_d_slptmp*(ages(j)-fsh_d50tmp)));
       }
    }  
  }
 }
 
 else if (fsh_sel_option==3)
 {
    bicubic_spline(fsh_scal_yr_nodes,fsh_scal_age_nodes,fsh_sel_par,log_sel_fish);
 }

 else if (fsh_sel_option==4)
  {
   get_fsh_cubic_spline_by_bin();    
  }
 }
  
   // exponentionate  log selectivity, fishery
 for (i=styr; i<=endyr_r; i++)
  {     
   if (i < fsh_sel_styr) 
      { 
      sel_fish(i)(1,nselages) = mfexp(log_sel_fish(fsh_sel_styr));
      }
   else if (i > fsh_sel_endyr)  
    {
    sel_fish(i)(1,nselages) = mfexp(log_sel_fish(fsh_sel_endyr));
    }
   else   { sel_fish(i)(1,nselages) = mfexp(log_sel_fish(i));}
  
   if (nselages<nages)  
      sel_fish(i)(nselages+1,nages)=sel_fish(i,nselages);
  
  for (s=1;s<=nsrv;s++)  // exponentionate  log selectivity, survey 
   {
    if (i < srv_sel_styr) 
      { 
       sel_srv(s,i)(1,nselages) = mfexp(log_sel_srv(s,srv_sel_styr)(1,nselages));
      }
    else if (i > srv_sel_endyr)  
      {
       sel_srv(s,i)(1,nselages) = mfexp(log_sel_srv(s,srv_sel_endyr)(1,nselages));
      }
    else   { sel_srv(s,i)(1,nselages) = mfexp(log_sel_srv(s,i)(1,nselages));}
    if (nselages < nages)
    {
      sel_srv(s,i)(nselages+1,nages) = sel_srv(s,i)(nselages);  
    }
   }     
  }
 

FUNCTION get_fsh_cubic_spline_by_bin
 // get the fishery selectivity by appyling the cubic spline by bin
 int count;
 count = 1;
      for (i=fsh_sel_styr; i<=fsh_sel_endyr; i++)
          {
            if (i == binstart_fsh_sel(count))
            {
             log_sel_fish(i)=cubic_spline(fsh_sel_par(count) );
             if( count < nbins_fsh_sel ) count++;   
            }
            if (i < endyr_r) log_sel_fish(i+1) = log_sel_fish(i);
          }


 
FUNCTION get_srv_q
 // get the survey q values across time
 int bincount_q;   // count the years within the q bins, for each survey

 //cout << " in get srv_q" << endl;

 for (j=1; j<= nsrv;j++){
   bincount_q = 1;
   for (i=styr_fish; i<=endyr_r-1; i++)  // set F to zero for backup years and years with no fishery
     {
       if (i == binstart_q(j,bincount_q))
         {
           q_srv(j,i) = exp(log_q_srv(j,bincount_q));
           if (bincount_q <  nbins_q(j)) bincount_q++;
         }
        q_srv(j,i+1) = q_srv(j,i);
      }
  }

FUNCTION get_mortality //----------------------------------------------------------------------------------
 
 // Calulate the values of F and Z for each age and year
   int bincount_M;   //  count the years within M bins
   bincount_M = 1; 

   //M = exp(logM);

   avg_fmort_dev=mean(fmort_dev);
   for (i=styr_rec; i<=styr_fish-1; i++)  // set F to zero for backup years and years with no fishery
   {
   	  if (nbins_M > 1)
           M(i) = mfexp(log_avg_M + M_devs(binindex_M(i)));
      else 
           M(i) = mfexp(log_avg_M);     
   

    //if (i == binstart_M(bincount_M))
    //  {
    //    M(i) = exp(logM(bincount_M));
    //    if (bincount_M < nbins_M) bincount_M++;
    //  }
    //  M(i+1) = M(i);

    for (j=1;j<=nages;j++)
 	    {
 	      F(i,j) = 0.0;
 	    }
    }
   
   for (i=styr_fish; i<=endyr_r; i++)  // use the selectivity values for the foreign fishery through 1988
   {
    {
      if (nbins_M > 1)
           M(i) = mfexp(log_avg_M + M_devs(binindex_M(i)));
      else 
           M(i) = mfexp(log_avg_M);     
   }
   

    //if (i == binstart_M(bincount_M))
    //  {
    //    M(i) = exp(logM(bincount_M));
    //    if (bincount_M < nbins_M) bincount_M++;
    //  }
    //  if (i < endyr_r) M(i+1) = M(i);

    for (j=1;j<=nages;j++)
   	{
   	   F(i,j)=sel_fish(i,j)*mfexp(log_avg_fmort + fmort_dev(i));       
  	}
   }

   
   for (i=styr_rec; i<=endyr_r; i++)
      Z(i) = F(i)+M(i);


   S=mfexp(-1.0*Z);
   spawner_S = mfexp(-spmo_frac*Z);


   
   
FUNCTION get_first_year  //---------------------------------------------------------------------------------------------
 
          // get the first year of the natage matrix. Note that in both cases below, the 'styr_rec' 
          // refers to the first year for which there is an estimated value of recruitment.  For the 
          // first year age comp option 2, this is simply the styr of the model.  
          // For option 1, this is the styr-(nages-1).  The first year of the spawning biomass vector is styr_rec
  
 //surv = mfexp(-1.0*M);
 if (fyear_ac_option == 1)  // First year stochastic recruitment devs are coupled with regular recruitment deviations
 {
  natage(styr_rec,1) = mfexp(mean_log_rec)*exp((sigr*sigr)/2);
  for (j=2; j<=nages; j++)
    natage(styr_rec,j) = natage(styr_rec,j-1)*mfexp(-1.0*M(styr_rec));
  natage(styr_rec,nages) /= (1.-mfexp(-1.0*M(styr_rec)));

  for (j=styr_rec; j<=styr_rec_dev; j++)   // deviations in the cohorts that make up the plus group are shared
    natage(j,1) = mfexp(mean_log_rec+rec_dev(styr_rec_dev));

  for (j=styr_rec_dev+1; j<styr; j++)
    natage(j,1) = mfexp(mean_log_rec+rec_dev(j));  

   for (j=styr_rec; j<styr; j++)   // get sp_biom and natage for years prior to styr
    {
      
      natage(j+1)(2,nages) = ++elem_prod(natage(j)(1,nages-1),S(j)(1,nages-1));
      natage(j+1,nages) += natage(j,nages)*S(j,nages);
    }  
 }

 else if (fyear_ac_option == 2) {  // Initial age comps are in equilibrium with historic catch
            			
 natage(styr,1) = mfexp(log_rinit)*exp((sigr*sigr)/2);		// first, write the first age, first year as rzero
  					
 for (j=2; j<=nages;j++)			// next, get the first year ages (2,nages)
    natage(styr,j)=natage(styr,j-1)*mfexp(-(historic_F*sel_fish(styr)(j-1)+M(styr)*M_mult_eq_styr));
 natage(styr,nages) /= (1-mfexp(-(historic_F*sel_fish(styr)(nages)+M(styr))));  	// Plus group for first year

    if (historic_catch > 0.) {  // estimate the historical catch
      ehc = 0;
      for (j=1;j<=nages;j++)
          {
          ehc += natage(styr,j)*wt_fsh(j)*(historic_F*sel_fish(styr,j))*
 	      (1.0-mfexp(-(historic_F*sel_fish(styr,j)+M(styr))))/(historic_F*sel_fish(styr,j)+M(styr));
          }
        } 
 }

  else if (fyear_ac_option == 3) {  // Initial age comps are stochastic, but have a different mean than 
  				   //  the other recruitments. fydev is noise around equilibrium decay.					
  for (j=2; j<=nages_dat;j++)			
    natage(styr,j)=mfexp((log_rinit)  -M(styr)*double(j-1) + fydev(j));
  if (nages>nages_dat)
    {                                  // for the 'extra' ages needed to account for aging error, 
     for (j=nages_dat+1; j<=nages;j++) // use a single fydev for all cohorts in the plus group                                        
       natage(styr,j)=mfexp((log_rinit)  -M(styr)*double(j-1)+fydev(nages_dat));
    }
  natage(styr,nages) = mfexp((log_rinit) - M(styr)*double(nages-1) + fydev(nages_dat))/(1-mfexp(-M(styr)));  // plus group
 }


FUNCTION get_numbers_at_age //--------------------------------------------------------------------------------
 
   // Get numbers for the age of recruitment for all years, and fill out the natage matrix

 // get the recruits  
 for (i=styr;i<=lastyr_rec;i++)  natage(i,1) = mfexp(mean_log_rec+rec_dev(i));   
 for (i=lastyr_rec+1;i<=endyr_r+1;i++) natage(i,1) = mfexp(mean_log_rec +sigr*sigr*0.5);   // ages where we fix the recruits

 for (i=styr;i<=endyr_r;i++)   	// get natage matrix 
  {
   natage(i+1)(2,nages)=++elem_prod(natage(i)(1,nages-1),S(i)(1,nages-1));
   natage(i+1,nages)+=natage(i,nages)*S(i,nages);  // survival of plus group
  }


  

FUNCTION dvar_matrix get_projection_numbers_at_age() //------------------------------------------------------------------------------------------
  // Get the numbers at age for the projection model, which uses the mean recruitment for year classes which
  // have not exceeded the criteria for the survey and/or fishery selectivity
  
  RETURN_ARRAYS_INCREMENT();

  // the last year of recruitment to meet the survey a10 condition 
  double lastyr_rec_a10;
  lastyr_rec_a10 = endyr_r - max(fixedrec_yrs,excludeage - (rec_age - 1));

  dvar_matrix natage_mean_tmp(lastyr_rec_a10+1,endyr_r+1,1,nages);    // numbers at age

  // get the first year
  natage_mean_tmp(lastyr_rec_a10+1) = natage(lastyr_rec_a10+1);
  
  // get the recruits  
  for (i=lastyr_rec_a10+1;i<=endyr_r+1;i++)  natage_mean_tmp(i,1) = mfexp(mean_log_rec +sigr*sigr*0.5);   
  
  for (i=lastyr_rec_a10+1;i<=endyr_r;i++)    // get natage matrix 
    {
   natage_mean_tmp(i+1)(2,nages)=++elem_prod(natage_mean_tmp(i)(1,nages-1),S(i)(1,nages-1));
   natage_mean_tmp(i+1,nages)+=natage_mean_tmp(i,nages)*S(i,nages);  // survival of plus group
    }

 RETURN_ARRAYS_DECREMENT();
 return natage_mean_tmp;
 

FUNCTION get_expected_values  //-----------------------------------------------------------------------------
    
   // get reproductive outputs, total biomass, and survey biomass   
   sp_biom.initialize();
   //sp_biom(styr_rec-rec_age,styr_rec-1) = elem_prod(wt_pop,maturity)*elem_prod(natage(styr_rec)/2.,spawner_S(styr_rec));
   //totbiom(styr_rec-rec_age,styr_rec-1) = natage(styr_rec)*wt_pop;

    for (j=styr_rec; j<=endyr_r; j++)   // get sp_biom and natagetmp for years prior to styr
    {
      sp_biom(j) = elem_prod(wt_pop,maturity)*elem_prod(natage(j)/2.,spawner_S(j));
      //cout << "sp_biom is " << sp_biom(j) << endl;
      totbiom(j) = natage(j)*wt_pop;
    }
   totbiom(endyr_r+1) = natage(endyr_r+1)*wt_pop;

  // compute the predicted values for the surveys
 //q_cpue = exp(logq_cpue);
 
 for (i=styr;i<=endyr_r;i++)
  mort(i) = elem_div((1.-mfexp(-Z(i))),Z(i));
 
 //for (i=1;i<=nyrs_cpue;i++){   // survey 1 is the cpue index
 //   pred_cpue(i)=q_cpue*elem_prod(natage(yrs_cpue(i)),mort(yrs_cpue(i)))*
 //      elem_prod(sel_fish(yrs_cpue(i)),wt_pop);}
 
 //for (i=styr_fish;i<=endyr_r;i++){   // survey 3 is the AI  trawl survey 
 //  pred_srv3(i)=prop_bio_ai(i)*q_srv3*elem_prod(natage(i),mort(i))*
 //     elem_prod(sel_srv3,wt_pop);
 //  pred_srv_ebs(i) = (1.0-prop_bio_ai(i))*q_srv_ebs*elem_prod(natage(i),mort(i))*
 //     elem_prod(sel_srv_ebs,wt_pop);
 //   }

 /*
 if (switch_prop_srv == 1)
  {
    for (s=1;s<=nsrv;s++){
     for (i=styr_fish;i<=endyr_r;i++) 
     { 
       pred_srv_bio(s,i)= prop_srv(s,i)*q_srv(s,i)*elem_prod(natage(i),mort(i))*elem_prod(sel_srv(s,i),wt_pop);
       pred_srv_abun(s,i)= prop_srv(s,i)*q_srv(s,i)*elem_prod(natage(i),mort(i))*sel_srv(s,i);        
     }
  } 
  }
  else 
  {
  */  
    for (s=1;s<=nsrv;s++){
     for (i=styr_fish;i<=endyr_r;i++) 
     { 
       pred_srv_bio(s,i)= q_srv(s,i)*elem_prod(natage(i),mort(i))*elem_prod(sel_srv(s,i),wt_pop);
       pred_srv_abun(s,i)= q_srv(s,i)*elem_prod(natage(i),mort(i))*sel_srv(s,i);        
     }
  }  
  //}


  
  

FUNCTION get_sr_inputs //--------------------------------------------------------------------------------------------------
  // get the inputs for the SR curve 
 // first, define rzero and set bzero, if needed
 
 
 if (active(log_rzero))
 { 
 rzero = mfexp(log_rzero);
 natagetmp = 0.0;
 natagetmp(1) = rzero;
 for (j=2; j<=nages; j++)
  natagetmp(j) = natagetmp(j-1)*mfexp(-1.0*mean(M));
 natagetmp(nages) /= (1.-mfexp(-1.0*mean(M)));
 bzero = elem_prod(wt_pop,maturity)*natagetmp*0.5;
 alpha = 0.8*rzero*steepness/(steepness-0.2);
 beta = 0.2*bzero*((1.-steepness)/(steepness-0.2));
 }
 est_rec(styr,lastyr_rec-rec_age) = column(natage,1)(styr+rec_age,lastyr_rec).shift(styr);  //assign the estimated rec to year class;  	
 dvar_vector Stmp(styr,lastyr_rec-rec_age); 		// temporary S
 Stmp = sp_biom(styr, lastyr_rec-rec_age);  //assign the ssb
 est_spb = Stmp;				// save the spawning biomass

 pred_rec = SRecruit(Stmp);			// get the predicted recruits
 
 // get the data for the fitted recruitment curve
 dvariable tmpsp=1.1*max(est_spb);
 for (i=1;i<=20;i++)
 {
   SRec_spawn(i)=tmpsp*double(i)/20.;
   SRec_rec(i)=SRecruit(SRec_spawn(i));
 }
 
FUNCTION get_catch_at_age // by using Baranov's Catch Equation//-----------------------------------------------
 
 for (i=styr_fish; i<=endyr_r; i++)
 {
    pred_catch(i) = 0.0;
    for (j=1;j<= nages;j++)
    {
 	catage(i,j) = natage(i,j)*F(i,j)*(1.0-S(i,j))/Z(i,j);
 	pred_catch(i)+=catage(i,j)*wt_fsh(j);
    }
  }
  
 
FUNCTION get_age_comps  // need to apply age error matrices  
 
  for (i=1;i<=nyrs_fish_unbiased_ac_r;i++)
   {
    eac_fish_unbiased_mod(yrs_fish_unbiased_ac_r(i))=catage(yrs_fish_unbiased_ac_r(i))/sum(catage(yrs_fish_unbiased_ac_r(i)));
    eac_fish_unbiased_mod(yrs_fish_unbiased_ac_r(i)) = unbiasedages*eac_fish_unbiased_mod(yrs_fish_unbiased_ac_r(i));
    eac_fish_unbiased_dat(yrs_fish_unbiased_ac_r(i))(1,nages_dat-1) = eac_fish_unbiased_mod(yrs_fish_unbiased_ac_r(i))(1,nages_dat-1);
    eac_fish_unbiased_dat(yrs_fish_unbiased_ac_r(i))(nages_dat) = sum(eac_fish_unbiased_mod(yrs_fish_unbiased_ac_r(i))(nages_dat,nages));
   }
 
  for (i=1;i<=nyrs_fish_lc_r;i++)
   {
    elc_fish_tempages(yrs_fish_lc_r(i))=catage(yrs_fish_lc_r(i))/sum(catage(yrs_fish_lc_r(i)));
    elc_fish(yrs_fish_lc_r(i))=translen*elc_fish_tempages(yrs_fish_lc_r(i));
   }
 
  
   for (i=1;i<=nsrv;i++){
    for (j=1;j<=nyrs_srv_ac_r(i);j++){
      eac_srv_mod(i,yrs_srv_ac_r(i,j))=elem_prod(sel_srv(i,yrs_srv_ac_r(i,j)),elem_prod(  natage(yrs_srv_ac_r(i,j)),  mort(yrs_srv_ac_r(i,j))  ))/
       (sel_srv(i,yrs_srv_ac_r(i,j))*elem_prod(  natage(yrs_srv_ac_r(i,j)),  mort(yrs_srv_ac_r(i,j))));
       eac_srv_mod(i,yrs_srv_ac_r(i,j)) = unbiasedages*eac_srv_mod(i,yrs_srv_ac_r(i,j));
       eac_srv_dat(i,yrs_srv_ac_r(i,j))(1,nages_dat-1) = eac_srv_mod(i,yrs_srv_ac_r(i,j))(1,nages_dat-1);
       eac_srv_dat(i,yrs_srv_ac_r(i,j))(nages_dat) = sum(eac_srv_mod(i,yrs_srv_ac_r(i,j))(nages_dat,nages));
    }
   }

  for (i=1;i<=nsrv;i++){
    for (j=1;j<=nyrs_srv_lc_r(i);j++){
       elc_srv_tempages(i,yrs_srv_lc_r(i,j))=elem_prod(sel_srv(i,yrs_srv_lc_r(i,j)),natage(yrs_srv_lc_r(i,j)))/(sel_srv(i,yrs_srv_lc_r(i,j))*natage(yrs_srv_lc_r(i,j)));
       elc_srv(i,yrs_srv_lc_r(i,j))=translen*elc_srv_tempages(i,yrs_srv_lc_r(i,j)); 
    }
   }

  

  
 //if (sd_phase())
 //{
 //  depletion = totbiom(endyr)/totbiom(styr);
 //  endbiom=totbiom(endyr);
 //}
 

FUNCTION get_binned
  // bin the natage matrix to match the plus group for the data
 
  for (i=styr_rec;i<=endyr_r+1;i++)
    {
       natage_bin(i)(1,nages_dat-1) = natage(i)(1,nages_dat-1);
       natage_bin(i)(nages_dat) = sum(natage(i)(nages_dat,nages)); 
    }
  maturity_bin(1,nages_dat-1) = maturity(1,nages_dat-1);
  maturity_bin(nages_dat) = mean(maturity(nages_dat,nages));
  

  

 


FUNCTION dvariable SRecruit(const dvariable& Stmp)
 RETURN_ARRAYS_INCREMENT();
 dvariable RecTmp;
 switch (sr_type)
 {
    case 1:
      RecTmp = mfexp(mean_log_rec +sigr*sigr*0.5);  // average recruitment
      break;
    case 2:
      RecTmp = alpha*Stmp*(1./(beta + Stmp)); // Beverton-Holt form
      break;
 }
 RETURN_ARRAYS_DECREMENT();
 return RecTmp;

FUNCTION dvar_vector SRecruit(const dvar_vector& Stmp)
 RETURN_ARRAYS_INCREMENT();
 dvar_vector RecTmp(Stmp.indexmin(),Stmp.indexmax());
 
 switch (sr_type)
 {
    case 1:
      RecTmp = mfexp(mean_log_rec +sigr*sigr*0.5);  // average recruitment
      break;
    case 2:
      RecTmp = elem_prod(alpha*Stmp,1./(beta + Stmp)); // Beverton-Holt form
      break;
 }
 RETURN_ARRAYS_DECREMENT();
 return RecTmp;

FUNCTION evaluate_the_objective_function //--------------------------------------------------------------------
 if (fyear_ac_option == 2 && historic_catch > 0.)  // computes the ssq for the historical F 
      histFpen();
 mat_likelihood();
 //cout <<" got mat_like"  << endl;
 rec_likelihood();
 //cout <<" got rec_like"  << endl;
 srv_likelihood();
 //cout <<" got srv_like"  << endl;
 cat_likelihood();
 //cout <<" got cat_like"  << endl;
 Fmort_pen();
 //cout <<" get Fmort_pen"  << endl;
 age_likelihood();
 //cout <<" get age_like"  << endl;
 prior();
 //cout <<" get prior"  << endl;
 sel_likelihood();
 //cout <<" sel_like"  << endl;
 
  
FUNCTION histFpen  // fit the historical catches if neccessary
 
 if (active(historic_F))
  {
  hf_pen = 500.*square(ehc - historic_catch);
  obj_fun += hf_pen;
  }
  
  
FUNCTION rec_likelihood   // fit the recruitment deviations
 
 rec_like.initialize();
 //chi = log(est_rec+1e-8) - log(pred_rec+1e-8) + (sigr*sigr*0.5);  // correct for bias;
 //cout << chi << endl;

 rec_rmse = sqrt(norm2(log(est_rec+1e-8) - log(pred_rec+1e-8))/size_count(est_rec)+1e-13);
 //sumrecdev = sum(chi); 
 
 rec_like(1) = norm2(rec_dev)/(2.*sigr*sigr) + size_count(rec_dev)*log(sigr);
 //rec_like(1) = norm2(chi)/(2.*sigr*sigr) + size_count(chi)*log(sigr);
 //rec_like(2) = square((rmse - sigr)/(sigr*sigr/size_count(chi)))/2. + log(sigr*sigr/size_count(chi)) +
 //   square(sumrecdev/(sigr/size_count(chi)))/2. + log(sigr/size_count(chi));
 
 if (fyear_ac_option == 3)
  rec_like(2) = norm2(fydev)/(2.*sigr*sigr) + size_count(fydev)*log(sigr);

 // fitting the SR curve
 if (sr_type==2){
   chi = log(est_rec+1e-8) - log(pred_rec+1e-8) + (sigr*sigr*0.5);  // correct for bias;
   rec_like(3) = norm2(chi)/(2.*sigr*sigr) + size_count(chi)*log(sigr); 
 }
 

 obj_fun +=lambda(1)*sum(rec_like);
 
 
 //  rec_like+=1.0*norm2(rec_dev_future); // the deviations for the future recruitments 
  
  
FUNCTION age_likelihood  // fit the age comps--------------------------------------------------------------------
 
 age_like=0.;
 age_like_sac=0.;
 age_like_slc=0.;

 int ii;
 int m;

 dvariable tmp1;
 dvariable tmp2;
 dvariable tmp3;
 dvariable tmp4;
 dvariable tmp5;

 fish_effn=0.;
 sac_effn=0.;
 slc_effn=0.;

 if (comp_like_switch==1) {    //  switch for multinomial likelihood function
 
 k = 0;
 for (i=1;i<=nyrs_fish_unbiased_ac_r;i++)
 {
   ii=yrs_fish_unbiased_ac_r(i);
   age_like(1)-=fish_unbiased_ac_samp_r(i)*oac_fish_unbiased_r(i)*log(eac_fish_unbiased_dat(ii)+1.e-13);
   fish_effn(1,i) = eac_fish_unbiased_dat(ii)*(1.-eac_fish_unbiased_dat(ii))/(norm2(eac_fish_unbiased_dat(ii)-oac_fish_unbiased_r(i)));
   for (j=1;j<=nages_dat;j++)
   {
     k = k+1;
     tmp1 = (oac_fish_unbiased_r(i,j)+0.00001) - (eac_fish_unbiased_dat(ii,j) + 0.00001);
     tmp2 = (eac_fish_unbiased_dat(ii,j)+0.00001)*(1.-(eac_fish_unbiased_dat(ii,j)+0.00001)  );
     fac_nr(k) = tmp1/sqrt(tmp2/stg1_fish_unbiased_ac_samp(i));
     fac_resid(k) = tmp1;
     fac_pearson(i,j) = tmp1/sqrt(tmp2/fish_unbiased_ac_samp_r(i));
     fac_pearson_vec(k) = fac_pearson(i,j);  
   }
   tmp3 = ages_dat_mid*oac_fish_unbiased_r(i);   // the mean of the observations 
   tmp4 = ages_dat_mid*eac_fish_unbiased_dat(ii);  // the mean of the predictions
   tmp5 = elem_prod(ages_dat_mid,eac_fish_unbiased_dat(ii))*ages_dat_mid - square(tmp4);  // the v term in Francis method
   fac_nr_fran(i) = (tmp3-tmp4)/sqrt(tmp5/stg1_fish_unbiased_ac_samp(i)); 
 }
 age_like(1)-=offset(1);
  
 k = 0;
 for (i=1;i<=nyrs_fish_lc_r;i++)
 {
   ii=yrs_fish_lc_r(i);
   age_like(2)-=fish_lc_samp_r(i)*olc_fish_r(i)*log(elc_fish(ii)+1.e-13);
   fish_effn(2,i) = elc_fish(ii)*(1.-elc_fish(ii))/(norm2(elc_fish(ii)-olc_fish_r(i)));
   for (j=1;j<=nlen;j++)
   {
     k = k+1;
     tmp1 = (olc_fish_r(i,j)+0.00001) - (elc_fish(ii,j) + 0.00001);
     tmp2 = (elc_fish(ii,j)+0.00001)*(1.-(elc_fish(ii,j)+0.00001)  );
     flc_nr(k) = tmp1/sqrt(tmp2/stg1_fish_lc_samp(i));
     flc_resid(k) = tmp1;
     flc_pearson(i,j) = tmp1/sqrt(tmp2/fish_lc_samp_r(i));
     flc_pearson_vec(k) = flc_pearson(i,j);
   }
   tmp3 = lengths_mid*olc_fish_r(i);   // the mean of the observations 
   tmp4 = lengths_mid*elc_fish(ii);  // the mean of the predictions
   tmp5 = elem_prod(lengths_mid,elc_fish(ii))*lengths_mid - square(tmp4);  // the v term in Francis method
   flc_nr_fran(i) = (tmp3-tmp4)/sqrt(tmp5/stg1_fish_lc_samp(i));
 }
 age_like(2)-=offset(2);

 
 for (i=1;i<=nsrv;i++){
  k = 0;
  for (j=1;j<=nyrs_srv_ac_r(i);j++){
    ii = yrs_srv_ac_r(i,j);
    age_like_sac(i) -= srv_ac_samp_r(i,j)*oac_srv_r(i,j)*log(eac_srv_dat(i,ii)+1.e-13);
    sac_effn(i,j) = eac_srv_dat(i,ii)*(1.-eac_srv_dat(i,ii))/(norm2(eac_srv_dat(i,ii)-oac_srv_r(i,j)));
    for (m=1;m<=nages_dat;m++)
    {
    k = k+1;
     tmp1 = (oac_srv_r(i,j,m)+0.00001) - (eac_srv_dat(i,ii,m) + 0.00001);
     tmp2 = (eac_srv_dat(i,ii,m)+0.00001)*(1.-(eac_srv_dat(i,ii,m)+0.00001)  );
     sac_nr(i,k) = tmp1/sqrt(tmp2/(stg1_srv_ac_samp(i,j)));
     sac_resid(i,k) = tmp1;
     sac_pearson(i,j,m) = tmp1/sqrt(tmp2/(srv_ac_samp_r(i,j)));
     sac_pearson_vec(i,k) = sac_pearson(i,j,m); 
    }
    tmp3 = ages_dat_mid*oac_srv_r(i,j);
    tmp4 = ages_dat_mid*eac_srv_dat(i,ii);
    tmp5 = elem_prod(ages_dat_mid,eac_srv_dat(i,ii))*ages_dat_mid - square(tmp4);  // the v term in Francis method
    sac_nr_fran(i,j) = (tmp3-tmp4)/sqrt(tmp5/stg1_srv_ac_samp(i,j)); 
  }
  age_like_sac(i)-=sac_offset(i);
  obj_fun += srv_age_flag(i)*age_like_sac(i);

  k = 0;
  for (j=1;j<=nyrs_srv_lc_r(i);j++){
    ii = yrs_srv_lc_r(i,j);
    age_like_slc(i) -= srv_lc_samp_r(i,j)*olc_srv_r(i,j)*log(elc_srv(i,ii)+1.e-13);
    slc_effn(i,j) = elc_srv(i,ii)*(1.-elc_srv(i,ii))/(norm2(elc_srv(i,ii)-olc_srv_r(i,j)));
    for (m=1;m<=nlen;m++)
   {
     k = k+1;
     tmp1 = (olc_srv_r(i,j,m)+0.00001) - (elc_srv(i,ii,m) + 0.00001);
     tmp2 = (elc_srv(i,ii,m)+0.00001)*(1.-(elc_srv(i,ii,m)+0.00001)  );
     slc_nr(i,k) = tmp1/sqrt(tmp2/stg1_srv_lc_samp(i,j)); 
     slc_resid(i,k) = tmp1;
     slc_pearson(i,j,m) = tmp1/sqrt(tmp2/srv_lc_samp_r(i,j));
     slc_pearson_vec(i,k) = slc_pearson(i,j,m);
   }
   tmp3 = lengths_mid*olc_srv_r(i,j);
   tmp4 = lengths_mid*elc_srv(i,ii);
   tmp5 = elem_prod(lengths_mid,elc_srv(i,ii))*lengths_mid - square(tmp4);  // the v term in Francis method
   slc_nr_fran(i,j) = (tmp3-tmp4)/sqrt(tmp5/stg1_srv_lc_samp(i,j)); 

  }
  age_like_slc(i)-=slc_offset(i);
  obj_fun += srv_len_flag(i)*age_like_slc(i); 
 } 

 //obj_fun = obj_fun + (age_like(1)*lambda(12) + age_like(2)*lambda(5) +age_like(3)*lambda(6) +age_like(4)*lambda(7));
 //obj_fun = obj_fun + (age_like(1)*lambda(5) +age_like(2)*lambda(6) + age_like(3)*lambda(7) +age_like(4)*lambda(8) +age_like(5)*lambda(9) +age_like(6)*lambda(10));
 

 //obj_fun += (fish_age_flag*age_like(1) + fish_len_flag*age_like(2) + srv_age_flag(1)*age_like_sac(1)  + srv_len_flag(1)*age_like_slc(1)  + srv_age_flag(2)*age_like_sac(2) + srv_len_flag(2)*age_like_slc(2));
 }

 else if (comp_like_switch==2) {     //  switch for D-M likelihood function

 //DM_fish_effn = 0.0;
 //DM_sac_effn = 0.0;
 //DM_slc_effn = 0.0;

  k = 0;
  for (i=1;i<=nyrs_fish_unbiased_ac_r;i++) {   //D-M likelihood for the fishery age comps
    ii=yrs_fish_unbiased_ac_r(i);
    fish_effn(1,i) = (1 + theta_fish(1) * nsamples_fish_unbiased_ac(i))/(1 + theta_fish(1));

    
    for (j=1;j<=nages_dat;j++) {
        k = k+1;
        age_like(1) += gammln(nsamples_fish_unbiased_ac(i) * (oac_fish_unbiased_r(i,j) + 0.00001) + 1) + gammln(theta_fish(1) * nsamples_fish_unbiased_ac(i) * (eac_fish_unbiased_dat(ii,j) + 0.00001)) 
        - gammln(nsamples_fish_unbiased_ac(i) * (oac_fish_unbiased_r(i,j) + 0.00001) + theta_fish(1) * nsamples_fish_unbiased_ac(i) * (eac_fish_unbiased_dat(ii,j) + 0.00001)) 
        + (gammln(nsamples_fish_unbiased_ac(i) + theta_fish(1) * nsamples_fish_unbiased_ac(i)) - gammln(theta_fish(1) * nsamples_fish_unbiased_ac(i)) - gammln(nsamples_fish_unbiased_ac(i) + 1)) / nages_dat;
        
       tmp1 = (oac_fish_unbiased_r(i,j)+0.00001) - (eac_fish_unbiased_dat(ii,j) + 0.00001);
       tmp2 = (eac_fish_unbiased_dat(ii,j)+0.00001)*(1.-(eac_fish_unbiased_dat(ii,j)+0.00001)  );
       fac_resid(k) = tmp1;
       fac_pearson(i,j) = tmp1/sqrt((tmp2/nsamples_fish_unbiased_ac(i))*( (nsamples_fish_unbiased_ac(i)+nsamples_fish_unbiased_ac(i)*theta_fish(1))/(1+nsamples_fish_unbiased_ac(i)*theta_fish(1)))); 
       fac_pearson_vec(k) = fac_pearson(i,j);
    }      
   }

   
  k = 0;
  for (i=1;i<=nyrs_fish_lc_r;i++) {             //D-M likelihood for the fishery length comps
   ii=yrs_fish_lc_r(i);
   fish_effn(2,i) = (1 + theta_fish(2) * nsamples_fish_lc(i))/(1 + theta_fish(2));
   for (j=1;j<=nlen;j++) {
        k = k+1;
        age_like(2) += gammln(nsamples_fish_lc(i) * (olc_fish_r(i,j) + 0.00001) + 1) + gammln(theta_fish(2) * nsamples_fish_lc(i) * (elc_fish(ii,j) + 0.00001)) 
        - gammln(nsamples_fish_lc(i) * (olc_fish_r(i,j) + 0.00001) + theta_fish(2) * nsamples_fish_lc(i) * (elc_fish(ii,j) + 0.00001)) 
        + (gammln(nsamples_fish_lc(i) + theta_fish(2) * nsamples_fish_lc(i)) - gammln(theta_fish(2) * nsamples_fish_lc(i)) - gammln(nsamples_fish_lc(i) + 1)) / nlen;

        tmp1 = (olc_fish_r(i,j)+0.00001) - (elc_fish(ii,j) + 0.00001);
        tmp2 = (elc_fish(ii,j)+0.00001)*(1.-(elc_fish(ii,j)+0.00001)  );
        flc_resid(k) = tmp1;
        flc_pearson(i,j) = tmp1/sqrt((tmp2/nsamples_fish_lc(i))*( (nsamples_fish_lc(i)+nsamples_fish_lc(i)*theta_fish(2))/(1+nsamples_fish_lc(i)*theta_fish(2)))); 
        flc_pearson_vec(k) = flc_pearson(i,j);
   }  
   

  }
 
  for (i=1;i<=nsrv;i++){                        // D-M likelihood for the survey comps
    k = 0;
    for (j=1;j<=nyrs_srv_ac_r(i);j++){          // D-M likelihood for the survey age comps
      ii = yrs_srv_ac_r(i,j);
      sac_effn(i,j) = (1 + theta_sac(i) * nsamples_srv_ac(i,j))/(1 + theta_sac(i));
      for (m=1;m<=nages_dat;m++){
        k = k+1;
        age_like_sac(i) += gammln(nsamples_srv_ac(i,j) * (oac_srv_r(i,j,m) + 0.00001) + 1) + gammln(theta_sac(i) * nsamples_srv_ac(i,j) * (eac_srv_dat(i,ii,m) + 0.00001)) 
        - gammln(nsamples_srv_ac(i,j) * (oac_srv_r(i,j,m) + 0.00001) + theta_sac(i) * nsamples_srv_ac(i,j) * (eac_srv_dat(i,ii,m) + 0.00001)) 
        + (gammln(nsamples_srv_ac(i,j) + theta_sac(i) * nsamples_srv_ac(i,j)) - gammln(theta_sac(i) * nsamples_srv_ac(i,j)) - gammln(nsamples_srv_ac(i,j) + 1)) / nages_dat;
        
        tmp1 = (oac_srv_r(i,j,m)+0.00001) - (eac_srv_dat(i,ii,m) + 0.00001);
        tmp2 = (eac_srv_dat(i,ii,m)+0.00001)*(1.-(eac_srv_dat(i,ii,m)+0.00001)  );
        sac_resid(i,k) = tmp1;
        sac_pearson(i,j,m) = tmp1/sqrt((tmp2/nsamples_srv_ac(i,j))*( (nsamples_srv_ac(i,j)+nsamples_srv_ac(i,j)*theta_sac(i))/(1+nsamples_srv_ac(i,j)*theta_sac(i)))); 
        sac_pearson_vec(i,k) = sac_pearson(i,j,m); 
      }
    }

    obj_fun += srv_age_flag(i)*age_like_sac(i);

    k = 0;
    for (j=1;j<=nyrs_srv_lc_r(i);j++){            // D-M likelihood for the survey length comps
      ii = yrs_srv_lc_r(i,j);
      slc_effn(i,j) = (1 + theta_slc(i) * nsamples_srv_lc(i,j))/(1 + theta_slc(i));
      for (m=1;m<=nlen;m++){  
        k = k+1;
        age_like_slc(i) += gammln(nsamples_srv_lc(i,j) * (olc_srv_r(i,j,m) + 0.00001) + 1) + gammln(theta_slc(i) * nsamples_srv_lc(i,j) * (elc_srv(i,ii,m) + 0.00001)) 
        - gammln(nsamples_srv_lc(i,j) * (olc_srv_r(i,j,m) + 0.00001) + theta_slc(i) * nsamples_srv_lc(i,j) * (elc_srv(i,ii,m) + 0.00001)) 
        + (gammln(nsamples_srv_lc(i,j) + theta_slc(i) * nsamples_srv_lc(i,j)) - gammln(theta_slc(i) * nsamples_srv_lc(i,j)) - gammln(nsamples_srv_lc(i,j) + 1)) / nlen;
        
        tmp1 = (olc_srv_r(i,j,m)+0.00001) - (elc_srv(i,ii,m) + 0.00001);
        tmp2 = (elc_srv(i,ii,m)+0.00001)*(1.-(elc_srv(i,ii,m)+0.00001)  );
        slc_resid(i,k) = tmp1;
        slc_pearson(i,j,m) = tmp1/sqrt((tmp2/nsamples_srv_lc(i,j))*( (nsamples_srv_lc(i,j)+nsamples_srv_lc(i,j)*theta_slc(i))/(1+nsamples_srv_lc(i,j)*theta_slc(i))));
        slc_pearson_vec(i,k) = slc_pearson(i,j,m);
    }   
  }
  
  obj_fun += srv_len_flag(i)*age_like_slc(i);                                               

  }                                               // end of loop for D-M survey comps
 }                                                // end of loop for switch between D-M and multinomial
 obj_fun += (fish_age_flag*age_like(1) + fish_len_flag*age_like(2));

 
FUNCTION dvariable update_compweight_ta11(const int& nyrs,const dvector& yrs,const dvector& samp, const dmatrix& obs,const dvar_matrix& est)

 RETURN_ARRAYS_INCREMENT();
 dvariable tmp6;
 dvariable tmp7;
 int ii;
 int z;
 dvar_vector wgt(1,nyrs);
 dvar_vector wgt_inv(1,nyrs);
 dvariable newcompweight_ta11;

 for (z=1;z<=nyrs;z++)
 {
   ii=yrs(z);
   tmp6 = (est(ii)+0.00001)*(1.-(est(ii)+0.00001));
   tmp7 = ((obs(z)+0.00001) - (est(ii) + 0.00001))*
             ((obs(z)+0.00001) - (est(ii) + 0.00001));
   wgt(z) = (tmp6/tmp7)/samp(z);
   wgt_inv(z) =  1.0/wgt(z);
   }
      if (har_flag==1) newcompweight_ta11 = 1.0/mean(wgt_inv);
          else   newcompweight_ta11 = mean(wgt);    
        
 RETURN_ARRAYS_DECREMENT();
 return newcompweight_ta11;


  


FUNCTION prior  // compute the prior parts for q_surv3 and M--------------------------------------------------------------------
 
 prior_M=0.;
 prior_q=0.;

 if (active(log_avg_M))
 {  
   if(switch_prior_M == 1) 
    {
      for (i=1;i<=nbins_M;i++)
        prior_M(i) = square((log_avg_M+M_devs(i)) - log(priormean_M(i)) + square(priorcv_M(i))/2.0)/(2.*square(priorcv_M(i)));
    }
   if (switch_M_devs_pen == 1 && active(M_devs))
    {
      M_dev_pen = norm2(M_devs)/(2.*sigma_M_devs*sigma_M_devs);
    }
 }     
 

 for (i=1;i<=nsrv;i++)
   {
    if (switch_prior_q(i)== 1 && active(log_q_srv(i))) 
      {
       for (j=1;j<=nbins_q(i);j++)
         prior_q(i,j) = square(log_q_srv(i,j) - log(priormean_q(i,j)) + square(priorcv_q(i,j))/2.0)/(2.*square(priorcv_q(i,j))); 
     }
   }
 
 obj_fun += (sum(prior_M) + sum(prior_q));
 obj_fun += M_dev_pen;
 

FUNCTION dvariable get_spr(dvariable Ftemp)    // calculation of equilibrium SPR for equilibrium recruitment
 
  dvariable phi;
  dvar_vector Ntmp(1,nages_dat);
  

  Ntmp(1)=1.;
  for (j=2;j<=nages_dat;j++)
     Ntmp(j)=Ntmp(j-1)*exp(-(recent_M+Ftemp*recent_fish_sel(j-1)));  // fills in matrix for ages 2 through nages_dat-1           
   Ntmp(nages_dat)=Ntmp(nages_dat-1)*exp(-(recent_M+Ftemp*recent_fish_sel(nages_dat-1)))/(1.-exp(-(recent_M+Ftemp*recent_fish_sel(nages_dat))));
  
   // Kill them off until they spawn
  for (j=1;j<=nages_dat;j++) 
   phi = 0.5*elem_prod(Ntmp,maturity_bin)*elem_prod(wt_pop_bin,exp(-spmo_frac*(recent_M+Ftemp*recent_fish_sel)));

   return(phi);
   

FUNCTION dvariable get_spr_rates(double spr_percent)
 
  dvariable df=1.e-8;
  dvariable F1;
  dvariable dd;
  F1 = 0.0;
  if (recent_M<0.2)  
    F1 = 1.2*recent_M*(1-spr_percent);
  else
    F1 = 0.5*recent_M*(1-spr_percent);
  dvariable F2;
  dvariable F3;
  dvariable yld1;
  dvariable yld2;
  dvariable yld3;
  dvariable dyld;
  dvariable dyldp;
  // Newton Raphson stuff to go here
  //for (int ii=1;ii<=12;ii++)
  dd = 1.0;
  while (dd > 1.e-10)   
 {
    F2     = F1 + df;
    F3     = F1 - df;
    yld1   = -1000*square(log(spr_percent/(get_spr(F1)/SB0)));
    yld2   = -1000*square(log(spr_percent/(get_spr(F2)/SB0)));
    yld3   = -1000*square(log(spr_percent/(get_spr(F3)/SB0)));
    dyld   = (yld2 - yld3)/(2*df);                          // First derivative (to find the root of this)
    //dyldp  = (yld3-(2*yld1)+yld2)/(df*df);  // Newton-Raphson approximation for second derivitive
    //F1    -= dyld/dyldp;
    F1 -= yld1/dyld;
    dd = fabs(log(spr_percent/(get_spr(F1)/SB0)));  
  }
  return(F1); 
  

FUNCTION srv_likelihood  // fit to indices (lognormal) ---------------------------------------------------------
  
 srv_bio_like=0;
 srv_bio_rmse=0;
 srv_abun_like=0;
 srv_abun_rmse=0;

 int ii;
 
  for (s=1;s<=nsrv;s++)
  {
   for (i=1;i<=nyrs_srv_bio_abun_r(s);i++)  // likelihood, by survey
     {
      ii=yrs_srv_bio(s,i);
      srv_bio_like(s) += square(log(obs_srv_bio(s,i)+1e-13) - log(pred_srv_bio(s,ii)+1e-13))/(2.*cv_bio_srv(s,i)*cv_bio_srv(s,i));
      srv_abun_like(s) += square(log(obs_srv_abun(s,i)+1e-13) - log(pred_srv_abun(s,ii)+1e-13))/(2.*cv_abun_srv(s,i)*cv_abun_srv(s,i));

      survey_bio_nr(s,i) = (log(obs_srv_bio(s,i)+1e-13) - log(pred_srv_bio(s,ii)+1e-13))/cv_bio_srv(s,i);
      survey_abun_nr(s,i) = (log(obs_srv_abun(s,i)+1e-13) - log(pred_srv_abun(s,ii)+1e-13))/cv_abun_srv(s,i);


      srv_bio_rmse(s) += square(log(obs_srv_bio(s,i)+1e-13) - log(pred_srv_bio(s,ii)+1e-13));
      srv_abun_rmse(s) += square(log(obs_srv_abun(s,i)+1e-13) - log(pred_srv_abun(s,ii)+1e-13));

     }
     if(nyrs_srv_bio_abun_r(s)>0)
     {
      srv_bio_rmse(s) = sqrt(srv_bio_rmse(s)/nyrs_srv_bio_abun_r(s));
      srv_abun_rmse(s) = sqrt(srv_abun_rmse(s)/nyrs_srv_bio_abun_r(s));
      survey_bio_sdnr(s) = std_dev(survey_bio_nr(s));
      survey_abun_sdnr(s) = std_dev(survey_abun_nr(s));
     }
     obj_fun+= srv_bio_flag(s)*srv_bio_like(s);
     obj_fun+= srv_abun_flag(s)*srv_abun_like(s);    
  }

 //obj_fun+= lambda(2)*surv_like(1) + lambda(15)*surv_like(2);
 //obj_fun+= srv_bio_flag(1)*srv_bio_like(1) + srv_bio_flag(2)*srv_bio_like(2);
 
 
FUNCTION cat_likelihood  // fit the catches -------------------------------------------------------------
 
 catch_like=norm2(log(catch_bio(styr_fish,endyr_r)+0.0001) - log(pred_catch+0.00001));
 obj_fun+=fish_bio_flag*lambda(2)*catch_like;
 
 
 
 
 
FUNCTION Fmort_pen  // Phases less than 2, penalize high F's ---------------------------------------------
 
 fpen = 0.0;
 //if (current_phase()<2)
 //  fpen=10.*norm2(mfexp(fmort_dev+log_avg_fmort)-1.0);
 //else
 //  fpen=.01*norm2(mfexp(fmort_dev+log_avg_fmort)-.2);

 //if (active(fmort_dev))
   fpen+= 0.1*norm2(fmort_dev);
 
 //fpen+=100*square(avg_fmort_dev);
 obj_fun+=fpen;


 
 

FUNCTION sel_likelihood // penalty for smoothness on time-varying sel parameters
 
 sel_like = 0.0;
 dvariable z = 0.0;
 dvar_matrix trans_log_sel_fish = trans(log_sel_fish);
     

 //**** fishery selectivity
 // for logistic and double logistic curves with time-varying parameters, penalize the param devs
 if (active(fsh_a50_devs))
 {
     sel_like(1) = norm2(fsh_a50_devs)/(2.*fsh_sigma_a50*fsh_sigma_a50);   
 }

 if (active(fsh_aslope_devs))
 {
     sel_like(2) = norm2(fsh_aslope_devs)/(2.*fsh_sigma_aslp*fsh_sigma_aslp);
 }
 if (active(fsh_d50_devs))
 {
     sel_like(3) = norm2(fsh_d50_devs)/(2.*fsh_sigma_d50*fsh_sigma_d50);   
 }

 if (active(fsh_dslope_devs))
 {
     sel_like(4) = norm2(fsh_dslope_devs)/(2.*fsh_sigma_dslp*fsh_sigma_dslp);
 }
 // for double logistic  and bicubic spline, penalize the dome-shape, 

  if (fsh_sel_option==2 || fsh_sel_option==3 || fsh_sel_option==4)
   {
    for (i=fsh_sel_styr;i<=fsh_sel_endyr;i++)
     {
       for (j=1;j<=nselages-1;j++)
        {
         if (log_sel_fish(i,j)>log_sel_fish(i,j+1))
           {
             sel_like(5) += lambda(3)*square(log_sel_fish(i,j)-log_sel_fish(i,j+1));  // penalize the dome shape
           }
        }
      }
    }
  // for bicubic spline and cubic spline, penalize the smoothness across ages and years, and interannual differences 
 if(fsh_sel_option==3 || fsh_sel_option==4 )
  {                   // the smoothness penalty (across ages)
   for (i=fsh_sel_styr;i<=fsh_sel_endyr;i++)
     {
       z = mean(log_sel_fish(i));
       sel_like(9) += 10000*z*z;   
       dvar_vector df2 = first_difference(first_difference(log_sel_fish(i)));
       sel_like(6) += lambda(4)/nselages*df2*df2;    
     }
 
   for (j=1;j<=nselages;j++)
     {
       dvar_vector df1 = first_difference(trans_log_sel_fish(j));
       sel_like(7) += lambda(5)/(fsh_sel_endyr-fsh_sel_styr+1)*df1*df1;  // the penalty for interannual variation

       dvar_vector df2 = first_difference(df1);           // the penalty for smoothness over time
       sel_like(8) += lambda(6)/(fsh_sel_endyr-fsh_sel_styr+1)*df2*df2; 
     }
  }    

 

 //**** survey selectivity
 // for logistic and double logistic curves with time-varying parameters, penalize the param devs
 for (s=1;s<=nsrv;s++)
  {
   if (active(srv_a50_devs(s)))
     {
       sel_like(10) += norm2(srv_a50_devs(s))/(2.*srv_sigma_a50(s)*srv_sigma_a50(s));   
     }
   if (active(srv_aslope_devs(s)))
     {
       sel_like(11) = norm2(srv_aslope_devs(s))/(2.*srv_sigma_aslp(s)*srv_sigma_aslp(s));
      }
   if (active(sel_srv_mu_devs(s)))
     {
       sel_like(12) = norm2(sel_srv_mu_devs(s))/(2.*srv_sigma_mu(s)*srv_sigma_mu(s));
      }
   if (active(sel_srv_dist_devs(s)))
     {
       sel_like(13) = norm2(sel_srv_dist_devs(s))/(2.*srv_sigma_dist(s)*srv_sigma_dist(s));
      }
   if (active(sel_srv_sig1_devs(s)))
     {
       sel_like(14) = norm2(sel_srv_sig1_devs(s))/(2.*srv_sigma_sig1(s)*srv_sigma_sig1(s));
      }
   if (active(sel_srv_sig2_devs(s)))
     {
       sel_like(15) = norm2(sel_srv_sig2_devs(s))/(2.*srv_sigma_sig2(s)*srv_sigma_sig2(s));
      }
   // for bicubic spline and cubic spline, penalize the smoothness across ages and years, and interannual differences 
 if(srv_sel_option(s)==3 || srv_sel_option(s)==4 )
  {                   // the smoothness penalty (across ages)
   for (i=srv_sel_styr;i<=srv_sel_endyr;i++)
     {
       z = mean(log_sel_srv(s,i));
       sel_like(16) += 10000*z*z;   
       dvar_vector df2 = first_difference(first_difference(log_sel_srv(s,i)));
       sel_like(17) += lambda(4)/nselages*df2*df2;    
     }
 
   dvar_matrix trans_log_sel_srv = trans(log_sel_srv(s));
   for (j=1;j<=nselages;j++)
     {
       dvar_vector df1 = first_difference(trans_log_sel_srv(j));
       sel_like(18) += lambda(5)/(srv_sel_endyr-srv_sel_styr+1)*df1*df1;  // the penalty for interannual variation

       dvar_vector df2 = first_difference(df1);           // the penalty for smoothness over time
       sel_like(19) += lambda(6)/(srv_sel_endyr-srv_sel_styr+1)*df2*df2; 
     }
  }
 // for double normal  and bicubic spline, penalize the dome-shape, 

  if (srv_sel_option(s)==2 || srv_sel_option(s)==3 || srv_sel_option(s)==4)
   {
      for (i=srv_sel_styr;i<=srv_sel_endyr;i++)
       {
         for (j=1;j<=nselages-1;j++)
          {
           if (log_sel_srv(s,i,j)>log_sel_srv(s,i,j+1))
             {
               sel_like(20) += lambda(3)*square(log_sel_srv(s,i,j)-log_sel_srv(s,i,j+1));  // penalize the dome shape
             }
          }
        }
    }



  


  } 
  
 



 obj_fun += sum(sel_like);
 
 

FUNCTION mat_likelihood  // fit the maturity curve
 
 
 if(mat_input_switch!=1)
 {
 mat_like = 0.0;
 int ii;

 
 for (j=1;j<=nmat_datasets;j++)
 {
   for (i=1;i<=nages_mat(j);i++)
   {
    ii = ages_mat(j,i) - rec_age +1;
    mat_like += -0.01*mat_lambda(ii)*(y_mat(j,i)*log(mat_theta(ii)) + (n_mat(j,i) - y_mat(j,i))*log(1.-mat_theta(ii)+1e-15)); // -ln like
   }
 }

 //for (i=1;i<=nages_S;i++)
 //{
 //ii = ages_S(i) - rec_age +1;
 //mat_like += -0.01*mat_lambda(ii)*(S_y(i)*log(mat_theta(ii)) + (S_n(i) - S_y(i))*log(1.-mat_theta(ii)+1e-15)); // -ln like, Shawdata
 //}
 //}
 

 obj_fun += mat_like;
 }
 
 
FUNCTION dvar_vector cubic_spline(const dvar_vector& spline_coffs)
  {
  
  RETURN_ARRAYS_INCREMENT();
  int nodes=size_count(spline_coffs);
  dvector ia(1,nodes);
  dvector fa(1,nselages);
  ia.fill_seqadd(0,1./(nodes-1));
  fa.fill_seqadd(0,1./(nselages-1));
  vcubic_spline_function ffa(ia,spline_coffs);
  RETURN_ARRAYS_DECREMENT();
  
 
  return(ffa(fa));
  
  }


 
FUNCTION comp_metrics // get metrics like rmse, sndr, effn, and weights sample sizes for the compostion data  
 if (nyrs_fish_unbiased_ac_r>0)
 {
    fish_rmse(1) = sqrt(mean(elem_prod(fac_resid,fac_resid)));
    fish_mean_effn(1) =  (sum(fish_effn(1)))/nyrs_fish_unbiased_ac_r;  
    fish_sdnr(1) = std_dev(fac_pearson_vec);  
    if (comp_like_switch==1){
    fish_mean_samp_wts(1) =  (sum(fish_unbiased_ac_samp_r))/nyrs_fish_unbiased_ac_r;
    }
     
 }
   

 if (nyrs_fish_lc_r>0)
 {
   fish_rmse(2) = sqrt(mean(elem_prod(flc_resid,flc_resid)));
   fish_mean_effn(2) =  (sum(fish_effn(2)))/nyrs_fish_lc_r;
   fish_sdnr(2) = std_dev(flc_pearson_vec);
   if (comp_like_switch==1){
   fish_mean_samp_wts(2) =  (sum(fish_lc_samp_r))/nyrs_fish_lc_r;
   }
 }
 
 for (i=1;i<=nsrv;i++){
  if (nyrs_srv_ac_r(i)>0){
    sac_rmse(i) = sqrt(mean(elem_prod(sac_resid(i),sac_resid(i))));
    sac_mean_effn(i) =  (sum(sac_effn(i)))/nyrs_srv_ac_r(i);
    sac_sdnr(i) = std_dev(sac_pearson_vec(i));
    if (comp_like_switch==1){
    sac_mean_samp_wts(i) =  (sum(srv_ac_samp_r(i)))/nyrs_srv_ac_r(i);
    }
    }   
 }
 

 for (i=1;i<=nsrv;i++){
  if (nyrs_srv_lc_r(i)>0){
    slc_rmse(i) = sqrt(mean(elem_prod(slc_resid(i),slc_resid(i))));
    slc_mean_effn(i) =  (sum(slc_effn(i)))/nyrs_srv_lc_r(i);
    slc_sdnr(i) = std_dev(slc_pearson_vec(i));
    if (comp_like_switch==1){
    slc_mean_samp_wts(i) =  (sum(srv_lc_samp_r(i)))/nyrs_srv_lc_r(i);
    }
    }  
 }
 


 //FUNCTION future_projections
 // Start calculation for first year of numbers at age matrix in projection
 //nage_future(styr_fut)(2,nages)=++elem_prod(natage(endyr)(1,nages-1),S(endyr)(1,nages-1));
 //nage_future(styr_fut,nages)+=natage(endyr,nages)*S(endyr,nages);
 // Set future total catch biomass to zero
 //catch_future = 0.;
 //biomass_future = 0.;
 //ssb_future = 0.;
 // Compute recent F levels
 //mean_recent_fs = mfexp(sum(fmort_dev(endyr-4,endyr))/5 + log_avg_fmort);

 
// Loop to cycle different fishing mortality values through the projections
 //for (int l=1;l<=num_proj_Fs;l++)
 // {
 //    switch(l)
 //      {
 //        case 1:
 //         ftmp = F40;
 //         break;
 //        case 2:
 //         ftmp = F40/2.0;
 //         break;
 //        case 3:
 //         ftmp = F30;
 //         break;
 //        case 4:
 //         ftmp = mean_recent_fs;
 //         break; 
 // 	 case 5:
 //         ftmp = 0.0;
 //         break;
 	
 //	}
   
  // Calculation of future F's, Z and survival (S)
  //Z_future = M;
  //for (i=styr_fut;i<=endyr_fut;i++)
  //{
  //  F_future(i) = sel_fish*ftmp;
  //  Z_future(i) +=F_future(i);
  //  S_future(i) = exp(-Z_future(i));
  //}
  // Calculation of future recruitment and spawners
  //Mean average recruitment of the time-series is used for projection
  //  dvariable Rectmp=mfexp(mean_log_rec);
  //  for (i=styr_fut;i<endyr_fut;i++)
  //  {
  //   nage_future(i,1) = Rectmp*mfexp(rec_dev_future(i));
    
    // Now graduate for the next year
   //   nage_future(i+1)(2,nages) = ++elem_prod(nage_future(i)(1,nages-1),S_future(i)(1,nages-1));
   //   nage_future(i+1,nages) += nage_future(i,nages)*S_future(i,nages);
   // }
   // nage_future(endyr_fut,1)= Rectmp*mfexp(rec_dev_future(i));
  
// Calculation of catch at predicted future age composition
    //for (i=styr_fut; i<=endyr_fut;i++)
    //{
      //catage_future(i) = 0.;
      //catage_future(i) += elem_prod(nage_future(i),
        //                  elem_prod(F_future(i),
        //                  elem_div ((1.-S_future(i)),Z_future(i))));
      //if (l!=num_proj_Fs) catch_future(l,i) += catage_future(i)*wt_pop;
      //biomass_future(l,i) += nage_future(i)*wt_pop;
      //ssb_future(l,i) += (nage_future(i)/2)*elem_prod(wt_pop,maturity);
    //}
 //}
FUNCTION get_age10
 // get the age at which the survey selectivity exceeds 10% (as potentially modified by the natural mortality rate)
  

  dvector tmp;
  
  
  tmp = value(sel_srv(1,endyr_r)) - 0.10;

  //cout << "in get_age10, tmp is " << tmp << endl;


  
  for (j=2;j<=nages;j++)
    {
     if(tmp(j-1) < 0.0 &  tmp(j) >= 0 )  firstage = ages(j); 
    }

    //cout << "first age is "<< firstage  << endl;

    if (firstage >=25) firstage = 25;
    firstage += round(0.05/average_M);  // modify by the natural morality
    excludeage = firstage -1;          // exclude ages at and below excludeage

  
     
FUNCTION update_compweights   // update if the comp is estmated, otherwise carry over previous comp weight
  
  
  // *_ta11 -- McAllister-Ianelli weights (method TA11 in Francis 2011)
  // *_ta12 -- weight by inverse of variance of normalized resids (Method TA1.2 in Francis 2011)
  // *_ta18 -- The weights for the Francis method

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r > 0 ) 
     { 
      compweightsnew_ta11_fsh(1) = update_compweight_ta11(nyrs_fish_unbiased_ac_r,yrs_fish_unbiased_ac_r,stg1_fish_unbiased_ac_samp,oac_fish_unbiased_r,eac_fish_unbiased_dat);    
      compweightsnew_ta12_fsh(1) = 1./var(fac_nr);
        if (nyrs_fish_unbiased_ac_r >1) compweightsnew_ta18_fsh(1) = 1/(var(fac_nr_fran)*((nyrs_fish_unbiased_ac_r - 1.0)/(nyrs_fish_unbiased_ac_r*1.0)));
        else compweightsnew_ta18_fsh(1) = 1/(var(flc_nr_fran)*((nyrs_fish_lc_r - 1.0)/(nyrs_fish_lc_r*1.0)));   // if only one data point, pair with flc  
     }
   else 
   {
     compweightsnew_ta11_fsh(1) = compweights_fsh(1);
     compweightsnew_ta12_fsh(1) = compweights_fsh(1);
     compweightsnew_ta18_fsh(1) = compweights_fsh(1);
   }
  
  if (fish_len_flag>0 && nyrs_fish_lc_r > 0 ) 
     { 
      compweightsnew_ta11_fsh(2) = update_compweight_ta11(nyrs_fish_lc_r,yrs_fish_lc_r,stg1_fish_lc_samp, olc_fish_r,elc_fish);
      compweightsnew_ta12_fsh(2) = 1./var(flc_nr);
        if (nyrs_fish_lc_r >1) compweightsnew_ta18_fsh(2) = 1/(var(flc_nr_fran)*((nyrs_fish_lc_r - 1.0)/(nyrs_fish_lc_r*1.0)));
        else compweightsnew_ta18_fsh(2) = 1/(var(fac_nr_fran)*((nyrs_fish_unbiased_ac_r - 1.0)/(nyrs_fish_unbiased_ac_r*1.0)));  // if only one data point, pair with fac 
     }
   else 
   {
     compweightsnew_ta11_fsh(2) = compweights_fsh(2);
     compweightsnew_ta12_fsh(2) = compweights_fsh(2);
     compweightsnew_ta18_fsh(2) = compweights_fsh(2);
   }
  
   for (i=1;i<=nsrv;i++){
   if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) 
     { 
      compweightsnew_ta11_sac(i) =  update_compweight_ta11(nyrs_srv_ac_r(i),yrs_srv_ac_r(i),stg1_srv_ac_samp(i),  oac_srv_r(i)  ,eac_srv_dat(i)  );
      compweightsnew_ta12_sac(i) = 1./var(sac_nr(i));  
        if (nyrs_srv_ac_r(i) >1) compweightsnew_ta18_sac(i) = 1/(var(sac_nr_fran(i))*((nyrs_srv_ac_r(i) - 1.0)/(nyrs_srv_ac_r(i)*1.0)));
        else compweightsnew_ta18_sac(i) = 1/(var(slc_nr_fran(i))*((nyrs_srv_lc_r(i) - 1.0)/(nyrs_srv_lc_r(i)*1.0)));  // if only one data point, pair with slc   
     }
   else 
   {
     compweightsnew_ta11_sac(i) = compweights_sac(i);
     compweightsnew_ta12_sac(i) = compweights_sac(i);
     compweightsnew_ta18_sac(i) = compweights_sac(i);
   }
  }

 
  for (i=1;i<=nsrv;i++){
   if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) 
     { 
      compweightsnew_ta11_slc(i) =  update_compweight_ta11(nyrs_srv_lc_r(i),yrs_srv_lc_r(i),stg1_srv_lc_samp(i),  olc_srv_r(i)  ,elc_srv(i)  );
      compweightsnew_ta12_slc(i) = 1./var(slc_nr(i));
        if (nyrs_srv_lc_r(i) >1) compweightsnew_ta18_slc(i) = 1/(var(slc_nr_fran(i))*((nyrs_srv_lc_r(i) - 1.0)/(nyrs_srv_lc_r(i)*1.0)));
        else compweightsnew_ta18_slc(i) = 1/(var(sac_nr_fran(i))*((nyrs_srv_ac_r(i) - 1.0)/(nyrs_srv_ac_r(i)*1.0)));  // if only one data point, pair with slc   
     }
   else 
   {
     compweightsnew_ta11_slc(i) = compweights_slc(i);
     compweightsnew_ta12_slc(i) = compweights_slc(i);
     compweightsnew_ta18_slc(i) = compweights_slc(i);
   }
  }

FUNCTION get_jitter_err
 //--------------------------------------------------------------------------------------------
 // get the errors for jittering
       random_number_generator rng(jitter_seed);  
       jitterstdnorm.fill_randn(rng);      // get the standard normal errors for jittering
       srand(jitter_seed);
       
    for (i=1;i<= 100;i++) 
      {
          jittererr(i) = jitter_stddev*jitterstdnorm(i);
      }


FUNCTION double apply_jitter(const double& start_num, const int& phase,const int& mult_switch,int& err_count)
 RETURN_ARRAYS_INCREMENT();
 double new_start_num;
 if (phase<0)
  {
     new_start_num = start_num;
  } else {
      if (mult_switch==1)
       {
         new_start_num = start_num*exp(jittererr(err_count) - jitter_stddev*jitter_stddev*0.5);
       } else {
         new_start_num = start_num + jittererr(err_count);
       }  
   err_count += 1;
   }    
 RETURN_ARRAYS_DECREMENT();
 return new_start_num;
  
       

REPORT_SECTION //-------------------------------------------------------------------------------------------
 //cout << "start of report section " << endl;
 //report <<"Final gradient is "<<objective_function_value::pobjfun->gmax << endl;

 get_jitter_err();
 report << "the jitter errors are " << endl;
 report << jittererr << endl;


 int m;
 rescaled_F = value(mfexp(log_avg_fmort + fmort_dev));

  for (i=styr;i<=endyr_r;i++)
      {
        rescaled_sel_fish(i) = value(sel_fish(i)(1,nages_dat));
        rescaled_F(i) = rescaled_F(i)*max(rescaled_sel_fish(i));
        rescaled_sel_fish(i) = rescaled_sel_fish(i)/max(rescaled_sel_fish(i));
        exp_biom(i) = elem_prod(natage_bin(i),wt_pop_bin)*rescaled_sel_fish(i);
      }
  
  //cout << " before the survey selecitvity_ndat  " << endl;

  for (s=1;s<=nsrv;s++)
  {
   for (i=styr;i<=endyr_r;i++)
      {
         srv_sel_ndat(s,i) = value(sel_srv(s,i)(1,nages_dat));
      }
  }

  //cout << " after the survey selecitvity_ndat  " << endl;         

  for (j=1;j<=nages_dat;j++)
       recent_fish_sel(j) = mean(column(rescaled_sel_fish,j)(endyr_r-4,endyr_r));
  recent_M = mean(value(M(endyr_r-4,endyr_r)));     
  average_M = mean(value(M(styr,endyr_r)));
  //cout << " after the average M  " << endl;             

  // the last year of recruitment to meet the survey a10 condition 
  double lastyr_rec_a10;
  get_age10();
  lastyr_rec_a10 = endyr_r - max(fixedrec_yrs,excludeage - (rec_age - 1));
  //cout << " after the lastyr_rec_a10  " << endl;
  //cout << "lastyr_rec_age_10 is " << lastyr_rec_a10 << endl;

  // numbers at age with mean recruitments for the survey age10 year classes
   dvar_matrix natage_mean(lastyr_rec_a10+1,endyr_r+1,1,nages);    
   //cout << " b4 get_proj_numbers  " << endl;
   natage_mean = get_projection_numbers_at_age();
   //cout << " b4 the spr stuff  " << endl;

 SB0 = get_spr(0.0);
 F40 = get_spr_rates(0.4);
 F35 = get_spr_rates(0.35);
 F30 = get_spr_rates(0.30);
 SBF40 = get_spr(F40);
 SBF35 = get_spr(F35);
 SBF30 = get_spr(F30);

 //cout << " after the spr stuff  " << endl;
 

 

 

 report << "the firstage is "<< firstage <<endl;
 report << "the excludeage is "<< excludeage <<endl;
 report << "the last year of recruitment is "  <<lastyr_rec << endl;
 report << "the last year of recruitment for a10 is "  << lastyr_rec_a10 << endl;
 report << "the first year where we do the new projection is " << lastyr_rec_a10+1 << endl; 
 report << "Total number of fish: years " <<styr<<" to " << endyr_r+1<< endl;
  report << rowsum(natage) << endl;
  report << "Numbers of fish: ages "  <<ages_dat(1)<<" to " << ages_dat(nages_dat)<< endl;
  for (i=styr;i<= endyr_r+1;i++)
      report << i <<" "<<natage_bin(i) << endl;
  report << "Number in first year: ages "<<ages_dat(1)<<" to " << ages_dat(nages_dat)<< endl;
  report << natage_bin(styr) << endl;
  report << "Number in end year: ages " <<ages_dat(1)<<" to " << ages_dat(nages_dat)<< endl;
  report << natage_bin(endyr_r) << endl;
  report << "Number in year "<<endyr_r<< ", with mean for age10 yc:" << ages(1) <<" to "<< ages(nages) << endl;
  report << natage_mean(endyr_r) << endl;
  report << "SSB and recruitment for SR fitting" << endl;
  report << "Recruitments (age "<<rec_age<<"): years "<<styr+rec_age<<" to " << lastyr_rec << endl;
  report << est_rec(styr,lastyr_rec-rec_age) << endl;
  report << "Spawner biomass: years "<<styr<<" to " << lastyr_rec-rec_age << endl;
  report << est_spb(styr,lastyr_rec-rec_age) << endl;
  report << "time series of recruitment : years "<<styr<<" to " << lastyr_rec << endl;
  report << column(natage,1)(styr,lastyr_rec) << endl;
  report << "time series of recruitment : years "<<styr<<" to " << lastyr_rec << endl;
  report << column(natage,1)(styr,lastyr_rec) << endl;
  report << "time series spawner biomass: years "<<styr<<" to " << endyr_r << endl;
  report << sp_biom(styr,endyr_r) << endl;
   report << "SR curve SSB: seq(1,20)" << endl;
  report << SRec_spawn << endl;
  report << "SR curve recs: seq(1,20)"  << endl;
  report << SRec_rec << endl;
  
  for (i=1;i<=nsrv;i++){
      report <<srvname(i)<<": survey selectivity" <<ages_dat<< endl;
      report << srv_sel_ndat(i) << endl;
    }
  
  for (i=1;i<=nsrv;i++){
      report <<srvname(i)<<" observed survey biomass: years " <<yrs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" lower CI: years " <<yrs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_bio_lower(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" upper CI: years " <<yrs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_bio_upper(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" predicted biomass: years "<<styr_fish<<" to " << endyr_r << endl;
      report << pred_srv_bio(i) << endl;
    }

  for (i=1;i<=nsrv;i++){
      report <<srvname(i)<<" observed survey abundance: years " <<yrs_srv_abun(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_abun(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" lower abundance CI: years " <<yrs_srv_abun(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_abun_lower(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" upper abundance CI: years " <<yrs_srv_abun(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report << obs_srv_abun_upper(i)(1,nyrs_srv_bio_abun_r(i)) << endl;
      report <<srvname(i)<<" predicted abundance: years "<<styr_fish<<" to " << endyr_r << endl;
      report << pred_srv_abun(i) << endl;
    }    
  report << "Fishing mortality: years "<<styr<<" to " << endyr_r << endl;
  report << rescaled_F << endl;
 
  report << "Total biomass: years "<<styr_rec<<" to " << endyr_r+1 << endl;
  report << totbiom << endl;
  report << "Exploitable biomass: years "<<styr<<" to " << endyr_r << endl;
  report << exp_biom << endl;
  report << "Observed catch Biomass: years "<<styr_fish<<" to " << endyr_r << endl;
  report << catch_bio(styr_fish,endyr_r) << endl;
  report << "Predicted catch Biomass: years "<<styr_fish<<" to " << endyr_r << endl;
  report << pred_catch << endl;
  report << "Estimated historical catch: 'est_hist_catch'"  << endl;
  report << ehc << endl;

  
  
  if (comp_like_switch == 1) 
  {
  report << "Observed Prop(fishery lengths): year, sample_size, effn, lengths " <<lengths<< endl;
        for (i=1;i<=nyrs_fish_lc_r; i++)
  {          
    if (fish_lc_samp_r(i)>1)
            {
                  report << yrs_fish_lc_r(i)<<" "<<fish_lc_samp_r(i)<<" "<<fish_effn(2,i)<<"  "<<olc_fish_r(i)<< endl;
            }
  }
  report << "Predicted Prop(fishery lengths): year, sample_size, effn, lengths " <<lengths<< endl;
    for (i=1;i<=nyrs_fish_lc_r; i++)
  {
                  report << yrs_fish_lc_r(i)<<" "<<fish_lc_samp_r(i)<<" "<<fish_effn(2,i)<<"  "<<elc_fish(yrs_fish_lc_r(i))<< endl;
  }
 }

 if (comp_like_switch == 2) 
  {
  report << "Observed Prop(fishery lengths): year, sample_size, theta, effn, lengths " <<lengths<< endl;
        for (i=1;i<=nyrs_fish_lc_r; i++)
  {          
    if (nsamples_fish_lc(i)>1)
            {
                  report << yrs_fish_lc_r(i)<<" "<<nsamples_fish_lc(i)<<" "<<theta_fish(2)<<" "<<fish_effn(2,i)<<"  "<<olc_fish_r(i)<< endl;
            }
  }
  report << "Predicted Prop(fishery lengths): year, sample_size, effn, lengths " <<lengths<< endl;
    for (i=1;i<=nyrs_fish_lc_r; i++)
  {
                  report << yrs_fish_lc_r(i)<<" "<<nsamples_fish_lc(i)<<" "<<theta_fish(2)<<" "<<fish_effn(2,i)<<"  "<<elc_fish(yrs_fish_lc_r(i))<< endl;
  }
 }

  
  report << "Observed Prop(fishery unbiased): year, effn, ages " <<ages_dat<< endl;
        for (i=1;i<=nyrs_fish_unbiased_ac_r; i++)
  {
          if (fish_unbiased_ac_samp_r(i)>1)
            {
        report << yrs_fish_unbiased_ac_r(i)<<" "<<fish_effn(1,i)<<" "<<oac_fish_unbiased_r(i)<< endl;
      }
  }
  report << "Predicted Prop(fishery unbiased): year, effn, ages " <<ages_dat<< endl;
    for (i=1;i<=nyrs_fish_unbiased_ac_r; i++)
  {
    report << yrs_fish_unbiased_ac_r(i)  <<" "<<fish_effn(1,i)<<" "<<eac_fish_unbiased_dat(yrs_fish_unbiased_ac_r(i))<< endl;
  }

  for (i=1;i<=nsrv;i++){
    if(nyrs_srv_ac_r(i)>0 && srv_age_flag(i)>0){
      report <<srvname(i)<<": observed age comps: year, effn, ages " <<ages_dat<< endl;
      for (j=1;j<=nyrs_srv_ac_r(i);j++)
      {
           report << yrs_srv_ac_r(i,j)<<" "<<sac_effn(i,j)<<" "<<oac_srv_r(i,j)<< endl;
      }
      report <<srvname(i)<<": estimated age comps: year, effn, ages " <<ages_dat<< endl;
      for (j=1;j<=nyrs_srv_ac_r(i);j++)
      {
           report << yrs_srv_ac_r(i,j)<<" "<<sac_effn(i,j)<<" "<<eac_srv_dat(i,yrs_srv_ac_r(i,j))<< endl;
      }
    }
    if(nyrs_srv_lc_r(i)>0 && srv_len_flag(i)){
      report <<srvname(i)<<": observed length comps: year, effn, lengths " <<lengths<< endl;
      for (j=1;j<=nyrs_srv_lc_r(i);j++)
      {
           report << yrs_srv_lc_r(i,j)<<" "<<slc_effn(i,j)<<" "<<olc_srv_r(i,j)<< endl;
      }
      report <<srvname(i)<<": estimated age comps: year, effn, lengths " <<ages_dat<< endl;
      for (j=1;j<=nyrs_srv_lc_r(i);j++)
      {
           report << yrs_srv_lc_r(i,j)<<" "<<slc_effn(i,j)<<" "<<elc_srv(i,yrs_srv_lc_r(i,j))<< endl;
      }
    }
  }

  report << "fishery unbiased ages Pearson residuals: year, ages " <<ages_dat<< endl;
    for (i=1;i<=nyrs_fish_unbiased_ac_r; i++)
  {
    report << yrs_fish_unbiased_ac_r(i)  <<" "<<fac_pearson(i)<< endl;
  } 

  report << "fishery lengths Pearson residuals: year, lengths " <<lengths<< endl;
    for (i=1;i<=nyrs_fish_lc_r; i++)
  {
    report << yrs_fish_lc_r(i)<<"  "<<flc_pearson(i)<< endl;
  }

  for (i=1;i<=nsrv;i++){
    if(nyrs_srv_ac_r(i)>0 && srv_age_flag(i)>0){
      report <<srvname(i)<<": Pearson age residuals: year, ages " <<ages_dat<< endl;
      for (j=1;j<=nyrs_srv_ac_r(i);j++)
      {
           report << yrs_srv_ac_r(i,j)<<" "<<sac_pearson(i,j)<< endl;
      }
    }
    if(nyrs_srv_lc_r(i)>0 && srv_len_flag(i)){
      report <<srvname(i)<<": Pearson length residuals: year, lengths " <<ages_dat<< endl;
      for (j=1;j<=nyrs_srv_lc_r(i);j++)
      {
           report << yrs_srv_lc_r(i,j)<<" "<<slc_pearson(i,j)<< endl;
      }
    }
  }

  //cout << "got the pearson resids" << endl;






  report << " the mean effective N for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<fish_mean_effn(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<fish_mean_effn(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<sac_mean_effn(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<slc_mean_effn(i)<<" ";
  report << endl;

  report << " the sample weights for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<fish_mean_samp_wts(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<fish_mean_samp_wts(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<sac_mean_samp_wts(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<slc_mean_samp_wts(i)<<" ";
  report << endl;

  report << " the sdnr for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<fish_sdnr(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<fish_sdnr(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<sac_sdnr(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<slc_sdnr(i)<<" ";
  report << endl;

  report << "the sdnr for: ";
  for (i=1;i<=nsrv;i++)  report << " '"<<srvname(i)<<" biomass',"<< " '"<<srvname(i)<<" abundance',";
  report << endl;

  for (i=1;i<=nsrv;i++)  report << " "<<  survey_bio_sdnr(i)<< " "<<  survey_abun_sdnr(i);
  report << endl; 

  
  report << " the root mean square error for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<fish_rmse(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<fish_rmse(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<sac_rmse(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<slc_rmse(i)<<" ";
  report << endl;
  
  report << "the rmse for: 'rec', ";
  for (i=1;i<=nsrv;i++)  report << " '"<<srvname(i)<<" biomass',"<< " "<<srvname(i)<<" abundance',";
  report << endl;

  report << rec_rmse;
  for (i=1;i<=nsrv;i++)  report << " "<<srv_bio_rmse(i)<< " "<<srv_abun_rmse(i);;
  report << endl; 
  
  report << "The likelhood components: 'objfun', histFpen', 'selpen(1,9)', 'rec_likelihood','Fmort_pen','mat_like',";
  report << " 'obj_fun',";
  if (fish_bio_flag>0) report <<" 'cat_likelihood',";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  if (switch_M_devs_pen==1) report <<" 'M_dev_pen',";

  for (i=1;i<=nsrv;i++)  if (srv_bio_flag(i)>0) report << " '"<<srvname(i)<<" biomass',";
  for (i=1;i<=nsrv;i++)  if (srv_abun_flag(i)>0) report << " '"<<srvname(i)<<" abundance',"; 
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  
  report << obj_fun <<" "<<hf_pen<<" "<<sel_like<<" "<<sum(rec_like)<<" "<<fpen<<" "<<mat_like;
  if (fish_bio_flag>0) report <<" "<<catch_like;
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" "<<age_like(1);
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" "<<age_like(2);
  if (switch_M_devs_pen==1) report <<" "<<M_dev_pen;
  for (i=1;i<=nsrv;i++)  if (srv_bio_flag(i)>0) report << " "<<srv_bio_like(i);
  for (i=1;i<=nsrv;i++)  if (srv_abun_flag(i)>0) report << " "<<srv_abun_like(i); 
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) report << " "<<age_like_sac(i);
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report << " "<<age_like_slc(i);
  report << endl;
  
  report << "the prior components of the like: 'prior_M by bin',";
  report << prior_M<<" ";
  report << endl;
  
  report << "the prior components of the like: ,";
  for (i=1;i<=nsrv;i++)  if (switch_prior_q(i)==1) report << " '"<<srvname(i)<<" prior_q (by bin)',";
  for (i=1;i<=nsrv;i++)  if (switch_prior_q(i)==1) report << " "<<prior_q(i);
  report << endl;

 
  report << " the Var(NR) weights for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i) >0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<compweightsnew_ta12_fsh(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<compweightsnew_ta12_fsh(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<compweightsnew_ta12_sac(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<compweightsnew_ta12_slc(i)<<" ";
  report << endl;

  report << " the McAllister- Ianelli weights for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<compweightsnew_ta11_fsh(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<compweightsnew_ta11_fsh(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<compweightsnew_ta11_sac(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<compweightsnew_ta11_slc(i)<<" ";
  report << endl;

  report << " the Francis weights for the";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<" 'fish_ac',";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<" 'fish_lc',";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report << " '"<< srvname(i)<<" age comps',";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report << " '"<< srvname(i)<<" len comps',";
  report << endl;

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) report <<compweightsnew_ta18_fsh(1)<<" ";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) report <<compweightsnew_ta18_fsh(2)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) report <<compweightsnew_ta18_sac(i)<<" ";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) report <<compweightsnew_ta18_slc(i)<<" ";
  report << endl;

  report << "estimates of M (styr,endyr_r) " << endl;  
  report << M(styr,endyr_r) << endl;  

  report << "estimates of:  ";
  for (i=1;i<=nsrv;i++)  if (srv_bio_flag(i)>0) report << " '"<<srvname(i)<<" q',";
  report << endl;

  for (i=1;i<=nsrv;i++)  if (srv_bio_flag(i)>0) report << " "<<q_srv(i);
  report << endl;
  
  
  
  report << "the fishery selectivity by year and age "<< endl;   
  report << rescaled_sel_fish << endl;
  report << "the recent selectivity (last five years)" << endl;
  report << recent_fish_sel << endl;
  report << "F spr rates " << endl;
  report << F40 <<" "<< F35 << endl;
  report << " the objective function is " << endl;
  report << obj_fun << endl;
  if (fsh_sel_option==3){
    report << " the cubic spline parameter matrix is "<< endl;
    report << fsh_sel_par << endl;
  }

  report << "The F's by year and age are "  << endl;
  report << F << endl;

  report << "The catch at age (numbers) by year and age are "  << endl;
  report << catage << endl;

  report << "The catch at length (numbers) by year and length bin are "  << endl;
  for (i=styr;i<=endyr_r;i++)
   {
    report << translen*catage(i) << endl;
   }








   
  # include "pop24-s-report_exp_r.cxx"   // ADMB code to write the S-compatible report

    // write the age comp data, sample size, and pearson resids for a flatfile
  ofstream agecomps("agecomps.dat");
  for (i=1;i<=nsrv;i++)
  {
    if(nyrs_srv_ac_r(i)>0)
    {
      if (i==1) agecomps <<" index year n comp_unit obs pred pearson"  << endl;
      for (j=1;j<=nyrs_srv_ac_r(i);j++)
      {
        for (m=1;m<=nages_dat;m++)
        {    
          agecomps << srvname(i) <<" "<<yrs_srv_ac_r(i,j)<<" "<<srv_ac_samp_r(i,j)<<" "<< 
               " "<<ages_dat(m) <<" "<<oac_srv_r(i,j,m)<<" "<<eac_srv_dat(i,yrs_srv_ac_r(i,j),m)<<" "
                  <<sac_pearson(i,j,m) << endl;
        }   
      }
    }
  }
  
  for (j=1;j<=nyrs_fish_unbiased_ac_r; j++)
  {
    if (fish_unbiased_ac_samp_r(j)>1)
    {
      for (m=1;m<=nages_dat;m++)
        {   
          agecomps << "fishery" <<" "<<yrs_fish_unbiased_ac_r(j)<<" "<<fish_unbiased_ac_samp_r(j)<<" "<< 
          " "<<ages_dat(m) <<" "<<oac_fish_unbiased_r(j,m)   <<" "<<eac_fish_unbiased_dat(yrs_fish_unbiased_ac_r(j),m)<<" "
          <<fac_pearson(j,m)<< endl;        
        }
    }
   }

    // write the length comp data, sample size, and pearson resids for a flatfile
  ofstream lengthcomps("lengthcomps.dat");
  for (i=1;i<=nsrv;i++)
  {
    if(nyrs_srv_lc_r(i)>0)
    {
       if (i==1) lengthcomps <<" index year n comp_unit obs pred pearson"  << endl;
      for (j=1;j<=nyrs_srv_lc_r(i);j++)
      {
        for (m=1;m<=nlen;m++)
        {    
          lengthcomps << srvname(i) <<" "<<yrs_srv_lc_r(i,j)<<" "<<srv_lc_samp_r(i,j)<<" "<< 
               " "<<lengths(m) <<" "<<olc_srv_r(i,j,m)<<" "<<elc_srv(i,yrs_srv_lc_r(i,j),m)<<" "
                  <<slc_pearson(i,j,m) << endl;
        }   
      }
    }
  }
  
  
  for (j=1;j<=nyrs_fish_lc_r; j++)
  {
    if (fish_lc_samp_r(j)>1)
    {
      for (m=1;m<=nlen;m++)
        {   
          lengthcomps << "fishery" <<" "<<yrs_fish_lc_r(j)<<" "<<fish_lc_samp_r(j)<<" "<< 
          " "<<lengths(m) <<" "<<olc_fish_r(j,m)   <<" "<<elc_fish(yrs_fish_lc_r(j),m)<<" "
          <<flc_pearson(j,m)<< endl;        
        }
    }
   }
  
  
  ofstream projfile("popproj.dat");
  projfile << "BSAI_pop"  << endl; 
  projfile << 0 <<" # SSL Species???" << endl;
  projfile << 0 <<" # Constant buffer of Dorn" << endl;
  projfile << 1 <<" # Number of fsheries" << endl;
  projfile << 1 <<" # Number of sexes??" << endl;
  projfile << mean(rescaled_F(endyr_r-5,endyr_r-1)) << " #  average 5 yr f " << endl;
  projfile << 1 <<" # author f" << endl;
  projfile << 0.4 <<" # ABC SPR" << endl;
  projfile << 0.35 <<" # MSY SPR" << endl;
  projfile << spawn_mo <<" # Spawnmo" << endl;
  projfile << nages_dat <<" # Number of ages" << endl;
  projfile << 1 <<" # Fratio" << endl;
  Mvec = recent_M;
  projfile << " # Natural mortality " << endl; 
  projfile << Mvec(1,nages_dat) << endl;
  projfile << " # Maturity " << endl; 
  projfile << maturity_bin << endl;
  projfile << " # Wt Spawn " << endl; 
  projfile << wt_pop_bin << endl; 
  projfile << " # Wt Fish " << endl; 
  projfile << wt_fsh_bin << endl;
  projfile << " # selectivity " << endl; 
  projfile << recent_fish_sel << endl;
  projfile << " # natage " << endl; 
  projfile << natage_bin(endyr_r) << endl; 
  projfile << " # Nrec " << endl; 
  projfile << lastyr_rec - (max(1977,styr) +rec_age -1)  << endl; 
  projfile << " # rec " << endl; 
  projfile << column(natage_bin,1)(max(1977,styr) +rec_age,lastyr_rec) << endl;
  projfile << " # ssb " << endl; 
  projfile << sp_biom(max(1977,styr),lastyr_rec-rec_age) << endl;

  ofstream projfile2("popproj_age10.dat");
  projfile2 << "BSAI_pop"  << endl; 
  projfile2 << 0 <<" # SSL Species???" << endl;
  projfile2 << 0 <<" # Constant buffer of Dorn" << endl;
  projfile2 << 1 <<" # Number of fsheries" << endl;
  projfile2 << 1 <<" # Number of sexes??" << endl;
  projfile2 << mean(rescaled_F(endyr_r-5,endyr_r-1)) << " #  average 5 yr f " << endl;
  projfile2 << 1 <<" # author f" << endl;
  projfile2 << 0.4 <<" # ABC SPR" << endl;
  projfile2 << 0.35 <<" # MSY SPR" << endl;
  projfile2 << spawn_mo <<" # Spawnmo" << endl;
  projfile2 << nages_dat <<" # Number of ages" << endl;
  projfile2 << 1 <<" # Fratio" << endl;
  Mvec = recent_M;
  projfile2 << " # Natural mortality " << endl; 
  projfile2 << Mvec(1,nages_dat) << endl;
  projfile2 << " # Maturity " << endl; 
  projfile2 << maturity_bin << endl;       
  projfile2 << " # Wt Spawn " << endl; 
  projfile2 << wt_pop_bin << endl; 
  projfile2 << " # Wt Fish " << endl; 
  projfile2 << wt_fsh_bin << endl;
  projfile2 << " # selectivity " << endl; 
  projfile2 << recent_fish_sel << endl;
  projfile2 << " # natage " << endl; 
  projfile2 << natage_bin(endyr_r) << endl;
  projfile2 << " # Nrec " << endl; 
  projfile2 << lastyr_rec_a10 - (max(1977,styr) +rec_age -1)  << endl;
  projfile2 << " # rec " << endl; 
  projfile2 << column(natage_bin,1)(max(1977,styr) +rec_age,lastyr_rec_a10) << endl;
  projfile2 << " # ssb " << endl; 
  projfile2 << sp_biom(max(1977,styr),lastyr_rec_a10-rec_age) << endl;


  
  
  ofstream compweightsnew_ta12_file("compweights_new_ta12.ctl");    // new comp weights based on method TA1.2 in Francis 2011
  compweightsnew_ta12_file << compweightsnew_ta12_fsh <<" ";
  compweightsnew_ta12_file << compweightsnew_ta12_sac <<" ";
  compweightsnew_ta12_file << compweightsnew_ta12_slc << endl; 
  
  ofstream compweightsnew_ta18_file("compweights_new_ta18.ctl");    // new comp weights based on Francis method (TA1.8)
  compweightsnew_ta18_file << compweightsnew_ta18_fsh <<" ";
  compweightsnew_ta18_file << compweightsnew_ta18_sac <<" ";
  compweightsnew_ta18_file << compweightsnew_ta18_slc <<endl;  

  ofstream compweightsnew_ta11_file("compweights_new_ta11.ctl");    // new comp weights McAllister-Ianelli method (TA1.1)
  compweightsnew_ta11_file << compweightsnew_ta11_fsh <<" ";
  compweightsnew_ta11_file << compweightsnew_ta11_sac <<" ";
  compweightsnew_ta11_file << compweightsnew_ta11_slc <<endl;

  


  ofstream newinitvaluesfile("initvalues_new.ctl");
  err_count = 1; 
  //int err_count;
  newinitvaluesfile << "### Initial parameter values  ###" << endl;
  newinitvaluesfile << "# initial value for log_q_srv_test (per survey) " << endl;         
  for (i=1;i<=nsrv;i++)
  {
    newinitvaluesfile << apply_jitter(log_q_srv_start(i),phase_s_sel_q(i),1,err_count) << " ";
  } 
  newinitvaluesfile << endl;
  newinitvaluesfile << "# initial value for log average M " << endl;
  newinitvaluesfile << apply_jitter(log_avg_M_start,phase_log_avg_M,1,err_count) << endl;
  newinitvaluesfile << "# initial value for mat_beta1 " << endl;
  newinitvaluesfile << mat_beta1_start << endl;
  newinitvaluesfile << " # initial value for mat_beta2 " << endl;
  newinitvaluesfile << mat_beta2_start << endl;
  newinitvaluesfile << " # initial value for log_rinit " << endl;
  newinitvaluesfile << apply_jitter(log_rinit_start,phase_log_rinit,1,err_count)<< endl;
  newinitvaluesfile << " # initial value for log_rzero " << endl;
  newinitvaluesfile << apply_jitter(log_rzero_start,sr_phase,1,err_count)<< endl;
  newinitvaluesfile << " # initial value for log_avg_fmort " << endl;
  newinitvaluesfile << apply_jitter(log_avg_fmort_start,phase_log_avg_fmort,1,err_count)<< endl;
  newinitvaluesfile << " # initial value for mean_log_rec " << endl;
  newinitvaluesfile << apply_jitter(mean_log_rec_start,1,1,err_count)<< endl;
  newinitvaluesfile << " # initial value for sel_aslope_fish " << endl;
  newinitvaluesfile << apply_jitter(sel_aslope_fish_start,phase_f_sel_ascend,1,err_count)<< endl;
  newinitvaluesfile << " # initial value for   sel_dslope_fish " << endl;
  newinitvaluesfile << apply_jitter(sel_dslope_fish_start,phase_f_sel_descend,1,err_count)<< endl;

  newinitvaluesfile << " # initial value for   sel_aslope_srv " << endl;
  for (i=1;i<=nsrv;i++)
    newinitvaluesfile << apply_jitter(sel_aslope_srv_start(i),phase_srv_sel_logistic(i),1,err_count)<< " ";
  newinitvaluesfile << endl;  
   
  newinitvaluesfile << " # initial value for   sel_a50_fish " << endl;
  newinitvaluesfile << apply_jitter(sel_a50_fish_start,phase_f_sel_ascend,1,err_count)<< endl;

  newinitvaluesfile << " # initial value for   sel_d50_fish " << endl;
  newinitvaluesfile << apply_jitter(sel_d50_fish_start,phase_f_sel_descend,1,err_count)<< endl;

  newinitvaluesfile << " # initial value for   sel_a50_srv " << endl;
  for (i=1;i<=nsrv;i++)
    newinitvaluesfile << apply_jitter(sel_a50_srv_start(i),phase_srv_sel_logistic(i),1,err_count)<< " ";
  newinitvaluesfile << endl;  
  
  newinitvaluesfile << " # initial value for   steepness " << endl;
  newinitvaluesfile << apply_jitter(steepness_start,sr_phase,1,err_count)<< endl;
  
  newinitvaluesfile << " # initial value for  historic_F   " << endl;
  newinitvaluesfile << apply_jitter(historic_F_start,phase_historic_F,1,err_count)<< endl;     
  
  newinitvaluesfile << " # initial value for  theta_fish " << endl;
  for (i=1;i<=2;i++)
    newinitvaluesfile << apply_jitter(theta_fish_start(i),ph_D_M_fish(i),1,err_count)<< " ";
  newinitvaluesfile << endl;  

  newinitvaluesfile << " # initial value for  theta_sac " << endl;
  for (i=1;i<=nsrv;i++)
    newinitvaluesfile << apply_jitter(theta_sac_start(i),ph_D_M_sac(i),1,err_count)<< " ";
  newinitvaluesfile << endl;  

  newinitvaluesfile << " # initial value for  theta_slc " << endl;
  for (i=1;i<=nsrv;i++)
    newinitvaluesfile << apply_jitter(theta_slc_start(i),ph_D_M_slc(i),1,err_count)<< " ";
  newinitvaluesfile << endl;
  newinitvaluesfile << " # jitter seed " << endl;
  newinitvaluesfile << jitter_seed +1 << endl;  
  newinitvaluesfile << " # jitter standard deviation " << endl;
  newinitvaluesfile << jitter_stddev << endl;

  ofstream jitteroutfile("jitter_out.dat");
  jitteroutfile << obj_fun <<" "<<log_avg_M <<" ";
  for (i=1;i<=nsrv;i++)  jitteroutfile <<log_q_srv(i)<<" ";   
  jitteroutfile <<" "<<mean_log_rec <<" "<<sel_aslope_srv <<" "<<sel_a50_srv;
  jitteroutfile <<" "<<sel_aslope_fish <<" "<<sel_a50_fish <<" "<<totbiom(endyr_r) << endl;



  


 	
RUNTIME_SECTION //------------------------------------------------------------------------------------------
    convergence_criteria 1.e-4 1.e-4 1.e-4 1.e-7 1.e-7 1.e-7
    maximum_function_evaluations 1000, 1000, 1000, 10000, 20000, 20000
    
TOP_OF_MAIN_SECTION
  arrmblsize = 1000000;
  

GLOBALS_SECTION
 # include "admodel.h"          // Include AD class definitions
 # include "mhp-s-funcs.cpp"    // Include S-compatible output functions (needs preceding)
 adstring_array srvname;



 void function_minimizer::mcmc_eval(void)
        {
                // |---------------------------------------------------------------------------|
                // | Added DIC calculation.  Martell, Jan 29, 2013                             |
                // |---------------------------------------------------------------------------|
                // | DIC = pd + dbar
                // | pd  = dbar - dtheta  (Effective number of parameters)
                // | dbar   = expectation of the likelihood function (average f)
                // | dtheta = expectation of the parameter sample (average y)

          gradient_structure::set_NO_DERIVATIVES();
          initial_params::current_phase=initial_params::max_number_phases;
          uistream * pifs_psave = NULL;

        #if defined(USE_LAPLACE)
        #endif

        #if defined(USE_LAPLACE)
            initial_params::set_active_random_effects();
            int nvar1=initial_params::nvarcalc();
        #else
          int nvar1=initial_params::nvarcalc(); // get the number of active parameters
        #endif
          int nvar;

          pifs_psave= new
            uistream((char*)(ad_comm::adprogram_name + adstring(".psv")));
          if (!pifs_psave || !(*pifs_psave))
          {
            cerr << "Error opening file "
                    << (char*)(ad_comm::adprogram_name + adstring(".psv"))
               << endl;
            if (pifs_psave)
            {
              delete pifs_psave;
              pifs_psave=NULL;
              return;
            }
          }
          else
          {
            (*pifs_psave) >> nvar;
            if (nvar!=nvar1)
            {
              cout << "Incorrect value for nvar in file "
                   << "should be " << nvar1 << " but read " << nvar << endl;
              if (pifs_psave)
              {
                delete pifs_psave;
                pifs_psave=NULL;
              }
              return;
            }
          }

          int nsamp = 0;
          double sumll = 0;
          independent_variables y(1,nvar);
          independent_variables sumy(1,nvar);

          do
          {
            if (pifs_psave->eof())
            {
              break;
            }
            else
            {
              (*pifs_psave) >> y;
              sumy = sumy + y;
              if (pifs_psave->eof())
              {
                double dbar = sumll/nsamp;
                int ii=1;
                y = sumy/nsamp;
                initial_params::restore_all_values(y,ii);
                initial_params::xinit(y);
                double dtheta = 2.0 * get_monte_carlo_value(nvar,y);
                double pd     = dbar - dtheta;
                double dic    = pd + dbar;
                double dicValue      = dic;
                double dicNoPar      = pd;

                cout<<"Number of posterior samples    = "<<nsamp    <<endl;
                cout<<"Expectation of log-likelihood  = "<<dbar     <<endl;
                cout<<"Expectation of theta           = "<<dtheta   <<endl;
                cout<<"Number of estimated parameters = "<<nvar1    <<endl;
                    cout<<"Effective number of parameters = "<<dicNoPar <<endl;
                    cout<<"DIC                            = "<<dicValue <<endl;
                break;
              }
              int ii=1;
              initial_params::restore_all_values(y,ii);
              initial_params::xinit(y);
              double ll = 2.0 * get_monte_carlo_value(nvar,y);
              sumll    += ll;
              nsamp++;
              // cout<<sumy(1,3)/nsamp<<" "<<get_monte_carlo_value(nvar,y)<<endl;
            }
          }
          while(1);
          if (pifs_psave)
          {
            delete pifs_psave;
            pifs_psave=NULL;
          }
          return;
        }



   
 
 

       
