#ifdef DEBUG
  #ifndef __SUNPRO_C
    #include <cfenv>
    #include <cstdlib>
  #endif
#endif
 # include "admodel.h"                      // Include AD class definitions
 // logs input for checking
	/**
	\def log_input(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef log_input
	#define writeinput(object) log_input << "# " #object "\n" << object << endl;
  ofstream log_input("model_input.log");
#ifdef DEBUG
  #include <chrono>
#endif
#include <admodel.h>
#ifdef USE_ADMB_CONTRIBS
#include <contrib.h>

#endif
  extern "C"  {
    void ad_boundf(int i);
  }
#include <akplcss.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  adstring tmpstring;
  tmpstring=adprogram_name + adstring(".dat");
  if (argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-ind"))>-1)
    {
      if (on>argc-2 || argv[on+1][0] == '-')
      {
        cerr << "Invalid input data command line option"
                " -- ignored" << endl;
      }
      else
      {
        tmpstring = adstring(argv[on+1]);
      }
    }
  }
  global_datafile = new cifstream(tmpstring);
  if (!global_datafile)
  {
    cerr << "Error: Unable to allocate global_datafile in model_data constructor.";
    ad_exit(1);
  }
  if (!(*global_datafile))
  {
    delete global_datafile;
    global_datafile=NULL;
  }
  pad_evalout = new ofstream("akplcss.mcmc.out");;
  q_in.allocate("q_in");
  q_phase.allocate("q_phase");
  styr.allocate("styr");
  endyr.allocate("endyr");
  styr_fut.allocate("styr_fut");
  endyr_fut.allocate("endyr_fut");
  phase_F40.allocate("phase_F40");
  median_rec.allocate("median_rec");
  nages.allocate("nages");
 cout <<nages<<endl;
  nselages.allocate("nselages");
 cout<<nselages<<endl;
  nselages_srv1.allocate("nselages_srv1");
 cout<<nselages_srv1<<endl;
  phase_logistic_sel.allocate("phase_logistic_sel");
  nlen.allocate("nlen");
 cout<<nlen<<endl;
  nobs_fish.allocate("nobs_fish");
  yrs_fish.allocate(1,nobs_fish,"yrs_fish");
 cout<<yrs_fish(1,nobs_fish)<<endl;
  nsamples_fish.allocate(1,2,1,nobs_fish,"nsamples_fish");
  nobs_srv1.allocate("nobs_srv1");
 cout <<nobs_srv1<<endl;         //# of years of shelf survey biomass data
  yrs_srv1.allocate(1,nobs_srv1,"yrs_srv1");
 cout <<yrs_srv1(1,nobs_srv1)<<endl;
  nobs_srv1_length.allocate("nobs_srv1_length");
  yrs_srv1_length.allocate(1,nobs_srv1_length,"yrs_srv1_length");
 cout <<yrs_srv1_length(1,nobs_srv1_length)<<endl;
 writeinput(yrs_srv1_length);
  nsamples_srv1_length.allocate(1,2,1,nobs_srv1_length,"nsamples_srv1_length");
 writeinput(nsamples_srv1_length);
  obs_p_srv1_length.allocate(1,2,1,nobs_srv1_length,1,nlen,"obs_p_srv1_length");
 writeinput(obs_p_srv1_length);
  obs_p_fish.allocate(1,2,1,nobs_fish,1,nlen,"obs_p_fish");
 writeinput(obs_p_fish);
  catch_bio.allocate(styr,endyr,"catch_bio");
 writeinput(catch_bio);
 cout<<catch_bio(styr,endyr)<<endl;
  obs_srv1.allocate(1,nobs_srv1,"obs_srv1");
 writeinput(obs_srv1);
  obs_srv1_sd.allocate(1,nobs_srv1,"obs_srv1_sd");
 writeinput(obs_srv1_sd);
  wt.allocate(1,2,1,nages,"wt");
 writeinput(wt);
  maturity.allocate(1,nages,"maturity");
 cout<<  maturity(1,nages)        <<endl;
  lenage.allocate(1,2,1,nages,1,nlen,"lenage");
 cout<<  lenage(2,nages)<<endl;
 cout <<nobs_srv1<<endl;
  bottom_temps.allocate(1,nobs_srv1,"bottom_temps");
 cout<<bottom_temps(1,nobs_srv1)<<endl;
  nobs_srv1_age.allocate("nobs_srv1_age");
 cout <<"Nobs survey"<<endl;
 cout <<nobs_srv1_age<<endl;
  yrs_srv1_age.allocate(1,nobs_srv1_age,"yrs_srv1_age");
 writeinput(yrs_srv1_age);
  nsamples_srv1_age.allocate(1,2,1,nobs_srv1_age,"nsamples_srv1_age");
 writeinput(nsamples_srv1_age);
  obs_p_srv1_age.allocate(1,2,1,nobs_srv1_age,1,nages,"obs_p_srv1_age");
 writeinput(obs_p_srv1_age);
  nobs_fish_age.allocate("nobs_fish_age");
 cout<<nobs_fish_age<<endl;
  yrs_fish_age.allocate(1,nobs_fish_age,"yrs_fish_age");
 cout<<yrs_fish_age<<endl;
  nsamples_fish_age.allocate(1,2,1,nobs_fish_age,"nsamples_fish_age");
  obs_p_fish_age.allocate(1,2,1,nobs_fish_age,1,nages,"obs_p_fish_age");
  cv_srv1.allocate(1,nobs_srv1);
   writeinput(nobs_fish_age);
   writeinput(yrs_fish_age);
   writeinput(nsamples_fish_age);
   writeinput(obs_p_fish_age);
   styr_rec=styr-nages+1;
   if(nselages>nages) nselages=nages;
   if(nselages_srv1>nages) nselages_srv1=nages;  
   
  //calculate cv for surveys
    cv_srv1=elem_div(obs_srv1_sd,obs_srv1);   //shelf survey CV
   
  //change weights to tons
   // wt=wt;
  //  cout<<nobs_srv1<<endl;
  //  cout<<wt<<endl;
  obs_sexr.allocate(1,nobs_fish);
  obs_sexr_srv1_2.allocate(1,nobs_srv1_length);
  pred_sexr.allocate(styr,endyr);
}

void model_parameters::initializationfunction(void)
{
  F40.set_initial_value(.20);
  F35.set_initial_value(.21);
  F30.set_initial_value(.23);
  mean_log_rec.set_initial_value(13.);
  log_avg_fmort.set_initial_value(-5.);
  q1.set_initial_value(q_in);
  fmort_dev.set_initial_value(0.00001);
  fish_slope_f.set_initial_value(.4);
  fish_sel50_f.set_initial_value(5.);
  fish_slope_m.set_initial_value(.4);
  fish_sel50_m.set_initial_value(8);
  srv1_slope_f.set_initial_value(.4);
  srv1_sel50_f.set_initial_value(8.);
  srv1_slope_m.set_initial_value(.4);
  srv1_sel50_m.set_initial_value(8.);
  alpha.set_initial_value(1.);
  beta.set_initial_value(0.);
  if (global_datafile)
  {
    delete global_datafile;
    global_datafile = NULL;
  }
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  q1.allocate(0.5,2.0,q_phase,"q1");
  alpha.allocate(-4,"alpha");
  beta.allocate(-4,"beta");
  mean_log_rec.allocate(1,"mean_log_rec");
  rec_dev.allocate(styr_rec,endyr-1,-15,15,2,"rec_dev");
  log_avg_fmort.allocate(2,"log_avg_fmort");
 cout <<log_avg_fmort<<endl;
  fmort_dev.allocate(styr,endyr,-10,10,1,"fmort_dev");
  fish_slope_f.allocate(.1,5.,phase_logistic_sel,"fish_slope_f");
  fish_sel50_f.allocate(1.,15.,phase_logistic_sel,"fish_sel50_f");
  fish_slope_m.allocate(.1,5,phase_logistic_sel,"fish_slope_m");
  fish_sel50_m.allocate(1.,25.,phase_logistic_sel,"fish_sel50_m");
  srv1_slope_f.allocate(.1,5.,phase_logistic_sel,"srv1_slope_f");
  srv1_sel50_f.allocate(1.,12.,phase_logistic_sel,"srv1_sel50_f");
  srv1_slope_m.allocate(.1,5,phase_logistic_sel,"srv1_slope_m");
  srv1_sel50_m.allocate(1.,12.,phase_logistic_sel,"srv1_sel50_m");
  F40.allocate(0.01,1.,phase_F40,"F40");
  F35.allocate(0.01,1.,phase_F40,"F35");
  F30.allocate(0.01,1.,phase_F40,"F30");
  sel.allocate(1,2,1,nages,"sel");
  #ifndef NO_AD_INITIALIZE
    sel.initialize();
  #endif
  sel_srv1.allocate(1,2,1,nages,"sel_srv1");
  #ifndef NO_AD_INITIALIZE
    sel_srv1.initialize();
  #endif
  popn.allocate(1,2,styr,endyr,"popn");
  #ifndef NO_AD_INITIALIZE
    popn.initialize();
  #endif
  totn_srv1.allocate(1,2,styr,endyr,"totn_srv1");
  #ifndef NO_AD_INITIALIZE
    totn_srv1.initialize();
  #endif
  M.allocate(1,2,"M");
  #ifndef NO_AD_INITIALIZE
    M.initialize();
  #endif
  temp1.allocate(1,nages,"temp1");
  #ifndef NO_AD_INITIALIZE
    temp1.initialize();
  #endif
  temp2.allocate(1,nages,"temp2");
  #ifndef NO_AD_INITIALIZE
    temp2.initialize();
  #endif
  explbiom.allocate(styr,endyr,"explbiom");
  #ifndef NO_AD_INITIALIZE
    explbiom.initialize();
  #endif
  pred_bio.allocate(styr,endyr,"pred_bio");
  #ifndef NO_AD_INITIALIZE
    pred_bio.initialize();
  #endif
  fspbio.allocate(styr,endyr,"fspbio");
  #ifndef NO_AD_INITIALIZE
    fspbio.initialize();
  #endif
  pred_srv1.allocate(styr,endyr,"pred_srv1");
  #ifndef NO_AD_INITIALIZE
    pred_srv1.initialize();
  #endif
  pred_p_fish.allocate(1,2,styr,endyr,1,nlen,"pred_p_fish");
  #ifndef NO_AD_INITIALIZE
    pred_p_fish.initialize();
  #endif
  pred_p_srv1_age.allocate(1,2,1,nobs_srv1_age,1,nages,"pred_p_srv1_age");
  #ifndef NO_AD_INITIALIZE
    pred_p_srv1_age.initialize();
  #endif
  pred_p_fish_age.allocate(1,2,1,nobs_fish_age,1,nages,"pred_p_fish_age");
  #ifndef NO_AD_INITIALIZE
    pred_p_fish_age.initialize();
  #endif
  pred_p_srv1_len.allocate(1,2,1,nobs_srv1_length,1,nlen,"pred_p_srv1_len");
  #ifndef NO_AD_INITIALIZE
    pred_p_srv1_len.initialize();
  #endif
  pred_catch.allocate(styr,endyr,"pred_catch");
  #ifndef NO_AD_INITIALIZE
    pred_catch.initialize();
  #endif
  natage.allocate(1,2,styr,endyr,1,nages,"natage");
  #ifndef NO_AD_INITIALIZE
    natage.initialize();
  #endif
  catage.allocate(1,2,styr,endyr,1,nages,"catage");
  #ifndef NO_AD_INITIALIZE
    catage.initialize();
  #endif
  Z.allocate(1,2,styr,endyr,1,nages,"Z");
  #ifndef NO_AD_INITIALIZE
    Z.initialize();
  #endif
  F.allocate(1,2,styr,endyr,1,nages,"F");
  #ifndef NO_AD_INITIALIZE
    F.initialize();
  #endif
  S.allocate(1,2,styr,endyr,1,nages,"S");
  #ifndef NO_AD_INITIALIZE
    S.initialize();
  #endif
  fmort.allocate(styr,endyr,"fmort");
  #ifndef NO_AD_INITIALIZE
    fmort.initialize();
  #endif
  rbar.allocate("rbar");
  #ifndef NO_AD_INITIALIZE
  rbar.initialize();
  #endif
  surv.allocate(1,2,"surv");
  #ifndef NO_AD_INITIALIZE
    surv.initialize();
  #endif
  offset.allocate(1,4,"offset");
  #ifndef NO_AD_INITIALIZE
    offset.initialize();
  #endif
  rec_like.allocate("rec_like");
  #ifndef NO_AD_INITIALIZE
  rec_like.initialize();
  #endif
  catch_like.allocate("catch_like");
  #ifndef NO_AD_INITIALIZE
  catch_like.initialize();
  #endif
  sexr_like.allocate("sexr_like");
  #ifndef NO_AD_INITIALIZE
  sexr_like.initialize();
  #endif
  age_like.allocate("age_like");
  #ifndef NO_AD_INITIALIZE
  age_like.initialize();
  #endif
  length_like.allocate(1,4,"length_like");
  #ifndef NO_AD_INITIALIZE
    length_like.initialize();
  #endif
  fish_age_like.allocate("fish_age_like");
  #ifndef NO_AD_INITIALIZE
  fish_age_like.initialize();
  #endif
  fpen.allocate("fpen");
  #ifndef NO_AD_INITIALIZE
  fpen.initialize();
  #endif
  surv1_like.allocate("surv1_like");
  #ifndef NO_AD_INITIALIZE
  surv1_like.initialize();
  #endif
  endbiom.allocate("endbiom");
  depletion.allocate("depletion");
  obj_fun.allocate("obj_fun");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  tmp.allocate("tmp");
  #ifndef NO_AD_INITIALIZE
  tmp.initialize();
  #endif
  pred_sexr.allocate(styr,endyr,"pred_sexr");
  #ifndef NO_AD_INITIALIZE
    pred_sexr.initialize();
  #endif
  preds_sexr.allocate(styr,endyr,"preds_sexr");
  #ifndef NO_AD_INITIALIZE
    preds_sexr.initialize();
  #endif
  sigmar.allocate("sigmar");
  #ifndef NO_AD_INITIALIZE
  sigmar.initialize();
  #endif
  ftmp.allocate("ftmp");
  #ifndef NO_AD_INITIALIZE
  ftmp.initialize();
  #endif
  SB0.allocate("SB0");
  #ifndef NO_AD_INITIALIZE
  SB0.initialize();
  #endif
  SBF40.allocate("SBF40");
  #ifndef NO_AD_INITIALIZE
  SBF40.initialize();
  #endif
  SBF35.allocate("SBF35");
  #ifndef NO_AD_INITIALIZE
  SBF35.initialize();
  #endif
  SBF30.allocate("SBF30");
  #ifndef NO_AD_INITIALIZE
  SBF30.initialize();
  #endif
  sprpen.allocate("sprpen");
  #ifndef NO_AD_INITIALIZE
  sprpen.initialize();
  #endif
  Nspr.allocate(1,4,1,nages,"Nspr");
  #ifndef NO_AD_INITIALIZE
    Nspr.initialize();
  #endif
  nage_future.allocate(1,2,styr_fut,endyr_fut,1,nages,"nage_future");
  #ifndef NO_AD_INITIALIZE
    nage_future.initialize();
  #endif
  fspbiom_fut.allocate(1,4,styr_fut,endyr_fut,"fspbiom_fut");
  #ifndef NO_AD_INITIALIZE
    fspbiom_fut.initialize();
  #endif
  F_future.allocate(1,2,styr_fut,endyr_fut,1,nages,"F_future");
  #ifndef NO_AD_INITIALIZE
    F_future.initialize();
  #endif
  Z_future.allocate(1,2,styr_fut,endyr_fut,1,nages,"Z_future");
  #ifndef NO_AD_INITIALIZE
    Z_future.initialize();
  #endif
  S_future.allocate(1,2,styr_fut,endyr_fut,1,nages,"S_future");
  #ifndef NO_AD_INITIALIZE
    S_future.initialize();
  #endif
  catage_future.allocate(1,2,styr_fut,endyr_fut,1,nages,"catage_future");
  #ifndef NO_AD_INITIALIZE
    catage_future.initialize();
  #endif
  avg_rec_dev_future.allocate("avg_rec_dev_future");
  #ifndef NO_AD_INITIALIZE
  avg_rec_dev_future.initialize();
  #endif
  avg_F_future.allocate(1,4,"avg_F_future");
  #ifndef NO_AD_INITIALIZE
    avg_F_future.initialize();
  #endif
  catch_future.allocate(1,3,styr_fut,endyr_fut,"catch_future");
  future_biomass.allocate(1,4,styr_fut,endyr_fut,"future_biomass");
  explbiom_fut.allocate(styr_fut,endyr_fut,"explbiom_fut");
  #ifndef NO_AD_INITIALIZE
    explbiom_fut.initialize();
  #endif
  maxsel_fish.allocate("maxsel_fish");
  #ifndef NO_AD_INITIALIZE
  maxsel_fish.initialize();
  #endif
  maxsel_srv1.allocate("maxsel_srv1");
  #ifndef NO_AD_INITIALIZE
  maxsel_srv1.initialize();
  #endif
  mlike.allocate("mlike");
  #ifndef NO_AD_INITIALIZE
  mlike.initialize();
  #endif
  qlike.allocate("qlike");
  #ifndef NO_AD_INITIALIZE
  qlike.initialize();
  #endif
  flike.allocate("flike");
  #ifndef NO_AD_INITIALIZE
  flike.initialize();
  #endif
}

void model_parameters::preliminary_calculations(void)
{

#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
  obs_mean_sexr=0.34;  //initial value for avg proportion of male population estimated from shelf surveys; calculated below
  obs_SD_sexr=0.0485;  //initial value for standard deviation of mean male population proportion: calculated below
  for(i=1; i<=nobs_fish;i++)
  {
    obs_sexr(i) = sum(obs_p_fish(1,i))/sum(obs_p_fish(1,i) + obs_p_fish(2,i)); 
  }
  for(i=1; i<=nobs_srv1_length;i++)
    obs_sexr_srv1_2(i) = (sum(obs_p_srv1_length(2,i)))/
                         (sum(obs_p_srv1_length(1,i)) + sum(obs_p_srv1_length(2,i)));
    obs_mean_sexr=mean(obs_sexr_srv1_2);
    obs_SD_sexr=std_dev(obs_sexr_srv1_2);
 // cout<< " thru sex ratio "<<endl;
 //Compute offset for multinomial and length bin proportions
 // offset is a constant nplog(p) is added to the likelihood     
 // magnitude depends on nsamples(sample size) and p's_
  //k is sex loop
  offset.initialize(); 
  for (i=1; i <= nobs_fish; i++)
  {
    double sumtot ;
    sumtot = sum(obs_p_fish(1,i)+obs_p_fish(2,i));
    obs_p_fish(1,i) = obs_p_fish(1,i) / sumtot; // observed fishery lengths
    obs_p_fish(2,i) = obs_p_fish(2,i) / sumtot; 
    for(k=1; k<=2;k++)
      offset(1) -= nsamples_fish(k,i)*obs_p_fish(k,i) * log(obs_p_fish(k,i)+.0001);
  }
 //shelf survey length offset and bin proportions
  for (i=1; i <= nobs_srv1_length; i++)
  {
    double sumtot ;
    sumtot = sum(obs_p_srv1_length(1,i)+obs_p_srv1_length(2,i));
    obs_p_srv1_length(1,i) = obs_p_srv1_length(1,i) / sumtot; 
    obs_p_srv1_length(2,i) = obs_p_srv1_length(2,i) / sumtot; 
    for(k=1; k<=2;k++)
      offset(2) -= nsamples_srv1_length(k,i)*obs_p_srv1_length(k,i) * log(obs_p_srv1_length(k,i)+.0001);
  }
  for (i=1; i <= nobs_srv1_age; i++)
  {
    double sumtot ;
    sumtot = sum(obs_p_srv1_age(1,i)+obs_p_srv1_age(2,i));
    obs_p_srv1_age(1,i) = obs_p_srv1_age(1,i) / sumtot; 
    obs_p_srv1_age(2,i) = obs_p_srv1_age(2,i) / sumtot; 
    for(k=1; k<=2;k++)
      offset(3) -= nsamples_srv1_age(k,i)*obs_p_srv1_age(k,i) * log(obs_p_srv1_age(k,i)+.0001);
  }
  //fishery age offset 
  for (i=1; i <= nobs_fish_age; i++)
  {
    double sumtot ;
    sumtot = sum(obs_p_fish_age(1,i)+obs_p_fish_age(2,i));
    obs_p_fish_age(1,i) = obs_p_fish_age(1,i) / sumtot; 
    obs_p_fish_age(2,i) = obs_p_fish_age(2,i) / sumtot; 
    for(k=1; k<=2;k++)
      offset(4) -= nsamples_fish_age(k,i)*obs_p_fish_age(k,i) * log(obs_p_fish_age(k,i)+.0001);
  }
  M(1)=0.13;  //females
  M(2)=0.13;  //males
}

void model_parameters::userfunction(void)
{
  obj_fun =0.0;
  ofstream& evalout= *pad_evalout;
  get_selectivity();
  get_mortality();
  surv(1)=mfexp(-1.0* M(1));
  surv(2)=mfexp(-1.0* M(2));
  get_numbers_at_age();
  get_catch_at_age();
  if (active(F40))
    compute_spr_rates();
  if (last_phase())
  {
    Future_projections();
  }
  evaluate_the_objective_function();
  if (sd_phase() || mceval_phase())
  {
    if (mceval_phase())
    {
      evalout << obj_fun << " " ;
      // loop over years and print in one long row.
     for (i=styr;i<=endyr;i++)
      evalout<<  fspbio(i) << " ";
      for (i=styr;i<=endyr;i++)    
      evalout<< natage(1,i)*wt(1) + natage(2,i)*wt(2) <<" ";
      for (i=styr;i<=endyr;i++)
      evalout << 2*natage(1,i,1) <<" ";
      // hit carriage return on file
      evalout <<  endl;
    }
  }
#ifdef DEBUG
  std::cout << "DEBUG: Total gradient stack used is " << gradient_structure::get()->GRAD_STACK1->total() << " out of " << gradient_structure::get_GRADSTACK_BUFFER_SIZE() << std::endl;;
  std::cout << "DEBUG: Total dvariable address used is " << gradient_structure::get()->GRAD_LIST->total_addresses() << " out of " << gradient_structure::get_MAX_DLINKS() << std::endl;;
  std::cout << "DEBUG: Total dvariable address used is " << gradient_structure::get()->ARR_LIST1->get_max_last_offset() << " out of " << gradient_structure::get_ARRAY_MEMBLOCK_SIZE() << std::endl;;
#endif
}

void model_parameters::get_selectivity(void)
{
  ofstream& evalout= *pad_evalout;
            for (j=1;j<=nages;j++)
            { 
                 sel(1,j)=1./(1.+mfexp(-1.*fish_slope_f*(double(j)-fish_sel50_f)));
                 sel(2,j)=1./(1.+mfexp(-1.*fish_slope_m*(double(j)-fish_sel50_m)));
                 sel_srv1(1,j)=1./(1.+mfexp(-1.*srv1_slope_f*(double(j)-srv1_sel50_f)));
                 sel_srv1(2,j)=1./(1.+mfexp(-1.*srv1_slope_m*(double(j)-srv1_sel50_m)));
           } 
}

void model_parameters::get_mortality(void)
{
  ofstream& evalout= *pad_evalout;
  // cout <<log_avg_fmort<<endl<< endl<< mfexp(log_avg_fmort)<<endl<<fmort_dev<<endl;
  fmort = mfexp( log_avg_fmort+fmort_dev);
  for(k=1;k<=2;k++)
  {
    for (i=styr;i<=endyr;i++)
    {
      F(k,i)=sel(k)*fmort(i);
      Z(k,i)=F(k,i) + M(k);
    }
  }
  S = mfexp(-1.*Z);
 // cout<<"to end of get_mortality"<<endl;
}

void model_parameters::get_numbers_at_age(void)
{
  ofstream& evalout= *pad_evalout;
  maxsel_fish=max(sel(1));
  if(maxsel_fish<max(sel(2)))
    maxsel_fish=max(sel(2));
  maxsel_srv1=max(sel_srv1(1));
  if(maxsel_srv1<max(sel_srv1(2)))
    maxsel_srv1=max(sel_srv1(2)); 
  int itmp;
 //calc initial population  
  for (j=1;j<nages;j++)
    {
      itmp=styr+1-j;
      natage(1,styr,j)=mfexp(mean_log_rec-(M(1)*double(j-1))+rec_dev(itmp));
      natage(2,styr,j)=mfexp(mean_log_rec-(M(2)*double(j-1))+rec_dev(itmp));
      //cout<<"natage"<<natage(1,styr,j)<<endl;
    }
    itmp=styr+1-nages;
  //last age    
    natage(1,styr,nages)=mfexp(mean_log_rec+rec_dev(itmp)-(M(1)*(nages-1)))/(1.- surv(1));
    natage(2,styr,nages)=mfexp(mean_log_rec+rec_dev(itmp)-(M(2)*(nages-1)))/(1.- surv(2));
 // Now do for next several years----------------------------------
  for (i=styr+1;i<=endyr;i++)
  {
    //for age 1 recruits in the last year use value read in from data file
     if(i<=(endyr-1))
     {
      natage(1,i,1)=mfexp(mean_log_rec+rec_dev(i));
      natage(2,i,1)=natage(1,i,1);
     }
     else
     {
       natage(1,i,1)=median_rec;
       natage(2,i,1)=natage(1,i,1);
     }
  }
 //numbers at age
  for(k=1;k<=2;k++)
  {
    for (i=styr;i< endyr;i++)
    {
      //subvector - avoids writing a j loop  =++ increments the right side 
      //(1,nages-1) to 1+1 to nages-1+1 then does the assignment x(i)(1,n) 
      //takes the ith row of x the columns 1 to n
      //      natage(k,i+1)(2,nages)=++elem_prod(natage(k,i)(1,nages-1),S(k,i)(1,nages-1));
      for(j=1;j<nages;j++)
      {
        natage(k,i+1,j+1)=natage(k,i,j)*S(k,i,j); 
      }
      //accumulates oldest ages
      // cout<<"done with j loop"<<endl;
      natage(k,i+1,nages)+=natage(k,i,nages)*S(k,i,nages);
      // cout<<"done with natage nages"<<endl;
      //popn is exploitable numbers
      popn(k,i)= natage(k,i)*sel(k);
      // cout<<"popn "<<endl;
      // cout<<popn(k,i)<<endl;
    }
    // cout<<"to popn"<<endl; 
    popn(k,endyr)=natage(k,endyr)*sel(k);
  }
  for (i=styr;i<=endyr;i++)
  {
      pred_sexr(i)=sum(natage(2,i))/(sum((natage(1,i)+natage(2,i))));  //calculation of prop. of males in pred. population 
    for(k=1;k<=2;k++)
    {
      totn_srv1(k,i)=q1*(natage(k,i)*sel_srv1(k)); // not used in further calculations
    }
  }
  //predicted survey values
  fspbio.initialize(); 
  explbiom.initialize();
  pred_bio.initialize();
  pred_srv1.initialize();
  //qtime=q1;
  for (i=styr;i<=endyr;i++)
  {
    fspbio(i) = elem_prod(mfexp(-0.25*Z(1,i)),natage(1,i)) *elem_prod(wt(1),maturity);
    //    fspbio(i) = fspbio(i)*mfexp(-0.25*Z(1,i)); // spawn in April so need 3 months total mortality before spawning
    //    if (i>=1982 )      //catchability calculation for survey years
   // if (i>=1982 && i-1981 <= nobs_srv1)      //JNI catchability calculation for survey years
    //qtime(i)=q1*mfexp(-alpha+beta*bottom_temps(i-1981));
    //dvar_vector Stmp(1,nages);
    for(k=1;k<=2;k++)
    {
      //pred_srv1(i) += qtime(i)*(natage(k,i)*elem_prod(sel_srv1(k),wt(k)))/maxsel_srv1;   //shelf survey, dividing by the maxsel constrains female selectivity to be 1.0
      // pred_srv1(i) += q1*(natage(k,i)*elem_prod(sel_srv1(k),wt(k)))/maxsel_srv1;   //shelf survey, without temperature q modeling
      // Note this kills fish off till mid-year....
      pred_srv1(i) += q1*(elem_prod(pow(S(k,i),.5),natage(k,i))*elem_prod(sel_srv1(k),wt(k)))/maxsel_srv1;   //shelf survey, without temperature q modeling
      //Stmp = elem_div((1.-mfexp(-Z(k,i))),Z(k,i));
      //pred_srv1(i) += q1*(elem_prod(Stmp , natage(k,i))*elem_prod(sel_srv1(k),wt(k)))/maxsel_srv1;   //shelf survey, without temperature q modeling
      //next line used to fix q1 to 1.0 - problem is if you start from a bin file, even if the bounds
      // are set different in the tpl file the program will take to value from the bin file and use that 
      //   pred_srv1(i)=1.0*(natage(i)*elem_prod(sel_srv1,wt));
      explbiom(i)+=natage(k,i)*elem_prod(sel(k),wt(k))/maxsel_fish;
      pred_bio(i)+=natage(k,i)*wt(k);
    }
  }
      // cout <<q3<<endl<<pred_srv3<<endl;exit(1);
    //don't need to divide by max_sel because totn_srv1 is calculated using selectivities and the
    //max_sel would cancel out.
    // Fitting the survey length compositions
    for(i=1; i<=nobs_srv1_length;i++)
    {
      ii = yrs_srv1_length(i);
      pred_p_srv1_len(1,i) = q1 * elem_prod(sel_srv1(1), elem_prod(pow(S(1,ii),.5),natage(1,ii)) ) * lenage(1);
      pred_p_srv1_len(2,i) = q1 * elem_prod(sel_srv1(2), elem_prod(pow(S(2,ii),.5),natage(2,ii)) ) * lenage(2);
      dvariable sum_tot = sum(pred_p_srv1_len(1,i)+pred_p_srv1_len(2,i));
      pred_p_srv1_len(1,i) /= sum_tot;
      pred_p_srv1_len(2,i) /= sum_tot;
    }
    //Calculation of survey age composition
    for(i=1; i<=nobs_srv1_age;i++)
    {
      ii = yrs_srv1_age(i);
      pred_p_srv1_age(1,i) = q1 * elem_prod(sel_srv1(1),elem_prod(pow(S(1,ii),0.5), natage(1,ii)));
      pred_p_srv1_age(2,i) = q1 * elem_prod(sel_srv1(2),elem_prod(pow(S(2,ii),0.5), natage(2,ii)));
      dvariable sum_tot = sum(pred_p_srv1_age(1,i)+pred_p_srv1_age(2,i));
      pred_p_srv1_age(1,i) /= sum_tot;
      pred_p_srv1_age(2,i) /= sum_tot;
    }
  depletion=pred_bio(endyr)/pred_bio(styr);
  endbiom=pred_bio(endyr);
}

void model_parameters::get_catch_at_age(void)
{
  ofstream& evalout= *pad_evalout;
  int ii=1;
  for (i=styr; i<=endyr; i++)
  {
    pred_catch(i)=0.;
   //cout <<i<<" Here"<<endl;
    for(k=1;k<=2;k++)
    {      
      //--Baranov's equation here-----------------------------------
      for (j = 1 ; j<= nages; j++)
      {
        catage(k,i,j) = natage(k,i,j)*F(k,i,j)*(1.-S(k,i,j))/Z(k,i,j);
        pred_catch(i) += catage(k,i,j)*wt(k,j);
      }
      pred_p_fish(k,i)     = elem_prod(sel(k),natage(k,i))*lenage(k)/(popn(1,i)+popn(2,i)); // predicted fishery length composition
    } //cout <<yrs_fish_age<<" Here"<<endl;
    if (i==yrs_fish_age(ii))
    {
      // cout <<ii<<" Here"<<endl;
      for(k=1;k<=2;k++)
      {
        pred_p_fish_age(k,ii) = elem_prod(sel(k),natage(k,i))/(popn(1,i)+popn(2,i));  // predicted fishery age composition
      }
      if (ii<nobs_fish_age) ii++;
    }
  }
}

void model_parameters::Future_projections(void)
{
  ofstream& evalout= *pad_evalout;
  for(k=1;k<=2;k++)
  {
    nage_future(k,styr_fut)(2,nages)=++elem_prod(natage(k,endyr)(1,nages-1),S(k,endyr)(1,nages-1));
    nage_future(k,styr_fut,nages)+=natage(k,endyr,nages)*S(k,endyr,nages);
   }
    future_biomass.initialize();
    catch_future.initialize();
    for (int l=1;l<=4;l++)
    {
      switch (l)
      {
        case 1:
          ftmp=F40;
          break;
        case 2:
          ftmp=F35;
          break;
        case 3:
          ftmp=F30;
          break;
        case 4:
          ftmp.initialize();
          break;
      }
      // Get future F's
     for(k=1;k<=2;k++)
     {
      for (i=endyr+1;i<=endyr_fut;i++)
      {
        for (j=1;j<=nages;j++)
        {
          F_future(k,i,j) = (sel(k,j)/maxsel_fish)*ftmp;
          Z_future(k,i,j) = F_future(k,i,j)+M(k);
          S_future(k,i,j) = exp(-1.*Z_future(k,i,j));
        }
      }
    // Future Recruitment (and spawners)
      for (i=styr_fut;i<endyr_fut;i++)
      {
        nage_future(k,i,1)  = median_rec;
       // Now graduate for the next year....
        nage_future(k,i+1)(2,nages) = ++elem_prod(nage_future(k,i)(1,nages-1),S_future(k,i)(1,nages-1));
        nage_future(k,i+1,nages)   += nage_future(k,i,nages)*S_future(k,i,nages);
      }
      nage_future(k,endyr_fut,1)  = median_rec;
      // Now get catch at future ages
      for (i=styr_fut; i<=endyr_fut; i++)
      {
        for (j = 1 ; j<= nages; j++)
        {
          catage_future(k,i,j) = nage_future(k,i,j) * F_future(k,i,j) * ( 1.- S_future(k,i,j) ) / Z_future(k,i,j);
         if(k==1)
          {
          fspbiom_fut(l,i) += nage_future(1,i,j)*wt(1,j)*maturity(j);
          }
        }
        if (l!=4) catch_future(l,i)   += catage_future(k,i)*wt(k);
        future_biomass(l,i) += nage_future(k,i)*wt(k);
      }   //end loop over future years
     }   //end loop over sex
     fspbiom_fut(l)=0.;
     for(i=styr_fut;i<=endyr_fut;i++)
       fspbiom_fut(l,i) = elem_prod(nage_future(1,i),wt(1)) * maturity;
    }   //End of loop over F's
}

void model_parameters::compute_spr_rates(void)
{
  ofstream& evalout= *pad_evalout;
  //Compute SPR Rates and add them to the likelihood for Females 
  SB0.initialize();
  SBF40.initialize();
  SBF35.initialize();
  SBF30.initialize();
  // Initialize the recruit (1) for each F  (F40 etc)
  for (i=1;i<=3;i++)
    Nspr(i,1)=1.;
  for (j=2;j<nages;j++)
  {
    Nspr(1,j)=Nspr(1,j-1)*exp(-1.*M(1));
    Nspr(2,j)=Nspr(2,j-1)*exp(-1.*(M(1)+F40*sel(1,j-1)/maxsel_fish));
    Nspr(3,j)=Nspr(3,j-1)*exp(-1.*(M(1)+F35*sel(1,j-1)/maxsel_fish));
    Nspr(4,j)=Nspr(4,j-1)*exp(-1.*(M(1)+F30*sel(1,j-1)/maxsel_fish));
  }
  //cout<<F40<<" "<<F30<<" "<<Nspr<<endl; 
 // cout<<"spr calc"<<endl;
 // Now do plus group
  Nspr(1,nages)=Nspr(1,nages-1)*exp(-1.*M(1))/(1.-exp(-1.*M(1)));
  Nspr(2,nages)=Nspr(2,nages-1)*exp(-1.* (M(1)+F40*sel(1,nages-1)/maxsel_fish))/ (1.-exp(-1.*(M(1)+F40*sel(1,nages)/maxsel_fish)));
  Nspr(3,nages)=Nspr(3,nages-1)*exp(-1.* (M(1)+F35*sel(1,nages-1)/maxsel_fish))/ (1.-exp(-1.*(M(1)+F35*sel(1,nages)/maxsel_fish)));
  Nspr(4,nages)=Nspr(4,nages-1)*exp(-1.* (M(1)+F30*sel(1,nages-1)/maxsel_fish))/ (1.-exp(-1.*(M(1)+F30*sel(1,nages)/maxsel_fish)));
 //cout<<"plus group"<<endl;
  for (j=1;j<=nages;j++)
  {
   // Kill them off till april (0.25) Alaska plaice spawn in spring so put in 0.25
   //         Number   ProportMat  Wt    Amount die off prior to spawning (within that year)
    SB0    += Nspr(1,j)*maturity(j)*wt(1,j)*exp(-0.25*M(1));
    SBF40  += Nspr(2,j)*maturity(j)*wt(1,j)*exp(-0.25*(M(1)+F40*sel(1,j)/maxsel_fish));
    SBF35  += Nspr(3,j)*maturity(j)*wt(1,j)*exp(-0.25*(M(1)+F35*sel(1,j)/maxsel_fish));
    SBF30  += Nspr(4,j)*maturity(j)*wt(1,j)*exp(-0.25*(M(1)+F30*sel(1,j)/maxsel_fish));
  }
  sprpen    = 200.*square((SBF40/SB0)-0.4);
  sprpen   += 200.*square((SBF35/SB0)-0.35);
  sprpen   += 200.*square((SBF30/SB0)-0.30);
}

void model_parameters::evaluate_the_objective_function(void)
{
  ofstream& evalout= *pad_evalout;
  length_like.initialize();
  age_like.initialize();
  fish_age_like.initialize();
  fpen.initialize();
  rec_like.initialize();
  surv1_like.initialize();
  catch_like.initialize();
  sexr_like.initialize();
  obj_fun.initialize();
  if (active(rec_dev))
  {
    length_like.initialize();   //length-like vector has the likelihoods for the 2 components: 1) fishery length 2) shelf survey lengths 
    int ii;
    //recruitment likelihood - norm2 is sum of square values   
    rec_like = norm2(rec_dev);
    for(k=1;k<=2;k++)
    {
      for (i=1; i <= nobs_fish; i++)
      {
        ii=yrs_fish(i);
        //fishery length likelihood fitting
          length_like(1) -= nsamples_fish(k,i)*(1e-5+obs_p_fish(k,i))*log(pred_p_fish(k,ii)+1e-5);
      }
    }
    //add the fishery length offset to the likelihood   
    length_like(1)-=offset(1);
    //shelf survey length composition fitting
    for(k=1;k<=2;k++)
      for (i=1; i <=nobs_srv1_length; i++)
        length_like(2)-=nsamples_srv1_length(k,i)*(1e-3+obs_p_srv1_length(k,i))*log(pred_p_srv1_len(k,i)+1e-3);
    length_like(2)-=offset(2);
     //shelf survey age composition fitting
    for(k=1;k<=2;k++)
      for (i=1; i <=nobs_srv1_age; i++)
        age_like-=nsamples_srv1_age(k,i)*(1e-3+obs_p_srv1_age(k,i))*log(pred_p_srv1_age(k,i)+1e-3);
    age_like-=offset(3);
    //fishery age composition fitting
    for(k=1;k<=2;k++)
      for (i=1; i <=nobs_fish_age; i++)
        fish_age_like-=nsamples_fish_age(k,i)*(1e-3+obs_p_fish_age(k,i))*log(pred_p_fish_age(k,i)+1e-3);
    fish_age_like-=offset(4);
  //end of if(active (rec_dev))
  }
  // Fit to indices (lognormal) 
  //weight each years estimate by 1/(2*variance) - use cv as an approx to s.d. of log(biomass) 
   // surv1_like = norm2(elem_div(log(obs_srv1+.01)-log(pred_srv1(yrs_srv1)+.01),sqrt(2)*cv_srv1));
    // surv_like+=lambda(2)*square(log(obs_srv1(i)+ 1e-13)-log(pred_srv1(yrs_srv1(i))+1e-13))/ (2.*cv_srv1(i)*cv_srv1(i));
  for (i=1;i<=nobs_srv1;i++)
  {
    surv1_like += square(log(obs_srv1(i)+ 1e-13)-log(pred_srv1(yrs_srv1(i))+1e-13))/ (2.*cv_srv1(i)*cv_srv1(i));
  }
   // surv1_like = norm2(elem_div(log(obs_srv1+1e-13)-log(pred_srv1(yrs_srv1)+1e-13),sqrt(2)*cv_srv1));
   catch_like=norm2(log(catch_bio+.000001)-log(pred_catch+.000001));
   // sex ratio likelihood
   //  sexr_like=0.5*norm2((obs_mean_sexr-pred_sexr)/obs_SD_sexr); 
  // Phases less than 3, penalize High F's
    if (current_phase()<2)
    {
       //F's are low for Alaska plaice so penalty is deviations in F's from 0.01 in first two phases
       //don't know if makes any difference since the penalty is reduced at the end
       fpen=10.*norm2(mfexp(fmort_dev+log_avg_fmort)-.01);
    }
    else
    {
      // fpen=.001*norm2(mfexp(fmort_dev+log_avg_fmort)-.01);
    }
    if (active(fmort_dev))
    {
      fpen+=.01*norm2(fmort_dev);
    }
  obj_fun += rec_like;
  obj_fun += length_like(1);     
  obj_fun += length_like(2);    
  obj_fun += age_like;         
  obj_fun += fish_age_like;   
  obj_fun += surv1_like;     
  obj_fun += 300*catch_like;        // large emphasis to fit observed catch
  obj_fun += fpen;
  obj_fun += sprpen;
  //obj_fun += 1.*sexr_like;             // male proportion prior, emphasis factor = 1
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  report << "Estimated numbers of female fish " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<natage(1,i)<< endl;
  report << "Estimated numbers of male fish " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<natage(2,i)<<endl;
  report << "Estimated catch numbers for females " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<catage(1,i)<< endl;
  report << "Estimated catch numbers for males  " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<catage(2,i)<<endl;
  report << "Estimated female F mortality " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<F(1,i)<< endl;
  report << "Estimated male F mortality " << endl;
    for (i=styr; i<=endyr;i++)
      report <<"   Year: " << i << "," <<F(2,i)<<endl;
  report << "Estimated fishery selectivity for females " << endl;  // j is incremented by +2 so output is labeled ages 3-25 (nages =23)
    for (j=1; j<=nages;j++)
      report <<"   Age: " << j+2 << "," <<sel(1,j)<< endl;
  report << "Estimated fishery selectivity for males " << endl;
    for (j=1; j<=nages;j++)
      report <<"   Age: " << j+2 << "," <<sel(2,j)<<endl;    // j is incremented by +2 so output is labeled ages 3-25 (nages =23)
  report << "Estimated shelf survey selectivity for females " << endl;
    for (j=1; j<=nages;j++)
      report <<"   Age: " << j+2 << "," <<sel_srv1(1,j)/maxsel_srv1<< endl;   // j is incremented by +2 so output is labeled ages 3-25 (nages =23)
  report << "Estimated shelf survey selectivity for males " << endl;
    for (j=1; j<=nages;j++)
      report <<"   Age: " << j+2 << "," <<sel_srv1(2,j)/maxsel_srv1<<endl;  // j is incremented by +2 so output is labeled ages 3-25 (nages =23)
  report << endl << "Survey Biomass " << endl;
  report << "Bering Sea shelf survey"  << endl;
  report << "Year" << "," << "observed biomass " << ","<< "predicted biomass" << endl;
    for (i=1; i<=nobs_srv1;i++)
      report << yrs_srv1(i) << ","<< obs_srv1(i) << "," << pred_srv1(yrs_srv1(i)) << endl;
  report <<" Observed female shelf survey length composition " << endl;
    for (i=1; i<=nobs_srv1_length; i++)
      report << yrs_srv1_length(i) << ","<< obs_p_srv1_length(1,i) << endl;
  report <<" Predicted female shelf survey length composition " << endl;
    for (i=1; i<=nobs_srv1_length; i++)
      report << yrs_srv1_length(i) << "," << pred_p_srv1_len(1,i) << endl;
  report <<" Observed male shelf survey length composition " << endl;
    for (i=1; i<=nobs_srv1_length; i++)
      report << yrs_srv1_length(i) << ","<< obs_p_srv1_length(2,i) << endl;
  report <<" Predicted male shelf survey length composition " << endl;
    for (i=1; i<=nobs_srv1_length; i++)
      report << yrs_srv1_length(i) << "," << pred_p_srv1_len(2,i) << endl;
   report <<" Observed female shelf survey age composition " << endl;
    for (i=1; i<=nobs_srv1_age; i++)
      report << yrs_srv1_age(i) << ","<< obs_p_srv1_age(1,i) << endl;
  report <<" Predicted female shelf survey age composition " << endl;
    for (i=1; i<=nobs_srv1_age; i++)
      report << yrs_srv1_age(i) << "," << pred_p_srv1_age(1,i) << endl;
  report <<" Observed male shelf survey age composition " << endl;
    for (i=1; i<=nobs_srv1_age; i++)
      report << yrs_srv1_age(i) << ","<< obs_p_srv1_age(2,i) << endl;
  report <<" Predicted male shelf survey age composition " << endl;
    for (i=1; i<=nobs_srv1_age; i++)
      report << yrs_srv1_age(i) << "," << pred_p_srv1_age(2,i) << endl;
  report <<" Observed female fishery length composition " << endl;
    for (i=1; i<=nobs_fish; i++)
      report << yrs_fish(i) << ","<< obs_p_fish(1,i) << endl;
  report <<" Predicted female fishery length composition " << endl;
    for (i=1; i<=nobs_fish; i++)
      report << yrs_fish(i) << ","<< pred_p_fish(1,yrs_fish(i)) << endl;
  report <<" Observed male fishery length composition " << endl;
    for (i=1; i<=nobs_fish; i++)
      report << yrs_fish(i) << ","<< obs_p_fish(2,i) << endl;
  report <<" Predicted male fishery length composition " << endl;
    for (i=1; i<=nobs_fish; i++)
      report << yrs_fish(i) << ","<< pred_p_fish(2,yrs_fish(i)) << endl;
  report <<" Observed female fishery age composition " << endl;
    for (i=1; i<=nobs_fish_age; i++)
      report << yrs_fish_age(i) << ","<< obs_p_fish_age(1,i) << endl;
  report <<" Predicted female fishery age composition " << endl;
    for (i=1; i<=nobs_fish_age; i++)
      report << yrs_fish_age(i) << "," << pred_p_fish_age(1,i) << endl;
  report <<" Observed male fishery age composition " << endl;
    for (i=1; i<=nobs_fish_age; i++)
      report << yrs_fish_age(i) << ","<< obs_p_fish_age(2,i) << endl;
  report <<" Predicted male fishery age composition " << endl;
    for (i=1; i<=nobs_fish_age; i++)
      report << yrs_fish_age(i) << "," << pred_p_fish_age(2,i) << endl;
  report <<" Observed Catch " << endl;
    for (i=styr;  i<=endyr; i++)
       report << "year " << i <<","<< catch_bio(i) << endl;
  report  << "Predicted Catch "<< endl;
    for (i=styr;  i<=endyr;  i++)
        report  <<"year " << i <<","<< pred_catch(i) << endl;
  report  << "Female spawning biomass , Total biomass" <<  endl;
      for (i=styr;  i<=endyr;  i++)
         report << "year " << i <<","<< fspbio(i) <<","<<natage(1,i)*wt(1) + natage(2,i)*wt(2)<< endl;
  report << endl<<endl;
  report << "F40=  " << F40 << endl;
  report << "F35=  " << F35 << endl;
  report << "F30=  " << F30 << endl;
  report << "spawning biomass per recruit at F40 harvest rate "<< SBF40<< endl;
  report << "spawning biomass per recruit at F35 harvest rate "<< SBF35 << endl;
  report << "spawning biomass per recruit at F30 harvest rate "<< SBF30 << endl;
  report << "spawning biomass per recruit with no fishing " << SB0 << endl;
  report << "Likelihood components" << endl;
  report << "shelf survey like component " << surv1_like << endl;
  report << "fishery length composition likelihood " << length_like(1) << endl;
  report << "recruitment likelihood component est.  " << rec_like << endl;
  report << "catch likelihood component est.  "<< catch_like << endl;
  report << "sex ratio likelihood component " << sexr_like << endl;
  report << "shelf survey age composition  " << age_like << endl;
  report << "shelf survey length composition " << length_like(2) << endl;
  report << "Projected biomass" << endl;
  report << future_biomass << endl;
  report <<"projected future female spawning biomass " << endl;
  report <<fspbiom_fut << endl;
  report << "Future yield " << endl;
  report << catch_future << endl;
  report << "shelf survey q =" << q1 << endl;
  report << " female natural mortality for this run = " << M(1) << endl;
  report << " male natural mortality for this run = " << M(2)  << endl;
  //report <<endl << "temperature effect (q) for the shelf survey "<< endl;
  // for (i=1;i<=nobs_srv1;i++)
  //   report <<yrs_srv1(i)<<","<<bottom_temps(i)<<","<<qtime(yrs_srv1(i))<<endl;
  //report << "predicted male proportion in population" << endl;
  // for (i=styr;i<=endyr;i++)
  //    report << i << " " << pred_sexr(i) << endl;
  report << "mean observed prop. male in shelf surveys = "<< obs_mean_sexr << endl;
  report << "stdev of mean observed prop. male in shelf surveys = " << obs_SD_sexr << endl;
  report <<"alpha = "<<alpha<<endl;
  report << "beta= " << beta << endl;
  report << " Go drink coffee " << endl;
 //----------------------------------------------------------------------------------------------------------------------------------------
  report << "SARA file for Angie Greig" << endl;
  report << "Alaska plaice       # stock  " << endl;
  report << "BSAI       # region     (AI AK BOG BSAI EBS GOA SEO WCWYK)" << endl;
  report << "2014       # ASSESS_YEAR - year assessment is presented to the SSC" << endl;
  report << "3a         # TIER  (1a 1b 2a 2b 3a 3b 4 5 6) " << endl;
  report << "none       # TIER2  if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6)" << endl;
  report << "full       # UPDATE (new benchmark full partial)" << endl;
  report << "2          # LIFE_HIST - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "2          # ASSES_FREQ - SAIP ratings (0 1 2 3 4 5) " << endl;
  report << "4          # ASSES_LEV - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "5          # CATCH_DAT - SAIP ratings (0 1 2 3 4 5) " << endl;
  report << "2          # ABUND_DAT - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "input      # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year" << endl;
  report << "input      # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year" << endl;
  report << "input      # BMSY  is equilibrium spawning biomass at MSY (Tiers 1-2) or 7/8 x B40% (Tier 3)" << endl;
  report << "ADMB       # MODEL - Required only if NMFS toolbox software used; optional otherwise " << endl;
  report << "NA         # VERSION - Required only if NMFS toolbox software used; optional otherwise" << endl;
  report << "2          # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE) " << endl;
  report << "1          # number of fisheries" << endl;
  report << "1000000    # multiplier for recruitment, N at age, and survey number (1,1000,1000000)" << endl;
  report << "1          # recruitment age used by model or size" << endl;
  report << "1          # age+ or mmCW+ used for biomass estimate" << endl;
  report << "\"Single age\"        # Fishing mortality type such as \"Single age\" or \"exploitation rate\"" << endl;
  report << "\"Age model\"         # Fishing mortality source such as \"Model\" or \"(total catch (t))/(survey biomass (t))\"" << endl;
  report << "\"Age of maximum F\"  # Fishing mortality range such as \"Age of maximum F\"" << endl; 
  report << "#FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...)" << endl; 
  report << "ALL" << endl; 
  report <<"#FISHERYYEAR - list years used in the model " << endl;
   for (i=styr;  i<=endyr; i++)
      report << i << "	";
      report<<endl;  
  report<<"#AGE - list of ages used in the model"<<endl;
   for (i=3; i<=25;i++)
      report << i << "	";
      report<<endl;    
  report <<"#RECRUITMENT - Number of recruits by year " << endl;
   for (i=styr;  i<=endyr;  i++)
	   report  << natage(1,i,1)+natage(2,i,1) << "	";
	   report<<endl;     
  report <<"#SPAWNBIOMASS - Spawning biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << fspbio(i) << "	";
      report<<endl;  
  report <<"#TOTALBIOMASS - Total biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << natage(1,i)*wt(1)+natage(2,i)*wt(2) << "	";
      report<<endl;
  report <<"#TOTFSHRYMORT - Fishing mortality rate by year " << endl;
	for (i=styr;  i<=endyr;  i++)
	   report  << (F(1,i,23)+ F(2,i,23))/2<< "	";
	   report<<endl;
  report <<"#TOTALCATCH - Total catch by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << catch_bio(i) << "	";
      report<<endl;
  report <<"#MATURITY - Maturity ratio by age (females only)" << endl;  
      for (i=1;  i<=23;  i++) 
      report  << maturity(i) <<"	";
      report<< endl; 
  report <<"#SPAWNWT - Average spawning weight (in kg) for ages 8-25"<< endl; 
      report <<"0.342	0.423	0.504	0.583	0.659	0.730	0.797	0.859	0.916	0.967	1.014	1.056	1.0932	1.127	1.157	1.183	1.207	1.228";
      report<<endl;
  report <<"#NATMORT - Natural mortality rate for females then males"<< endl; 
     for (i=1;  i<=23;  i++) 
  report  << 0.13 <<"	";
  report<< endl;   
    for (i=1;  i<=23;  i++) 
  report  << 0.13 <<"	";
  report<< endl;
  report << "#N_AT_AGE - Estimated numbers of female (first) then male (second) fish at age " << endl;
    for (i=styr; i<=endyr;i++)
    report <<natage(1,i)<< "	";
    report<<endl;
  for (i=styr; i<=endyr;i++)
    report <<natage(2,i)<< "	";
    report<<endl;
  report <<"#FSHRY_WT_KG - Fishery weight at age (in kg) females (first) males (second), only one fishery"<< endl;   
    for (i=1;  i<=23; i++)   
  report <<wt(1,i) << "	";
  report<<endl;
    for (i=1; i<=23; i++)
  report<<wt(2,i) << " ";
  report<<endl;
  report<< "SELECTIVITY - Estimated fishery selectivity for females (first) males (second) at age "<<endl;
    for (j=1; j<=nages; j++)
     report<<" "<<sel(1,j)<< "     ";
     report<<endl;
    for (j=1;  j<=nages; j++)
     report<<" "<<sel(2,j)<<"     ";
     report<<endl;
  report << "#SURVEYDESC"<<endl;
  report<<"EBS_trawl_survey "<<endl;
  report<<"#SURVEYMULT"<<endl;
  report<<"1"<<endl;
  report << "#EBS_trawl_survey - Bering Sea shelf survey biomass (Year, Obs_biomass, Pred_biomass) " << endl;
   for (i=1; i<=nobs_srv1; i++)
     report << yrs_srv1(i) << "	";
     report<<endl;
   for (i=1; i<=nobs_srv1; i++) 
     report<< obs_srv1(i)<< "	";
     report<< endl;
  report<<"#STOCKNOTES"<<endl;
  report<<"\"SAFE report indicates that this stock was not subjected to overfishing in 2012 and is neither overfished nor approaching a condition of being overfished in 2013.\""<<endl;
  ofstream lab_r_rep("R_Report_Labeled.dat");
  lab_r_rep << "$srv_yrs_age"<<endl;
  lab_r_rep << yrs_srv1_age <<endl;
  lab_r_rep <<"$obs_age_f"<<endl;
  lab_r_rep << obs_p_srv1_age(1)<<endl;
  lab_r_rep <<"$pred_age_f"<<endl;
  lab_r_rep << pred_p_srv1_age(1)<<endl;
  lab_r_rep <<"$obs_age_m"<<endl;
  lab_r_rep << obs_p_srv1_age(2)<<endl;
  lab_r_rep <<"$pred_age_m"<<endl;
  lab_r_rep << pred_p_srv1_age(2)<<endl;
  lab_r_rep.close();
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{4000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{1e-3 1e-4 1e-7}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{
  delete pad_evalout;
  pad_evalout = NULL;
}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  //gradient_structure::set_MAX_NVAR_OFFSET(300);
  //gradient_structure::set_GRADSTACK_BUFFER_SIZE(10000000);
  //gradient_structure::set_CMPDIF_BUFFER_SIZE(4000000);
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(800); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  arrmblsize=10000000;
    gradient_structure::set_NO_DERIVATIVES();
#ifdef DEBUG
  #ifndef __SUNPRO_C
std::feclearexcept(FE_ALL_EXCEPT);
  #endif
  auto start = std::chrono::high_resolution_clock::now();
#endif
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint = defaults::iprint;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
#ifdef DEBUG
  std::cout << endl << argv[0] << " elapsed time is " << std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - start).count() << " microseconds." << endl;
  #ifndef __SUNPRO_C
bool failedtest = false;
if (std::fetestexcept(FE_DIVBYZERO))
  { cerr << "Error: Detected division by zero." << endl; failedtest = true; }
if (std::fetestexcept(FE_INVALID))
  { cerr << "Error: Detected invalid argument." << endl; failedtest = true; }
if (std::fetestexcept(FE_OVERFLOW))
  { cerr << "Error: Detected overflow." << endl; failedtest = true; }
if (std::fetestexcept(FE_UNDERFLOW))
  { cerr << "Error: Detected underflow." << endl; }
if (failedtest) { std::abort(); } 
  #endif
#endif
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
