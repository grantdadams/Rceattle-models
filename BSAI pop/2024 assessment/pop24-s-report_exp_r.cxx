  //------- pop<nn>-s-report.cxx-----------------------------------------------
  // ------ Write results into S-compatible file -- PDS July 2003   -------
  // ------ Based on work from Michael Prager
  // ------ The file will be named <name>.rdat, where <name> is the TPL file name
  // ------ The file can be read by S with the statement: x <- dget("<name>.rdat")

  const char* cc = ", ";    // To reduce clutter later on
  int i, il, y;                // Vars used as indices
  int count;                 // count iterations in loop 
  
  // Open the S output file
  ofstream sfile ((char*)(adprogram_name + ".rdat"));

  // ------------------ START OVERALL S STRUCTURE (LIST) -------------------------

  sfile << "structure(list(";

   
  // ------------------ TIME SERIES DATA, 1960-2002 -------------------------

  // Write start of data frame for continous time series from 1960-2002
      sfile << "t.series = structure(list(" << endl;

  // Write vector of years:
      for(y=styr; y<=endyr_r; y++) {xdum2[y] = double(y);}
      write_s_yrcol(sfile, value(xdum2), "year", styr, endyr_r, 0);


      
  // Write time series data (1960-2002)
      write_s_yrcol(sfile, value(rowsum(natage)), "totnum", styr, endyr_r, 0);
      write_s_yrcol(sfile, value(column(natage,1)(styr,endyr_r)), "a3recs", styr, endyr_r, 0);
      write_s_yrcol(sfile, value(sp_biom), "spbiom", styr, endyr_r, 0);
      write_s_yrcol(sfile, rescaled_F, "fmort", styr, endyr_r, 0);
      for (i=1;i<=nsrv;i++)  write_s_yrcol(sfile, value(pred_srv_bio(i)), srvname(i) + "_pred_bio", styr, endyr_r, 0);
      for (i=1;i<=nsrv;i++)  write_s_yrcol(sfile, value(pred_srv_abun(i)), srvname(i) + "_pred_abun", styr, endyr_r, 0);  
      write_s_yrcol(sfile, value(totbiom), "totbiom", styr, endyr_r, 0);
      write_s_yrcol(sfile, value(M(styr,endyr_r)), "M", styr, endyr_r, 0);
      for (i=1;i<=nsrv;i++){  
        if (srv_bio_flag(i) || srv_abun_flag(i)>0)
              write_s_yrcol(sfile, value(q_srv(i)), srvname(i) + "_srv_q", styr, endyr_r, 0);
      }
      write_s_yrcol(sfile, catch_bio, "catchbio", styr, endyr_r, 1);
      write_s_rownames_int(sfile, styr, endyr_r, "df", 0);

  
   
  // ------------------ START N-AT-AGE MATRIX -------------------------
  // Matrix of numbers at age, with ages as columns & years as rows

  write_s_matrix(sfile, value(natage_bin), "natage", styr, endyr_r, 1, nages_dat);
  write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names

   
 // ---------------------------PREDICTED RECRUITMENT CURVE -------------------- -------------
 // Write start of data frame for predicted recruitment curve
      
      sfile << "reccurve = structure(list(" << endl;     
      write_s_yrcol(sfile, value(SRec_spawn), "Srec.spawn", 1, 20, 0);
      write_s_yrcol(sfile, value(SRec_rec), "Srec.rec", 1, 20, 1);
      write_s_rownames_int(sfile, 1, 20, "df", 0);

  

// ------------------ FISHERY SELECTIVITY MATRIX -------------------------
// Matrix of fishery selectivity by year, with ages as columns & years as rows

  write_s_matrix(sfile, rescaled_sel_fish, "selfish", styr, endyr_r, 1, nages_dat);
  write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names

  

 // ------------------------------SURVEY SELECTIVITY MATRICES ----------------------------------------
 // write start of survey age-based selectivity curves

    //sfile << "srv_sel = structure(list(" << endl;
    for (i=1;i<=nsrv;i++){
        write_s_matrix(sfile,srv_sel_ndat(i), srvname(i) + "_sel", styr, endyr_r, 1, nages_dat);
        write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names
    //write_s_rownames_int(sfile, styr, endyr_r, "df", 0);    




    //if (i < nsrv) write_s_yrcol(sfile, value(sel_srv(i)(1,nages_dat)), srvname(i) + "_sel", 1, nages_dat, 0);
    //else {write_s_yrcol(sfile, value(sel_srv(i)(1,nages_dat)), srvname(i) + "_sel", 1, nages_dat, 1); 
    //write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "df", 0);}
    }

  
 // ------------------ OBSERVED TRAWL SURVEY TIME SERIES DATA, 1982-2002 -------------------------
        for (i=1;i<=nsrv;i++){
        sfile << srvname(i) + "_obs_bio_abun = structure(list(" << endl;
        write_s_yrcol(sfile, obs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_bio", 1, nyrs_srv_bio_abun_r(i), 0);
        write_s_yrcol(sfile, obs_srv_bio_lower(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_bio.lower", 1, nyrs_srv_bio_abun_r(i), 0);
        write_s_yrcol(sfile, obs_srv_bio_upper(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_bio.upper", 1, nyrs_srv_bio_abun_r(i), 0);
        write_s_yrcol(sfile, obs_srv_abun(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_abun", 1, nyrs_srv_bio_abun_r(i), 0);
        write_s_yrcol(sfile, obs_srv_abun_lower(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_abun.lower", 1, nyrs_srv_bio_abun_r(i), 0);
        write_s_yrcol(sfile, obs_srv_abun_upper(i)(1,nyrs_srv_bio_abun_r(i)), "obssrv_abun.upper", 1, nyrs_srv_bio_abun_r(i), 1);
        write_s_rownames_vec(sfile, yrs_srv_bio(i)(1,nyrs_srv_bio_abun_r(i)), 1, nyrs_srv_bio_abun_r(i), "df", 0);
      }

      
 // ----------------------------AGE AND LENGTH COMPOSITIONS ----------------------------------------
 // write start of age composition matrices

// fishery unbiased ages
    if(nyrs_fish_unbiased_ac_r>0)
    {
      write_s_imatrix(sfile, value(oac_fish_unbiased_r), yrs_fish_unbiased_ac_r, "oac.f.unbiased", 1,nyrs_fish_unbiased_ac_r, 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names 

      write_s_matrix(sfile, value(eac_fish_unbiased_dat), "eac.f.unbiased", styr_fish,endyr_r, 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names
    }

    //  fishery lengths
    if(nyrs_fish_lc_r>0)
    {
      write_s_imatrix(sfile, value(olc_fish_r), yrs_fish_lc_r, "olc.fish", 1,nyrs_fish_lc_r, 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names

      write_s_matrix(sfile, value(elc_fish), "elc.fish", styr_fish, endyr_r, 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names
    } 
    
    
// survey ages

  for (i=1;i<=nsrv;i++){
    if(nyrs_srv_ac_r(i)>0 )
    {
      write_s_imatrix(sfile, value(oac_srv_r(i)), yrs_srv_ac_r(i), "oac."+srvname(i), 1,nyrs_srv_ac_r(i), 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names

      write_s_matrix(sfile, value(eac_srv_dat(i)), "eac."+srvname(i), styr ,endyr_r, 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names
    }
  }

// survey lengths
for (i=1;i<=nsrv;i++){
    if(nyrs_srv_lc_r(i)>0 )
    {
      write_s_imatrix(sfile, value(olc_srv_r(i)), yrs_srv_lc_r(i), "olc."+srvname(i), 1,nyrs_srv_lc_r(i), 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names

      write_s_matrix(sfile, value(elc_srv(i)), "elc."+srvname(i), styr, endyr_r, 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names
    }
  }


// ------------------------------COMPOSITION SAMPLE SIZES ----------------------------------------
 // write the sample sizes, and effective Ns for the age and length compositions

    sfile << "fac_sample_size = structure(list(" << endl;
    
    if(comp_like_switch==1){
    write_s_yrcol(sfile, fish_unbiased_ac_samp_r, "sampsize", 1, nyrs_fish_unbiased_ac_r, 0);
    write_s_yrcol(sfile, value(fish_effn(1)(1,nyrs_fish_unbiased_ac_r)), "effn", 1, nyrs_fish_unbiased_ac_r, 1);
    write_s_rownames_vec(sfile, yrs_fish_unbiased_ac_r, 1, nyrs_fish_unbiased_ac_r, "df", 0);}
    if(comp_like_switch==2){
    write_s_yrcol(sfile, nsamples_fish_unbiased_ac(1,nyrs_fish_unbiased_ac_r), "sampsize", 1, nyrs_fish_unbiased_ac_r, 0);
    write_s_yrcol(sfile, value(fish_effn(1)(1,nyrs_fish_unbiased_ac_r)), "effn", 1, nyrs_fish_unbiased_ac_r, 1);
    write_s_rownames_vec(sfile, yrs_fish_unbiased_ac_r, 1, nyrs_fish_unbiased_ac_r, "df", 0);}
    
    sfile << "flc_sample_size = structure(list(" << endl;
    
    if(comp_like_switch==1){
    write_s_yrcol(sfile, fish_lc_samp_r, "sampsize", 1, nyrs_fish_lc_r, 0);
    write_s_yrcol(sfile, value(fish_effn(2)(1,nyrs_fish_lc_r)), "effn", 1, nyrs_fish_lc_r, 1);
    write_s_rownames_vec(sfile, yrs_fish_lc_r, 1, nyrs_fish_lc_r, "df", 0);}
    if(comp_like_switch==2){
    write_s_yrcol(sfile, nsamples_fish_lc(1,nyrs_fish_lc_r), "sampsize", 1, nyrs_fish_lc_r, 0);
    write_s_yrcol(sfile, value(fish_effn(2)(1,nyrs_fish_lc_r)), "effn", 1, nyrs_fish_lc_r, 1);
    write_s_rownames_vec(sfile, yrs_fish_lc_r, 1, nyrs_fish_lc_r, "df", 0);}

    for (i=1;i<=nsrv;i++){
     if(nyrs_srv_ac_r(i)>0 && srv_age_flag(i)>0){
     sfile << "sac_sample_size."+srvname(i)+"= structure(list(" << endl;
     if(comp_like_switch==1){
     write_s_yrcol(sfile, srv_ac_samp_r(i), "sampsize", 1, nyrs_srv_ac_r(i), 0);
     write_s_yrcol(sfile, value(sac_effn(i)(1,nyrs_srv_ac_r(i))), "effn", 1, nyrs_srv_ac_r(i), 1);
     write_s_rownames_vec(sfile, yrs_srv_ac_r(i), 1, nyrs_srv_ac_r(i), "df", 0);}
     if(comp_like_switch==2){
     write_s_yrcol(sfile, nsamples_srv_ac(i)(1,nyrs_srv_ac_r(i)), "sampsize", 1, nyrs_srv_ac_r(i), 0);
     write_s_yrcol(sfile, value(sac_effn(i)(1,nyrs_srv_ac_r(i))), "effn", 1, nyrs_srv_ac_r(i), 1);
     write_s_rownames_vec(sfile, yrs_srv_ac_r(i), 1, nyrs_srv_ac_r(i), "df", 0);}
     }
    }
    
    for (i=1;i<=nsrv;i++){     
     if(nyrs_srv_lc_r(i)>0 && srv_len_flag(i)>0){
     sfile << "slc_sample_size."+srvname(i)+"= structure(list(" << endl;
     if(comp_like_switch==1){
     write_s_yrcol(sfile, srv_lc_samp_r(i), "sampsize", 1, nyrs_srv_lc_r(i), 0);
     write_s_yrcol(sfile, value(slc_effn(i)(1,nyrs_srv_lc_r(i))), "effn", 1, nyrs_srv_lc_r(i), 1);
     write_s_rownames_vec(sfile, yrs_srv_lc_r(i), 1, nyrs_srv_lc_r(i), "df", 0);}
     if(comp_like_switch==2){
     write_s_yrcol(sfile, nsamples_srv_lc(i)(1,nyrs_srv_lc_r(i)), "sampsize", 1, nyrs_srv_lc_r(i), 0);
     write_s_yrcol(sfile, value(slc_effn(i)(1,nyrs_srv_lc_r(i))), "effn", 1, nyrs_srv_lc_r(i), 1);
     write_s_rownames_vec(sfile, yrs_srv_lc_r(i), 1, nyrs_srv_lc_r(i), "df", 0);}
     }
    }





  // -------------------PEARSON RESIDUALS------------------------------------------------
  // fishery unbiased ages
    if(nyrs_fish_unbiased_ac_r>0)
    {
      write_s_imatrix(sfile, value(fac_pearson), yrs_fish_unbiased_ac_r, "fac.pearson", 1,nyrs_fish_unbiased_ac_r, 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names 
    }

  // fishery lengths
    if(nyrs_fish_lc_r>0)
    {
      write_s_imatrix(sfile, value(flc_pearson), yrs_fish_lc_r, "flc.pearson", 1,nyrs_fish_lc_r, 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names
    }

  // survey ages and lengths
    for (i=1;i<=nsrv;i++){
    if(nyrs_srv_ac_r(i)>0 )
    {
      write_s_imatrix(sfile, value(sac_pearson(i)), yrs_srv_ac_r(i), "sac.pearson."+srvname(i), 1,nyrs_srv_ac_r(i), 1, nages_dat);
      write_s_rownames_vec(sfile, ages_dat, 1, nages_dat, "ma", 0);  // this writes COLUMN names
    }
    if(nyrs_srv_lc_r(i)>0 )
    {
      write_s_imatrix(sfile, value(slc_pearson(i)), yrs_srv_lc_r(i), "slc.pearson."+srvname(i), 1,nyrs_srv_lc_r(i), 1, nlen);
      write_s_rownames_vec(sfile, lengths, 1, nlen, "ma", 0);  // this writes COLUMN names  
    }
  }

 
  // -------------------WRTIE D-M THETA PARAMETERS---------------------------
  // Write start of likelihood components vector:
  if (comp_like_switch==2){

      sfile << "theta = structure(c(";

  //Write theta parameters:

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) {count +=1; sfile <<theta_fish(1);}
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) {count +=1; if (count == 1) sfile <<theta_fish(2); else sfile << cc << theta_fish(2);}
  
  for (i=1;i<=nsrv;i++)
  {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<theta_sac(i); else sfile << cc << theta_sac(i);}
    }   

  for (i=1;i<=nsrv;i++)
  { if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0)
      {count += 1; if (count == 1) sfile <<theta_slc(i); else sfile << cc << theta_slc(i);}
    }

   
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile <<theta_slc(i);
  sfile << ")," << endl;

  //Write names of thetas:
  sfile << ".Names = c(";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) {count +=1; sfile <<"'theta_fac'";}
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) {count +=1; if (count == 1) sfile <<"'theta_flc'"; else sfile << ",'theta_flc'";}
  
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<".theta_sac'"; else sfile << ",'"<<srvname(i)<<".theta_sac'";}
    }
  
  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<".theta_slc'";else sfile <<",'"<<srvname(i)<<".theta_slc'";}
    }  
  
  //if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) sfile <<"'theta_fac'";
  //if (fish_len_flag>0 && nyrs_fish_lc_r >0) sfile <<",'theta_flc'";
  //for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) sfile <<",'"+srvname(i)+".theta_sac'";
  //for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile <<",'"+srvname(i)+".theta_slc'";
  sfile << "))," << endl;   // Close vector of parameter values
 }


  // ------------------ START DATA LIKELIHOOD COMPONENTS VECTOR (including overall_likelihood) -------------------------
  // The vector of likelihood components will be stored as an S numeric vector.

  // Write start of likelihood components vector:
      sfile << "datalikecomp = structure(c(";
      count = 0;

  //Write likelihood component values:

  sfile << obj_fun << cc <<hf_pen<< cc << sum(sel_like) << cc <<sum(rec_like)<< cc <<fpen << cc;
  if (fish_bio_flag>0) sfile << catch_like << cc;
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) sfile <<age_like(1) << cc;
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) sfile <<age_like(2) << cc;
  for (i=1;i<=nsrv;i++)  sfile <<srv_bio_like(i) << cc;
  for (i=1;i<=nsrv;i++)  sfile <<srv_abun_like(i) << cc;
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) sfile <<age_like_sac(i) << cc;
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile <<age_like_slc(i) << cc;
  sfile << mat_like;  
  sfile << ")," << endl;

  //Write names of likelihood components:
  sfile << ".Names = c('obj_fun', 'histFpen', 'selpen', 'rec_likelihood','Fmort_pen'";
  if (fish_bio_flag>0) sfile << ",'catch.like'";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) sfile <<",'fish.ac'";
  if (fish_len_flag>0 && nyrs_fish_lc_r >0) sfile <<",'fish.lc'";
  for (i=1;i<=nsrv;i++)   sfile <<",'"+srvname(i)+".biom'";
  for (i=1;i<=nsrv;i++)   sfile <<",'"+srvname(i)+".abun'";  
  for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) sfile <<",'"+srvname(i)+".sac'";
  for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile <<",'"+srvname(i)+".slc'";
  sfile << ",'mat_like'"; 
  sfile << "))," << endl;   // Close vector of parameter values

  

  // ------------------ START PENALTY LIKELIHOOD COMPONENTS VECTOR -------------------------
  // The vector of likelihood components will be stored as an S numeric vector.
  // This only contains the likelihood for the priors and penalties (not the data)

  // Write start of likelihood components vector:
      sfile << "pen_likecomp = structure(c(";

  //Write likelihood component values:
      sfile << hf_pen  << cc;
      sfile << sum(rec_like) << cc;
      sfile << M_dev_pen << cc;
      for (j=1;j<=nbins_M;j++)
            sfile <<prior_M(j) << cc;

      for (i=1;i<=nsrv;i++) {
          for (j=1;j<=nbins_q(i);j++)
            sfile <<prior_q(i,j) << cc;
      }

      sfile << fpen << cc << sel_like(1) << cc;
      sfile << sel_like(2) << cc << sel_like(3) << cc << sel_like(4) << cc;
      sfile << sel_like(5) << cc << sel_like(6) << cc << sel_like(7) << cc;
      sfile << sel_like(8) << cc << sel_like(9);
      sfile << ")," << endl; 

  //Write names of likelihood components:
      sfile << ".Names = c('histFpen','reclike','M_dev_pen',";
      for (j=1;j<=nbins_M;j++){
            sfile <<"'prior_M_bin_";
            sfile <<j;
            sfile <<"',";
            }
      for (i=1;i<=nsrv;i++){
          for (j=1;j<=nbins_q(i);j++){
            sfile <<"'"+srvname(i)+"_prior_q_bin_";
            sfile <<j;
            sfile <<"',";
            }          
      }
      sfile << "'Fmortpen',";
      sfile << "'sel_like(1)','sel_like(2)','sel_like(3)', 'sel_like(4)',"<< endl;
      sfile << "'sel_like(5)','sel_like(6)','sel_like(7)', 'sel_like(8)','sel_like(9)'"<< endl;
      sfile << "))," << endl;   // Close vector of parameter values:

 

// ------------------ START MEAN SAMPLE WEIGHTS VECTOR -------------------------
  // The vector of mean sample weights for the age and length comps will be stored as an S numeric vector.

  // Write start of vector:
      sfile << "avgsampwts = structure(c(";

  count = 0;
  //Write the values:
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; sfile <<fish_mean_samp_wts(1);} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<fish_mean_samp_wts(2); else sfile << cc << fish_mean_samp_wts(2);}

   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<sac_mean_samp_wts(i); else sfile << cc << sac_mean_samp_wts(i);}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<slc_mean_samp_wts(i); else sfile << cc << slc_mean_samp_wts(i);}
    }  

  sfile << ")," << endl; 

  //Write names:

  count = 0;
  sfile << ".Names = c(";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; sfile <<"'fish_ac'";} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<"'fish_lc'"; else sfile << ",'fish_lc'";}
   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_age_comps'"; else sfile << ",'"<<srvname(i)<<"_age_comps'";}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_len_comps'";else sfile <<",'"<<srvname(i)<<"_len_comps'";}
    }  

  sfile << "))," << endl;   // Close vector of parameter values:

  
  
  // ------------------ START MEAN EFFECTIVE N VECTOR -------------------------
  // The vector of mean sample weights for the age and length comps will be stored as an S numeric vector.

  // Write start of vector:
      sfile << "meaneffn = structure(c(";

    count = 0;
  //Write the values:
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; sfile <<fish_mean_effn(1);} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<fish_mean_effn(2); else sfile << cc << fish_mean_effn(2);}

   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<sac_mean_effn(i); else sfile << cc << sac_mean_effn(i);}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<slc_mean_effn(i); else sfile << cc << slc_mean_effn(i);}
    }  

  sfile << ")," << endl; 

  //Write names:

  count = 0;
  sfile << ".Names = c(";
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; sfile <<"'fish_ac'";} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<"'fish_lc'"; else sfile << ",'fish_lc'";}
   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_age_comps'"; else sfile << ",'"<<srvname(i)<<"_age_comps'";}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_len_comps'";else sfile <<",'"<<srvname(i)<<"_len_comps'";}
    }  

  sfile << "))," << endl;   // Close vector of parameter values:
  


// ------------------ START RMSE VECTOR -------------------------
  // The vector of root mean square error for the reccruitment and survey data will be stored as an S numeric vector.

  // Write start of vector:
      sfile << "rmse = structure(c(";

  //Write likelihood component values:
      sfile << rec_rmse;
      for (i=1;i<=nsrv;i++)   sfile << cc <<srv_bio_rmse(i)<< cc <<srv_abun_rmse(i);
      if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) sfile << cc << fish_rmse(1);
      if (fish_len_flag>0 && nyrs_fish_lc_r >0) sfile << cc <<fish_rmse(2);
      for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 ) sfile << cc <<sac_rmse(i);
      for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile << cc <<slc_rmse(i);  
      sfile << ")," << endl; 

  //Write names of likelihood components:
      sfile << ".Names = c(";
      sfile << "'rec'";
      for (i=1;i<=nsrv;i++)   sfile <<",'"+srvname(i)+".biom'"<<",'"+srvname(i)+".abun'";
      if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) sfile <<",'fish.ac'";
      if (fish_len_flag>0 && nyrs_fish_lc_r >0) sfile <<",'fish.lc'";
      for (i=1;i<=nsrv;i++)  if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i) >0) sfile <<",'"+srvname(i)+".sac'";
      for (i=1;i<=nsrv;i++)  if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0) sfile <<",'"+srvname(i)+".slc'";
      sfile << "))," << endl;   // Close vector of parameter values:  
    
     

// ------------------ START SDNR VECTOR -------------------------
  // The standard deviation of normalized residuals of the survey and the age and length comps 

  // Write start of vector:
      sfile << "sdnr = structure(c(";

   count = 0;
   for (i=1;i<=nsrv;i++)  
    {
      {count +=1; if (count == 1) sfile <<survey_bio_sdnr(i); else sfile << cc <<survey_bio_sdnr(i);}
      {count +=1; if (count == 1) sfile <<survey_abun_sdnr(i); else sfile << cc <<survey_abun_sdnr(i);}
    }
   
  //Write the values:
  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; if (count==1) sfile <<fish_sdnr(1); else sfile << cc << fish_sdnr(1);} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<fish_sdnr(2); else sfile << cc << fish_sdnr(2);}

   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<sac_sdnr(i); else sfile << cc << sac_sdnr(i);}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<slc_sdnr(i); else sfile << cc << slc_sdnr(i);}
    }  

  sfile << ")," << endl; 

  //Write names:

  count = 0;
  sfile << ".Names = c(";

  for (i=1;i<=nsrv;i++)  
    {
      {count +=1; if (count == 1) sfile <<"'"<<srvname(i)+".biom'"; else sfile << ",'"<<srvname(i)+".biom'";}
      {count +=1; if (count == 1) sfile <<"'"<<srvname(i)+".abun'"; else sfile << ",'"<<srvname(i)+".abun'";}
    }    

  if (fish_age_flag>0 && nyrs_fish_unbiased_ac_r >0) 
    {count += 1; if (count == 1) sfile <<"'fish_ac'"; else sfile <<",'fish_ac'";} 

  if (fish_len_flag>0 && nyrs_fish_lc_r >0)
    {count += 1; if (count == 1) sfile <<"'fish_lc'"; else sfile << ",'fish_lc'";}
   
  for (i=1;i<=nsrv;i++)
    {if (srv_age_flag(i)>0 && nyrs_srv_ac_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_age_comps'"; else sfile << ",'"<<srvname(i)<<"_age_comps'";}
    }

  for (i=1;i<=nsrv;i++)
    {if (srv_len_flag(i)>0 && nyrs_srv_lc_r(i)>0 )
      {count += 1; if (count == 1) sfile <<"'"<<srvname(i)<<"_len_comps'";else sfile <<",'"<<srvname(i)<<"_len_comps'";}
    }  

  sfile << "))," << endl;   // Close vector of parameter values:
  

// ------------------ START SELECTIVITY BINS -------------------------
  // The number of time bins for the fishery and survey selectivity

  // Write start of vector:
      sfile << "fishselbins = structure(c(";

  //Write the values:
      sfile << nbins_fsh_sel;
      sfile << ")," << endl;

  //Write names:
      sfile << ".Names = c('fishselbins'"<< endl; 
      sfile << "))," << endl;   // Close 

  
  // Write start of vector:
      sfile << "srvselbins = structure(c(";
      
  //Write the values:
      count = 0;
      for (i=1;i<=nsrv;i++)
      {
         {count +=1; if (count == 1) sfile <<nbins_srv_sel(i); else sfile << cc <<nbins_srv_sel(i);}
      }   
      sfile << ")," << endl;

  //Write names:
      sfile << ".Names = c(";
      count = 0;
      for (i=1;i<=nsrv;i++)  
      {
        {count +=1; if (count == 1) sfile <<"'"<<srvname(i)+"'"; else sfile << ",'"<<srvname(i)+"'";}
      }

      sfile << "))," << endl;   // Close 

  

  // ------------------ START  CONTROL RULE VECTOR -------------------------
  // The vector of info needed for mapping the control rule will be stored as an S numeric vector.

  // Write start of likelihood components vector:
      sfile << "controlrule = structure(c(";

  //Write likelihood component values:
      sfile << F40 << cc;
      sfile << F35 << cc;
      sfile << SBF40 << cc;
      sfile << SBF35 << cc;
      sfile << max(1977,styr)+rec_age << cc;
      sfile << lastyr_rec << cc;
      sfile << lastyr_rec_a10; 
      sfile << ")," << endl; 

  //Write names:
      sfile << ".Names = c('F40','F35','SBF40','SBF35','styr_rec','endyr_rec','endyr_rec_a10'"<< endl;
      sfile << "))" << endl;   // Close vector of control rule values:

   
  //-------- Write close of overall S list structure:--------------------------------------------
  // This is the closing punctuation plus the names of the constituent elements of the list

     sfile << "))" << endl;   // Close 
  
  //sfile << "), .Names = c('t.series'";
  //sfile << "))" << endl;

  

  //-------- END OF CXX FILE --------------------------------------------
