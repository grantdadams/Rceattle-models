//-----TRANSFORMATION FUNCION AGE->LENGTH--------------------------------------------------
FUNCTION Get_Age2length
// This subroutine allows convert an age composition to length composition. For example: if there is a matrix C(1,nyears,1,nages),
// the vectorial operation: Cl=C*Prob_length,  returns a matrix Cl(1,nyears,1,nlength) whose sum over all the lengths is the same
// the sum over all age groups..
// (Cristian Canales)
// by default values
// Linf=Linfprior;// Asymptotic length
// k_coeff=kprior;
// Lo=Loprior;// first length (corresponds to first age-group)
// sdage=sdageprior;// coefficient of variation of length-at-age
// if some of these are estimated.
Linf    = mfexp(log_Linf);
k_coeff = mfexp(log_k);
Lo      = mfexp(log_Lo);
sdage   = mfexp(log_sdage);
for (r=1;r<=ngrowth;r++)
{
    int i, j;
    mu_age(r,1)=Lo(r); // first length (modal)
    for (i=2;i<=nages;i++)
        mu_age(r,i) = Linf(r)*(1.-exp(-k_coeff(r)))+exp(-k_coeff(r))*mu_age(r,i-1); // the mean length by age group
    wt_age_vb(r) = lw_a * pow(mu_age(r), lw_b);
    maturity_vb(r) = 1/(1 + exp(32.93 - 1.45*mu_age(r)));
    sigma_age(r)=sdage(r)*mu_age(r); // standard deviation of length-at-age
    P_age2len(r) = ALK( mu_age(r), sigma_age(r), len_bins);
}
FUNCTION dvar_matrix ALK(dvar_vector& mu, dvar_vector& sig, dvector& x)
    //RETURN_ARRAYS_INCREMENT();
    int i, j;
dvariable z1;
dvariable z2;
int si,ni; si=mu.indexmin(); ni=mu.indexmax();
int sj,nj; sj=x.indexmin(); nj=x.indexmax();
dvar_matrix pdf(si,ni,sj,nj);
double xs;
pdf.initialize();
for(i=si;i<=ni;i++) //loop over ages
{
    for(j=sj;j<=nj;j++) //loop over length bins
    {
        if (j<nj)
            xs=0.5*(x[sj+1]-x[sj]);  // accounts for variable bin-widths...?
        z1=((x(j)-xs)-mu(i))/sig(i);
        z2=((x(j)+xs)-mu(i))/sig(i);
        pdf(i,j)=cumd_norm(z2)-cumd_norm(z1);
    }//end nbins
    pdf(i)/=sum(pdf(i));
}//end nage
//RETURN_ARRAYS_DECREMENT();
return(pdf);