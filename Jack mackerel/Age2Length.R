# Age and length bins
nages = 12
len_bins = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50)
nlengths = length(len_bins)
mu_age <- c()

# Growth parameters
Linf    = 73.56
k_coeff = 0.16
Lo      = 13.56
sdage   = 0.09
lw_a = 0.007778994e-3 # LW parameters from Talcahuano sampling, 2008
lw_b = 3.089248476 # LW parameters from Talcahuano sampling, 2008

# Calculate mean length-at-age
mu_age[1]=Lo # first length (modal)
for (i in 2:nages){
    mu_age[i] = Linf*(1.-exp(-k_coeff))+exp(-k_coeff)*mu_age[i-1] # the mean length by age group
}
wt_age_vb = lw_a * mu_age^lw_b
maturity_vb = 1/(1 + exp(32.93 - 1.45*mu_age))
sigma_age=sdage*mu_age # standard deviation of length-at-age
P_age2len = ALK(mu_age, sigma_age, len_bins)


# Calculate transition matrix
pdf <- matrix(0, nages, nlengths)
for(i in 1:nages){
    for(j in 1:nlengths){
        if(j<nlengths){
            xs=0.5*(len_bins[2]-len_bins[1])  # accounts for variable bin-widths...?
        }
        z1=((len_bins[j]-xs)-mu_age[i])/sigma_age[i]
        z2=((len_bins[j]+xs)-mu_age[i])/sigma_age[i]
        pdf[i,j]=pnorm(z2)-pnorm(z1)
    }
    pdf[i,] = pdf[i,]/sum(pdf[i,])
}

write.csv(pdf, file = "JJM_pdf.csv")
