# Grant Adams, Kirstin Holsman, Andre Punt - April 2019
# Function to run CEATTLE model in TMB
# Citation:
# Holsman, K. K., Ianelli, J., Aydin, K., Punt, A. E., and Moffitt, E. A. 2015. A comparison of fisheries biological reference points estimated from temperature-specific multi-species and single-species climate-enhanced stock assessment models. Deep-Sea Research Part II: Topical Studies in Oceanography, 134: 360–378.

# Install devtools if you don't already have it
install.packages("devtools")
# On Windows install the Rtools matching your R-version.Install rtools (https://cran.r-project.org/bin/windows/Rtools/).
# Install TMB 
# Instructions can be found here for non-pc: https://github.com/kaskr/adcomp/wiki/Download
install.packages('TMB', type = 'source')
# Try "TMB::runExample(all = TRUE)" to see if TMB works
# Most common errors are related to package versioning or installation failure
# Please make sure that the r session is restarted
# For example, installing the packages "glue" and "Rcpp" may have to be done independently

# Install Rceattle
devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")



library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
# data(BS2017SS) # ?BS2017SS for more information on the data
# Write data to excel
# Rceattle::write_data(data_list = BS2017SS, file = "BS2017SS.xlsx")


