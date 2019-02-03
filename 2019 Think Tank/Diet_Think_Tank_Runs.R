library(Rceattle)
data(BS2017SS) 

###################################################
# Run in single species mode
ss_no_re <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ss_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

###################################################
# Run in multi species mode
data(BS2017MS)
ms_no_re <- Rceattle(data_list = BS2017MS,
                   inits = ms_no_re$estimated_params, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ms_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 1, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)


###################################################
# Run with length gamma
ms_run1 <- Rceattle(data_list = BS2017MS,
                     inits = ms_no_re$estimated_params, # Initial parameters = 0
                     file_name = "2019 Think Tank/Models/ms_diet1", # Don't save
                     debug = 0, # Estimate
                     random_rec = FALSE, # No random recruitment
                     msmMode = 1, # Single species mode
                     suitMode = 1,
                     avgnMode = 0,
                     silent = TRUE)

###################################################
# Run with time-varying length gamma
ms_run2 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "2019 Think Tank/Models/ms_diet2", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 2,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying weight gamma
ms_run3 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "2019 Think Tank/Models/ms_diet3", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 3,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with length log-normal
ms_run4 <- Rceattle(data_list = BS2017MS,
                    inits = mod$estimated_params, # Initial parameters = 0
                    file_name = "2019 Think Tank/Models/ms_diet4", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 4,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying length log-normal
ms_run5 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "2019 Think Tank/Models/ms_diet5", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 5,
                    avgnMode = 0,
                    silent = FALSE)

###################################################
# Run with time-varying weight log-normal
ms_run6 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "2019 Think Tank/Models/ms_diet6", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 6,
                    avgnMode = 0,
                    silent = TRUE)
