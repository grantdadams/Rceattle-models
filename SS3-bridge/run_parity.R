# Run the SS3 -> Rceattle parity gates G1 and G2 for one stock.
#
# From the Rceattle-models folder:
#   Rscript SS3-bridge/run_parity.R "AI cod - Dev"
#   Rscript SS3-bridge/run_parity.R "GOA cod"
#
# The stock's Bridging/ss3_to_ceattle_forward_pass.R builds the Rceattle model
# with SS3's MLE injected (fp) and reads the SS3 report (ss3_rep); parity_check.R
# scores it. The result is saved as Bridging/_parity_latest.rds.

args  <- commandArgs(trailingOnly = TRUE)
stock <- if (length(args)) args[1] else stop("give the stock folder, e.g. \"AI cod - Dev\"")
setwd(stock)
source("Bridging/ss3_to_ceattle_forward_pass.R")
source("../SS3-bridge/parity_check.R")
# The stock's forward pass may define `fixed_in_ss3`: Rceattle parameter blocks
# whose SS3 counterparts have a negative phase. SS3 never moved those, so its
# solution says nothing about their gradient.
res <- parity_report(fp, ss3_rep,
                     fixed_in_ss3 = if (exists("fixed_in_ss3")) fixed_in_ss3 else character())
saveRDS(res, "Bridging/_parity_latest.rds")
