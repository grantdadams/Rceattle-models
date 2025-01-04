#' plot_ssb
#'
#' @description Function the plots the mean ssb and 95% CI trends as estimated from Rceattle
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param species Which species to plot e.g. c(1,4). Default = NULL plots them all
#' @param spnames Species names for legend
#' @param add_ci If the confidence interval is to be added
#' @param lwd Line width as specified by user
#' @param right_adj Multiplier for to add to the right side of the figure for fitting the legend.
#' @param mohns data.frame of mohn's rows extracted from \code{\link{retrospective}}
#' @param minyr First year to plot
#' @param height
#' @param width
#' @param save Save ssb?
#' @param incl_proj TRUE/FALSE, include projection years
#' @param mod_cex Cex of text for model name legend
#' @param mod_avg Vector of length Rceattle denoting if it is a model average object
#' @param mse Is if an MSE object from \code{\link{load_mse}} or \code{\link{mse_run}}
#' @param OM if mse == TRUE, use the OM (TRUE) or EM (FALSE) for plotting?
#'
#' @export
#'
#' @return Returns and saves a figure with the population trajectory.
plot_ssb <- function(Rceattle,
                     file = NULL,
                     model_names = NULL,
                     line_col = NULL,
                     species = NULL,
                     spnames = NULL,
                     add_ci = FALSE,
                     lwd = 3,
                     save = FALSE,
                     right_adj = 0,
                     width = 7,
                     height = 6.5,
                     minyr = NULL,
                     ymax = NULL,
                     maxyr = NULL,
                     incl_proj = FALSE,
                     mod_cex = 1,
                     alpha = 0.4,
                     mod_avg = rep(FALSE, length(Rceattle)),
                     mse = FALSE,
                     OM = TRUE,
                     reference = NULL) {
  
  # Convert mse object to Rceattle list
  if(mse){
    if(OM){
      Rceattle <- lapply(Rceattle, function(x) x$OM)
    }
    if(!OM){
      Rceattle <- lapply(Rceattle, function(x) x$EM[[length(x$EM)]])
    }
    nmse = length(Rceattle)
    add_ci = TRUE
    incl_proj = TRUE
  }
  
  
  # Convert single one into a list
  if(class(Rceattle) == "Rceattle"){
    Rceattle <- list(Rceattle)
  }
  
  # Add reference model
  if(!is.null(reference)){
    Rceattle <- c(Rceattle, list(reference))
  }
  
  # Species names
  if(is.null(spnames)){
    spnames =  Rceattle[[1]]$data_list$spnames
  }
  
  # Extract data objects
  Endyrs <-  sapply(Rceattle, function(x) x$data_list$endyr)
  years <- lapply(Rceattle, function(x) x$data_list$styr:x$data_list$endyr)
  if(incl_proj){
    years <- lapply(Rceattle, function(x) x$data_list$styr:x$data_list$projyr)
  }
  
  max_endyr <- max(unlist(Endyrs), na.rm = TRUE)
  nyrs_vec <- sapply(years, length)
  nyrs <- max(nyrs_vec)
  if(is.null(maxyr)){maxyr <- max((sapply(years, max)))}
  if(is.null(minyr)){minyr <- min((sapply(years, min)))}
  
  nspp <- Rceattle[[1]]$data_list$nspp
  
  minage <- Rceattle[[1]]$data_list$minage
  estDynamics <- Rceattle[[1]]$data_list$estDynamics
  
  
  if(is.null(species)){
    species <- 1:nspp
  }
  spp <- species
  
  
  # Get depletion
  quantity <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  quantity_sd <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  log_quantity_sd <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  log_quantity_mu <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  ptarget = matrix(NA, nrow = length(Rceattle), ncol = nspp)
  plimit = matrix(NA, nrow = length(Rceattle), ncol = nspp)
  
  for (i in 1:length(Rceattle)) {
    
    # - Get quantities
    ptarget[i,] <- Rceattle[[i]]$data_list$Ptarget
    plimit[i,] <- Rceattle[[i]]$data_list$Plimit
    quantity[, 1:nyrs_vec[i] , i] <- Rceattle[[i]]$quantities$biomassSSB[,1:nyrs_vec[i]]
    
    # Get SD of quantity
    if(add_ci & !mse){
      sd_temp <- which(names(Rceattle[[i]]$sdrep$value) == "biomassSSB")
      sd_temp <- Rceattle[[i]]$sdrep$sd[sd_temp]
      quantity_sd[,  1:nyrs_vec[i], i] <-
        replace(quantity_sd[, 1:nyrs_vec[i], i], values = sd_temp[1:(nyrs_vec[i] * nspp)])
    }
    
    # - Model average
    if(mod_avg[i]){
      log_quantity_sd[,  1:nyrs_vec[i], i] <- apply(Rceattle[[i]]$asymptotic_samples$biomassSSB[,1:nyrs_vec[i],], c(1,2), function(x) sd(as.vector(log(x))))
      log_quantity_mu[,  1:nyrs_vec[i], i] <- apply(Rceattle[[i]]$asymptotic_samples$biomassSSB[,1:nyrs_vec[i],], c(1,2), function(x) mean(as.vector(log(x))))
    }
  }
  
  ## Get confidence intervals
  # - Single model
  if(!mse){
    quantity_upper95 <- quantity + quantity_sd * 1.92
    quantity_lower95 <- quantity - quantity_sd * 1.92
    
    quantity_upper50 <- quantity + quantity_sd * 0.674
    quantity_lower50 <- quantity - quantity_sd * 0.674
  }
  
  # - MSE objects
  if(mse){
    ptarget <- ptarget[1,]
    plimit <- plimit[1,]
    
    # -- Get quantiles and mean across simulations
    quantity_upper95 <- apply( quantity[,,1:nmse], c(1,2), function(x) quantile(x, probs = 0.975) )
    quantity_lower95 <- apply( quantity[,,1:nmse], c(1,2), function(x) quantile(x, probs = 0.025) )
    quantity_upper50 <- apply( quantity[,,1:nmse], c(1,2), function(x) quantile(x, probs = 0.75) )
    quantity_lower50 <- apply( quantity[,,1:nmse], c(1,2), function(x) quantile(x, probs = 0.25) )
    
    # -- Put back in array for indexing below
    if(is.null(reference)){
      quantity <- array(apply( quantity[,,1:nmse], c(1,2), mean ), dim = c(nspp, nyrs,  1))
      quantity_upper95 <- array(quantity_upper95, dim = c(nspp, nyrs,  1))
      quantity_lower95 <- array(quantity_lower95, dim = c(nspp, nyrs,  1))
      quantity_upper50 <- array(quantity_upper50, dim = c(nspp, nyrs,  1))
      quantity_lower50<- array(quantity_lower50, dim = c(nspp, nyrs,  1))
    } else {
      quantity_upper95 <- array(c(quantity_upper95, quantity[,,nmse+1]), dim = c(nspp, nyrs,  2))
      quantity_lower95 <- array(c(quantity_lower95, quantity[,,nmse+1]), dim = c(nspp, nyrs,  2))
      quantity_upper50 <- array(c(quantity_upper50, quantity[,,nmse+1]), dim = c(nspp, nyrs,  2))
      quantity_lower50<- array(c(quantity_lower50, quantity[,,nmse+1]), dim = c(nspp, nyrs,  2))
      quantity <- array(c(apply( quantity[,,1:nmse], c(1,2), mean ), quantity[,,nmse+1]), dim = c(nspp, nyrs,  2))
    }
  }
  
  # - Model Average
  for (i in 1:length(Rceattle)) {
    if(mod_avg[i]){
      quantity[,,i] <- qlnorm(0.5, meanlog = log_quantity_mu[,,i], sdlog = log_quantity_sd[,,i])
      quantity_upper95[,,i] <- qlnorm(0.975, meanlog = log_quantity_mu[,,i], sdlog = log_quantity_sd[,,i])
      quantity_lower95[,,i] <- qlnorm(0.025, meanlog = log_quantity_mu[,,i], sdlog = log_quantity_sd[,,i])
    }
  }
  
  # - Rescale
  quantity <- quantity / 1000000
  quantity_upper95 <- quantity_upper95 / 1000000
  quantity_lower95 <- quantity_lower95 / 1000000
  quantity_upper50 <- quantity_upper50 / 1000000
  quantity_lower50 <- quantity_lower50 / 1000000
  
  
  ## Save
  if (save) {
    for (i in 1:nspp) {
      dat <- data.frame(quantity[i, , ])
      datup <- data.frame(quantity_upper95[i, , ])
      datlow <- data.frame(quantity_lower95[i, , ])
      
      dat_new <- cbind(dat[, 1], datlow[, 1], datup[, 1])
      colnames(dat_new) <- rep(model_names[1], 3)
      
      for (j in 2:ncol(dat)) {
        dat_new2 <- cbind(dat[, j], datlow[, j], datup[, j])
        colnames(dat_new2) <- rep(model_names[j], 3)
        dat_new <- cbind(dat_new, dat_new2)
        
      }
      
      write.csv(dat_new, file = paste0(file, "_ssb_species_", i, ".csv"))
    }
  }
  
  
  # - Plot limits
  if(is.null(ymax)){
    ymax <- c()
    ymin <- c()
    for (sp in 1:nspp) {
      if (add_ci & (estDynamics[sp] == 0)) {
        ymax[sp] <- max(c(quantity_upper95[sp, , ], 0), na.rm = T)
        ymin[sp] <- min(c(quantity_upper95[sp, , ], 0), na.rm = T)
      } else{
        ymax[sp] <- max(c(quantity[sp, , ], 0), na.rm = T)
        ymin[sp] <- min(c(quantity[sp, , ], 0), na.rm = T)
      }
    }
    ymax <- ymax * 1.2
  }
  ymin = c(0,0,0)
  
  
  # - Line colors
  if (is.null(line_col)) {
    if(!mse){
      line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
    }
    if(mse){
      line_col <- 1
    }
  }
  if(!is.null(reference)){
    line_col <- c(line_col, 1)
  }
  
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (i in 1:loops) {
    if (i == 2) {
      filename <- paste0(file, "_ssb_trajectory", ".png")
      png(
        file = filename ,
        width = width,# 169 / 25.4,
        height = height,# 150 / 25.4,
        units = "in",
        res = 300
      )
    }
    
    # Plot configuration
    layout(matrix(1:(length(spp) + 2), nrow = (length(spp) + 2)), heights = c(0.1, rep(1, length(spp)), 0.2))
    par(
      mar = c(0, 3 , 0 , 1) ,
      oma = c(0 , 0 , 0 , 0),
      tcl = -0.35,
      mgp = c(1.75, 0.5, 0)
    )
    plot.new()
    
    for (j in 1:length(spp)) {
      plot(
        y = NA,
        x = NA,
        ylim = c(ymin[spp[j]], ymax[spp[j]]),
        xlim = c(minyr, maxyr + (maxyr - minyr) * right_adj),
        xlab = "Year",
        ylab = NA,
        xaxt = c(rep("n", length(spp) - 1), "s")[j]
      )
      
      if(j == 2){
        mtext("Spawning stock biomass (million mt)", side = 2, line = 1.7, cex = 0.9)
      }
      
      # Horizontal line at end yr
      if(incl_proj){
        abline(v = Rceattle[[length(Rceattle)]]$data_list$meanyr, lwd  = lwd, col = "grey", lty = 2)
      }
      
      # Legends
      legend("topleft",
             legend = spnames[spp[j]],
             bty = "n",
             cex = 1)
      
      if (spp[j] == 1) {
        if(!is.null(model_names)){
          legend(
            "topright",
            legend = model_names,
            lty = rep(1, length(line_col)),
            lwd = lwd,
            col = line_col,
            bty = "n",
            cex = mod_cex
          )
        }
      }
      
      
      # Credible interval
      if(estDynamics[spp[j]] == 0){
        if (add_ci) {
          for (k in 1:dim(quantity)[3]) {
            # - 95% CI
            polygon(
              x = c(years[[k]], rev(years[[k]])),
              y = c(quantity_upper95[spp[j], 1:length(years[[k]]), k], rev(quantity_lower95[spp[j], 1:length(years[[k]]), k])),
              col = adjustcolor( line_col[k], alpha.f = alpha/2),
              border = NA
            )
            
            # - 50% CI
            if(mse){
              polygon(
                x = c(years[[k]], rev(years[[k]])),
                y = c(quantity_upper50[spp[j], 1:length(years[[k]]), k], rev(quantity_lower50[spp[j], 1:length(years[[k]]), k])),
                col = adjustcolor( line_col[k], alpha.f = alpha),
                border = NA
              )
            }
          }
        }
      }
      
      # Mean quantity
      for (k in 1:dim(quantity)[3]) {
        lines(
          x = years[[k]],
          y = quantity[spp[j], 1:length(years[[k]]), k],
          lty = 1,
          lwd = lwd,
          col = line_col[k]
        ) # Median
      }
    }
    
    
    if (i == 2) {
      dev.off()
    }
  }
}
