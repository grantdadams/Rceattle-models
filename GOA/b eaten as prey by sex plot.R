
#' Plot biomass eaten as prey by sex
#'
#' @description Function the plots the ration across ages (minage:nages) as estimated from Rceattle. Returns and saves a figure with the ration trajectory. Ration is multiplied by biomass-at-age/sex to get population level estimates
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param minage minage to plot ration (i.e. age "minage"+)
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param spnames Species names for legend
#' @param species Which species to plot e.g. c(1,4). Default = NULL plots them all
#' @param lwd Line width as specified by user
#' @param right_adj Multiplier for to add to the right side of the figure for fitting the legend.
#' @param minyr first year to plot
#' @param incl_proj TRUE/FALSE include projections years
#' @param incl_mean TRUE/FALSE include time series mean as horizontal line
#' @param add_ci TRUE/FALSE, includes 95 percent confidence interval
#'
#' @export
#'
plot_b_eaten_sex <-
  function(Rceattle,
           file = NULL,
           minage = 1,
           model_names = NULL,
           line_col = NULL,
           spnames = NULL,
           species = NULL,
           lwd = 3,
           lty = 1,
           right_adj = 0,
           minyr = NULL,
           width = 7,
           height = 6.5,
           incl_proj = FALSE,
           incl_mean = FALSE,
           add_ci = FALSE) {
    
    # Convert single one into a list
    if(class(Rceattle) == "Rceattle"){
      Rceattle <- list(Rceattle)
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
    maxyr <- max((sapply(years, max)))
    if(is.null(minyr)){minyr <- min((sapply(years, min)))}
    nsex <- Rceattle[[1]]$data_list$nsex
    nspp <- Rceattle[[1]]$data_list$nspp
    nages <- Rceattle[[1]]$data_list$nages
    
    if(is.null(species)){
      species <- 1:nspp
    }
    
    # Get ration across ages
    ration <-
      array(NA, dim = c(nspp, 2, nyrs, length(Rceattle)))
    for (i in 1:length(Rceattle)) {
      for(sp in 1:nspp){
        for(yr in 1:nyrs_vec[i]){
          ration[sp, , yr, i] <- rowSums(Rceattle[[i]]$quantities$B_eaten_as_prey[sp,,1:nages[sp],yr])/1e6
        }
      }
    }
    
    # Plot limits
    ymax <- matrix(0, nrow = nspp, ncol = 2)
    ymin <- matrix(0, nrow = nspp, ncol = 2)
    for (i in 1:nspp) {
      for(sex in 1:nsex[i]){
        ymax[i,sex] <- max(c(ration[i,sex,, ],0), na.rm = T)
        ymin[i,sex] <- min(c(ration[i,sex,, ]), na.rm = T)
      }
    }
    ymax <- ymax + 0.15 * ymax
    ymin <- ymin - 0.15 * ymin
    
    if (is.null(line_col)) {
      line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
    }
    
    
    if(length(lty) != length(Rceattle)){
      lty = rep(lty, length(Rceattle))
    }
    
    
    # Plot trajectory
    loops <- ifelse(is.null(file), 1, 2)
    for (i in 1:loops) {
      if (i == 2) {
        filename <- paste0(file, "_biomass_eaten_as_prey_trajectory.png")
        png(
          file = filename ,
          width = width,
          height = height,
          units = "in",
          res = 300
        )
      }
      
      # Plot configuration
      layout(matrix(1:(sum(nsex[species]) + 2), nrow = (sum(nsex[species]) + 2)), heights = c(0.1, rep(1, sum(nsex[species])), 0.2))
      par(
        mar = c(0, 3 , 0 , 1) ,
        oma = c(0 , 0 , 0 , 0),
        tcl = -0.35,
        mgp = c(1.75, 0.5, 0)
      )
      plot.new()
      ind = 0
      
      for (j in 1:length(species)) {
        
        spp = species[j]
        
        for(sex in 1:nsex[spp]){
          ind = ind+1
          
          # Get sex for legend
          legend_sex = sex
          legend_sex2 = ifelse(sex == 1, "female", "male")
          if(nsex[spp] == 1){
            legend_sex <- 0
            legend_sex2 = "combined"
          }
          
          plot(
            y = NA,
            x = NA,
            ylim = c(ymin[spp, sex], ymax[spp,sex]),
            xlim = c(minyr, maxyr + (maxyr - minyr) * right_adj),
            xlab = "Year",
            ylab = paste0("Biomass eaten (million  mt)"),
            xaxt = c(rep("n", sum(nsex[species]) - 1), "s")[ind]
          )
          
          
          # Horizontal line
          if(incl_proj){
            abline(v = Rceattle[[length(Rceattle)]]$data_list$meanyr, lwd  = lwd, col = "grey", lty = 2)
          }
          
          # Species legends
          legend("topleft", paste0(spnames[spp], " ", legend_sex2), bty = "n", cex = 1)
          
          # Model names legends
          if (ind == 1) {
            if(!is.null(model_names)){
              legend(
                "topright",
                legend = model_names,
                lty = rep(1, length(line_col)),
                lwd = lwd,
                col = line_col,
                bty = "n",
                cex = 0.72
              )
            }
          }
          
          # M-at-age
          for (k in 1:dim(ration)[4]) {
            lines(
              x = years[[k]],
              y = ration[spp, sex, 1:length(years[[k]]), k],
              lty = lty[k],
              lwd = lwd,
              col = line_col[k]
            ) # Median
          }
          
          
          # Average across time
          if(incl_mean){
            abline(h = mean(ration[spp, sex, 1:length(years[[1]]), ]), lwd  = lwd, col = "grey", lty = 1)
          }
        }
      }
      
      
      if (i == 2) {
        dev.off()
      }
    }
  }

