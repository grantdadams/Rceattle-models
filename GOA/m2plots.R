

#' Plot M1 + M2
#'
#' @description Function the plots the M1 and M2 as estimated from Rceattle
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param incl_proj Include the projection years (TRUE/FALSE)
#' @param zlim zlim for M1 + M2 plots. Character - use max range across species in model. NULL - use species specific ranges. Vector of two.
#' @param type 0 = Tiles, 1 = contour, 2 = facet lines, 3 = persp
#' @param width Plot width when saved "inches"
#' @param height Plot height when saved "inches"
#' @param title Additional title to add. Will also add species names if not NULL
#' @param title_cex Font size for title
#' @param spp Species to plot. Plots all if null.
#' @param log TRUE/FALSE use log M1 + M2
#' @param minyr First year to plot
#' @param theta theta for persp plot
#' @param maxage Plot up to this age. Plots all ages if NULL
#' @param M2 TRUE/FALSE Use M2 only (True) or total M (False)
#'
#' @export
plot_mortality <-
  function(Rceattle,
           file = NULL,
           incl_proj = FALSE,
           zlim = NULL,
           type = 0,
           width = 8,
           height = 5.5,
           title = NULL,
           log = FALSE,
           minyr = NULL,
           theta = 155,
           spp = NULL,
           maxage = NULL,
           title_cex = 10,
           M2 = TRUE) {
    
    # Convert single one into a list
    if(class(Rceattle) == "Rceattle"){
      Rceattle <- list(Rceattle)
    }
    
    if(length(Rceattle) > 1){
      stop("Can only plot one model")
    }
    
    # Extract data objects
    if(is.null(minyr)){ minyr <- Rceattle[[1]]$data_list$styr}
    
    Years <- minyr:Rceattle[[1]]$data_list$endyr
    if(incl_proj){
      Years <- minyr:Rceattle[[1]]$data_list$projyr
    }
    nyrs_vec <- length(Years)
    nyrs <- max(nyrs_vec)
    maxyr <- max((sapply(Years, max)))
    
    nspp <- Rceattle[[1]]$data_list$nspp
    spnames <- Rceattle[[1]]$data_list$spnames
    estdynamics <- Rceattle[[1]]$data_list$estDynamics
    nages <- Rceattle[[1]]$data_list$nages
    
    if(!is.null(maxage)){
      nages <- sapply(nages, function(x) ifelse(x > maxage, maxage, x))
    }
    
    
    minage <- Rceattle[[1]]$data_list$minage
    nsex <- Rceattle[[1]]$data_list$nsex
    
    # Get M
    M_array <-
      array(NA, dim = c(nspp, 2, max(nages), nyrs, length(Rceattle)))
    M1_array <-
      array(NA, dim = c(nspp, 2, max(nages), length(Rceattle)))
    for (i in 1:length(Rceattle)) {
      M1_array[, , ,i] <- Rceattle[[i]]$quantities$M1[,,1:max(nages)]
      if(!M2){
        M_array[, , , ,i] <- Rceattle[[i]]$quantities$M[,,1:max(nages),(1:nyrs)+(minyr - Rceattle[[1]]$data_list$styr)]
      }
      if(M2){
        M_array[, , , ,i] <- Rceattle[[i]]$quantities$M2[,,1:max(nages),(1:nyrs)+(minyr - Rceattle[[1]]$data_list$styr)]
      }
    }
    
    if(log){
      M1_array = log(M1_array)
      M_array = log(M_array)
    }
    
    # Plot limits
    zmax <- c()
    zmin <- c()
    for (i in 1:dim(M_array)[1]) {
      zmax[i] <- max(c(M_array[i,,,,], 0), na.rm = T)
      zmin[i] <- min(c(M_array[i,,,,], 0), na.rm = T)
    }
    
    
    # Plot trajectory
    loops <- ifelse(is.null(file), 1, 2)
    
    #################################
    # Mortality time series
    #################################
    if(is.null(spp)){
      spp <- 1:nspp
    }
    
    
    # Species
    for(j in 1:nspp){
      sp <- j
      
      if(estdynamics[j] == 0 & sp %in% spp){
        
        # Sexes
        for(sex in 1:nsex[sp]){
          
          # Get sex for legend
          legend_sex = sex
          legend_sex2 = ifelse(sex == 1, "Female", "Male")
          if(nsex[sp] == 1){
            legend_sex <- 0
            legend_sex2 = "Combined"
          }
          
          # Save
          for (i in 1:loops) {
            if (i == 2) {
              filename <- paste0(file, "predation_and_residual_mortality_spp_",sp,"_sex_",legend_sex2,".png")
              png(
                file = filename ,
                width = width,
                height = height,
                units = "in",
                res = 300
              )
            }
            
            # Subset mortality data
            m_subset <- (M_array[j, sex, (1:nages[sp]), 1:nyrs, 1])
            
            # Get ages
            ages <- (1:(nages[sp])) - 1 + minage[sp]
            
            # Rearrange data
            data <- data.frame(Year = rep(Years, each = length(ages)), Age = rep(ages, length(Years)), M = c(m_subset))
            
            # Plot limits
            if(is.null(zlim)){
              zlim <- c(zmin[sp], zmax[sp])
            }
            
            if(is.character(zlim)){
              zlim <- c(min(zmin), max(zmax))
            }
            
            # Plot as tiles
            if(type == 0){
              p = ggplot2::ggplot(data, aes(y = Age, x = Year, zmin = zlim[1], zmax = zlim[2])) + geom_tile(aes(fill = M))  + scale_y_continuous(expand = c(0, 0), breaks=seq(0,max(ages),round(nages[sp]/5))) + coord_equal() +  scale_x_continuous(expand = c(0, 0))+ theme( panel.border = element_rect(colour = "black", fill=NA, size=1))
              if(!is.null(title)){
                p = p + ggtitle(paste0(title,": ",spnames[j] )) + theme(plot.title = element_text(size = title_cex))
              }
              if(log){
                p = p + scale_fill_viridis_c("log(M1 + M2)", limits = c(zlim[1], zlim[2]))
              } else {
                p = p + scale_fill_viridis_c("M1 + M2", limits = c(zlim[1], zlim[2]))
              }
              print(p)
            }
            
            
            # Plot as contours
            if(type == 1){
              print(ggplot2::ggplot(data, aes(y = Age, x = Year, z = M, zmin = zlim[1], zmax = zlim[2])) + geom_contour(colour = 1, size = 0.5) + geom_contour_filled()  + scale_y_continuous(expand = c(0, 0), breaks=seq(0,max(ages),round(nages[sp]/5))) +  scale_x_continuous(expand = c(0, 0)) + theme( panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_fill_viridis_d("M1 + M2"))
            }
            
            # Plot as facets
            if(type == 2){
              p = ggplot(data=data, aes(x=Year, y = M, colour = Age, group = Age)) + theme( panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + geom_line(size = 2) + scale_color_viridis_c("Age")
              print(p)
            }
            
            # Plot as persp
            if(type == 3){
              par( mar=c(1 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  1 ,  0) ,  oma=c(0 , 2 , 0 , 0))
              pmat = persp(y = Years, x = ages, z = m_subset, zlab = NA, zlim = zlim, xlab = "Age", ylab = "Year", theta = theta, ticktype = "detailed")
              mtext(ifelse(M2, "M2", "M"), side = 2, line = 0.5, at = 0)
              if(M2){
                text(-0.25,.15, labels = paste0("M1 = ",round((M1_array[j, sex, 1, 1]), 3)))
              }
              
              
              if(nsex[sp] == 1){
                mtext(paste0(title,": ",spnames[j]), side = 3, line = -2, at = 0)
              }
              if(nsex[sp] == 2){
                mtext(paste0(title,": ",spnames[j], " ",legend_sex2), side = 3, line = -2, at = 0)
              }
            }
            
            if (i == 2) {
              dev.off()
            }
          }
        }
      }
    }
  }

