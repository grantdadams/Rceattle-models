# Diet composition fits

plot_diet <- function(Rceattle,
                      file = NULL,
                      model_names = NULL,
                      line_col = NULL,
                      species = NULL,
                      predator_species = 2,  # ATF
                      predator_sex = 1,      # 1 = female, 2 = male, 0 = combined/hake
                      prey_species = 1,      # Hake (NULL for all prey)
                      right_adj = 0,
                      top_adj = 0.05,
                      single.plots = FALSE,
                      width = NULL,
                      height = NULL) {
  
  # Convert single model into a list
  if(class(Rceattle) == "Rceattle"){
    Rceattle <- list(Rceattle)
  }
  
  # Species names
  if(is.null(species)){
    species <- Rceattle[[1]]$data_list$spnames
  }
  
  # Extract diet data objects
  Diet_obs_list <- list()
  Diet_hat_list <- list()
  
  for(i in 1:length(Rceattle)){
    # Get observed diet data
    Diet_obs_list[[i]] <- Rceattle[[i]]$data_list$diet_data
    Diet_obs_list[[i]]$Observation <- Diet_obs_list[[i]]$Stomach_proportion_by_weight
    
    # Get predicted diet data  
    Diet_hat_list[[i]] <- Rceattle[[i]]$data_list$diet_data
    Diet_hat_list[[i]]$Observation <- Rceattle[[i]]$quantities$diet_hat[,2] # Column 2 has the predictions
    
    # Filter for the predator species of interest
    Diet_obs_list[[i]] <- Diet_obs_list[[i]] %>%
      dplyr::filter(Pred == predator_species)
    
    Diet_hat_list[[i]] <- Diet_hat_list[[i]] %>%
      dplyr::filter(Pred == predator_species)
    
    # Filter for predator sex if specified (important for ATF)
    if(predator_sex > 0){
      Diet_obs_list[[i]] <- Diet_obs_list[[i]] %>%
        dplyr::filter(Pred_sex == predator_sex)
      
      Diet_hat_list[[i]] <- Diet_hat_list[[i]] %>%
        dplyr::filter(Pred_sex == predator_sex)
    }
    
    # Filter for specific prey species if specified (prey is always hake = 1)
    if(!is.null(prey_species)){
      Diet_obs_list[[i]] <- Diet_obs_list[[i]] %>%
        dplyr::filter(Prey == prey_species)
      
      Diet_hat_list[[i]] <- Diet_hat_list[[i]] %>%
        dplyr::filter(Prey == prey_species)
    }
  }
  
  # Get unique prey species for plotting
  unique_prey <- sort(unique(Diet_obs_list[[1]]$Prey))
  nprey <- length(unique_prey)
  
  # Get age range for predator
  min_pred_age <- min(Diet_obs_list[[1]]$Pred_age)
  max_pred_age <- max(Diet_obs_list[[1]]$Pred_age)
  
  # Calculate plot limits
  ymax <- c()
  ymin <- c()
  
  for(prey in 1:nprey){
    for(i in 1:length(Rceattle)){
      prey_ind <- which(Diet_obs_list[[i]]$Prey == unique_prey[prey])
      ymax[prey] <- max(c(Diet_obs_list[[i]]$Observation[prey_ind], 
                          Diet_hat_list[[i]]$Observation[prey_ind], 
                          ymax[prey]), na.rm = TRUE)
      ymin[prey] <- min(c(Diet_obs_list[[i]]$Observation[prey_ind], 
                          Diet_hat_list[[i]]$Observation[prey_ind], 
                          ymin[prey]), na.rm = TRUE)
    }
  }
  ymax <- ymax + top_adj * (ymax - ymin)
  ymin <- pmax(ymin - top_adj * (ymax - ymin), 0)  # Don't go below 0 for proportions
  
  # Assume colors if not provided
  if (is.null(line_col)) {
    line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
  }
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (j in 1:loops) {
    
    # Plot/save each prey individually
    if(single.plots == TRUE){
      if(is.null(width)) width <- 5
      if(is.null(height)) height <- 3.5
      
      for(prey in 1:nprey){
        Par <- list(mfrow = c(1,1), 
                    mar = c(3.5, 3.5, 0.5, 0.1), 
                    mgp = c(2., 0.5, 0), 
                    tck = -0.02, 
                    cex = 0.8)
        
        # Save
        if(j == 2){
          sex_label <- ifelse(predator_sex > 0, paste0("_sex", predator_sex), "")
          filename <- paste0(file, "_predator", predator_species, sex_label,
                             "_prey", unique_prey[prey], "_diet_fit.png")
          png(file = filename, width = width, height = height, res = 200, units = "in")
        }
        
        par(Par)
        plot(NA, NA, 
             ylab = "Diet Proportion", 
             xlab = "Predator Age", 
             ylim = c(ymin[prey], ymax[prey]), 
             xlim = c(min_pred_age, max_pred_age + (max_pred_age - min_pred_age) * right_adj), 
             type = 'n', 
             xaxt = "n", 
             yaxt = "n")
        axis(1, labels = TRUE, cex = 0.8)
        axis(2, labels = TRUE, cex = 0.8)
        
        # Loop through models
        for (k in 1:length(Rceattle)) {
          # Subset data by prey and model
          diet_obs_tmp <- Diet_obs_list[[k]] %>%
            dplyr::filter(Prey == unique_prey[prey])
          
          diet_hat_tmp <- Diet_hat_list[[k]] %>%
            dplyr::filter(Prey == unique_prey[prey])
          
          # Plot predicted diet proportions
          lines(diet_hat_tmp$Pred_age, diet_hat_tmp$Observation, 
                lwd = 2, col = line_col[k])
          
          # Plot observed diet proportions
          points(diet_obs_tmp$Pred_age, diet_obs_tmp$Stomach_proportion_by_weight, 
                 pch = 21, bg = "white", col = 1, cex = 1.2)
        }
        
        # Create title with sex information
        prey_name <- ifelse(unique_prey[prey] <= length(species), 
                            species[unique_prey[prey]], 
                            paste("Species", unique_prey[prey]))
        
        if(predator_sex == 1){
          title_text <- paste("Predator:", species[predator_species], "Female - Prey:", prey_name)
        } else if(predator_sex == 2){
          title_text <- paste("Predator:", species[predator_species], "Male - Prey:", prey_name)
        } else {
          title_text <- paste("Predator:", species[predator_species], "- Prey:", prey_name)
        }
        
        legend('topleft', title_text, bty = "n", y.intersp = -0.2, cex = 0.8)
        
        # Model names
        if(!is.null(model_names)){
          legend("topright",
                 legend = model_names,
                 pch = rep(16, length(line_col)), 
                 cex = 0.8,
                 col = line_col,
                 bty = "n")
        }
        
        # Save plot
        if(j == 2){dev.off()}
      }
    }
    
    # Plot all prey together
    if(single.plots == FALSE){
      # Set dimensions
      if(is.null(width)) width <- 7
      if(is.null(height)) {
        height <- ifelse(nprey == 1, 5, 
                         ifelse(nprey == 2, 3., 2.5)) * round(nprey/2 + 0.01, 0)
      }
      
      Par <- list(mfrow = c(round(nprey/2 + 0.01, 0), ifelse(nprey == 1, 1, 2)),
                  mai = c(0.35, 0.15, 0, 0.15),
                  omi = c(0.2, 0.25, 0.2, 0) + 0.1,
                  mgp = c(2, 0.5, 0), 
                  tck = -0.02, 
                  cex = 0.8)
      
      # Save
      if(j == 2){
        sex_label <- ifelse(predator_sex > 0, paste0("_sex", predator_sex), "")
        filename <- paste0(file, "_predator", predator_species, sex_label, "_diet_fits.png")
        png(file = filename, width = width, height = height, res = 200, units = "in")
      }
      par(Par)
      
      for(prey in 1:nprey){
        xlim <- c(min_pred_age, max_pred_age)
        if(prey == 1){
          xlim <- c(min_pred_age, max_pred_age + (max_pred_age - min_pred_age) * right_adj)
        }
        
        plot(NA, NA, 
             ylab = "", 
             xlab = "", 
             ylim = c(ymin[prey], ymax[prey]), 
             xlim = xlim, 
             type = 'n', 
             xaxt = "n", 
             yaxt = "n")
        axis(1, labels = TRUE, cex = 0.8)
        axis(2, labels = TRUE, cex = 0.8)
        
        # Prey name
        prey_name <- ifelse(unique_prey[prey] <= length(species), 
                            species[unique_prey[prey]], 
                            paste("Species", unique_prey[prey]))
        legend('topleft', prey_name, bty = "n", y.intersp = -0.2, cex = 0.8)
        
        # Model names (only on first plot)
        if(prey == 1){
          if(!is.null(model_names)){
            legend("topright",
                   legend = model_names,
                   pch = rep(16, length(line_col)), 
                   cex = 0.8,
                   col = line_col,
                   bty = "n")
          }
        }
        
        # Loop through models
        for (k in 1:length(Rceattle)) {
          # Subset data by prey and model
          diet_obs_tmp <- Diet_obs_list[[k]] %>%
            dplyr::filter(Prey == unique_prey[prey])
          
          diet_hat_tmp <- Diet_hat_list[[k]] %>%
            dplyr::filter(Prey == unique_prey[prey])
          
          # Plot predicted diet proportions
          lines(diet_hat_tmp$Pred_age, diet_hat_tmp$Observation, 
                lwd = 2, col = line_col[k])
          
          # Plot observed diet proportions
          points(diet_obs_tmp$Pred_age, diet_obs_tmp$Stomach_proportion_by_weight, 
                 pch = 21, bg = "white", col = 1, cex = 1.2)
        }
      }
      
      mtext(paste("Predator Age"), side = 1, outer = TRUE, at = 0.5, line = 1, cex = 1)
      mtext(paste("Diet Proportion"), side = 2, outer = TRUE, at = 0.5, line = 1, cex = 1)
      if(j == 2){dev.off()}
    }
  }
} # End of function


#Diet residuals
plot_diet_residuals <- function(Rceattle,
                                file = NULL,
                                model_names = NULL,
                                line_col = NULL,
                                species = NULL,
                                predator_species = 2,  # ATF
                                predator_sex = 1,      # 1 = female, 2 = male, 0 = combined/hake
                                prey_species = 1,      # Hake (NULL for all prey)
                                width = NULL,
                                height = NULL) {
  
  # Convert single model into a list
  if(class(Rceattle) == "Rceattle"){
    Rceattle <- list(Rceattle)
  }
  
  # Species names
  if(is.null(species)){
    species <- Rceattle[[1]]$data_list$spnames
  }
  
  # Extract and calculate residuals
  Diet_residual_list <- list()
  
  for(i in 1:length(Rceattle)){
    # Get diet data
    diet_data <- Rceattle[[i]]$data_list$diet_data
    diet_hat <- Rceattle[[i]]$quantities$diet_hat[,2]
    
    # Calculate residuals
    Diet_residual_list[[i]] <- diet_data
    Diet_residual_list[[i]]$Residual <- diet_data$Stomach_proportion_by_weight - diet_hat
    
    # Filter for predator species
    Diet_residual_list[[i]] <- Diet_residual_list[[i]] %>%
      dplyr::filter(Pred == predator_species)
    
    # Filter for predator sex if specified (important for ATF)
    if(predator_sex > 0){
      Diet_residual_list[[i]] <- Diet_residual_list[[i]] %>%
        dplyr::filter(Pred_sex == predator_sex)
    }
    
    # Filter for prey species if specified
    if(!is.null(prey_species)){
      Diet_residual_list[[i]] <- Diet_residual_list[[i]] %>%
        dplyr::filter(Prey == prey_species)
    }
  }
  
  # Get unique prey species and age range
  unique_prey <- sort(unique(Diet_residual_list[[1]]$Prey))
  nprey <- length(unique_prey)
  min_pred_age <- min(Diet_residual_list[[1]]$Pred_age)
  max_pred_age <- max(Diet_residual_list[[1]]$Pred_age)
  
  # Calculate plot limits
  ymax <- ymin <- c()
  for(prey in 1:nprey){
    for(i in 1:length(Rceattle)){
      prey_ind <- which(Diet_residual_list[[i]]$Prey == unique_prey[prey])
      ymax[prey] <- max(c(Diet_residual_list[[i]]$Residual[prey_ind], ymax[prey]), na.rm = TRUE)
      ymin[prey] <- min(c(Diet_residual_list[[i]]$Residual[prey_ind], ymin[prey]), na.rm = TRUE)
    }
  }
  
  # Assume colors if not provided
  if (is.null(line_col)) {
    line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
  }
  
  # Offset positions for multiple models
  positions <- seq(-0.1, 0.1, length.out = length(Rceattle))
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (j in 1:loops) {
    
    # Set dimensions
    if(is.null(width)) width <- 7
    if(is.null(height)) {
      height <- ifelse(nprey == 1, 5, 
                       ifelse(nprey == 2, 3., 2.5)) * round(nprey/2 + 0.01, 0)
    }
    
    Par <- list(mfrow = c(round(nprey/2 + 0.01, 0), ifelse(nprey == 1, 1, 2)),
                mai = c(0.35, 0.15, 0, 0.15),
                omi = c(0.2, 0.25, 0.2, 0) + 0.1,
                mgp = c(2, 0.5, 0), 
                tck = -0.02, 
                cex = 0.8)
    
    # Save
    if(j == 2){
      sex_label <- ifelse(predator_sex > 0, paste0("_sex", predator_sex), "")
      filename <- paste0(file, "_predator", predator_species, sex_label, "_diet_residuals.png")
      png(file = filename, width = width, height = height, res = 200, units = "in")
    }
    par(Par)
    
    for(prey in 1:nprey){
      plot(NA, NA, 
           ylab = "", 
           xlab = "", 
           ylim = c(ymin[prey], ymax[prey]), 
           xlim = c(min_pred_age, max_pred_age), 
           type = 'n', 
           xaxt = "n", 
           yaxt = "n")
      abline(h = 0, lty = 2, col = "grey")
      axis(1, labels = TRUE, cex = 0.8)
      axis(2, labels = TRUE, cex = 0.8)
      
      # Prey name
      prey_name <- ifelse(unique_prey[prey] <= length(species), 
                          species[unique_prey[prey]], 
                          paste("Species", unique_prey[prey]))
      legend('topleft', prey_name, bty = "n", y.intersp = -0.2, cex = 0.8)
      
      # Model names (only on first plot)
      if(prey == 1){
        if(!is.null(model_names)){
          legend("topright",
                 legend = model_names,
                 pch = rep(21, length(line_col)), 
                 cex = 0.8,
                 pt.bg = line_col,
                 bty = "n")
        }
      }
      
      # Loop through models
      for (k in 1:length(Rceattle)) {
        # Subset data by prey and model
        diet_res_tmp <- Diet_residual_list[[k]] %>%
          dplyr::filter(Prey == unique_prey[prey])
        
        # Plot residuals with slight offset for multiple models
        for(age_idx in 1:nrow(diet_res_tmp)){
          lines(rep(diet_res_tmp$Pred_age[age_idx] + positions[k], 2),
                c(0, diet_res_tmp$Residual[age_idx]), 
                col = line_col[k])
        }
        points(diet_res_tmp$Pred_age + positions[k], 
               diet_res_tmp$Residual, 
               col = 1, pch = 21, bg = line_col[k])
      }
    }
    
    mtext(paste("Predator Age"), side = 1, outer = TRUE, at = 0.5, line = 1, cex = 1)
    mtext(paste("Diet Residual (Obs - Pred)"), side = 2, outer = TRUE, at = 0.5, line = 1, cex = 1)
    if(j == 2){dev.off()}
  }
} # End of function


## USAGE EXAMPLE -------------------------

#load("results/ATF_model/ATF_run_ms_LN_M.Rdata")
my_model<- run_ms_LN_M
#my_model<- rw_model

head(my_model$data_list$diet_data)
my_model$quantities$diet_hat[,2]

# Plot female ATF eating hake
plot_diet(my_model, predator_species = 2, predator_sex = 1, prey_species = 1)

# Plot male ATF eating hake  
plot_diet(my_model, predator_species = 2, predator_sex = 2, prey_species = 1)

######## THIS IS HARD TO SEE, SO USE THE FOLLOWING TO SPLIT THE PLOTS:
##=============== plot diet hat only
plot_diet_hat_values <- function(Rceattle,
                                 predator_species = 2,  # ATF
                                 predator_sex = 2,      # Male
                                 prey_species = 1,      # Hake
                                 title = NULL) {
  
  # Get diet data
  diet_data <- Rceattle$data_list$diet_data
  diet_hat <- Rceattle$quantities$diet_hat[,2]  # Column 2 has predictions
  
  # Filter for predator species
  pred_filter <- diet_data$Pred == predator_species
  
  # Filter for predator sex if specified
  if(predator_sex > 0){
    pred_filter <- pred_filter & (diet_data$Pred_sex == predator_sex)
  }
  
  # Filter for prey species if specified
  if(!is.null(prey_species)){
    pred_filter <- pred_filter & (diet_data$Prey == prey_species)
  }
  
  # Get filtered data
  filtered_data <- diet_data[pred_filter, ]
  filtered_predictions <- diet_hat[pred_filter]
  
  # Create plot title if not provided
  if(is.null(title)){
    species_names <- Rceattle$data_list$spnames
    pred_name <- ifelse(predator_species <= length(species_names), 
                        species_names[predator_species], 
                        paste("Species", predator_species))
    prey_name <- ifelse(prey_species <= length(species_names), 
                        species_names[prey_species], 
                        paste("Species", prey_species))
    
    sex_label <- ""
    if(predator_sex == 1) sex_label <- " Female"
    if(predator_sex == 2) sex_label <- " Male"
    
    title <- paste("Diet_hat Values:", pred_name, sex_label, "eating", prey_name)
  }
  
  # Create the plot
  plot(filtered_data$Pred_age, filtered_predictions,
       type = "b",  # Both points and lines
       pch = 16,    # Solid circles
       col = "blue",
       lwd = 2,
       xlab = "Predator Age",
       ylab = "Diet_hat Value",
       main = title,
       cex.main = 0.9)
  
  # Add grid for easier reading
  grid(col = "lightgray", lty = "dotted")
  
  # Add some diagnostic text
  text(x = max(filtered_data$Pred_age) * 0.7, 
       y = max(filtered_predictions) * 0.9,
       labels = paste("Range:", round(min(filtered_predictions), 6), "to", round(max(filtered_predictions), 6)),
       cex = 0.8, col = "red")
  
  # Print summary statistics
  cat("\n=== Diet_hat Summary ===\n")
  cat("Predator:", pred_name, sex_label, "\n")
  cat("Prey:", prey_name, "\n")
  cat("Number of observations:", length(filtered_predictions), "\n")
  cat("Range:", round(min(filtered_predictions), 6), "to", round(max(filtered_predictions), 6), "\n")
  cat("Mean:", round(mean(filtered_predictions), 6), "\n")
  cat("Standard deviation:", round(sd(filtered_predictions), 6), "\n")
  cat("Are all values identical?", length(unique(round(filtered_predictions, 10))) == 1, "\n")
  cat("======================\n\n")
  
  # Return the data invisibly for further analysis
  invisible(data.frame(
    Pred_age = filtered_data$Pred_age,
    Diet_hat = filtered_predictions,
    Observed = filtered_data$Stomach_proportion_by_weight
  ))
}


# Compare diet_hat values across multiple models
compare_diet_hat_values <- function(model_list,
                                    model_names = NULL,
                                    predator_species = 2,  # ATF
                                    predator_sex = 2,      # Male
                                    prey_species = 1) {    # Hake
  
  if(is.null(model_names)){
    model_names <- paste("Model", 1:length(model_list))
  }
  
  # Colors for different models
  colors <- rainbow(length(model_list))
  
  # Get data from first model to set up plot
  first_data <- plot_diet_hat_values(model_list[[1]], predator_species, predator_sex, prey_species)
  
  # Set up the plot
  plot(first_data$Pred_age, first_data$Diet_hat,
       type = "n",  # No plotting yet
       xlab = "Predator Age",
       ylab = "Diet_hat Value",
       main = paste("Diet_hat Comparison:", model_names[1], "vs Others"),
       ylim = range(sapply(model_list, function(m) {
         d <- m$data_list$diet_data
         pred_filter <- d$Pred == predator_species & d$Prey == prey_species
         if(predator_sex > 0) pred_filter <- pred_filter & (d$Pred_sex == predator_sex)
         range(m$quantities$diet_hat[pred_filter, 2])
       })))
  
  # Plot each model
  for(i in 1:length(model_list)){
    data_i <- plot_diet_hat_values(model_list[[i]], predator_species, predator_sex, prey_species)
    
    lines(data_i$Pred_age, data_i$Diet_hat,
          col = colors[i], lwd = 2, type = "b", pch = 16)
  }
  
  # Add legend
  legend("topright", 
         legend = model_names,
         col = colors,
         lwd = 2,
         pch = 16,
         bty = "n")
  
  # Add grid
  grid(col = "lightgray", lty = "dotted")
}


plot_diet_hat_vs_observed <- function(Rceattle,
                                      predator_species = 2,  # ATF
                                      predator_sex = 2,      # Male  
                                      prey_species = 1) {    # Hake
  
  # Get the data
  data_df <- plot_diet_hat_values(Rceattle, predator_species, predator_sex, prey_species)
  
  # Create side-by-side plot
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  # Plot 1: Diet_hat predictions
  plot(data_df$Pred_age, data_df$Diet_hat,
       type = "b", pch = 16, col = "blue", lwd = 2,
       xlab = "Predator Age", ylab = "Diet Proportion",
       main = "Predicted (diet_hat)")
  grid(col = "lightgray", lty = "dotted")
  
  # Plot 2: Observed values  
  plot(data_df$Pred_age, data_df$Observed,
       type = "b", pch = 16, col = "red", lwd = 2,
       xlab = "Predator Age", ylab = "Diet Proportion", 
       main = "Observed")
  grid(col = "lightgray", lty = "dotted")
  
  # Reset plotting parameters
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  
  # Print comparison stats
  cat("\n=== Prediction vs Observation Comparison ===\n")
  cat("Predicted range:", round(min(data_df$Diet_hat), 6), "to", round(max(data_df$Diet_hat), 6), "\n")
  cat("Observed range:", round(min(data_df$Observed), 6), "to", round(max(data_df$Observed), 6), "\n")
  cat("Ratio (Obs/Pred):", round(mean(data_df$Observed) / mean(data_df$Diet_hat), 2), "times\n")
  cat("==========================================\n\n")
}

# 1. Simple plot of diet_hat values by age
plot_diet_hat_values(run_ms_LN_M, predator_species = 2, predator_sex = 2, prey_species = 1)
plot_diet_hat_values(run_ms_LN_M_wg, predator_species = 2, predator_sex = 2, prey_species = 1)
plot_diet_hat_values(rw_model, predator_species = 2, predator_sex = 2, prey_species = 1)

# 2. Compare diet_hat vs observed side by side
plot_diet_hat_vs_observed(run_ms_LN_M, predator_species = 2, predator_sex = 2, prey_species = 1)

# 3. Compare across different models (if you have multiple)
compare_diet_hat_values(list(run_ms_LN_M, rw_model), c("LN_model", "rw_LN_model"), 2, 2, 1)

plot_ration(run_ms_LN_M)
plot_ration(rw_model)





