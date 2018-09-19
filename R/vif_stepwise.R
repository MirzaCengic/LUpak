#' VIF stepwise calculation
#'
#' @param in_frame Input data
#' @param thresh VIF threshold
#' @param trace Verbose
#' @param ... Passed to lm function.
#'
#' @return Obj
#' @export
#'
#' @examples None
#' @import fmsb
vif_stepwise <- function(in_frame, thresh = 10, trace = FALSE, ...){


  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)

  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)

  if(vif_max < thresh){
    if (trace){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat("lalalalla",'\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{

    in_dat<-in_frame

    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){

      vif_vals<-NULL
      var_names <- names(in_dat)

      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]

      vif_max<-as.numeric(vif_vals[max_row,2])

      if(vif_max<thresh) break

      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat("Blabla", '\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }

      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    # Return list of final VIF values under threshold, and vector of variable names
    vif_vals_out <- as.data.frame(vif_vals)
    names(vif_vals_out) <- c("Var", "Vals")

    out_data <- list()
    out_data[[1]] <- vif_vals_out
    out_data[[2]] <- names(in_dat)
    return(out_data)
  }
}


# Wrapper around the vif_stepwise function
#' Select variables based on VIF.
#'
#' @param input_rasters Raster for which to calculate VIF
#' @param points_number Number of sampled points
#' @param thresh VIF threshold.
#'
#' @return RasterStack.
#' @export
#'
#' @examples None.
#' @importFrom raster sampleRandom subset

vif_select_vars <- function(input_rasters, points_number = 10000, thresh)
{
  # Sample raster, default value of points_number is 10000
  raster_vals_noNA_sample <- raster::sampleRandom(raster_data, points_number, na.rm=TRUE)
  # Remove categorical variables from the VIF calculations
  categorical_rasters <- grepl("catg", names(input_rasters))
  my_VIF <- vif_stepwise(raster_vals_noNA_sample[, !categorical_rasters], thresh)
  # Put back categorical rasters
  raster_names <- c(colnames(raster_vals_noNA_sample[, categorical_rasters]), my_VIF)
  # Subset original rasterstack
  regions_rasters_VIF <- raster::subset(input_rasters, raster_names)
  return(regions_rasters_VIF)
}

