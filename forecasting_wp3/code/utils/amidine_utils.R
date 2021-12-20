## amedine helper functions


packages <- c("data.table","ProbCast","gamlss.tr","crch","lubridate","mgcv")
install.packages(setdiff(packages, rownames(installed.packages())))

library(data.table)
library(ProbCast)


### memory save row deletion function for data.tables
## DT is the data.table
## del.idxs is the indexes to delete
delete_memsafe <- function(DT, del.idxs) { ## del.idxs here is the rows to remove...
  keep.idxs <- setdiff(DT[, .I], del.idxs);
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs]);
  setnames(DT.subset, cols[1]);
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]];
    DT[, (col) := NULL];  # delete
  }
  return(DT.subset);
}


#' Generate LV/MV Hypothetical Network Hierarchies
#'
#' This function generates a hypothetical hierarchy from smart meter data
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{id,date_time,demand}, where each combination of the three is a unique row. So keep the date_time column in UTC.
#' @param fdrs_per_ss Vector of integers which are sampled to give a pseudo-random number of feeders per secondary substation
#' @param sms_per_fdr Vector of integers which are sampled at each feeder to give a pseudo-random number of smart meters per feeder.
#' @param na.rm \code{boolean}. remove \code{NA} values?
#' @details In this function we only generate 1 primary substation hierarchy.
#' @note Check the number of feeder sand SMs per feeder for the last secondary substation, which can be small due to random sampling and the fact that every ID is included in the hierarchy, and change seed if necessary
#' @return a list of two \code{data.tables} <<dt>>$hier_ref contains the SM, feeder, and secondary substation assignments, <<dt>>$hier_dt contains the aggregated time-series 
#' @export
gen_hierarchy <- function(melted_dt,
                          fdrs_per_ss = 4:7,
                          sms_per_fdr = 16:45,
                          na.rm = TRUE){
  
  # melted_dt <- copy(lcl_data)
  # fdrs_per_ss = 4:7
  # sms_per_fdr = 16:45
  # na.rm = TRUE

  # get SM ids from melted dt
  ids <- as.character(unique(melted_dt[,id]))
  # set up reference dt for describing hierarchy
  ref_dt <- data.table(id=ids)
  
  ## second_sub count
  s <- 1
  
  # list for feeder sums
  fdr <- list()
  
  # while there's still some ids to aggregate
  while(length(ids)!=0){
    
    
    # sample number of feeders per ss
    feeder_no <- sample(fdrs_per_ss,1)
    # sample number of SMs per feeder
    smartm_no <- sample(sms_per_fdr,feeder_no)
  
      
      for(i in 1:feeder_no){
        
          ## if the current sampled smart meter number is greater than the amount of SM IDs left to sample from, use remianing IDs
          if(length(ids)<smartm_no[i]){
            smartm_no[i] <- length(ids)
          }
        
          # randomly sample smartm_no[i] SM ids for the ith feeder
          sm_ids <- sample(ids,smartm_no[i])
          # remove these sampled ids from the id vector
          ids <- ids[-c(which(ids%in%sm_ids))]
          
          # calculate consumption for ss_s and fdr_i
          fdr[[paste0("ss",s,"_fdr",i)]] <- melted_dt[id%in%sm_ids,.(demand=sum(demand,na.rm = na.rm)),by=.(date_time)]
          
          ## need to have columns in the final dt.
          #--> aggregation (SM,FDR,SS,PS)
          #---> substations (1,2,3,4,5,all) ---> ss_id
          #----> feeder (1,2,3,4,5,all) ---> fdr_id
          #-----> smart meter (1,2,3,4,5,...,N,all) ---> sm_id
          fdr[[paste0("ss",s,"_fdr",i)]][,ss_id := paste0("ss",s)]
          fdr[[paste0("ss",s,"_fdr",i)]][,fdr_id := paste0("fdr",i)]
          
          # add ids to reference table
          ref_dt[id%in%sm_ids,fdr_id := paste0("fdr",i)]
          ref_dt[id%in%sm_ids,ss_id := paste0("ss",s)]
          
          # break iff we have no IDs left
          if(length(ids)==0){
            break
          }
        
      
      }
      
    ## go to next secondary substation
    s <- s+1
      
      
  }
    
    
  fdr <- rbindlist(fdr,idcol = "id")
  fdr[,aggregation:= "fdr"]
  fdr[,sm_id:= "all"]
  
  # sum to the secondary substation level
  ss <- fdr[,.(demand=sum(demand,na.rm = na.rm)),by=.(date_time,ss_id,sm_id)]
  ## add agg. ids
  ss[,aggregation:= "ss"]
  ss[,id := ss_id]
  ss[,fdr_id := "all"]
  
  # sum to the primary substation level (top)
  ps <- ss[,.(demand=sum(demand,na.rm = na.rm)),by=.(date_time,fdr_id,sm_id)]
  ps[,id:="ps1"]
  ps[,ss_id := "all"]
  ps[,aggregation:= "ps"]
  
  ref_dt[,ps_id := "ps1"]
  # add sm_id level
  ref_dt[,sm_id := paste0("sm",sprintf("%02d",1:.N)),by=.(ps_id,ss_id,fdr_id)]
  
  # return required dts...
  return(list(hier_dt=rbindlist(list(fdr,ss,ps),use.names=TRUE),hier_ref=ref_dt))
  
  
  
}


# set.seed(6)
# tmp <- gen_hierarchy(melted_dt = lcl_data)
# tmp$hier_ref[,.N,keyby=.(ps_id,ss_id,fdr_id)]
# tmp$hier_dt
# tmp$hier_ref






#' Add calendar variable columns to gen_hierarchy aggregated time-series data.table
#'
#' This function adds some calendar variables to the hierarchy data.table
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{id,date_time}, date_time column in UTC.
#' @details deetz
#' @return Function modifies \code{melted_dt} in place, so doesn't need to return an object
#' @export
add_calendar <- function(melted_dt){
  
  melted_dt[,date := lubridate::floor_date(date_time,unit="1 day"),by=.(date_time)]
  
  gb_time <- melted_dt$date_time
  attr(gb_time, "tzone") <- "GB"
  melted_dt[,date_time_uk := gb_time]
  melted_dt[,date_uk := lubridate::floor_date(date_time_uk,unit="1 day"),by=.(date_time_uk)]
  rm(gb_time)
  invisible(gc())
  
  setkey(melted_dt,date_time_uk,id)
  
  
  melted_dt[,tod_uk := as.numeric(hour(date_time_uk) + minute(date_time_uk)/60),by=.(date_time_uk)]
  melted_dt[,doy_uk := yday(date_uk),by=.(date_uk)]
  melted_dt[,dow_uk := wday(date_uk),by=.(date_uk)]
  melted_dt[,woy_uk := isoweek(date_uk),by=.(date_uk)]
  melted_dt[,moy_uk := month(date_uk),by=.(date_uk)]
  
  
  # melted_dt[,tod := as.numeric(hour(date_time_uk) + minute(date_time_uk)/60),by=.(date_time_uk)]
  # melted_dt[,doy := yday(date_uk),by=.(date_uk)]
  # melted_dt[,dow := wday(date_uk),by=.(date_uk)]
  # melted_dt[,woy := isoweek(date_uk),by=.(date_uk)]
  # melted_dt[,moy := month(date_uk),by=.(date_uk)]
  
  
  
}

# add_calendar(melted_dt = tmp$hier_dt)
# tmp$hier_dt



#### wrapper probcast for multiple locations - para_gamlss
#' wrapper for probcast for multiple locations - para_gamlss
#'
#' This function generates a list of \code{PPD} objects for multiple locations
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{vars,split_col,sort_cols}.
#' @param vars character vector of columns in \code{melted_dt} which will be used in the fitting. Remember to include the response
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}. Sometimes \code{split.data.table} can re-arrange the order of the sub-lists? should probs double check now lol.
#' @param ncores number of cores for the outer loop, i.e. locations. Defaults to number of locations.
#' @param ncores_inner number of cores for the inner loop, i.e. folds. Defaults to number of folds.
#' @param dt_tempdir is a character string containing a file path to save modeling tables temporarily, for large tables this can save a lot of memory
#' @param parallel_inner perform inner loop in parallel? useful on aws
#' @param lite return only the predictive parameter estimates and family? useful for reducing memory footprint
#' @details fill in...
#' @return a list of ordered \code{ppd} objects  for each location
#' @export
ml_pc_gamlss <- function(melted_dt,
                         vars,
                         split_col="id",
                         sort_cols = "date_uk",
                         ncores = NULL,
                         ncores_inner = NULL,
                         dt_tempdir="../../saved_data/",
                         parallel_inner = TRUE,
                         lite = FALSE,
                         ...){

  # input_data = pklcl_data[aggregation=="ss"]
  # vars = c("demand","demandpk_l1")
  # split_col = "id"
  # sort_cols = "date_time"
  # ncores = 3
  # ncores_inner = 3
  # dt_tempdir="../saved_data/"
  
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  # too memory intensive to send input data to each worker, let's save and load
  lapply(input_data,function(input_dt){
    input_dt <- input_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",vars)]
    save(input_dt,file = paste0(dt_tempdir,unique(input_dt[,get(split_col)]),".rda"))
  })
  

  # Calculate the number of cores
  if(is.null(ncores)){
    no_cores <- length(input_data)} else{
      no_cores <- ncores
  }
  
  if(is.null(ncores_inner)){ncores_inner <- length(input_data[,unique(kfold)])}
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  iterations <- length(input_data)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ids <- names(input_data)
  rm(input_data)
  # this next line is really really really important for the memory
  gc()
  
  
  ppd_out <- foreach(i=ids,.packages = c('data.table',"ProbCast","gamlss.tr")
                     ,.options.snow = opts,.export="...") %dopar% {
    
    # # if(!is.null(trunc_dist)){
    # # 
    # #   eval(parse(text=trunc_dist))
    # # 
    # # }
    # 
    # gamlss.tr::gen.trun(par=c(0),family="NO", name="tr", type="left")
    
    load(paste0(dt_tempdir,i,".rda"))
    
    x <- try(Para_gamlss(data = input_dt,
                        parallel = parallel_inner,
                        cores = ncores_inner,
                        pckgs = "data.table",
                        ... = NULL,
                        ...),silent = TRUE)
    
    if('try-error' %in% class(x)){
      return(paste0("problem fitting model to node ",i))

    } else{
      
      
      if(lite){
        
        out <- list()
        out$params <- PPD_2_MultiQR(models = x, 
                                    data = input_dt,
                                    params=TRUE)
        out$family <- x[[1]]$family[1]
        
        out$pdf_fun <- eval(parse(text=paste0("gamlss.dist::d",out$family)))
        out$cdf_fun <- eval(parse(text=paste0("gamlss.dist::p",out$family)))
        out$q_fun <- eval(parse(text=paste0("gamlss.dist::q",out$family)))
        
        class(out) <- "ppd_lite"
        
        x <- out
        
      } else{
       
        return(x) 
        
      }

    }
    
  }
  
  close(pb)
  stopCluster(cl)
  
  names(ppd_out) <- ids
  
  # delete temporary tables
  lapply(ids,function(input_dt){
    file.remove(paste0(dt_tempdir,input_dt,".rda"))
  })
  
  return(ppd_out)
  
  
}


# tmp <- pc_ml_gamlss(input_data = pklcl_data[aggregation=="ss"],
#                     vars = c("demand","demandpk_l1"),
#                     split_col = "id",
#                     sort_cols = "date_time",
#                     ncores = 3,ncores_inner = 3,
#                     formula = demand~demandpk_l1,
#                     family = NO,
#                     method=mixed(20,10))



#### wrapper probcast for multiple locations - para_gamboostLSS
#' wrapper for probcast for multiple locations - para_gamboostLSS
#'
#' This function generates a list of \code{PPD} objects for multiple locations
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{vars,split_col,sort_cols}.
#' @param vars character vector of columns in \code{melted_dt} which will be used in the fitting. Remember to include the response
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}. Sometimes \code{split.data.table} can re-arrange the order of the sub-lists? should probs double check now lol.
#' @param ncores number of cores for the outer loop, i.e. locations. Defaults to number of locations.
#' @param ncores_inner number of cores for the inner loop, i.e. folds. Defaults to number of folds.
#' @param dt_tempdir is a character string containing a file path to save modeling tables temporarily, for large tables this can save a lot of memory
#' @param parallel_inner perform inner loop in parallel? useful on aws
#' @param trunc_dist if a truncated response is requires this should be a character string with the function call from \code{gamlss.tr::gen.trunc}. Note that for the function to work this family should be specified outside this function, as typically done for a truncated gamlss model. Also don't run inner_parallel loop here.
#' @details fill in...
#' @return a list of ordered \code{ppd} objects  for each location
#' @export
ml_pc_gamb <- function(melted_dt,
                       vars,
                       split_col="id",
                       sort_cols = "date_uk",
                       ncores = NULL,
                       ncores_inner = NULL,
                       dt_tempdir="../../saved_data/",
                       parallel_inner = TRUE,
                       trunc_dist = NULL,...){
  
  # input_data = pklcl_data[aggregation=="ss"]
  # vars = c("demand","demandpk_l1")
  # split_col = "id"
  # sort_cols = "date_time"
  # ncores = 3
  # ncores_inner = 3
  # dt_tempdir="../saved_data/"
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  # too memory intensive to send input data to each worker, let's save and load
  lapply(input_data,function(input_dt){
    input_dt <- input_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",vars)]
    save(input_dt,file = paste0(dt_tempdir,unique(input_dt[,get(split_col)]),".rda"))
  })
  
  
  # Calculate the number of cores
  if(is.null(ncores)){
    no_cores <- length(input_data)} else{
      no_cores <- ncores
    }
  
  if(is.null(ncores_inner)){ncores_inner <- length(input_data[,unique(kfold)])}
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  iterations <- length(input_data)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ids <- names(input_data)
  rm(input_data)
  # this next line is really really really important for the memory
  gc()
  
  
  ppd_out <- foreach(i=ids,.packages = c('data.table',"ProbCast"),.options.snow = opts,.export="...") %dopar% {
    
    
    
    load(paste0(dt_tempdir,i,".rda"))
    
    
    x <- try(Para_gamboostLSS(data = input_dt,
                              parallel = parallel_inner,
                              cores = ncores_inner,
                              pckgs = c("data.table"),
                              ...),silent = TRUE)
    
    if('try-error' %in% class(x)){
      return(paste0("problem fitting model to node ",i))
      
    } else{
      
      return(x)
      
    }
    
  }
  
  close(pb)
  stopCluster(cl)
  
  names(ppd_out) <- ids
  
  # delete temporary tables
  lapply(ids,function(input_dt){
    file.remove(paste0(dt_tempdir,input_dt,".rda"))
  })
  
  return(ppd_out)
  
  
}




#### wrapper probcast for multiple locations - gbm_MQR 
#' wrapper for probcast for multiple locations - gbm_MQR
#'
#' This function generates a list of \code{MultiQR} objects for multiple locations
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{vars,split_col,sort_cols}.
#' @param vars character vector of columns in \code{melted_dt} which will be used in the fitting. Remember to include the response
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}. Sometimes \code{split.data.table} can re-arrange the order of the sub-lists? should probs double check now lol.
#' @param ncores number of cores for the outer loop, i.e. locations. Defaults to number of locations.
#' @param ncores_inner number of cores for the inner loop, i.e. folds. Defaults to number of folds.
#' @param dt_tempdir is a character string containing a file path to save modeling tables temporarily, for large tables this can save a lot of memory
#' @param parallel_inner perform inner loop in parallel? useful on aws
#' @param ... further arguments to \code{ProbCast::MQR_gbm} 
#' @details fill in...
#' @return a list of ordered \code{ppd} objects  for each location
#' @export
ml_pc_gbm <- function(melted_dt,
                      vars,
                      split_col="id",
                      sort_cols = "date_uk",
                      ncores = NULL,
                      ncores_inner = NULL,
                      dt_tempdir="../../saved_data/",
                      parallel_inner = TRUE,
                      ...){
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  # too memory intensive to send input data to each worker, let's save and load
  lapply(input_data,function(input_dt){
    input_dt <- input_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",vars)]
    save(input_dt,file = paste0(dt_tempdir,unique(input_dt[,get(split_col)]),".rda"))
  })
  
  
  # Calculate the number of cores
  if(is.null(ncores)){
    no_cores <- length(input_data)} else{
      no_cores <- ncores
    }
  
  if(is.null(ncores_inner)){ncores_inner <- length(input_data[,unique(kfold)])}
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  iterations <- length(input_data)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ids <- names(input_data)
  rm(input_data)
  # this next line is really really really important for the memory
  gc()
  
  
  mqr_out <- foreach(i=ids,.packages = c('data.table',"ProbCast"),.options.snow = opts,.export="...") %dopar% {
    
    
    
    load(paste0(dt_tempdir,i,".rda"))
    
    
    x <- try(MQR_gbm(data = input_dt,
                     parallel = parallel_inner,
                     cores = ncores_inner,
                     pckgs = c("data.table"),
                     ...),silent = TRUE)
    
    
    if('try-error' %in% class(x)){
      return(paste0("problem fitting model to node ",i))
      
    } else{
      
      return(x)
      
    }
    
  }
  
  close(pb)
  stopCluster(cl)
  
  names(mqr_out) <- ids
  
  # delete temporary tables
  lapply(ids,function(input_dt){
    file.remove(paste0(dt_tempdir,input_dt,".rda"))
  })
  
  return(mqr_out)
  
  
}



#### wrapper mgcv 
#' wrapper for mgcv for multiple locations
#'
#' This function generates a list of \code{PPD} objects for multiple locations
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{vars,split_col,sort_cols}.
#' @param vars character vector of columns in \code{melted_dt} which will be used in the fitting. Remember to include the response
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}. Sometimes \code{split.data.table} can re-arrange the order of the sub-lists? should probs double check now lol.
#' @param ncores number of cores for the outer loop, i.e. locations. Defaults to number of locations.
#' @param ncores_inner number of cores for the inner loop, i.e. folds. Defaults to number of folds.
#' @param dt_tempdir is a character string containing a file path to save modeling tables temporarily, for large tables this can save a lot of memory
#' @param parallel_inner perform inner loop in parallel? useful on aws
#' @param trunc_dist if a truncated response is requires this should be a character string with the function call from \code{gamlss.tr::gen.trunc}. Note that for the function to work this family should be specified outside this function, as typically done for a truncated gamlss model. Also don't run inner_parallel loop here.
#' @details fill in...
#' @return a list of ordered \code{ppd} objects  for each location
#' @export
ml_pc_mgcv <- function(melted_dt,
                       vars,
                       split_col="id",
                       sort_cols = "date_uk",
                       ncores = NULL,
                       ncores_inner = NULL,
                       dt_tempdir="../../saved_data/",
                       ...){
  
  # input_data = pklcl_data[aggregation=="ss"]
  # vars = c("demand","demandpk_l1")
  # split_col = "id"
  # sort_cols = "date_time"
  # ncores = 3
  # ncores_inner = 3
  # dt_tempdir="../saved_data/"
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  # too memory intensive to send input data to each worker, let's save and load
  lapply(input_data,function(input_dt){
    input_dt <- input_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",vars)]
    save(input_dt,file = paste0(dt_tempdir,unique(input_dt[,get(split_col)]),".rda"))
  })
  
  
  # Calculate the number of cores
  if(is.null(ncores)){
    no_cores <- length(input_data)} else{
      no_cores <- ncores
    }
  
  if(is.null(ncores_inner)){ncores_inner <- length(input_data[,unique(kfold)])}
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  iterations <- length(input_data)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ids <- names(input_data)
  rm(input_data)
  # this next line is really really really important for the memory
  gc()
  
  
  ppd_out <- foreach(i=ids,.packages = c('data.table',"ProbCast","mgcv"),
                     .options.snow = opts,.export=c("...","mgcv_tte","predict.mgcv_tte")) %dopar% {
    
    
    load(paste0(dt_tempdir,i,".rda"))
    
    x <- try(mgcv_tte(data = input_dt,
                      cores = ncores_inner,
                      pckgs = c("data.table"),
                      ...),silent = TRUE)
    
    # if('try-error' %in% class(x)){
    #   return(paste0("problem fitting model to node ",i))
    #   
    # } else{
      
      return(x)
    #   
    # }
    
  }
  
  close(pb)
  stopCluster(cl)
  
  names(ppd_out) <- ids
  
  # delete temporary tables
  lapply(ids,function(input_dt){
    file.remove(paste0(dt_tempdir,input_dt,".rda"))
  })
  
  return(ppd_out)
  
  
}


############################################################ mgcv tte kfold
#' @keywords 
#' @importFrom foreach %dopar%
#' @export
mgcv_tte <- function(data,
                     formula,
                     cv_folds = NULL,
                     cores = 1,
                     pckgs = NULL,
                     exclude_train = NULL,
                     ...){
  
  
  output <- list()
  output$call <- match.call()
  
  # set-up cv folds & do checks
  cv_labs <- ProbCast:::cv_control(data = data,cv_folds = cv_folds)
  output$kfold_index <- cv_labs$idx
  output$model_names <- cv_labs$fold_loop
  
  # exclude points from training? & do checks
  output$exclude_index <- ProbCast:::exclude_fun(data = data,exclude_train = exclude_train)
  
  
  # set up parallel workers, defaults to one worker....
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  #set up progress bar
  iterations <- length(output$model_names)
  pb <- utils::txtProgressBar(max = iterations, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  gc()
  
  
  # fit the models: returns list quantile --> kfold
  output$models <- foreach::foreach(q = output$model_names,.packages = c("mgcv",pckgs),.options.snow = opts) %dopar% {
    

      
    temp <- mgcv::gam(formula=formula,
                      data = data[output$kfold_index!=q & output$kfold_index!="Test" & output$exclude_index==0 & !is.na(data[[formula[[2]]]]),],
                      ...)
      
  }
  
  close(pb)
  parallel::stopCluster(cl)
  names(output$models) <- output$model_names
  
  #set new class & default model for prediction
  class(output) <- "mgcv_tte"
  output$default_model <- if("Test"%in%output$model_names){"Test"}else{output$model_names[1]}
  
  
  # get cross validation mqr predictions, unless cv_folds = NULL
  output$prob_pred <- NULL
  if(!is.null(cv_folds)){

    #create container for cv output

    for(fold in output$model_names){

      output$prob_pred[[fold]] <- predict.mgcv_tte(output,
                                                  newdata = data,
                                                  model_name = fold)
      

    }


    output$prob_pred <- rbindlist(output$prob_pred,use.names = TRUE)
    setkeyv(output$prob_pred,colnames(output$prob_pred)[1])
    output$prob_pred[,c(colnames(output$prob_pred)[1]):=NULL]
    colnames(output$prob_pred) <- paste0("p",colnames(output$prob_pred))
    

  } else{
    # if no cv delete output$kfold_index...
    output$kfold_index <- NULL
  }
  
    return(output)
  
  
}
# #
# test_ms <- mgcv_tte(data = pamm_data[id=="ps1"],
#                     # formula = peak ~  s(time_ind,bs="cp") + s(doy_uk,bs="cp") + demandpk_l1,
#                     # knots=list(time_ind=c(0,48),doy=c(0,365)),
#                     formula = peak ~  s(time_ind,bs="cp"),
#                     knots=list(time_ind=c(0,48)),
#                     cv_folds = "kfold",
#                     family = stats::binomial())



predict.mgcv_tte <- function(object,
                             newdata = NULL,
                             model_name = NULL,
                             what = "prob"){
  
  
  if(class(object)!="mgcv_tte"){stop("object of wrong class, expecting \"mgcv_tte\"")}
  
  if(is.null(model_name)){
    model_name <- object$default_model
  } else{ if(sum(model_name%in%object$model_names)!=1){
    
    stop(paste0(model_name," not in names(object)"))
    
  }
  }
  
  # get predictors
  cols <- labels(object$models[[object$default_model]]$terms)
  
  # get kfold data and only dp during peak
  predict_data <- newdata[kfold==model_name & peak==1,.SD,by=.(id,date_uk),.SDcols = cols]
  
  # each day lasts 48 HH
  predict_data <- predict_data[,lapply(.SD,function(x){rep(x,48)}),by=.(id,date_uk)]
  predict_data[,time_ind:=1:48,by=c("id","date_uk")]
  setkey(predict_data,"id","date_uk","time_ind")
  
  
  # predict
  predict_data[,hazard:= mgcv::predict.gam(object$models[[object$default_model]],newdata = .SD,type="response")]
  predict_data[,surv:=cumprod(1-hazard),by=.(date_uk)]
  predict_data[,prob:=1-surv]
  

  ## shape to wide format
  out <- dcast.data.table(predict_data,
                          formula = date_uk~time_ind,
                          value.var = what)
  
  
  return(out)
  
  
}

# predict.mgcv_tte(test_ms,
#                  newdata = pamm_data[id=="ps1"],
#                  model_name = "Test")























#### wrapper probcast for multiple locations - evaluation
#' wrapper for probcast for multiple locations - evaluation
#'
#' This function generates a list of \code{data.table} objects for multiple locations containing evaluation metrics
#' @param mqr_list a named list of \code{MultiQR} objects, where names correspond to those in \code{split_col}
#' @param metric character string of either \code{rel} or \code{pball} for reliability or pinball evaluation
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{obs_col,split_col,sort_cols}.
#' @param obs_col The column name of observations in \code{melted_dt}
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to the evaluation functions
#' @details fill in....
#' @return a list of ordered \code{data.table} objects  for each location
#' @export
ml_eval_mqr <- function(mqr_list,
                        metric = "rel", # or pball
                        melted_dt,
                        obs_col="demand",
                        split_col="id",
                        sort_cols = "date_uk",
                        ...){
  
  input_data <- split(melted_dt[,.SD,.SDcols=c(obs_col,split_col,sort_cols,"kfold")], by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(mqr_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  mapply(FUN = function(mqr_obj,split_list,...){
    
    if(metric=="rel"){  
      
      data.table(reliability(qrdata = mqr_obj,realisations = split_list[,get(obs_col)],kfolds = split_list[,kfold],...))
      
    } else if(metric=="pball"){
      
      data.table(pinball(qrdata = mqr_obj,realisations = split_list[,get(obs_col)],kfolds = split_list[,kfold],...))
      
    } else{
      
      stop("metric not recognised")
    }
    
    ## default is use.names=TRUE
  },mqr_obj = mqr_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}




#### wrapper probcast for multiple locations - ppd2multiqr
#' wrapper for probcast for multiple locations - ppd2multiqr
#'
#' This function generates a list of \code{MultiQR} objects for multiple locations
#' @param ppd_list a named list of \code{PPD} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{split_col,sort_cols,...} where the extra columns were used in the model fitting.
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to the evaluation functions
#' @details fill in....
#' @return a list of ordered \code{MultiQR} objects  for each location
#' @export
ml_ppd2mqr <- function(ppd_list,
                       melted_dt,
                       split_col="id",
                       sort_cols = "date_uk",
                       ...){
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(ppd_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  # could simplify this to just mapply on the actual function, but this is more flexible if we need to change anything
  mapply(FUN = function(ppd_obj,split_list,...){

    PPD_2_MultiQR(data=split_list,
                  models = ppd_obj,
                  ...)

    ## default is use.names=TRUE
  },ppd_obj = ppd_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}




#### wrapper probcast for multiple locations - ppd2ppd_lite
#' wrapper for probcast for multiple locations - ppd2ppd_lite
#'
#' This function generates a list of \code{MultiQR} objects for multiple locations
#' @param ppd_list a named list of \code{PPD} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{split_col,sort_cols,...} where the extra columns were used in the model fitting.
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @details fill in....
#' @return a list of ordered \code{MultiQR} objects  for each location
#' @export
ml_ppd2ppd_lite <- function(ppd_list,
                       melted_dt,
                       split_col="id",
                       sort_cols = "date_uk"){
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  
  if(sum(names(input_data)==names(ppd_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  # could simplify this to just mapply on the actual function, but this is more flexible if we need to change anything
  mapply(FUN = function(ppd_obj,split_list){
    
    out <- list()
    out$params <- PPD_2_MultiQR(models = ppd_obj, 
                                data = split_list,
                                params=TRUE)
    out$family <- ppd_obj[[1]]$family[1]
    
    out$pdf_fun <- eval(parse(text=paste0("gamlss.dist::d",out$family)))
    out$cdf_fun <- eval(parse(text=paste0("gamlss.dist::p",out$family)))
    out$q_fun <- eval(parse(text=paste0("gamlss.dist::q",out$family)))
    
    class(out) <- "ppd_lite"
    return(out)
    
    ## default is use.names=TRUE
  },ppd_obj = ppd_list, split_list = input_data,SIMPLIFY = F)
  
  
}





#### wrapper probcast for multiple locations - ppdPIT
#' wrapper for probcast for multiple locations - ppdPIT
#'
#' This function generates a list of PIT values objects for multiple locations
#' @param ppd_list a named list of \code{PPD} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{split_col,sort_cols,...} where the extra columns were used in the model fitting.
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @details fill in....
#' @return a list of ordered \code{MultiQR} objects  for each location
#' @export
ml_PIT <- function(ppd_list,
                   melted_dt,
                   split_col="id",
                   sort_cols = "date_uk"){
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(ppd_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  mapply(FUN = function(ppd_obj,split_list){
    
    PIT(ppd_obj,data = split_list)
    
    ## default is use.names=TRUE
  },ppd_obj = ppd_list, split_list = input_data,SIMPLIFY = F)
  
  
}


#### wrapper CRPS for multiple locations - evaluation
#' wrapper for CRPS for multiple locations - evaluation
#'
#' This function generates a list of \code{data.table} objects for multiple locations containing evaluation metrics
#' @param mqr_list a named list of \code{MultiQR} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{obs_col,split_col,sort_cols}.
#' @param obs_col The column name of observations in \code{melted_dt}
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to \code{scoringRules::crps_sample}
#' @details fill in....
#' @return a list of ordered \code{data.table} objects  for each location
#' @export
ml_crps_mqr <- function(mqr_list,
                        melted_dt,
                        obs_col="demand",
                        split_col="id",
                        sort_cols = "date_uk",
                        mod_name = NULL,
                        ...){
  
  input_data <- split(melted_dt[,.SD,.SDcols=c(obs_col,split_col,sort_cols,"kfold")], by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  
  if(sum(names(input_data)==names(mqr_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  
  mapply(FUN = function(mqr_obj,split_list,...){
    
    
    split_list[,crps:=scoringRules::crps_sample(y=get(obs_col),dat = as.matrix(mqr_obj),...)]
    out_dt <- split_list[,.SD,.SDcols=c(sort_cols,"kfold","crps")]
    
    if(!is.null(mod_name)){
      
      setnames(out_dt,"crps",mod_name)
      
    } 
    
    return(out_dt)
    ## default is use.names=TRUE
  },mqr_obj = mqr_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}



#####  bootstrap evaluation (or improvement)
#' bootstrap evaluation (or improvement)
#'
#' This function generates a \code{data.table} of bootstrap averages for scores with sub-groups
#' @param melted_evaldt a melted \code{data.table} object which must contain columns \code{by_cols,eval_cols}.
#' @param by_cols Column names of subgroups for the bootstrap sampling
#' @param eval_cols The column names for bootstrap averaging
#' @param nboot number of bootstraps to perform
#' @param skillscore_b  column for improvement benchmark. Defaults to NULL, i.e. averages are returned
#' @param tidy return melted table? good for ggplot etc.
#' @param ... extra arguments to plot
#' @details fill in....
#' @return a \code{data.table} of bootstrap score averages
#' @export
eval_boot <- function(melted_evaldt,
                      by_cols = c("aggregation","kfold"),
                      eval_cols,
                      nboot = 1000, 
                      na.rm = TRUE,
                      skillscore_b = NULL,
                      tidy = TRUE,
                      ...){
  
  
  boot <- list()
  for(i in 1:nboot) {
    
    boot[[i]] <- melted_evaldt[,.SD[sample(1:.N,.N,replace = TRUE),lapply(.SD, mean, na.rm = na.rm)],keyby=by_cols,.SDcols=eval_cols]
      
  }
  
  boot <- rbindlist(boot,idcol = "boot_no")
  
  if(!is.null(skillscore_b)){
    boot <- boot[,lapply(.SD,function(x){((get(skillscore_b)-x)/get(skillscore_b))*100}),.SDcols=eval_cols,keyby=c("boot_no",by_cols)]
    
  }
  
  if(tidy){
    
    boot <- melt(boot, measure.vars = eval_cols,variable.name = "model_id",value.name = "score")

  } 
    
    
    return(boot)

  
}





#### wrapper gamlss for inspecting fit including pred.intervals
#' wrapper gamlss for inspecting fit including pred.intervals
#'
#' This plots the marginal output for one of the learners in a gamlss model, assuming constant input of other variables
#' @param mod a gamlss model
#' @param test_data a \code{data.table} containing the required input features
#' @param input_data  a \code{data.table} containing the data used to train \code{mod}
#' @param varying_x a character string containing column name of varying parameter in \code{test_data}
#' @param plot_points plot the training data points? caution this sometimes doesn't make sense if the model varies strongly with more than \code{varying_x}
#' @details fill in....
#' @return a marginal output plot
#' @export
gamlss_plot <- function(mod,test_data,input_data,varying_x,plot_points = FALSE,...){
  
  
  pred <- predictAll(mod,newdata = test_data,data = input_data)
  
  test_data <- cbind(test_data,as.data.table(pred))
  
  qs = c(0.01,0.05,0.25,0.5,0.75,0.95,0.99)
  
  if(length(mod$parameters)==2){
    for(i in 1:nrow(test_data)){
      eval(parse(text=paste0("test_data[i,paste0(\"q\",qs*100):=as.list(q",mod$family[1],"(qs,mu,sigma))]")))
    }
  }else if(length(mod$parameters)==3){
    for(i in 1:nrow(test_data)){
      eval(parse(text=paste0("test_data[i,paste0(\"q\",qs*100):=as.list(q",mod$family[1],"(qs,mu=mu,sigma=sigma,nu=nu))]")))
    }
  }else if(length(mod$parameters)==4){
    for(i in 1:nrow(test_data)){
      eval(parse(text=paste0("test_data[i,paste0(\"q\",qs*100):=as.list(q",mod$family[1],"(qs,mu=mu,sigma=sigma,nu=nu,tau=tau))]")))
    }
  }
  test_data[,plot(get(varying_x),q50,type="l",col="blue",xlab=varying_x,...)]
  test_data[,polygon(c(get(varying_x),rev(get(varying_x))),c(q1,rev(q99)),col = adjustcolor("blue",alpha.f = 0.1),lty=0)]
  test_data[,polygon(c(get(varying_x),rev(get(varying_x))),c(q5,rev(q95)),col = adjustcolor("blue",alpha.f = 0.2),lty=0)]
  test_data[,polygon(c(get(varying_x),rev(get(varying_x))),c(q25,rev(q75)),col = adjustcolor("blue",alpha.f = 0.5),lty=0)]
  
  if(plot_points){
    input_data[,points(get(varying_x),get(as.character(formula(mod)[[2]])),pch="o")]
  }
  
  
}





#### kernel density estimation with truncated gaussian kernel
#' this function returns a kernel density estimate using a truncated gaussian, it returns the functions
#'
#' @param y_vec vector of input data
#' @param bw  ...not currently used, SJ bandwith etsimator is used \code{bw.SJ}
#' @param lower_b  lower boundary of the density
#' @param kernel a character string of either \code{trunc_gaus}, or \code{gaus}
#' @param check_pdf check the integration of the kde for unity? defaults to \code{FALSE}
#' @details fill in....
#' @return a list of functions containing the density, distribution, and quantile function for the estimate. And optionally a check on the integration of the pdf to check for unity
#' @export
fit_kde <- function(y_vec, bw = "SJ", lower_b = 0, upper_b = NULL, kernel = "trunc_gaus",check_pdf = FALSE, lite = FALSE){
  
  
  
  n <- length(y_vec)
  miny <- min(y_vec)
  maxy <- max(y_vec)
  
  
  ### bandwidth estimator from here --- tends to work well from previous work, valid for trunc_gaus?
  ### https://www.jstor.org/stable/pdf/2345597.pdf
  # we could use a crps style minimisation like crch for bandwidth estimation?
  # crps
  bw <- try(bw.SJ(y_vec),silent = T)
  if('try-error' %in% class(bw)){
    bw <- try(bw.nrd0(y_vec),silent = T)
  }
  if('try-error' %in% class(bw)){
    stop("error in bandwidth calculation")
  }
  
  
  if(is.null(upper_b)){
    upper_b <- maxy + bw 
  }
    
  y_space <- seq(lower_b,upper_b, length = 200)
  
  
  
  h_vec <- rep(bw, n)
  
  if(kernel=="gaussian"){
    
    ## boundary correction for 0 as in --- https://www.sciencedirect.com/science/article/pii/S0305048314001546#!
    ## no need for trunc gaussian i think...
    h_vec <- pmin(y_vec, h_vec)
    
  }
  
  
  pdf <- sapply(seq_along(y_vec),function(i){
    
    
    if(kernel=="trunc_gaus"){
      
      crch::dtnorm(x = y_space,mean=y_vec[i],sd=h_vec[i],left = lower_b) * (n)^-1
      
    } else if(kernel=="gaus"){
      
      dnorm((y_space - y_vec[i])/h_vec[i]) * (n*h_vec[i])^-1
    }
    
  })
  pdf <- rowSums(pdf)
  
  
  cdf <- sapply(seq_along(y_vec),function(i){
    
    
    if(kernel=="trunc_gaus"){
      
      crch::ptnorm(q = y_space,mean=y_vec[i],sd=h_vec[i],left = lower_b) * (n)^-1
      
    } else if(kernel=="gaus"){
      
      pnorm((y_space - y_vec[i])/h_vec[i]) * (n)^-1
    }
    
  })
  cdf <- rowSums(cdf)
  
  
  
  out <- list()
  # samp <- function(..){sample(x,size=1)}
  
  if(lite){
    out$grid <- matrix(c(y_space,pdf,cdf),nrow = length(y_space))
    colnames(out$grid) <- c("y_space","pdf","cdf")
    
    class(out) <- "kde_est_lite"
    return(out)
    
  } else {
    
    ## generate pdf, cdf and quantile function
    out$pdf_fun <- approxfun(y_space,pdf, rule = 2)
    out$cdf_fun <- approxfun(y_space,cdf, rule = 2)
    out$q_fun <- approxfun(cdf,y_space, rule = 2)
    
    out$support <- c(min(y_space),max(y_space))
    
    if(check_pdf){
      out$pdf_chk <- try(print(integrate(pdf_fun,lower = min(y_space),upper = max(y_space),subdivisions = 2000)))
    }
    
    class(out) <- "kde_est"
    return(out) 
    
    
  }
  
  
}



# fit_kde(lcl_data[id=="N0003",demand])
# fit_kde(lcl_data[id=="N0003",demand],lite=TRUE)



#### function for fitting kde estmates in a kfold cv style
#' this function returns a kernel density estimate using a truncated gaussian, using cross validation
#'
#' @param data A \code{data.table} containing the variable of \code{target_var}. Must also include \code{kfold} labeled folds and "\code{Test}" for test data.
#' @param target_var  column name of the variable for kernel density estimation
#' @param by_var an optional character string, containing a factor/character columname for conditioning, e.g. time of day
#' @param ... extra arguments to \code{fit_kde}
#' @details fill in....
#' @return a list containing the kde functions cn a kfold  (and by_var) basis
#' @export
kde_pc <- function(data,
                   target_var,
                   by_var = NULL,...){
  
  
  modelList <- list()
  
  for(fold in unique(data$kfold)){
    print(fold)
    
    if(!is.null(by_var)){
      
      for(j in unique(data[,get(by_var)])){
        
        temp <- fit_kde(y_vec = data[kfold!=fold & kfold!="Test" & get(by_var)==j,get(target_var)],
                        ...)
        
        modelList[[fold]][[j]] <- temp
        
      }
      
      
    } else{
      
      
      temp <- fit_kde(y_vec = data[kfold!=fold & kfold!="Test",get(target_var)],
                      ...)
      
      modelList[[fold]] <- temp
    }
    
    
  }
  
  modelList$by_var <- by_var
  
  return(modelList)
  
  
}

# tmp_mod <- kde_pc(data=lcl_data[id=="N0003"],target_var = "demand")
# tmp_mod2 <- kde_pc(data=lcl_data[id=="N0003"],target_var = "demand",by_var = "dow_ftr")
# 
# 
# tmp_mod3 <- kde_pc(data=lcl_data[id=="N0003"],target_var = "demand",lite = TRUE)
# tmp_mod4 <- kde_pc(data=lcl_data[id=="N0003"],target_var = "demand",by_var = "dow_ftr", lite = TRUE)




#### function for converting kde estimate to a \code{MultiQR} object
####
#' @param data A \code{data.table} containing the training kfold indexes and  \code{by_var} indexes if used.
#' @param models A \code{list} output from \code{kde_pc}.
#' @param quantiles Vector of quantiles to be included in the returned \code{MultiQR} object.
#' @param by_var an optional character string, containing a factor/character column name for conditioning, e.g. time of day
#' @return A \code{MultiQR} object derived from kde estimates
#' @export
kde_2_MultiQR <- function(data,kde_models,quantiles=seq(0.05,0.95,by=0.05),by_var = NULL){
  
  
  multipleQuantiles <- matrix(NA,nrow=nrow(data),ncol=length(quantiles))
  
  if(!is.null(kde_models$by_var)){
    by_var <- kde_models$by_var
  }
  
  
  for(fold in unique(data$kfold)){
    # print(fold)
    
    if(!is.null(by_var)){
      
      for(j in unique(data[,get(by_var)])){
        
        if(class(kde_models[[fold]][[j]])=="kde_est_lite"){
          
          q_fun <- approxfun(kde_models[[fold]][[j]]$grid[,"cdf"],
                             kde_models[[fold]][[j]]$grid[,"y_space"],rule = 2)
          
          temp <- t(replicate(data[kfold==fold & get(by_var)==j,.N],q_fun(quantiles)))
          
          
        } else{
          
          temp <- t(replicate(data[kfold==fold & get(by_var)==j,.N],kde_models[[fold]][[j]]$q_fun(quantiles)))
          
          
        }
        
        multipleQuantiles[data[kfold==fold & get(by_var)==j,which=TRUE],] <- temp  
        
        
      }
      
      
    } else{
      
      if(class(kde_models[[fold]])=="kde_est_lite"){
        
        q_fun <- approxfun(kde_models[[fold]]$grid[,"cdf"],
                           kde_models[[fold]]$grid[,"y_space"],rule = 2)
        
        multipleQuantiles[data$kfold==fold,] <- t(replicate(data[kfold==fold,.N],q_fun(quantiles)))
        
        
      } else{
        
        multipleQuantiles[data$kfold==fold,] <- t(replicate(data[kfold==fold,.N],kde_models[[fold]]$q_fun(quantiles)))
        
      }
      
      
    }
  
  }
  
  
  
  colnames(multipleQuantiles) <- paste0("q",100*quantiles)
  multipleQuantiles <- as.data.frame(multipleQuantiles)
  class(multipleQuantiles) <- c("MultiQR",class(multipleQuantiles))
  
  return(multipleQuantiles)
  
  
}

#### function for multiple locations for pred/fitting


# tmp_mqr <-  kde_2_MultiQR(data=lcl_data[id=="N0003"],kde_models = tmp_mod)
# tmp_mqr2 <-  kde_2_MultiQR(data=lcl_data[id=="N0003"],kde_models = tmp_mod2,by_var = "dow_ftr")
# 
# 
# tmp_mqr3 <-  kde_2_MultiQR(data=lcl_data[id=="N0003"],kde_models = tmp_mod3)
# tmp_mqr4 <-  kde_2_MultiQR(data=lcl_data[id=="N0003"],kde_models = tmp_mod4)
# 
# # 
# plot(tmp_mqr)
# lines(lcl_data[id=="N0003",demand])
# plot(tmp_mqr3)
# lines(lcl_data[id=="N0003",demand])
# 
# 
# 
# plot(tmp_mqr2)
# lines(lcl_data[id=="N0003",demand])
# plot(tmp_mqr4)
# lines(lcl_data[id=="N0003",demand])



#### wrapper for multiple locations - truncated kde
#' wrapper for multiple locations - truncated kde
#'
#' This function generates a list of \code{kde} objects for multiple locations
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{vars,split_col,sort_cols}.
#' @param target_var  column name of the variable for kernel density estimation
#' @param by_var an optional character string, containing a factor/character columname for conditioning, e.g. time of day
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}. Sometimes \code{split.data.table} can re-arrange the order of the sub-lists? should probs double check now lol.
#' @param ncores number of cores for the outer loop, i.e. locations. Defaults to number of locations.
#' @param ... arguments to \code{fit_kde}
#' @details fill in...
#' @return a list of ordered \code{kde} objects  for each location
#' @export
ml_kde <- function(melted_dt,
                   target_var,
                   by_var = NULL,
                   split_col="id",
                   sort_cols = "date_uk",
                   ncores = NULL,
                   ...=NULL){
  
  #reduce size for memory saving
  melted_dt <- melted_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",target_var,by_var)]
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  
  # Calculate the number of cores
  if(is.null(ncores)){
    no_cores <- length(input_data)} else{
      no_cores <- ncores
    }
  
  #Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  iterations <- length(input_data)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  ids <- names(input_data)
  # this next line is really really really important for the memory
  gc()
  
  
  kde_out <- foreach(i=input_data,.packages = c('data.table',"crch"),.options.snow = opts,.export=c("...","kde_pc","fit_kde")) %dopar% {
    
    
    x <- try(kde_pc(data=i,
                    target_var = target_var,
                    by_var = by_var,
                    ...),silent = TRUE)
    
    
    if('try-error' %in% class(x)){
      return(paste0("problem fitting model to node ",i))

    } else{

      return(x)

    }
    
  }
  
  close(pb)
  stopCluster(cl)
  
  names(kde_out) <- ids
  
  return(kde_out)
  
  
}

# tmp_mlkde <- ml_kde(melted_dt = tmp,target_var = "demand")
# tmp_mlkde2 <- ml_kde(melted_dt = tmp,target_var = "demand",by_var="dow_ftr")




#### wrapper kde predictions for multiple locations - kde_2_MultiQR
#' wrapper for probcast for multiple locations - kde_2_MultiQR
#'
#' This function generates a list of \code{MultiQR} objects for multiple locations
#' @param kde_list a named list of \code{kde} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{split_col,sort_cols,...} where the extra columns were used in the model fitting.
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to the evaluation functions
#' @details fill in....
#' @return a list of ordered \code{MultiQR} objects  for each location
#' @export
ml_kde2mqr <- function(kde_list,
                       melted_dt,
                       target_var = "demand",
                       by_var = NULL,
                       split_col="id",
                       sort_cols = "date_uk",
                       ...){
  
  #reduce size for memory saving
  melted_dt <- melted_dt[,.SD,.SDcols=c(sort_cols,split_col,"kfold",target_var,by_var)]
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(kde_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  # could simplify this to just mapply on the actual function, but this is more flexible if we need to change anything
  mapply(FUN = function(ppd_obj,split_list,...){
    
    
    kde_2_MultiQR(data=split_list,
                  kde_models = ppd_obj,
                  by_var = by_var,
                  ...)
    
    
    ## default is use.names=TRUE
  },ppd_obj = kde_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}


# tmp_mqrml <-  ml_kde2mqr(kde_list = tmp_mlkde,melted_dt = tmp)
# plot(tmp_mqrml$N0003)
# lines(tmp[id=="N0003",demand])
# plot(tmp_mqrml$N2206)
# lines(tmp[id=="N2206",demand])
# 
# 
# tmp_mqrml2 <-  ml_kde2mqr(kde_list = tmp_mlkde2,
#                           melted_dt = tmp,
#                           by_var="dow_ftr",
#                           quantiles = seq(0.01,0.99,0.01))
# plot(tmp_mqrml2$N0003)
# lines(tmp[id=="N0003",demand])
# plot(tmp_mqrml2$N2206)
# lines(tmp[id=="N2206",demand])



#' Produce a DensityFC object from a Parametric Predictive Distribution
#' 
#' @description Produce probability density forecasts at defined regions as a \code{DensityFC} object
#' from a \code{PPD} object. Alternatively
#' return predicted parameters from the \code{PPD} models.
#' 
#' Note that this function may be superseded by an S3 method in future versions of
#' \code{ProbCast}.
#'
#' @param data A \code{data.frame} containing explanatory variables required by \code{models}.
#' @param models A \code{PPD} object.
#' @param quantiles Vector of input quantiles, i.e. values at which to evaluate the density or distribution function
#' @param cdf evaluate the distribution function? Defaults to \code{FALSE}, i.e. the density function
#' @return A \code{DensityFC} object derived from gamlss predictive distributions. Alternatively, a matrix containing the parameters of the predictive gamlss distributions.
#' @export
PPD_2_DensityFC <- function(data,models,quantiles,params=F,cdf = FALSE){
  
  
  # picking out 1st class in-case 'data' is a data.table
  if(class(data)[1]!="data.frame"){
    data <- as.data.frame(data)
  }
  
  # Arrange kfold cross-validation
  if(is.null(data$kfold)){
    data$kfold<-1
  }else{
    data$kfold[is.na(data$kfold)] <- "Test"
  }
  
  # Initialise containers for parameters and quantile forecasts
  parameters <- matrix(1,nrow=nrow(data),ncol=4)
  colnames(parameters) <- c("mu", "sigma", "nu", "tau")
  
  distFamily <- c()
  for(fold in unique(data$kfold)){
    
    tempdata <- data[,which(colnames(data)%in%c(all.names(models[[1]]$mu.formula),
                                                all.names(models[[1]]$sigma.formula),
                                                all.names(models[[1]]$nu.formula),
                                                all.names(models[[1]]$tau.formula)))]
    
    # NAs not allowed in newdata. Flags required to record position.
    gooddata <- rowSums(is.na(tempdata))==0
    
    tempPred <- predictAll(object = models[[fold]],
                           newdata = tempdata[data$kfold==fold & gooddata,],
                           data = na.omit(tempdata[data$kfold!=fold & data$kfold!="Test",]))
    
    for(i in 1:(length(tempPred)-1)){
      parameters[data$kfold==fold & gooddata,i] <- tempPred[[i]]
    }
    
    distFamily <- unique(c(distFamily,models[[fold]]$family[1]))
    
  }
  
  if(params){
    return(parameters)
  }
  
  ### Calculate Quanties
  if(length(distFamily)!=1){stop("length(distFamily)!=1 - Only a single parametric distribution family is allowed.")}
  
  multipleQuantiles <- matrix(NA,nrow=nrow(data),ncol=length(quantiles))
  
  
  if(cdf){
    
    fun <- "p"
    
  } else{
    
    fun <- "d"
  }
  
  input <- list()
  if("mu"%in%gsub(".link","",names(as.list(args(distFamily))))){
    input$mu=parameters[,1]
  }
  if("sigma"%in%gsub(".link","",names(as.list(args(distFamily))))){
    input$sigma=parameters[,2]
  }
  if("nu"%in%gsub(".link","",names(as.list(args(distFamily))))){
    input$nu=parameters[,3]
  }
  if("tau"%in%gsub(".link","",names(as.list(args(distFamily))))){
    input$tau=parameters[,4]
  }
  
  
  for(i in 1:length(quantiles)){
    
    if(cdf){
      input$q <- quantiles[i]
    } else{
      
      input$x <- quantiles[i]
      
    }
    
    multipleQuantiles[,i] <- do.call(paste0(fun,distFamily),input)
  
  }
  
  
  colnames(multipleQuantiles) <- paste0(fun,quantiles)
  multipleQuantiles <- as.data.frame(multipleQuantiles)
  class(multipleQuantiles) <- c("DensityFC",class(multipleQuantiles))
  
  return(multipleQuantiles)
  
  
}


#### wrapper probcast for multiple locations - ppd2densityfc
#' wrapper for probcast for multiple locations - ppd2density
#'
#' This function generates a list of \code{MultiQR} objects for multiple locations
#' @param ppd_list a named list of \code{PPD} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{split_col,sort_cols,...} where the extra columns were used in the model fitting.
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to the evaluation functions
#' @details fill in....
#' @return a list of ordered \code{DensityFC} objects for each location
#' @export
ml_ppd2dens <- function(ppd_list,
                         melted_dt,
                         split_col="id",
                         sort_cols = "date_uk",
                         ...){
  
  input_data <- split(melted_dt, by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(ppd_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  # could simplify this to just mapply on the actual function, but this is more flexible if we need to change anything
  mapply(FUN = function(ppd_obj,split_list,...){
    
    PPD_2_DensityFC(data=split_list,
                    models = ppd_obj,
                    ...)
    
    ## default is use.names=TRUE
  },ppd_obj = ppd_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}




#### calculate the ranked probability score for multi-category probability forecasts
#' ranked probability score for multi-category probability forecasts
#'
#' This evaluation function calculated the ranked probability score for a vector of categorical observations and cumulative probability forecasts
#' @param obs a vector of categorical observations, i.e. 0:24 or 1:5, etc.
#' @param dist_fc a \code{data.frame} where the number of rows equals the length of the observation vector.
#' Columns contain cumulative probability forecasts for each category level.
#' Note that the column should be named p<<X>> where X is the the category, e.g. 0:24, or 1:5 etc.
#' @details fill in....
#' @return a vector of RPS scores, one value for each observation/forecast pair 
#'
#' @export
rps <- function(obs,dist_fc){
  
  # https://doi.org/10.1016/B978-0-12-815823-4.00009-2
  
  if(nrow(dist_fc)!=length(obs)){
    
    stop("nrow(dist_fc)!=length(obs)")
  }
  
  
  eval_dt <- data.table(obs=obs,dist_fc)
  
  #cumprob check
  n <- eval_dt[get(tail(colnames(dist_fc),1))!=1,.N]
  if(n!=0){
    
    warning(paste0("maximum fc probability != 1 in ",n," cases"))
  }
  
  # add index
  eval_dt[,ind:=1:.N]
  
  # melt so we have fc observation pairs by index and category
  eval_dt <- melt(eval_dt,id.vars = c("ind","obs"),value.name = "fc",variable.factor = FALSE,variable.name = "category")
  # use colnames to define level for each category
  eval_dt[,level:=as.numeric(gsub("p","",category))]
  setorder(eval_dt,ind,level)
  
  # calculate ranked probability score for each level
  eval_dt[,bs:=(fc-ifelse(level>=obs,1,0))^2]
  # now rps sum
  eval_dt <- eval_dt[,.(rps=sum(bs)),keyby=.(ind)]
  
  return(eval_dt$rps)
  
}



#### wrapper category evaluation for multiple locations - evaluation
#' wrapper for category evaluation for multiple locations - evaluation
#'
#' This function generates a list of \code{data.table} objects for multiple locations containing evaluation metrics
#' @param fc_list a named list of forecast objects, where names correspond to those in \code{split_col}.
#' For \code{metric = "rps"}, the forecasts should be cumulative probability forecasts for each category.
#' For \code{metric = "mcrd"}, the forecasts should be a \code{MultiQR} object.
#' @param metric either \code{"rps"} for ranked probability score, or \code{"mcrd"} for the multi-category reliability diagram
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{obs_col,split_col,sort_cols}.
#' @param obs_col The column name of observations in \code{melted_dt}
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to \code{scoringRules::crps_sample}
#' @details fill in....
#' @return a list of ordered \code{data.table} objects  for each location
#' @export
ml_eval_cat <- function(fc_list,
                        metric = "rps", #or mcrd
                        melted_dt,
                        obs_col="demand",
                        split_col="id",
                        sort_cols = "date_uk",
                        mod_name = NULL,
                        mcrd_sort = NULL,
                        mcrd_kfoldall = TRUE,
                        ...){
  
  input_data <- split(melted_dt[,.SD,.SDcols=c(obs_col,split_col,sort_cols,"kfold")], by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  
  if(sum(names(input_data)==names(fc_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  
  mapply(FUN = function(fc_obj,split_list,...){
    
    fc_cols <- colnames(fc_obj)
    split_list <- cbind(split_list,fc_obj)
    
    
    if(metric=="rps"){
      
      
      out_dt <- split_list[,rps(obs = get(obs_col),dist_fc = .SD,...),keyby=c(sort_cols,"kfold"),.SDcols=fc_cols]
      
      setnames(out_dt,"V1","rps")
      
      
    } else if(metric=="mcrd"){
      
      if(mcrd_kfoldall){
      
      split_list[,kfold2:=kfold]
      split_list[kfold!="Test",kfold2:="All_cv"]
      
      out_dt <- split_list[,mcrd(obs = get(obs_col),quant_fc = .SD,...),keyby=c(mcrd_sort,"kfold2"),.SDcols=fc_cols]
      
      setnames(out_dt,"kfold2","kfold")
      } else{
        
        out_dt <- split_list[,mcrd(obs = get(obs_col),quant_fc = .SD,...),keyby=c(mcrd_sort,"kfold"),.SDcols=fc_cols]
        
      }
      
         
    } else{
      
      stop("metric not recognised")
    }
    
    
    
    return(out_dt)
    ## default is use.names=TRUE
  },fc_obj = fc_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}



#' Multi Category Reliability Diagram for MultiQR
#' @param qrdata \code{MultiQR} object.
#' @param realisations Vector of realisations corresponding to
#' rows of \code{qrdata}. Missing data as \code{NA}s accepted.
#' @param nboot Calculate this number of boostrap samples
#' to estimate 95\% confdence interval
#' @details Missing values in \code{realisations} are handled by \code{na.rm=T} when
#' calculating average exceedence of a given quantile.
#' @return multi-category reliability diagram https://doi.org/10.1175/1520-0434(1997)012%3C0736:RDFMPF%3E2.0.CO;2
#' @export
mcrd <- function(obs,quant_fc,nboot=NULL,...){
  
  if(nrow(quant_fc)!=length(obs)){
    
    stop("nrow(quant_fc)!=length(obs)")
  }
  
  
  # # ### double check function with dummy data...try PO or large sample lize
  # quant_fc <- data.table(t(replicate(20000,qPO(p = seq(0.05,0.95,0.05),mu = 10))))
  # colnames(quant_fc) <- paste0("q",seq(0.05,0.95,0.05)*100)
  # obs <- rPO(n = 20000,mu = 10)
  eval_dt <- data.table(obs=obs,quant_fc)
  
  # add index
  eval_dt[,ind:=1:.N]
  
  # melt so we have fc observation pairs by index and category
  melt_dt <- melt(eval_dt,id.vars = c("ind","obs"),value.name = "fc",variable.factor = FALSE,variable.name = "quantile")
  # use colnames to define level for each category
  melt_dt[,level:=as.numeric(gsub("q","",quantile))/100]
  setorder(melt_dt,ind,level)
  
  # 
  melt_dt[obs<fc,calib:=1]
  melt_dt[obs>fc,calib:=0]
  # this gives the best reliability in the ideal case, but is different from the reference, i.e. we've included the '+1' term
  # as suggested in the paper seems biased? 
  # here ---> spread the probability between the quantiles within which the obs is observed
  melt_dt[obs==fc,calib:=cumsum(rep(1/(.N+1),.N)),by=.(ind)]
  # melt_dt[obs==fc,calib:=(level-level[1])/(level[.N]-level[1]),by=.(ind)]
  # melt_dt[is.nan(calib),calib:=.5]
  # matplot(rel_out,type = "l")
  # melt_dt[ind==1]

  rel_out <- melt_dt[,.(Empirical=mean(calib,na.rm = T)),keyby=.(Nominal=level)]
  # matplot(rel_out,type = "l")
  
  
  if(!is.null(nboot)){
    
    
    boot <- list()
    for(i in 1:nboot) {
      
      # sample indexes/ replacement
      bootdt <- eval_dt[sample(1:.N,.N,replace = TRUE)]
      bootdt[,ind:=1:.N]
      
      bootdt <- melt(bootdt,id.vars = c("ind","obs"),value.name = "fc",variable.factor = FALSE,variable.name = "quantile")
      
      # use colnames to define level for each category
      bootdt[,level:=as.numeric(gsub("q","",quantile))/100]
      setorder(bootdt,ind,level)
      
      
      # 
      bootdt[obs<fc,calib:=1]
      bootdt[obs>fc,calib:=0]
      bootdt[obs==fc,calib:=cumsum(rep(1/(.N+1),.N)),by=.(ind)]
      
      boot[[i]] <- bootdt[,.(Empirical=mean(calib)),keyby=.(Nominal=level)]
      
      
    }
    
    
    boot <- rbindlist(boot,idcol = "boot_no")
    boot <- boot[,.(upper=quantile(Empirical,probs=.975),
                    lower=quantile(Empirical,probs=0.025)),keyby=.(Nominal)]
    
    #merge
    rel_out <- rel_out[boot,on=.(Nominal)]
    
      
  }
  
  return(rel_out)
 
}


# matplot(rel_out,type = "l")



#' cumulative probability for time of peak prediction, seasonal climatology
#' @param data \code{data.table} object.
#' @param cv_folds colname of fold id in \code{data}
#' @param target_col colname of target variable
#' @param by_cols colname of seasonal variable
#' @param out_levels desired levels for probability predictions
#' @param sort_col colname to sort predictions
#' @details ...
#' @return
#' @export
clim_top <- function(data,
                     cv_folds = "kfold",
                     target_col = "time_ind",
                     by_cols = "seas",
                     out_levels = 1:48,
                     sort_col = "date_uk"){
  
  
  preds <- list()
  for(i in unique(data[[cv_folds]])){
    
    # get proportione, empirical pmf
    temp <- data[kfold!="Test" & kfold!=i,.(time_ind = out_levels,
                                            prob=as.numeric(table(factor(get(target_col),levels=out_levels))/.N)),keyby=by_cols]
    ## fix this
    setkeyv(temp,c(by_cols,"time_ind"))
    
    #empirical cdf
    temp[,cdf:= cumsum(prob),keyby=.(seas)]
    
    temp <- dcast(temp,seas~time_ind,value.var = "cdf")
    
    
    preds[[i]] <- data[kfold==i,.SD,.SDcols=c(sort_col,by_cols)]
    
    preds[[i]] <- temp[preds[[i]],on=c(by_cols)]
    
  }
  
  preds <- rbindlist(preds,use.names = T,idcol = cv_folds)
  
  setnames(preds,as.character(out_levels),paste0("p",out_levels))
  setkeyv(preds,sort_col)
  setcolorder(preds,sort_col)
  
  return(preds)
  
}










as.MultiQR.ppd_lite <- function(object,
                                quantiles = seq(0.05,0.95,0.05),
                                index = NULL){
  
  # object <- temppk
  # quantiles = seq(0.01,0.99,0.01)
  # index <- rep(1,20)
  
  if(is.null(index)){
    index <- 1:nrow(object$params)
  }
  
  
  cols <- formalArgs(object$q_fun)[formalArgs(object$q_fun)%in%colnames(object$params)]
  
  mqr_out <- sapply(quantiles,function(x){
    
    
    do.call(object$q_fun,as.list(cbind(p=x,data.frame(object$params[index,cols,drop = F]))))
    
    
    
  })
  
  if(length(index>1)){
    mqr_out <- data.table(mqr_out)
  } else{
    mqr_out <- data.table(t(mqr_out))
  }
  colnames(mqr_out) <- paste0("q",100*quantiles)
  class(mqr_out) <- c("MultiQR","data.frame")
  
  return(mqr_out)
  
  
  
}



PIT.ppd_lite <- function(object,
                         obs,
                         inverse = FALSE,
                         index = NULL){
  
  if(is.null(index)){
    index <- 1:nrow(object$params)
  }
  
  cols <- formalArgs(object$cdf_fun)[formalArgs(object$cdf_fun)%in%colnames(object$params)]
  
  pit_out <- lapply(data.table(obs),function(x){
    
    if(!inverse){
      do.call(object$cdf_fun,as.list(cbind(q=x,data.frame(object$params[index,cols]))))
    } else{
      
      do.call(object$q_fun,as.list(cbind(p=x,data.frame(object$params[index,cols])))) 
      
    }
    
    
  })
  
  if(length(pit_out)>1){
    pit_out <- data.frame(pit_out)
  }else{
    pit_out <- unname(unlist(pit_out))
  }
  
  # pit_out <- data.frame(pit_out)
  return(pit_out)
  
  
  
}

























#### wrapper CRPS for multiple locations using just models - evaluation
#' wrapper for CRPS for multiple locations - evaluation
#'
#' This function generates a list of \code{data.table} objects for multiple locations containing evaluation metrics
#' @param mod_list a named list of \code{MultiQR} objects, where names correspond to those in \code{split_col}
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{obs_col,split_col,sort_cols}.
#' @param obs_col The column name of observations in \code{melted_dt}
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to \code{scoringRules::crps_sample}
#' @details fill in....
#' @return a list of ordered \code{data.table} objects  for each location
#' @export
ml_crps_mods <- function(mod_list,
                         melted_dt,
                         obs_col="demand",
                         split_col="id",
                         sort_cols = "date_uk",
                         mod_name = NULL,
                         by_var = NULL,
                         quantiles = seq(0.01,0.99,0.01),
                         ...){
  
  input_data <- split(melted_dt[,.SD,.SDcols=c(obs_col,split_col,sort_cols,by_var,"kfold")], by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(mod_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  
  mapply(FUN = function(mod_obj,split_list,...){
    
    if(class(mod_obj)=="ppd_lite"){
      
      mqr_obj <- as.MultiQR.ppd_lite(mod_obj,
                                     quantiles =  quantiles)
      
    } else if(class(mod_obj)=="exp_mixture"){
      
      mqr_obj <- as.MultiQR.exp_mixture(mod_obj,
                                        quantiles =  quantiles)
      
    } else{
      
      
      mqr_obj <- kde_2_MultiQR(data = split_list,
                               kde_models = mod_obj,
                               quantiles = quantiles)
      
      
    }
    
    
    split_list[,crps:=scoringRules::crps_sample(y=get(obs_col),dat = as.matrix(mqr_obj),...)]
    out_dt <- split_list[,.SD,.SDcols=c(sort_cols,"kfold","crps")]
    
    if(!is.null(mod_name)){
      
      setnames(out_dt,"crps",mod_name)
      
    } 
    
    
    return(out_dt)
    ## default is use.names=TRUE
  },mod_obj = mod_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}





#### wrapper probcast for multiple locations - evaluation
#' wrapper for probcast for multiple locations - evaluation
#'
#' This function generates a list of \code{data.table} objects for multiple locations containing evaluation metrics
#' @param mqr_list a named list of \code{MultiQR} objects, where names correspond to those in \code{split_col}
#' @param metric character string of either \code{rel} or \code{pball} for reliability or pinball evaluation
#' @param melted_dt a melted \code{data.table} object which must contain columns \code{obs_col,split_col,sort_cols}.
#' @param obs_col The column name of observations in \code{melted_dt}
#' @param split_col The column name to split \code{melted_dt} on. This is usually a column containing a vector of location identifiers.
#' @param sort_cols This should be a character vector of columns names used to set the order of the split lists, e.g. \code{sort_cols = key(melted_dt)}.
#' @param ... extra arguments to the evaluation functions
#' @details fill in....
#' @return a list of ordered \code{data.table} objects  for each location
#' @export
ml_eval_mods <- function(mod_list,
                        metric = "rel", # or pball
                        melted_dt,
                        obs_col="demand",
                        split_col="id",
                        sort_cols = "date_uk",
                        by_var = NULL,
                        quantiles = seq(0.01,0.99,0.01),
                        ...){
  
  input_data <- split(melted_dt[,.SD,.SDcols=c(obs_col,split_col,sort_cols,by_var,"kfold")], by=split_col,sorted = TRUE)
  invisible(lapply(input_data,function(x){setorderv(x,cols=sort_cols)}))
  
  if(sum(names(input_data)==names(mod_list))!=length(input_data)){
    stop("check order of named lists")
  }
  
  mapply(FUN = function(mod_obj,split_list,...){
    
    
    
    if(class(mod_obj)=="ppd_lite"){
      
      mqr_obj <- as.MultiQR.ppd_lite(mod_obj,
                                     quantiles =  quantiles)
      
    } else if(class(mod_obj)=="exp_mixture"){
      
      mqr_obj <- as.MultiQR.exp_mixture(mod_obj,
                                        quantiles =  quantiles)
      
    } else{
      
      
      mqr_obj <- kde_2_MultiQR(data = split_list,
                               kde_models = mod_obj,
                               quantiles = quantiles)
      
      
    }
    
    if(metric=="rel"){  
      
      data.table(reliability(qrdata = mqr_obj,realisations = split_list[,get(obs_col)],kfolds = split_list[,kfold],...))
      
    } else if(metric=="pball"){
      
      data.table(pinball(qrdata = mqr_obj,realisations = split_list[,get(obs_col)],kfolds = split_list[,kfold],...))
      
    } else{
      
      stop("metric not recognised")
    }
    
    ## default is use.names=TRUE
  },mod_obj = mod_list, split_list = input_data,SIMPLIFY = F,MoreArgs = list(...))
  
  
}








pk_repeat <- function(model, repeat_vec){
  
  inds <- 1:nrow(model$params)
  if(length(inds)!=length(repeat_vec)){
    stop("check dimensions")
  }
  
  inds <- rep(inds, repeat_vec)
  
  model$params <- model$params[inds,]
  return(model)
  
  
}


## bit of a pain during daylight savings...
## just carries over in autumn and loses two points in spring.. 
extract_prob <- function(model, data_long){
  
  data_short <- cbind(data_long[,.N,keyby=.(date_uk)],model$prob_pred)
  
  data_short <- melt.data.table(data_short,
                                id.vars = c("date_uk","N"),
                                variable.name = "tod_uk",
                                value.name = "cumprob")
  
  data_short[,tod_uk:=(as.numeric(gsub("p","",tod_uk))-1)/2]
  setkey(data_short,date_uk,tod_uk)
  data_short[,prob:=c(cumprob[1],diff(cumprob)),by=.(date_uk)]
  
  data_ref <- data_long[,.(date_time,date_uk,tod_uk)]
  
  
  data_ref <- data_ref[data_short,on = .(date_uk,tod_uk)]
  data_ref <- data_ref[!is.na(date_time)]
  data_ref$prob
  
}




#### expert mixture for ppd_lite class of models as a beta transformed linear pool
#'
#' @param mod_da first model to mix, must be of class ppd_lite
#' @param mod_pk second model to mix, must be a class of ppd_lite
#' @param weight_2 weights for the second predictive dist
#' @param optimise_btlp ? TRUE is not implemented yet...
#' @details fill in....
#' @return a model of class exp_mixture
#' @export
exp_mixture <- function(mod_da, mod_pk, weight_pk, optimise_btlp = FALSE, data = NULL, hh_opt = FALSE){
  
  # mod_da <- hh_agg_models$m5$ss5_fdr1
  # mod_pk <- hh_agg_models$bench$ss5_fdr1
  # weight_pk <- rep(0.5,17184)
  # optimise_btlp <- TRUE
  # data <- lcl_data[id=="ss5_fdr1",.(kfold,demand)]
  # hh_opt <- TRUE
  
  
  # mod_da <- hh_agg_models$m5$ps1
  # mod_pk <- hh_agg_models$peak$ps1
  # weight_pk <- hh_agg_models$blend$ps1$weight_pk
  # optimise_btlp <- TRUE
  # data <- lcl_data[id=="ps1",.(kfold,demand,tod_uk)]
  # hh_opt <- TRUE

  
  if(class(mod_da)!=class(mod_pk)){
    stop("check model classes")
  }
  if(class(mod_da)!="ppd_lite"){
    stop("must be a model of class ppd_lite")
  }
  if(nrow(mod_da$params)!=nrow(mod_pk$params)){
    stop("nrow(mod_da$params)!=nrow(mod_pk$params)")
  }
  
  if(nrow(mod_da$params)!=length(weight_pk)){
    stop("nrow(mod_da$params)!=length(weight_pk)")
  }
  
  if(optimise_btlp & is.null(data)){
        stop("optimise_btlp & is.null(data)")
  }
  if(!is.null(data)){
    if(sum(c("kfold","demand")%in%colnames(data))!=2){
      stop("if data supplied, it must contain kfold and demand columns supplied")
    }
    if(nrow(data)!=nrow(mod_da$params)){
      stop("(nrow(data)!=nrow(mod_da$params)")
    }
  }
  
  
  
  out <- list()
  out$mod_da <- mod_da
  out$mod_pk <- mod_pk
  out$weight_pk <- weight_pk
  
  # need to get parameter arguments into these functions
  out$pdf_fun <- function(x,
                          pdf_da = out$mod_da$pdf_fun, pdf_pk = out$mod_pk$pdf_fun,
                          cdf_da = out$mod_da$cdf_fun, cdf_pk = out$mod_pk$cdf_fun,
                          da_params = list(),
                          pk_params = list(),
                          weight_pk = 0.5,
                          alpha = 1, beta = 1){
    
    if(is.null(unlist(da_params))){
      da_params <- formals(pdf_da)[names(formals(pdf_da))%in%c("mu","sigma","nu","tau")]
    }
    if(is.null(unlist(pk_params))){
      pk_params <- formals(pdf_pk)[names(formals(pdf_pk))%in%c("mu","sigma","nu","tau")]
    }
    
    
    pdf_daval <- do.call(pdf_da,append(list(x=x),da_params))
    pdf_pkval <- do.call(pdf_pk,append(list(x=x),pk_params))
    
    cdf_daval <- do.call(cdf_da,append(list(q=x),da_params))
    cdf_pkval <- do.call(cdf_pk,append(list(q=x),pk_params))
    
    pdf <- ((1-weight_pk)*pdf_daval)+(weight_pk*pdf_pkval)
    
    cdf <- ((1-weight_pk)*cdf_daval)+(weight_pk*cdf_pkval)
    scale <- stats::dbeta(cdf,shape1 = alpha, shape2 = beta)
    
    pdf*scale
    
  }
  
  
  
  out$cdf_fun <- function(q,
                          cdf_da = out$mod_da$cdf_fun, cdf_pk = out$mod_pk$cdf_fun,
                          da_params = list(),
                          pk_params = list(),
                          weight_pk = 0.5,
                          alpha = 1, beta = 1){
    
    if(is.null(unlist(da_params))){
      da_params <- formals(cdf_da)[names(formals(cdf_da))%in%c("mu","sigma","nu","tau")]
    }
    if(is.null(unlist(pk_params))){
      pk_params <- formals(cdf_pk)[names(formals(cdf_pk))%in%c("mu","sigma","nu","tau")]
    }
    
    
    
    cdf_daval <- do.call(cdf_da,append(list(q=q),da_params))
    cdf_pkval <- do.call(cdf_pk,append(list(q=q),pk_params))
    
    
    cdf <- ((1-weight_pk)*cdf_daval)+(weight_pk*cdf_pkval)
    cdf <- stats::pbeta(cdf,shape1 = alpha, shape2 = beta)
    cdf
    
  }
  
  ## need to approximate this...zzzz
  out$q_fun <- function(p,
                        q_da = out$mod_da$q_fun, q_pk = out$mod_pk$q_fun,
                        cdf_da = out$mod_da$cdf_fun, cdf_pk = out$mod_pk$cdf_fun,
                        da_params = list(),
                        pk_params = list(),
                        weight_pk = 0.5,
                        alpha = 1, beta = 1,
                        n = 1000L){
    
    if(is.null(unlist(da_params))){
      da_params <- formals(cdf_da)[names(formals(cdf_da))%in%c("mu","sigma","nu","tau")]
    }
    if(is.null(unlist(pk_params))){
      pk_params <- formals(cdf_pk)[names(formals(cdf_pk))%in%c("mu","sigma","nu","tau")]
    }
    
    if(length(da_params$mu)>1){
      stop("length(da_params$mu) must equal 1")
    }
    
    q_daval <- do.call(q_da,append(list(p=p),da_params))
    q_pkval <- do.call(q_pk,append(list(p=p),pk_params))
    
    
    ## if we have lots of quantiles (which we normally do), better to do interpolation
    # find quantile grid
    q <- c(q_daval,q_pkval)
    q <- seq(min(q),max(q),length.out = n)
    
    cdf_daval <- do.call(cdf_da,append(list(q=q),da_params))
    cdf_pkval <- do.call(cdf_pk,append(list(q=q),pk_params))
    
    
    cdf <- ((1-weight_pk)*cdf_daval)+(weight_pk*cdf_pkval)
    cdf <- stats::pbeta(cdf,shape1 = alpha, shape2 = beta)
    
    q_out <- approxfun(cdf,q, rule = 1)(p)
    
    
    # this next condition can happen via the beta transformation...
    # i.e. the cdf points we are looking for are transformed outwith the original bounds
    if(any(is.na(q_out))){
      
      inds <- which(is.na(q_out))
      
      cdfmix <- function(q){
        
        cdf_daval <- do.call(cdf_da,append(list(q=q),da_params))
        cdf_pkval <- do.call(cdf_pk,append(list(q=q),pk_params))
        
        
        cdf <- ((1-weight_pk)*cdf_daval)+(weight_pk*cdf_pkval)
        cdf <- stats::pbeta(cdf,shape1 = alpha, shape2 = beta)
        cdf
        
      }
      
      # just do a search, note this can be slow if there's lots of quantiles requested
      q_find <- function(i){
        uniroot(function(x){cdfmix(x)-p[i]},
                interval = range(c(q_daval[i],q_pkval[i])),
                tol = 10^{-16},extendInt = "yes")$root
      }
      q_out[inds] <- vapply(inds,q_find,numeric(1))
      
      
    }
    
    return(q_out)
    
    
  }
  
  
  
  
  out$alpha <- rep(1,length(weight_pk))
  out$beta <- rep(1,length(weight_pk))
  
  class(out) <- "exp_mixture"
  
  if(optimise_btlp){
    
    res <- list()
    for(i in unique(data$kfold)){
      # print(i)
      
      if(hh_opt){

        for(j in unique(data$tod_uk)){
          
          # print(j)

          inds <- data[kfold!="Test" & kfold!=i & tod_uk==j,which=TRUE]

          res[[i]][[paste0("t_",j)]] <- optim(par = c(1,1), fn = loglikbtlp, gr = grloglikbtlp,
                            object = out,obs = data[inds,demand],index = inds,
                            method = "L-BFGS-B",lower = c(0.0001,0.0001))

          inds_fill <- data[kfold==i & tod_uk==j,which=TRUE]

          out$alpha[inds_fill] <- res[[i]][[paste0("t_",j)]]$par[1]
          out$beta[inds_fill] <- res[[i]][[paste0("t_",j)]]$par[2]



        }

      } else{
        
        inds <- data[kfold!="Test" & kfold!=i,which=TRUE]
        
        res[[i]] <- optim(par = c(1,1), fn = loglikbtlp, gr = grloglikbtlp,
                          object = out,obs = data[inds,demand],index = inds,
                          method = "L-BFGS-B",lower = c(0.0001,0.0001))
        
        inds_fill <- data[kfold==i,which=TRUE]
        
        out$alpha[inds_fill] <- res[[i]]$par[1]
        out$beta[inds_fill] <- res[[i]]$par[2]  
        
        
      }
      
      
    }
    out$btlp_res <- res
    
  }
  
  
  return(out)
  
}


# mod_mix <- exp_mixture(mod_da = hh_agg_models$m5$ps1,
#                        mod_pk = hh_agg_models$peak$ps1,
#                        weight_pk = pk_t_agg_model$ps1,
#                        optimise_btlp = TRUE,
#                        data = lcl_data[id=="ps1",.(kfold,demand)])


# 
# mod_mix <- exp_mixture(mod_da = hh_agg_models$m5$ss3_fdr1,
#                        mod_pk = hh_agg_models$bench$ss3_fdr1,
#                        weight_pk = rep(0.5,lcl_data[id=="ss3_fdr1",.N]),
#                        optimise_btlp = TRUE,
#                        data = lcl_data[id=="ss3_fdr1",.(kfold,demand)])



as.MultiQR.exp_mixture <- function(object,
                                   quantiles = seq(0.05,0.95,0.05),
                                   index = NULL,
                                   ...){
  
  # object <- mod_mix
  # quantiles = seq(0.01,0.99,0.01)
  # index = NULL
  
  if(class(object)!="exp_mixture"){
    stop("class(object)!='exp_mixture'")
  }
  
  
  if(is.null(index)){
    index <- 1:nrow(object$mod_da$params)
  }
  
  
  cols_pk <- formalArgs(object$mod_pk$q_fun)[formalArgs(object$mod_pk$q_fun)%in%colnames(object$mod_pk$params)]
  cols_da <- formalArgs(object$mod_da$q_fun)[formalArgs(object$mod_da$q_fun)%in%colnames(object$mod_da$params)]
  
  
  
  mqr_out <- data.table(cbind(index,
                              object$mod_da$params[index,cols_da,drop = F],
                              object$weight_pk[index],
                              object$mod_pk$params[index,cols_pk,drop = F],
                              object$alpha[index],
                              object$beta[index]))
  colnames(mqr_out) <- c("ind",paste0("da_",cols_da),"wt",paste0("pk_",cols_pk),"alpha","beta")
  setkey(mqr_out,ind)
  
  da_params_call <- parse(text = paste0("list(",paste0(cols_da,"=",paste0("da_",cols_da),collapse = ","),")"))
  pk_params_call <- parse(text = paste0("list(",paste0(cols_pk,"=",paste0("pk_",cols_pk),collapse = ","),")"))
  
  mqr_out <- mqr_out[,as.list(object$q_fun(p = quantiles,
                                           da_params = eval(da_params_call),
                                           pk_params = eval(pk_params_call),
                                           weight_pk = wt,
                                           alpha = alpha,
                                           beta = beta,
                                           ...)),by=.(ind)]
  mqr_out[,ind:=NULL]
  
  
  colnames(mqr_out) <- paste0("q",100*quantiles)
  class(mqr_out) <- c("MultiQR","data.frame")
  
  return(mqr_out)
  
  
  
}


# plot(as.MultiQR.exp_mixture(mod_mix,index = 1:400))
# # plot(as.MultiQR.exp_mixture(mod_mix,index = 1:400,v1 = F))
# #
# t1 <- proc.time()
# x <- as.MultiQR.exp_mixture(mod_mix)
# print(proc.time()-t1)
# # t1 <- proc.time()
# # y <- as.MultiQR.exp_mixture(mod_mix,v1 = F)
# # print(proc.time()-t1)
# t1 <- proc.time()
# xx <- as.MultiQR.exp_mixture(mod_mix, n = 100)
# print(proc.time()-t1)
# # t1 <- proc.time()
# # yy <- as.MultiQR.exp_mixture(mod_mix, n = 100,v1 = F)
# # print(proc.time()-t1)
# 
# 
# t1 <- proc.time()
# x <- as.MultiQR.exp_mixture(mod_mix,quantiles = seq(0.01,.99,.01))
# print(proc.time()-t1)
# # t1 <- proc.time()
# # y <- as.MultiQR.exp_mixture(mod_mix,v1 = F,quantiles = seq(0.01,.99,.01))
# # print(proc.time()-t1)



PIT.exp_mixture <- function(object,
                            obs,
                            inverse = FALSE,
                            index = NULL,
                            ...){
  
  # object <- mod_mix
  # index <- 1:48
  # obs <- lcl_data[id=="ps1"][index,demand]
  # # obs <- lcl_data[id=="ps1"][index,.(runif(.N),runif(.N))]
  # inverse <- FALSE
  
  if(class(object)!="exp_mixture"){
    stop("class(object)!='exp_mixture'")
  }
  if(is.null(index)){
    index <- 1:nrow(object$mod_da$params)
  }
  obs <- as.matrix(obs)
  
  if(nrow(obs)!=length(index)){
    stop("nrow(obs)!=length(index) or length(obs)!=length(index)")
  }
  
  
  #####################################################
  
  cols_pk <- formalArgs(object$mod_pk$q_fun)[formalArgs(object$mod_pk$q_fun)%in%colnames(object$mod_pk$params)]
  cols_da <- formalArgs(object$mod_da$q_fun)[formalArgs(object$mod_da$q_fun)%in%colnames(object$mod_da$params)]
  
  
  if(!inverse){
    
    if(ncol(obs)>1){
      stop("ncol(obs)>1")
    }
    
    pit_out <- object$cdf_fun(q=obs[,1],
                              da_params=as.list(data.frame(object$mod_da$params[index,cols_da,drop = F])),
                              pk_params=as.list(data.frame(object$mod_pk$params[index,cols_pk,drop = F])),
                              weight_pk = object$weight_pk[index],
                              alpha = object$alpha[index],
                              beta = object$beta[index])
  } else{
    
    
    
    pit_out <- sapply(1:length(index),function(y){
      
      
      do.call(object$q_fun,list(p=obs[y,],
                                da_params=as.list(data.frame(object$mod_da$params[index[y],cols_da,drop = F])),
                                pk_params=as.list(data.frame(object$mod_pk$params[index[y],cols_pk,drop = F])),
                                weight_pk = object$weight_pk[index[y]],
                                alpha = object$alpha[index[y]],
                                beta = object$beta[index[y]],
                                ...))
      
      
      
    })
    
    
    
    
  }
  
  
  if(ncol(obs)>1){
    pit_out <- data.table(t(pit_out))
  }
  # pit_out <- data.frame(pit_out)
  return(pit_out)
  
  
  
}

# x <- PIT.exp_mixture(mod_mix,obs = lcl_data[id=="ps1"][,demand])
# hist(x,freq = F,ylim=c(0,1.2))
# hist(PIT.ppd_lite(mod_mix$mod_da,obs = lcl_data[id=="ps1"][,demand]),freq = F,ylim=c(0,1.2))
# 
# inds <- lcl_data[id=="ps1"][kfold!="Test",which=TRUE]
# hist(PIT.exp_mixture(mod_mix,obs = lcl_data[id=="ps1"][inds,demand],index = inds),freq = F,ylim=c(0,1.2))
# 
# dem <- PIT.exp_mixture(object = mod_mix,
#                        obs = x,
#                        inverse = TRUE)
# plot(lcl_data[id=="ps1"][,demand],dem,pch=".")
# lines(1:400,1:400,col="red")
# 
# 
# dem <- PIT.exp_mixture(object = mod_mix,
#                        obs = matrix(c(x,x),nrow = length(x)),
#                        inverse = TRUE)
# 
# cbind(lcl_data[id=="ps1"][,demand],data.table(dem))




loglikbtlp <- function(param,object,obs,index = NULL){
  
  
  # param <- c(1,.5)
  # object <- out
  # obs <- data[inds,demand]
  # index <- inds
  
  if(class(object)!="exp_mixture"){
    stop("class(object)!='exp_mixture'")
  }
  if(is.null(index)){
    index <- 1:nrow(object$mod_da$params)
  }
  obs <- as.matrix(obs)
  
  if(nrow(obs)!=length(index)){
    stop("nrow(obs)!=length(index) or length(obs)!=length(index)")
  }
  
  alpha <- param[1]
  beta <- param[2]
  
  
  cols_pk <- formalArgs(object$mod_pk$pdf_fun)[formalArgs(object$mod_pk$pdf_fun)%in%colnames(object$mod_pk$params)]
  cols_da <- formalArgs(object$mod_da$pdf_fun)[formalArgs(object$mod_da$pdf_fun)%in%colnames(object$mod_da$params)]
  
  lik_out <- object$pdf_fun(x = obs,
                            da_params=as.list(data.frame(object$mod_da$params[index,cols_da,drop = F])),
                            pk_params=as.list(data.frame(object$mod_pk$params[index,cols_pk,drop = F])),
                            weight_pk = object$weight_pk[index],
                            alpha = alpha,
                            beta = beta)
  
  
  # -sum(log(lik_out))
  -sum(log(lik_out[is.finite(lik_out) & lik_out>0]))
  
  
  
  
}






## faster than this if it works out.....
grloglikbtlp <- function(param,object,obs,index = NULL){
  
  
  if(class(object)!="exp_mixture"){
    stop("class(object)!='exp_mixture'")
  }
  if(is.null(index)){
    index <- 1:nrow(object$mod_da$params)
  }
  obs <- as.matrix(obs)
  
  if(nrow(obs)!=length(index)){
    stop("nrow(obs)!=length(index) or length(obs)!=length(index)")
  }
  
  alpha <- param[1]
  beta <- param[2]
  
  
  cols_pk <- formalArgs(object$mod_pk$pdf_fun)[formalArgs(object$mod_pk$pdf_fun)%in%colnames(object$mod_pk$params)]
  cols_da <- formalArgs(object$mod_da$pdf_fun)[formalArgs(object$mod_da$pdf_fun)%in%colnames(object$mod_da$params)]
  
  pit_out <- object$cdf_fun(q=obs,
                            da_params=as.list(data.frame(object$mod_da$params[index,cols_da,drop = F])),
                            pk_params=as.list(data.frame(object$mod_pk$params[index,cols_pk,drop = F])),
                            weight_pk = object$weight_pk[index],
                            alpha = 1,
                            beta = 1)
  
  x <- numeric(2)
  x[1] <- sum(log(pit_out))-(length(index)*(digamma(alpha)-digamma(alpha+beta)))
  x[2] <- sum(log(1-pit_out[pit_out<1]))-(length(index)*(digamma(beta)-digamma(alpha+beta)))
  
  
  return(-x)
  
  
  
  
}





save_plot <- function(fig_obj, dir = plot_save, name, width = 7.12, height = 5.00,...){
  
  setEPS()
  postscript(paste0(dir,name,".eps"),width = width, height = height,...)
  print(fig_obj)
  dev.off()
  
}


quick_hist <- function(values_vec,...) {
  res <- hist(values_vec, plot=FALSE, ...)
  
  dat <- data.frame(xmin=head(res$breaks, -1L),
                    xmax=tail(res$breaks, -1L),
                    ymin=0.0,
                    ymax=res$density)
}







