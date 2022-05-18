## Preamble ####

# options(repos='http://cran.rstudio.com/') ##restart?

packages <- c("rmarkdown","cli","data.table","ggplot2","mgcv",
              "gifski","rstudioapi","tidygraph","ggraph")

install.packages(setdiff(packages, rownames(installed.packages())))


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)
library(ggplot2)
library(ProbCast)



## Start ####

rm(list=ls())



data_save <- "../../saved_data/"
plot_save <- "../../outputs/paper_plots/"

# include utility functions
source("../utils/amidine_utils.R")

ncores <- parallel::detectCores()



### now let's load the data from the data exploration and preparation document
load(paste0(data_save,"prep_expl_smfc_outv2.rda"))

####
# Model naming:
# Problems: hh/PI/PT  :  Agg, House
# Versions: full, simplified, benchmark
#
# Change HH language to "Benchmark" "Simple"  "Full" ("Fusion")
# PI: kde1, kde2, "Simple" "Full"
# PT: Benchmark, Hazard
####

########################################################################
### aggregated time series
########################################################################

set.seed(1)
wk <- lcl_data[,sample(woy_uk,1)]
tmp_plot <- list()
for(i in c(1,10,100,1000)){
  
  ids <- lcl_data[aggregation=="sm",sample(id,size = i)]
  
  tmp_plot[[as.character(i)]] <- lcl_data[id%in%ids & woy_uk==wk,
                                          .(av_demand = mean(demand,na.rm=TRUE)),by=.(date_time)]
  
  
}

tmp_plot <- rbindlist(tmp_plot,idcol = "aggregation")
# tmp_plot[,aggregation:=sprintf("%04d",as.numeric(aggregation))]

p1 <- ggplot(data=tmp_plot,aes(x=date_time,y=av_demand))+
  labs(y = "Average demand [kWh]", x = "Date/Time [dd/mm]")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m")+
  geom_line(colour="steelblue")+
  facet_wrap(~aggregation,nrow=2) + 
  theme_bw() +
  theme(text=element_text(family="serif",size=8),strip.background =element_rect(fill="white"))


# save_plot(p1,name = "sm_agg_intro")
ggsave(paste0(plot_save,"sm_agg_intro.pdf"),plot = p1, width=180,height=120,units = "mm")
ggsave(paste0(plot_save,"sm_agg_intro_small.pdf"),plot = p1, width=90,height=60,units = "mm")
rm(tmp_plot,wk)






########################################################################
### example forecasts
########################################################################

load(paste0(data_save,"halfhourly_mixture_smfc_out.rda"))
sample_ids <- c("ps1","ss1","ss1_fdr1","N1174")
date_oi  <- as.POSIXct("2013-10-17", tz = "Europe/London")


# setEPS()
# postscript(paste0(plot_save,"exfc.eps"),width = 7.12, height = 5)
pdf(file=paste0(plot_save,"exfc.pdf"),width = 6, height = 4)

par(mar=c(3,3,1,1))  # Trim margin around plot [b,l,t,r]
par(tcl=0.35)  # Switch tick marks to insides of axes
par(mgp=c(1.5,0.2,0))  # Set margin lines; default c(3,1,0) [title,labels,line]
par(xaxs="r",yaxs="r")  # Extend axis limits by 4% ("i" does no extension)
par(family="serif",size=8) # Serif font

for(j in seq_along(date_oi)){
  par(mfrow=c(2,2))
  for(i in sample_ids){
    
    
    inds <- lcl_data[id==i][date_uk==date_oi[j],which=T]
    
    if(sum(grep("N",i))==0){
      
      mqr_tmp <-  as.MultiQR.exp_mixture(hh_agg_models$blend[[i]],
                                         quantiles = seq(0.01,0.99,0.01),
                                         index = inds)
    } else{
      
      
      mqr_tmp <-  as.MultiQR.exp_mixture(hh_sm_models$blend[[i]],
                                         quantiles = seq(0.01,0.99,0.01),
                                         index = inds) 
      
      
    }
    
    plot(mqr_tmp,q50_line = F,
         cols = colorRampPalette(c("gold", "red")),
         # ylim=lcl_data[id==i,range(demand)])
         ylim = c(0,lcl_data[id==i,max(demand)]+1.5),
         targetTimes = lcl_data[id==i][date_uk==date_oi[j],tod_uk],
         Legend = ifelse(i=="ps1",F,F),ylab = "Demand [kWh]",xlab = "Time of day [h]"
    )
    lcl_data[id==i][date_uk==date_oi[j],lines(tod_uk,demand)]
  }
  
  
  
}

dev.off()


rm(mqr_tmp,p1)


########################################################################
### peak intensity time series, plots, and example forecasts 
########################################################################



## lagged peak
lcl_data[peak_ind==1,demandpk_l1:=shift(demand,n=1L),by=.(id)]
lcl_data[peak_ind==1,demandpk_l7:=shift(demand,n=7L),by=.(id)]


## lagged daily variance
lcl_data[,dvar_demand:=var(demand),by=.(date_uk,id)]
lcl_data[peak_ind==1,dvar_demand_l1:=shift(dvar_demand,n=1L),by=.(id)]
# lcl _data[peak_ind==1,dvar_demand_l7:=shift(dvar_demand,n=7L),by=.(id)]

## weekday/weekend feature
lcl_data[,dow_ftr:=1]
lcl_data[dow_uk%in%c(1,7),dow_ftr:=0]
lcl_data[,dow_ftr := factor(dow_ftr)]


pklcl_data <- lcl_data[peak_ind==1]

pklcl_data[,empty_h:=0]
pklcl_data[dvar_demand<=0.001,empty_h:=1]


# run-length encoding for blocks of 'emptyness'
pklcl_data[,grp:=rleid(empty_h),by=.(id)]

# set a minimum of 7 days for a block to be defined empty
empty_grp <- pklcl_data[empty_h==1,.N,by=.(id,grp)][N>=7]

# redifine the empty indicator to be only these blocks
pklcl_data[,empty_h:=0]
for(i in unique(empty_grp$id)){
  
  
  pklcl_data[id==i & grp%in%empty_grp[id==i,grp],empty_h:=1]
  
  
}

# set nodes which have <1 month of empty data to zero, see comment
pklcl_data[id%in%pklcl_data[empty_h==1,.N,by=.(id)][N<31]$id,empty_h:=0]

# shift for prediction
pklcl_data[,empty_h_l1:=shift(empty_h,n=1L,fill = empty_h[1L]),by=.(id)]


pklcl_data[,empty_h:=as.factor(empty_h)]
pklcl_data[,empty_h_l1:=as.factor(empty_h_l1)]


# pklcl_data <- na.omit(pklcl_data)

plot_data <- pklcl_data[id%in%sample_ids]
plot_data[aggregation=="ps",id_plot:="primary substation"]
plot_data[aggregation=="ss",id_plot:="secondary substation"]
plot_data[aggregation=="fdr",id_plot:="feeder"]
plot_data[aggregation=="sm",id_plot:="household"]
plot_data[,id_plot := factor(id_plot,levels = c("primary substation","secondary substation",
                                                "feeder","household"))]

plot_data[,id_plot2 := factor(id,levels = sample_ids)]


# ggplot(data=plot_data,aes(x=as.Date(date_uk),y=demand))+
#   labs(y = "daily peak demand [kWh]", x = "date [dd/mm]")+
#   scale_x_date(date_breaks = "2 month",date_labels = "%d/%m")+
#   geom_line(colour="steelblue")+
#   facet_wrap(~id_plot,nrow=2,scales = "free_y")

p1 <- ggplot(data=plot_data,aes(x=as.Date(date_uk),y=demand))+
  labs(y = "Daily peak demand [kWh]", x = "Date/Time [dd/mm]")+
  scale_x_date(date_breaks = "2 month",date_labels = "%d/%m")+
  geom_line(colour="steelblue")+
  facet_wrap(~id_plot2,nrow=2,scales = "free_y")  + 
  theme_bw() +
  theme(text=element_text(family="serif",size=8),strip.background =element_rect(fill="white"))


# save_plot(p1,name = "peak_ts")
ggsave(paste0(plot_save,"peak_ts.pdf"),plot = p1, width=90,height=60,units = "mm")

rm(p1,plot_data)



pklcl_data <- na.omit(pklcl_data)
pklcl_data[aggregation=="ps",aggregation_plot:="Primary substation"]
pklcl_data[aggregation=="ss",aggregation_plot:="Secondary substations"]
pklcl_data[aggregation=="fdr",aggregation_plot:="Feeders"]
pklcl_data[aggregation=="sm",aggregation_plot:="Households"]
pklcl_data[,aggregation_plot := factor(aggregation_plot,levels = c("Primary substation","Secondary substations",
                                                                   "Feeders","Households"))]

p1 <- ggplot(data=rbind(pklcl_data[aggregation_plot!="Households",],pklcl_data[aggregation_plot=="Households",][sample(1:.N,size = 2e4)]),
             aes(y=demand,x=demandpk_l1))+
  labs(y = "Dialy peak demand [kWh]",x = "Daily peak demand lag 1 [kWh]")+
  geom_point(colour="steelblue",size=0.1)+
  facet_wrap(~aggregation_plot,nrow=2,scales = "free")  + 
  theme_bw() +
  theme(text=element_text(family="serif",size=8),strip.background =element_rect(fill="white"))


ggsave(paste0(plot_save,"peak_lag.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "peak_lag")

rm(p1)



# sample_ids <- c("ps1","ss1","ss1_fdr1","N1174")
# date_oi  <- as.POSIXct("2013-10-17", tz = "Europe/London")


# setEPS()
# postscript(paste0(plot_save,"exfc_peaki.eps"),width = 7.12, height = 5)
pdf(file=paste0(plot_save,"exfc_peaki.pdf"),width = 6, height = 4)
par(family="serif",size=8)

par(mar=c(3,3,1,1))  # Trim margin around plot [b,l,t,r]
par(tcl=0.35)  # Switch tick marks to insides of axes
par(mgp=c(1.5,0.2,0))  # Set margin lines; default c(3,1,0) [title,labels,line]
par(xaxs="r",yaxs="r")  # Extend axis limits by 4% ("i" does no extension)


for(j in seq_along(date_oi)){
  par(mfrow=c(2,2))
  for(i in sample_ids){
    
    
    inds <- lcl_data[id==i][date_uk==date_oi[j],which=T][1]
    
    
    if(sum(grep("N",i))==0){
      
      object <- hh_agg_models$blend[[i]]$mod_pk
    } else{
      
      
      object <- hh_sm_models$blend[[i]]$mod_pk
      
    }
    
    
    cols_pk <- formalArgs(object$pdf_fun)[formalArgs(object$pdf_fun)%in%colnames(object$params)]
    
    param_list <- as.list(object$params[inds,cols_pk,drop = T])
    param_list[["q"]] <- seq(lcl_data[id==i,min(demand)],lcl_data[id==i,max(demand)],length.out = 200)
    
    prob <- do.call(object$cdf_fun,args = param_list)
    
    param_list[["x"]] <-  param_list$q
    param_list$q <- NULL
    dens <- do.call(object$pdf_fun,args = param_list)
    
    
    plot(param_list[["x"]],prob,type = "l",col = "red",
         ylab = "Cumulative probability [-]",xlab = "Daily peak demand [kWh]")
    lcl_data[id==i][date_uk==date_oi[j],abline(v = max(demand),lty = 2, col = "black")]
    
    # plot(param_list[["x"]],dens,type = "l",col = "red",
    #      ylab = "density [-]",xlab = "daily peak demand [kwh]")
    # lcl_data[id==i][date_uk==date_oi[j],abline(v = max(demand),lty = 2, col = "black")]
  }
  
  
  
}
dev.off()






########################################################################
### peak timing plots, and example forecasts 
########################################################################


p1 <- ggplot(data=pklcl_data,aes(x = tod_uk, y = ..density..))+
  labs(x = "Time of daily peak [h]", y = "Density [-]")+
  geom_histogram(bins=48,colour="white",fill = "grey50")+
  facet_wrap(~aggregation_plot,nrow=2) + 
  theme_bw() +
  theme(text=element_text(family="serif",size=8),strip.background =element_rect(fill="white"))


ggsave(paste0(plot_save,"peak_timehist.pdf"),plot = p1, width=90,height=60,units = "mm")

# save_plot(p1,name = "peak_timehist")
rm(p1)


prob_fc <- list()
for(j in seq_along(date_oi)){
  for(i in sample_ids){
    
    
    inds <- lcl_data[id==i][date_uk==date_oi[j],which=T]
    
    if(sum(grep("N",i))==0){
      
      prob_fc[[i]] <- data.table(tod_uk = lcl_data[id==i][inds,tod_uk],
                                 prob = hh_agg_models$blend[[i]]$weight_pk[inds],
                                 meas = lcl_data[id==i][date_uk==date_oi[j] & peak_ind==1,tod_uk])
    } else{
      
      
      prob_fc[[i]] <- data.table(tod_uk = lcl_data[id==i][inds,tod_uk],
                                 prob = hh_sm_models$blend[[i]]$weight_pk[inds],
                                 meas =  lcl_data[id==i][date_uk==date_oi[j] & peak_ind==1,tod_uk])
      
    }
  }
  
  
}
prob_fc <- rbindlist(prob_fc,idcol = "id")


prob_fc[,id_plot2 := factor(id,levels = sample_ids)]


p1 <- ggplot(data=prob_fc,aes(x=tod_uk,y=prob))+
  labs(y = "Probability of daily peak [-]", x = "Time of day [h]")+
  geom_vline(data = prob_fc[tod_uk==meas],aes(xintercept = meas),col = "red",lty = 2)+
  geom_segment(aes(x=tod_uk,xend=tod_uk,y=0,yend=prob),colour="grey70")+
  geom_point(colour="steelblue")+
  facet_wrap(~id_plot2,nrow=2) + 
  theme_bw() +
  theme(text=element_text(family="serif",size=8),strip.background =element_rect(fill="white"))


ggsave(paste0(plot_save,"peak_timefc.pdf"),plot = p1, width=90,height=60,units = "mm")

# save_plot(p1,name = "peak_timefc")
rm(p1,prob_fc)




########################################################################
### visualise network
########################################################################

hier_ref[,fdr_id2:=paste0(ss_id,"_",fdr_id)]
# setkey(hier_ref,ps_id,ss_id,fdr_id2)
# 
# 
# sms <- hier_ref$id
# agg <- lcl_data[aggregation!="sm",sort(unique(id))]
# 
# smat <- matrix(nrow=length(agg),ncol=length(sms))
# colnames(smat) <- sms
# rownames(smat) <- agg
# 
# 
# for(i in sms){
#   
#   ids <- unlist(hier_ref[id==i,c(3:4,6)])
#   
#   for (j in ids){
#     smat[j,i] <- 1
#   }
# 
# 
# }
# smat[is.na(smat)] <- 0
# 
# smat <- rbind(smat,diag(length(sms)))
# rownames(smat) <- c(agg,sms)


# https://www.data-to-viz.com/graph/dendrogram.html
# https://www.data-imaginist.com/2017/ggraph-introduction-edges/
# transform it to a edge list!
edges_level1_2 <- hier_ref %>% select(ps_id, ss_id) %>% unique %>% rename(from=ps_id, to=ss_id)
edges_level2_3 <- hier_ref %>% select(ss_id, fdr_id2) %>% unique %>% rename(from=ss_id, to=fdr_id2)
edges_level3_4 <- hier_ref %>% select(fdr_id2, id) %>% unique %>% rename(from=fdr_id2, to=id)
edge_list <- rbind(edges_level1_2, edges_level2_3,edges_level3_4)


node_dt <- lcl_data[,.(max_d=max(demand),agg=unique(aggregation)),keyby=.(id)]
node_dt[,level:=factor(agg,levels = c("ps","ss","fdr","sm"))]
node_dt[level=="sm",level:=NA_real_]

graph <- tidygraph::tbl_graph(nodes = node_dt,edges = edge_list)


p1 <- ggraph::ggraph(graph, 'tree',circular = T) + 
  ggraph::geom_edge_diagonal(colour = "grey 70")+
  ggraph::geom_node_point(aes(size = 1/(as.integer(level)^2)),colour = "steelblue") + 
  coord_fixed()+guides(size = F)+theme(panel.background = element_blank())

ggsave(paste0(plot_save,"lv_network.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "lv_network")
rm(p1,graph,edge_list,edges_level1_2,edges_level2_3,edges_level3_4,node_dt)



########################################################################
### Results: Peak Intensity
########################################################################


load(paste0(data_save,"intensity_smfc_outv2.rda"))

### SM level
boot_data <- cbind(eval_sm_models$crps_kde_u,
                   eval_sm_models$crps_kde_wdwe[,.(kde2=kde_wdwe)],
                   eval_sm_models$crps_sm_bench[,.(gamlss1 = sm_bench)],
                   eval_sm_models$crps_sm_m4[,.(gamlss2 = sm_m4)])

setnames(boot_data,"kde_u","kde1")
boot_data[,aggregation:="household"]
ecols <- c(grep("kde",colnames(boot_data),value = T),
           grep("gamlss",colnames(boot_data),value = T))
boot_data[kfold=="All_cv",kfold:="All CV"]

t1 <- proc.time()
boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = ecols,
                     skillscore_b = "kde1")
print(proc.time()-t1)


# skill scores bootstraps
boot_dt[model_id=="kde1",model_id:="KDE1"]
boot_dt[model_id=="kde2",model_id:="KDE2"]
boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(~kfold)
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_peaki_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "boot_peaki_sm")


temp <- boot_data[,lapply(.SD,mean),by=.(id,kfold),.SDcols = ecols]
temp <- temp[,lapply(.SD,function(x){(1-(x/kde1))*100}),
             .SDcols=3:ncol(temp),keyby=.(id,kfold)]
temp <- melt(temp,id.vars = c("id","kfold"))

# skill score density
temp[variable=="kde2",variable:="KDE2"]
temp[variable=="gamlss1",variable:="Simple"]
temp[variable=="gamlss2",variable:="Full"]
p1 <- ggplot(data=temp[variable!="kde1"],aes(x = value, y = ..density..))+
  geom_histogram(binwidth = 1,fill = "grey50")+
  facet_grid(kfold~variable)+
  labs(x = "CRPS skill score [%]", y = "Density [-]")+
  geom_vline(xintercept = 0,colour = "red", linetype = "dashed") +
  theme_bw() + theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"dens_peaki_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "dens_peaki_sm")



# PIT histogram
pklcl_data[,kfold:=paste0("fold",ceiling(as.integer(format(date_uk,"%d"))/10))]
pklcl_data[kfold=="fold4",kfold:="fold1"]
pklcl_data[kfold=="fold3",kfold:="Test"]
t1 <- proc.time()
sm_eval_pit <- lapply(c("sm_bench","sm_m4"),function(x){
  
  rbindlist(lapply(names(sm_models[[x]]),function(y){
    
    
    pit_out <- pklcl_data[id==y]
    pit_out[,pit := PIT(sm_models[[x]][[y]],data = .SD)]
    # pit_out[,demand:=NULL]
    pit_out[kfold!="Test",kfold:="All CV"]
    pit_out <- pit_out[,.(id,kfold,pit)]
    
  }))
})
print(proc.time()-t1)
# names(sm_eval_pit) <- c("gamlss1","gamlss2")
names(sm_eval_pit) <- c("Simple","Full")
sm_eval_pit <- rbindlist(sm_eval_pit,idcol = "model_id")




temp <- sm_eval_pit[,as.list(quick_hist(pit, breaks=20)),by=.(kfold,model_id)]
p1 <- ggplot(data=temp, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  labs(y = "Density [-]", x = "Probability level [-]") + ylim(0,1.5)+
  geom_rect(fill = "grey75",color = "white")+
  geom_hline(yintercept = 1,colour = "red", linetype = "dashed")+
  facet_grid(kfold~model_id)+
  theme_bw() + theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"pit_peaki_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "pit_peaki_sm")


### AGG level

boot_data <- cbind(eval_agg_models$crps,
                   eval_agg_models$m3_crps[,.(gamlss2=m3)])
setnames(boot_data,"bench","gamlss1")

boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","id","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "gamlss1")
boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]

# bootstrap skill scores
boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
boot_dt[kfold=="All_cv",kfold:="All CV"]

p1 <- boot_dt[model_id=="Full",ggplot(data=.SD, aes(x=id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Node")
              +geom_boxplot(outlier.size = .1)
              +coord_flip()
              +facet_grid(aggregation~kfold,scales = "free_y",space = "free_y")
              +theme(axis.text.y = element_text(size=10,angle = 30, vjust = 0.5, hjust=1))
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"),
                     axis.text.y = element_blank())]
ggsave(paste0(plot_save,"boot_peaki_agg.pdf"),plot = p1, width=90,height=120,units = "mm")
# save_plot(p1,name = "boot_peaki_agg")


# pit histograms
invisible(lapply(names(eval_agg_models$m3_pit),function(x){
  
  pklcl_data[id==x,gamlss2:=eval_agg_models$m3_pit[[x]]]
  pklcl_data[id==x,gamlss1:=eval_agg_models$pit[[x]]]
  
  
}))

pit_data <- pklcl_data[aggregation!="sm",.(date_uk,kfold,id,aggregation,gamlss1,gamlss2)]
pit_data <- melt.data.table(pit_data,measure.vars = c("gamlss1","gamlss2"),
                            value.name = "pit",variable.name = "model_id")
pit_data[kfold!="Test",kfold:="All CV"]

temp <- pit_data[,as.list(quick_hist(pit, breaks=20)),by=.(aggregation,kfold,model_id)]
temp[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]

# watch y limits are slightly larger here..
temp[model_id=="gamlss1",model_id:="Simple"]
temp[model_id=="gamlss2",model_id:="Full"]

p1 <- ggplot(data=temp, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  labs(y = "Density [-]", x = "Probability level [-]") + ylim(0,2)+
  geom_rect(fill = "grey75",color = "white")+
  geom_hline(yintercept = 1,colour = "red", linetype = "dashed")+
  facet_grid(aggregation~kfold+model_id)+
  theme_bw()+
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"pit_peaki_agg.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "pit_peaki_agg")


########################################################################
### Results: Peak Timing ####
########################################################################

# let's clear the workspace
rm(list=ls())

data_save <- "../../saved_data/"
plot_save <- "../../outputs/paper_plots/"

# include utility functions
source("../utils/amidine_utils.R")

### now let's load the data from the data exploration and preparation document
load(paste0(data_save,"prep_expl_smfc_outv2.rda"))

# now load the timing results 
load(paste0(data_save,"timing_smfc_outv2.rda"))


### AGG level
boot_data <- cbind(eval_agg$bench,
                   eval_agg$m8[,.(gam=rps)])

setnames(boot_data,"rps","clim")
boot_data[kfold=="All_cv",kfold:="All CV"]

boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","id","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "clim")

boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]
# bootstrap skill scores
p1 <- boot_dt[model_id=="gam",ggplot(data=.SD, aes(x=id,y=score)) 
              +labs(y = "RPS skill score [%]", x = "Node")
              +geom_boxplot(outlier.size = .1)
              +coord_flip()
              +facet_grid(aggregation~kfold,scales = "free_y",space = "free_y")
              +theme(axis.text.y = element_text(size=10,angle = 30, vjust = 0.5, hjust=1))
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"),
                     axis.text.y = element_blank())]
ggsave(paste0(plot_save,"boot_peakt_agg.pdf"),plot = p1, width=90,height=120,units = "mm")
# save_plot(p1,name = "boot_peakt_agg")


### SM level
boot_data <- cbind(eval_sm$bench,
                   eval_sm$m4[,.(gam=rps)])
boot_data[kfold=="All_cv",kfold:="All CV"]

setnames(boot_data,"rps","clim")
boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "clim")

# skill scores bootstraps
boot_dt[model_id=="clim",model_id:="Benchmark"]
boot_dt[model_id=="gam",model_id:="Hazard"]
p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "RPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(~kfold)
              +theme(legend.position="top")
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_peakt_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# p1 <- save_plot(p1,name = "boot_peakt_sm")


temp <- boot_data[,lapply(.SD,mean),by=.(id,kfold),.SDcols = colnames(boot_data)[-c(1:4)]]
temp <- temp[,lapply(.SD,function(x){(1-(x/clim))*100}),
             .SDcols=3:ncol(temp),keyby=.(id,kfold)]
temp <- melt(temp,id.vars = c("id","kfold"))

# skill score density
p1 <- ggplot(data=temp[variable!="clim"],aes(x = value, y = ..density..))+
  geom_histogram(binwidth = 1,fill = "grey50")+
  facet_grid(~kfold)+
  labs(x = "RPS skill score [%]", y = "Density [-]")+
  geom_vline(xintercept = 0,colour = "red", linetype = "dashed")+
  theme_bw()+
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"dens_peakt_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "dens_peakt_sm")



########################################################################
### Results: Forecast Fusion ####
########################################################################


# let's clear the workspace
rm(list=ls())

data_save <- "../../saved_data/"
plot_save <- "../../outputs/paper_plots/"

# include utility functions
source("../utils/amidine_utils.R")

### now let's load the data from the data exploration and preparation document
load(paste0(data_save,"prep_expl_smfc_outv2.rda"))

## lagged values and omit so we have the same coverage of our forecasts
lcl_data[,demand_l1d:=shift(demand,n=48L),by=.(id)]
lcl_data[,demand_l7d:=shift(demand,n=48*7),by=.(id)]
lcl_data <- na.omit(lcl_data)


lcl_data[,kfold:=paste0("fold",ceiling(as.integer(format(date_uk,"%d"))/10))]
lcl_data[kfold=="fold4",kfold:="fold1"]
lcl_data[kfold=="fold3",kfold:="Test"]

# now load the fusion results 
load(paste0(data_save,"halfhourly_mixture_smfc_out.rda"))

#####################
### AGG level
#####################

boot_data <- cbind(hh_agg_eval$bench,
                   hh_agg_eval$m8[,.(gamlss2=crps)],
                   hh_agg_eval$blend[,.(fusion=crps)])
boot_data[kfold=="All_cv",kfold:="All CV"]

setnames(boot_data,"crps","gamlss1")

boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "gamlss1")
boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]


# bootstrap skill scores
boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
boot_dt[model_id=="fusion",model_id:="Fusion"]

p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(aggregation~kfold)
              +theme(legend.position="top")
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_hh_agg.pdf"),plot = p1, width=90,height=120,units = "mm")
# p1 <- save_plot(p1,name = "boot_hh_agg")


boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","id","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "gamlss1")
boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]

# bootstrap skill scores  by id
p1 <- boot_dt[model_id=="fusion",ggplot(data=.SD, aes(x=id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Node")
              +geom_boxplot(outlier.size = .1)
              +coord_flip()
              +facet_grid(aggregation~kfold,scales = "free_y",space = "free_y")
              +theme(axis.text.y = element_text(size=10,angle = 30, vjust = 0.5, hjust=1))
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"),
                     axis.text.y = element_blank())]
ggsave(paste0(plot_save,"boot_hh_aggid.pdf"),plot = p1, width=90,height=120,units = "mm")
# p1 <- save_plot(p1,name = "boot_hh_aggid")

########### pick out peak hours
boot_data <- boot_data[lcl_data[aggregation!="sm",.(id,date_time,peak_ind)],on=.(id,date_time)]
boot_data <- boot_data[peak_ind==1]
boot_data[,peak_ind:=NULL]


boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "gamlss1")
boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]


# bootstrap skill scores during peaks
boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
boot_dt[model_id=="fusion",model_id:="Fusion"]

p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(aggregation~kfold)
              +theme(legend.position="top")
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_hhpk_agg.pdf"),plot = p1, width=90,height=120,units = "mm")
# p1 <- save_plot(p1,name = "boot_hhpk_agg")


# bootstrap skill scores during peaks by id
boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","id","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "gamlss1")

boot_dt[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]

boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
boot_dt[model_id=="fusion",model_id:="Fusion"]

p1 <- boot_dt[model_id=="Fusion",ggplot(data=.SD, aes(x=id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Node")
              +geom_boxplot(outlier.size = .1)
              +coord_flip()
              +facet_grid(aggregation~kfold,scales = "free_y",space = "free_y")
              +theme(axis.text.y = element_text(size=10,angle = 30, vjust = 0.5, hjust=1))
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"),
                     axis.text.y = element_blank())]
ggsave(paste0(plot_save,"boot_hhpk_aggid.pdf"),plot = p1, width=90,height=120,units = "mm")
# save_plot(p1,name = "boot_hhpk_aggid")


# PIT histograms
hh_agg_eval_pit <- lapply(names(hh_agg_models),function(x){
  
  rbindlist(lapply(names(hh_agg_models[[x]]),function(y){
    
    
    pit_out <- lcl_data[id==y,.(id,aggregation,date_time,peak_ind,kfold,demand)]
    pit_out[,pit := PIT(hh_agg_models[[x]][[y]],obs = demand)]
    pit_out[,demand:=NULL]
    pit_out[kfold!="Test",kfold:="All_cv"]
    
  }))
  
  
})
# names(hh_agg_eval_pit) <- c("gamlss1","gamlss2","fusion")
names(hh_agg_eval_pit) <- c("Simple","Full","Fusion")
hh_agg_eval_pit <- rbindlist(hh_agg_eval_pit,idcol = "model_id")

temp <- hh_agg_eval_pit[,as.list(quick_hist(pit, breaks=20)),by=.(aggregation,kfold,model_id)]
temp[,aggregation:=factor(aggregation,levels = c("ps","ss","fdr"))]
# temp[,model_id:=factor(model_id,levels = c("gamlss1","gamlss2","fusion"))]
temp[,model_id:=factor(model_id,levels = c("Simple","Full","Fusion"))]
temp[kfold=="All_cv",kfold:="All CV"]

p1 <- ggplot(data=temp[model_id!="Simple"], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  labs(y = "Density [-]", x = "Probability level [-]") + ylim(0,1.55)+
  geom_rect(fill = "grey75",color = "white")+
  geom_hline(yintercept = 1,colour = "red", linetype = "dashed")+
  scale_x_continuous(breaks = c(0,0.5,1)) +
  facet_grid(aggregation~kfold+model_id)+
  theme_bw() +
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"pit_hh_agg.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "pit_hh_agg")


rm(boot_data,boot_dt,hh_agg_models,hh_agg_eval,hh_agg_eval_pit,hh_agg_eval_rel,temp)


#####################
### SM level ####
#####################

# let's clear the workspace
rm(list=ls())

data_save <- "../../saved_data/"
plot_save <- "../../outputs/paper_plots/"

# include utility functions
source("../utils/amidine_utils.R")


load(paste0(data_save,"halfhourly_sm_eval.rda"))

boot_data <- cbind(eval_sm$benchtod,
                   eval_sm$benchtoddow[,.(bench_toddow=crps)],
                   eval_sm$m1[,.(m1=crps)],
                   eval_sm$m2[,.(m2=crps)],
                   eval_sm$m4[,.(m4=crps)])
boot_data[kfold=="All_cv",kfold:="All CV"]

temp <- setorder(boot_data[kfold!="Test",lapply(.SD,mean),by=.(id),.SDcols = 5:ncol(boot_data)],-m2)
temp[m2>(m1*1.25) | m4>(m1*1.25)]

ids <- temp[m2>(m1*1.25) | m4>(m1*1.25),id]


# eval_sm$m6 <- setorder(rbind(eval_sm$m1[id%in%ids],eval_sm$m2[!id%in%ids]),id,date_time)
eval_sm$m7 <- setorder(rbind(eval_sm$m1[id%in%ids],eval_sm$m4[!id%in%ids]),id,date_time)

load(paste0(data_save,"halfhourly_mixture_sm_eval_blend.rda"))
eval_sm$blend <- crps
rm(crps)
invisible(gc())

rm(boot_data)
boot_data <- cbind(eval_sm$benchtod,
                   eval_sm$benchtoddow[,.(kde2=crps)],
                   eval_sm$m1[,.(gamlss1=crps)])

eval_sm$benchtod <- NULL
eval_sm$benchtoddow <- NULL
eval_sm$m1 <- NULL

boot_data <- cbind(boot_data,
                   eval_sm$m7[,.(gamlss2=crps)],
                   eval_sm$blend[,.(fusion=crps)])

setnames(boot_data,"crps","kde1")
boot_data[kfold=="All_cv",kfold:="All CV"]
rm(eval_sm)


boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "kde1",
                     nboot = 100)


# skill scores bootstraps
boot_dt[model_id=="kde1",model_id:="KDE1"]
boot_dt[model_id=="kde2",model_id:="KDE2"]
boot_dt[model_id=="gamlss1",model_id:="Simple"]
boot_dt[model_id=="gamlss2",model_id:="Full"]
boot_dt[model_id=="fusion",model_id:="Fusion"]

p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(~kfold)
              +theme(legend.position="top")
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_hh_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "boot_hh_sm")


temp <- boot_data[,lapply(.SD,mean),by=.(id,kfold),.SDcols = colnames(boot_data)[-c(1:4)]]
temp <- temp[,lapply(.SD,function(x){(1-(x/kde1))*100}),
             .SDcols=3:ncol(temp),keyby=.(id,kfold)]
temp <- melt(temp,id.vars = c("id","kfold"))

# skill score density
temp[variable=="kde1",variable:="KDE1"]
temp[variable=="kde2",variable:="KDE2"]
temp[variable=="gamlss1",variable:="Simple"]
temp[variable=="gamlss2",variable:="Full"]
temp[variable=="fusion",variable:="Fusion"]

p1 <- ggplot(data=temp[variable!="KDE1"],aes(x = value, y = ..density..))+
  geom_histogram(binwidth = 1,fill = "grey50")+
  facet_grid(kfold~variable)+
  labs(x = "CRPS skill score [%]", y = "Density [-]")+
  geom_vline(xintercept = 0,colour = "red", linetype = "dashed")+
  theme_bw() + 
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"dens_hh_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "dens_hh_sm")


########### pick out peak hours
### load the data from the data exploration and preparation document
load(paste0(data_save,"prep_expl_smfc_outv2.rda"))

## lagged values and omit so we have the same coverage of our forecasts
lcl_data[,demand_l1d:=shift(demand,n=48L),by=.(id)]
lcl_data[,demand_l7d:=shift(demand,n=48*7),by=.(id)]
lcl_data <- na.omit(lcl_data)


lcl_data <- lcl_data[aggregation=="sm",.(id,date_time,peak_ind,aggregation,date_uk,demand)]
lcl_data[,kfold:=paste0("fold",ceiling(as.integer(format(date_uk,"%d"))/10))]
lcl_data[kfold=="fold4",kfold:="fold1"]
lcl_data[kfold=="fold3",kfold:="Test"]




boot_data <- boot_data[lcl_data[aggregation=="sm",.(id,date_time,peak_ind)],on=.(id,date_time)]
boot_data <- boot_data[peak_ind==1]
boot_data[,peak_ind:=NULL]

setnames(boot_data,old = c("kde1","kde2","gamlss1","gamlss2","fusion"),new=c("KDE1","KDE2","Simple","Full","Fusion"))

boot_dt <- eval_boot(melted_evaldt = boot_data,
                     by_cols = c("aggregation","kfold"),
                     eval_cols = colnames(boot_data)[-c(1:4)],
                     skillscore_b = "KDE1")


# bootstrap skill scores during peaks
p1 <- boot_dt[,ggplot(data=.SD, aes(x=model_id,y=score)) 
              +labs(y = "CRPS skill score [%]", x = "Model")
              +geom_boxplot()
              +facet_grid(aggregation~kfold)
              +theme(legend.position="top")
              +geom_hline(yintercept = 0,colour = "red", linetype = "dashed")
              +theme_bw()
              +theme(legend.position="top",
                     text=element_text(family="serif",size=8),
                     strip.background =element_rect(fill="white"))]
ggsave(paste0(plot_save,"boot_hhpk_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# p1 <- save_plot(p1,name = "boot_hhpk_sm")


temp <- boot_data[,lapply(.SD,mean),by=.(id,kfold),.SDcols = colnames(boot_data)[-c(1:4)]]
temp <- temp[,lapply(.SD,function(x){(1-(x/KDE1))*100}),
             .SDcols=3:ncol(temp),keyby=.(id,kfold)]
temp <- melt(temp,id.vars = c("id","kfold"))

# skill score density during peaks
p1 <- ggplot(data=temp[variable!="KDE1"],aes(x = value, y = ..density..))+
  geom_histogram(binwidth = 1,fill = "grey50")+
  facet_grid(kfold~variable)+
  labs(x = "CRPS skill score [%]", y = "Density [-]")+
  geom_vline(xintercept = 0,colour = "red", linetype = "dashed") +
  xlim(c(-30,40))+
  theme_bw() + 
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"dens_hhpk_sm.pdf"),plot = p1, width=90,height=60,units = "mm")
# save_plot(p1,name = "dens_hhpk_sm")
rm(boot_data,boot_dt)


# PIT histograms

# Load models
load(paste0(data_save,"halfhourly_mixture_smfc_out.rda"))
rm(hh_agg_eval_pit,hh_agg_eval,hh_sm_eval,hh_agg_models)

hh_sm_eval_pit <- lapply(c("m7","blend"),function(x){
  
  rbindlist(lapply(names(hh_sm_models[[x]]),function(y){
    
    
    pit_out <- lcl_data[id==y,.(id,aggregation,date_time,peak_ind,kfold,demand)]
    pit_out[,pit := PIT(hh_sm_models[[x]][[y]],obs = demand)]
    pit_out[,demand:=NULL]
    pit_out[kfold!="Test",kfold:="All CV"]
    
  }))
  
  
})
names(hh_sm_eval_pit) <- c("Full","Fusion")
hh_sm_eval_pit <- rbindlist(hh_sm_eval_pit,idcol = "model_id")

temp <- hh_sm_eval_pit[,as.list(quick_hist(pit, breaks=20)),by=.(aggregation,kfold,model_id)]
temp[,model_id:=factor(model_id,levels = c("gamlss2","fusion"))]


p1 <- ggplot(data=temp, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  labs(y = "Density [-]", x = "Probability level [-]") + ylim(0,1.5)+
  geom_rect(fill = "grey75",color = "white")+
  geom_hline(yintercept = 1,colour = "red", linetype = "dashed")+
  facet_grid(kfold~model_id)+
  theme_bw() + 
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
ggsave(paste0(plot_save,"pit_hh_sm.pdf"),plot = p1,width=90,height=60,units = "mm")
# save_plot(p1,name = "pit_hh_sm")


##################################
## Skill vs Charateristics ####
##################################

rm(list=ls())

data_save <- "../../saved_data/"
plot_save <- "../../outputs/paper_plots/"

# include utility functions
source("../utils/amidine_utils.R")

### now let's load the data from the data exploration and preparation document
load(paste0(data_save,"prep_expl_smfc_outv2.rda"))

## lagged values and omit so we have the same coverage of our forecasts
lcl_data[,demand_l1d:=shift(demand,n=48L),by=.(id)]
lcl_data[,demand_l7d:=shift(demand,n=48*7),by=.(id)]
lcl_data <- na.omit(lcl_data)


lcl_data[,kfold:=paste0("fold",ceiling(as.integer(format(date_uk,"%d"))/10))]
lcl_data[kfold=="fold4",kfold:="fold1"]
lcl_data[kfold=="fold3",kfold:="Test"]

# now load the fusion results 
load(paste0(data_save,"halfhourly_mixture_smfc_out.rda"))
lcl_data[kfold!="Test",kfold:="All_cv"]

ss_summary <-lcl_data[,.(mean_d=mean(demand), sd_d=sd(demand)),keyby=.(aggregation,id, kfold)]
agg_test <- ss_summary[hh_agg_eval$blend[,.(crps=mean(crps)),by=.(aggregation, id, kfold)], on =.(aggregation, id, kfold)]
agg_test <-agg_test[hh_agg_eval$bench[,.(crps_b=mean(crps)),by=.(aggregation, id, kfold)], on =.(aggregation, id, kfold)]

sm_test <- ss_summary[hh_sm_eval$blend[,.(crps=mean(crps)),by=.(aggregation, id, kfold)], on =.(aggregation, id, kfold)]
sm_test <-sm_test[hh_sm_eval$bench_tod[,.(crps_b=mean(crps)),by=.(aggregation, id, kfold)], on =.(aggregation, id, kfold)]

all_test <- rbind(sm_test,agg_test)
all_test[, ss:= (1-(crps/crps_b))*100]
all_test[aggregation=="sm",agg_sm:="Household"]
all_test[aggregation!="sm",agg_sm:="Aggregation"]


# save(all_test,file="../../outputs/paper_plots/temp_data.Rda")
# load(file="../../outputs/paper_plots/temp_data.Rda")


all_test[aggregation=="sm",Aggregation:="Household"]
all_test[aggregation=="fdr",Aggregation:="Feeder"]
all_test[aggregation=="ps",Aggregation:="Primary"]
all_test[aggregation=="ss",Aggregation:="Secondary"]

p1 <- ggplot(data=all_test[kfold=="Test"], aes(x = sd_d/mean_d, y = ss,color=Aggregation,shape=Aggregation)) +
  labs(x = "Coefficient of Variation [-]", y = "CRPS Skill Score [-]") +
  # geom_hline(yintercept = 1,colour = "red", linetype = "dashed")+
   # geom_rect(fill = "grey75",color = "white")+
  geom_point()+
  ylim(c(-10,25))+
  facet_grid(~agg_sm,scales = "free_x")+
  theme_bw() + 
  theme(legend.position="top",
        text=element_text(family="serif",size=8),
        strip.background =element_rect(fill="white"))
  
p1
ggsave(paste0(plot_save,"Skill_vs_Variation.pdf"),plot = p1,width=90,height=60,units = "mm")



