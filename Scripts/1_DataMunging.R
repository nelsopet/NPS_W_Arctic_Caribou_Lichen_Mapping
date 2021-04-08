##GEE output source isn't specified but call rf_lich_oob ... read in pred outputs in WAH_build_mods?
require(tidyverse)
require(caret)
require(randomForest)
require(hexbin)
require(pdp)
#require(randomForestExplainer)
require(ranger)
require(sf)
require(viridis)
## Read in all Google Earth Engine (GEE) output from random forest model predictions for each lichen
## cover group and the associatedp Landsat derivative predictors
## Function slightly modified from Chris Fees response here https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once/21589176#21589176

path = "/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/GEE_output/20200309/";
wd = getwd()
list.files(path)
source("./Functions/Helpers.R")

##Load lichen cover by color group from nasa-veg-data-ingest-r repo 
##maintained by Macander on Gitlab
##
WAH_lich<-read_csv("/Users/peternelson 1/Documents/UMFK/Grants/NASA_ABOVE/Data/2018/Git/nasa-veg-data-ingest-r/training_csv/ARCN_FIA_WGS84_20191026.csv")
#432 rows of different lichen cover measures

#Old way to read in preds
#read_csv_batch(path)
#list.files(path)

##Old predictors
## Read in tabular data and clean up
##Remote system time
#rf_lich_oob<-pred_rf_lich_tot_20200307_74pred_oob_1000trees[,-1]
#rf_lich_oob<-pred_rf_yel_l4_tot_20190831_60pred_oob[,-1]

##New predictors: Load new preds from CCDC mods of Landsat reflectance and other env preds
##Source of preds is GEE script
#https://code.earthengine.google.com/bb4553a4ea95309d750db6e565211254?asset=users%2Fmmacander%2Fabove_mapping%2Fmodeling_202101%2Flichen_plots_normalized_20210112d_wPreds_byYear

CCDC <- read_csv("Data/lichen_plots_normalized_20210112d_wPreds_byYear.csv")
                          
not_preds<-c("system:index"
,"datasetId"                                 
,"landsatCcdcMeta_nBreaks"                   
,"landsatCcdcMeta_nextSegment_changeProb"    
,"landsatCcdcMeta_nextSegment_numObs"        
,"landsatCcdcMeta_nextSegment_tBreak"        
,"landsatCcdcMeta_nextSegment_tEnd"          
,"landsatCcdcMeta_nextSegment_tStart"        
,"landsatCcdcMeta_previousSegment_changeProb"
,"landsatCcdcMeta_previousSegment_numObs"    
,"landsatCcdcMeta_previousSegment_tBreak"    
,"landsatCcdcMeta_previousSegment_tEnd"      
,"landsatCcdcMeta_previousSegment_tStart"    
,"light_lichen_cover"                        
,"light_lichen_cover_i"                      
,"model_group"                               
,"orig_light_lichen_cover"                   
,"plotId"                                    
,"poly_uid"                                  
,"sample_year" 
,"veg_total_cover"
,".geo")    


#Read in the old data
GAAR_ENV<-read.csv(paste(wd,"/Data/Old/","GAAR_Plots.csv",sep=""), header=TRUE, sep=",", stringsAsFactors = FALSE)
#79 rows of env data
XY_ENV<-read.csv(paste(wd,"/Data/Old/","XYEnvironmental_Data.csv", sep=""), sep=",", header=TRUE, stringsAsFactors = FALSE)
#446 rows of env data
WAH_ENV<-read.table(paste(wd,"/Data/Old/","ENV_ARCN_Apr5_2017.txt", sep=""), header=TRUE, sep=",", stringsAsFactors = FALSE)
Full_Comm<-read.csv(paste(wd,"/Data/","full_community_14Jul20.csv", sep=""), sep=",", header=TRUE, stringsAsFactors = FALSE)
#402 rows of env data
Env<-read.table(paste(wd,"/Data/Old/","ENV_ARCN_June4_2019.txt", sep=""),sep=",", header=TRUE, stringsAsFactors = F) 
Env<- Env %>% dplyr::select(plot, colnames(Full_Comm[,2:11]))
#402 rows of env data

#Find plots not in list of ARCN FIA plot used in the lichen depth analysis
#WAH_ENV %>% dplyr::select(plot) %>% anti_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% anti_join(XY_ENV, by="plot") %>% View()
#Env %>% 
#  dplyr::select(plot) %>% 
#  anti_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% 
#  anti_join(XY_ENV, by="plot") %>% 
#  View()

#Change the names of the plots to match those from GAAR and the rest of ARCN
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="6L "       ,"6L",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JPALISADE1","Jpalisad",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="DUNE2"     ,"Dune2",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="DUNES1"    ,"Dunes1",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JLICHEN1"  ,"JLichen1",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JLICHEN2"  ,"JLichen2",WAH_ENV$plot);
#WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JMIXED1"   ,"JMixed1",WAH_ENV$plot);

Env$plot<-ifelse(Env$plot=="6L "       ,"6L",Env$plot);
Env$plot<-ifelse(Env$plot=="JPALISADE1","Jpalisad",Env$plot);
Env$plot<-ifelse(Env$plot=="DUNE2"     ,"Dune2",Env$plot);
Env$plot<-ifelse(Env$plot=="DUNES1"    ,"Dunes1",Env$plot);
Env$plot<-ifelse(Env$plot=="JLICHEN1"  ,"JLichen1",Env$plot);
Env$plot<-ifelse(Env$plot=="JLICHEN2"  ,"JLichen2",Env$plot);
Env$plot<-ifelse(Env$plot=="JMIXED1"   ,"JMixed1",Env$plot);

##Select the plots and needed columns from both the Gates and other ARCN parks
#tmp1<-WAH_ENV %>% 
#  dplyr::select(plot) %>% 
#  inner_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% 
#  mutate(samp_date = as.Date(GPStrmDate, "%m/%d/%y")) %>%
#  dplyr::select(plot, GPScorDD84, GPScorDD_1, samp_date) %>%
#  rename(lat = GPScorDD_1, long = GPScorDD84)
#
#tmp2<-WAH_ENV %>% 
#  dplyr::select(plot) %>% 
#  inner_join(XY_ENV, by="plot") %>% 
#  mutate(samp_date = as.Date(GPS_Date, "%m/%d/%y")) %>%
#  dplyr::select(plot, lat, long, samp_date)

##Merge Gates and other ARCN parks
#tmp3<-rbind(tmp1,tmp2)
#
##Change the names of the plots to match those in curated plant cover data
#tmp3$plot<-ifelse(tmp3$plot=="Jpalisad","JPalisad",tmp3$plot);
#tmp3$plot<-ifelse(tmp3$plot=="Lowerc88","Lowrc88",tmp3$plot);

veg_meas<-Full_Comm %>% 
  inner_join(Env, by=c(colnames(Full_Comm[,2:11])), keep=FALSE) %>% #colnames()
  dplyr::select(plot, AdjHeight) %>%
  inner_join(WAH_lich, by=c("plot"="plot_id"))# %>% colnames()
#New preds
preds<-CCDC %>% dplyr::select(-not_preds, plotId)  #%>% colnames()
##Old preds
#preds<-rf_lich_oob %>%
#dplyr::select(#-lich_fr   #,-lich_fo   #,-lich_m    #,-lich_cr   ##,-lich_tot  #,-drk_l4_tot#,-lgt_l4_tot#,yel_l4_tot#,-drk_l3_tot#,-lgt_l3_tot
#,-drk_l2_tot#,-lgt_l2_tot#,-blk_l8_tot#,-brn_l8_tot#,-gry_l8_tot#,-grg_l8_tot#,-grn_l8_tot#,-orn_l8_tot#,-wht_l8_tot#,-yel_l8_tot #,-hdatum
#,lat_gee#,-lich_tot_pred#,long_gee#,-model_grp#,-propertyNames#, randSel#,-samp_date#,-samp_year#,-src_data#,-yel_l4_tot#,-".geo") # %>% dim() #[1] 432  79
#


#preds_lich_tot<-rf_lich_oob %>% dplyr::select(plot_id, lich_tot, randSel)

##Old env preds
#Env_preds<-Env %>%
#  dplyr::select(	
#    plot
#    #,tas_decadal_mean
#    #,logs_cru
#    #,mu_ALT
#    #,mu_perma
#    #,pr_decadal_mean
#    ,dem_1km
#    #,slope_1km
#    ,FIREYEAR
#    ,PROBABILIT
#    #,vap_05_1990_1999
#    #,vap_08_1990_1999
#    #,rsds_05_1990_1999_AkAlbers
#    #,rsds_08_1990_1999_AkAlbers
#    ,AdjHeight
#  )
#
#Env_preds$FIREYEAR<-ifelse(is.na(Env_preds$FIREYEAR )==T,0,Env_preds$FIREYEAR)
#Env_preds$PROBABILIT<-ifelse(is.na(Env_preds$PROBABILIT)==T,0,Env_preds$PROBABILIT)

#Check stuff
#Env_preds %>% anti_join(preds, by=c("plot"="plot_id")) %>% dplyr::select(plot) %>% View()
#Env_preds %>% right_join(preds, by=c("plot"="plot_id")) %>% colnames() #dplyr::select(plot, plot_id) # %>% View()
#preds %>% full_join(Env_preds, by=c("plot_id"="plot")) %>% dplyr::select(plot_id) %>% View()
#preds %>% dplyr::select("plot_id") %>% View()

##Old
#lich_vol_df<-Env_preds %>% left_join(preds, by=c("plot"="plot_id"))  #%>% nrow() #dplyr::select(plot) %>% View()

#New
lich_vol_df<-preds %>% inner_join(veg_meas, by=c("plotId"="plot")) #%>% colnames()
lich_vol_df<-lich_vol_df %>% filter(is.na(AdjHeight)==FALSE) #%>% nrow()
  
#Make lichen volume columns
lich_vol_df$lich_vol<-lich_vol_df$AdjHeight*lich_vol_df$lich_tot

#Add random nmber of 80/20 split
  rand_col<-sample(seq_len(nrow(lich_vol_df)),size = floor(0.75*nrow(lich_vol_df)))
  lich_vol_df_cal<-lich_vol_df[rand_col,]
  lich_vol_df_val<-lich_vol_df[-rand_col,]
  lich_vol_df_cal$randSel<-0
  lich_vol_df_val$randSel<-1
  dim(lich_vol_df_val)
  dim(lich_vol_df_cal)
  
  lich_vol_df_train<-rbind(lich_vol_df_val,lich_vol_df_cal) %>% as.data.frame()
  hist(lich_vol_df_train$randSel)
  
  write_csv(lich_vol_df_train, paste("./Output/","lich_vol_df_train.csv"))
  
###OLD SPATIAL  
#  
#  ##  Make a shapefile
#arcn_gis_pts_lichvol <-lich_vol_df
#  #subset(lich_vol_df, is.na(lat_gee)==T) %>% length()
#
#arcn_gis_pts_lichvol<-st_as_sf(
#  lich_vol_df,
#    coords = c("long_gee","lat_gee"),
#    crs = 4326,
#    agr = "constant"
#  )
#plot(arcn_gis_pts_lichvol)
#st_write(arcn_gis_pts_lichvol,
#         dsn = paste0(
#           "ARCN_FIA_LICHVOL_WGS84",
#           "_",
#           format(Sys.time(), "%Y%m%d"),
#           ".shp",
#           sep = ""
#         ))
#
### Project shapefile
#arcn_gis_pts_lichvol_prj <- arcn_gis_pts_lichvol %>% st_transform(32606)
#
### Buffer plot center by 35m
#arcn_gis_pts_lichvol_prj_out <- arcn_gis_pts_lichvol_prj %>% st_buffer(35) %>% st_transform(4326)
#
### Write shapefile to local drive
#st_write(
#  arcn_gis_pts_lichvol_prj_out,
#  dsn = paste0(
#    "ARCN_FIA_LICHVOL_WGS84_BUFF35M",
#    "_",
#    format(Sys.time(), "%Y%m%d"),
#    ".shp",
#    sep = ""
#  )
#)
#
# Find  and remove intercorrelated variables and rerun ranger random forests      
corrMatrix <- lich_vol_df %>% 
  dplyr::select(-plot, -randSel, -AdjHeight, -lich_tot, -lich_vol, -FIREYEAR, -PROBABILIT) %>%
  as.data.frame() %>% 
  cor(use="complete.obs")
caret_findCorr <- findCorrelation(corrMatrix, cutoff = 0.97, verbose=TRUE, names=TRUE, exact=TRUE)
caret_findCorr  %>% sort()

  train_data_rf_vol<-lich_vol_df %>% 
  dplyr::select(-plot) %>% 
  subset(randSel<0.8) %>% 
  dplyr::select(-randSel, -AdjHeight, -lich_tot, -caret_findCorr) %>% as.data.frame()
#  dplyr::select(-randSel, -lich_tot) %>% as.data.frame()
  
  colnames(train_data_rf_vol)
  nrow(train_data_rf_vol)
  #rf_lich_vol_all_preds<-randomForest(lich_vol~., data=train_data_rf_vol, localImp = TRUE, ntree=10000, na.action=na.exclude) 
  rf_lich_vol_all_preds<-ranger(lich_vol~., data=train_data_rf_vol, localImp = TRUE, ntree=10000, na.action=na.exclude) 
  rf_lich_vol_all_preds
  varImpPlot(  rf_lich_vol_all_preds)
 rf_lich_vol_all_preds$importance %>% as.data.frame() %>% View()

