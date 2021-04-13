
set.seed(1234)
source("./Functions/Helpers.R")

##Load libraries
install_or_load_pack(c("tidyverse","caret","randomForest","hexbin","pdp","viridis","ModelMetrics","ranger","Metrics"))

##### 
#OLD preds, change path to new folder if needing to rerun
## Read in all Google Earth Engine (GEE) output from random forest model predictions for each lichen
## cover group and the associatedp Landsat derivative predictors
## Function slightly modified from Chris Fees response here https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once/21589176#21589176

##path = "/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/GEE_output/20200312/";

#perpos <- which(strsplit(files[1], "")[[1]]==".")
#assign(gsub(" ","",substr(files[1], 1, perpos-1)), read.csv(paste(path,files[1],sep="")))

##Old preds
#read_csv_batch(path)
#
#list.files(path) #These are the input files
## eg. "pred_rf_blk_l8_tot_20190831_60pred_oob.csv", "pred_rf_brn_l8_tot_20190831_60pred_oob.csv"
#####

##NEW
## List of target responses for each output file from GEE
resp_names<-c('lich_fr','lich_fo','lich_m' ,'lich_cr','lich_tot','drk_l4_tot'  ,'lgt_l4_tot'  ,'yel_l4_tot'  ,'drk_l3_tot'  ,'lgt_l3_tot'  ,'drk_l2_tot'  ,'lgt_l2_tot'  ,
              'blk_l8_tot'  ,'brn_l8_tot'  ,'gry_l8_tot'  ,'grg_l8_tot'  ,'grn_l8_tot'  ,'orn_l8_tot'  ,'wht_l8_tot'  ,'yel_l8_tot', 'lich_vol')
##### 
##OLD
#resp_names<-c('lich_fr','lich_fo', 'lich_tot','drk_l4_tot'  ,'lgt_l4_tot'  ,'yel_l4_tot'  ,'drk_l3_tot'  ,'lgt_l3_tot'  ,'drk_l2_tot'  ,'lgt_l2_tot'  ,
#              'brn_l8_tot'  ,'gry_l8_tot'  , 'wht_l8_tot'  ,'yel_l8_tot')

##NEW AND OLD BOTH
resp_full_names<-c('Fruticose lichen cover'     ,'Foliose lichen cover'     ,'Multiform lichen cover'      ,'Crustose lichen cover'     ,'Total lichen cover'    ,
                   'Dark (4 category) lichen cover'  ,'Light (4 category) lichen cover'  ,'Yellow (4 category) lichen cover'  ,'Dark (3 category) lichen cover'  ,'Light (3 category) lichen cover'  ,'Dark (2 category) lichen cover'  ,'Light (2 category) lichen cover'  ,'Black (8 category) lichen cover'  ,'Brown (8 category) lichen cover'  ,'Gray (8 category) lichen cover'  ,'Gray-green (8 category) lichen cover'  ,'Green (8 category) lichen cover'  ,'Orange (8 category) lichen cover'  ,'White (8 category) lichen cover'  ,'Yellow (8 category) lichen cover', 'Lichen volume')

##NEW
pred_names<-CCDC %>% dplyr::select(-not_preds) %>% colnames() #%>% as.character()

##For each object, generate list of inputs for analysis and plotting following syntax needed for those functions
##### 
##OLD
##input function call
#input<-Make_Inputs(resp_names)
#input %>% View()


##Make and observed vs predicted graph for each lichen color group
##OLD
## Unit test passes
#pdf(paste("obs_vs_pred_",input$lich[1],"_test.pdf", sep=""));
#output_tst<-ggplot(data=eval(parse(text =input$lich_data[1])), 
#       mapping=aes(x=eval(parse(text =input$lich[1])),y=eval(parse(text =input$lich_pred[1])), xmax=100, ymax=100))+
#      geom_hex(bins=15)+
#      #geom_point()+
#      labs(x=input$lich[1], y=input$lich_pred[1])+
#      #theme_minimal()+
#      #theme(panel.background = element_blank())+
#      axis.line = element_line(colour = 'black', size = 2)+
#      #axis.ticks = element_line(colour = 'black', size = 2)+
#      geom_abline(aes(slope=1,intercept=0))
#output_tst
#dev.off()
##Interesting how the histogram of the observed is much longer tailed than the predicted
#hist(pred_rf_lich_tot_20190831_60pred_oob$lich_tot)
#hist(pred_rf_lich_tot_20190831_60pred_oob$lich_tot_pred)

#OLD list of preds
#pred<-c("aet"              ,"blue_p010"       ,"blue_p025"       ,"blue_p050"       ,"blue_p075"       ,"blue_p090"       ,"def"             ,"evi_p010"       
#        ,"evi_p025"        ,"evi_p050"        ,"evi_p075"        ,"evi_p090"        ,"green_p010"     ,"green_p025"      ,"green_p050"      ,"green_p075"      
#        ,"green_p090"       ,"nbr_p010"        ,"nbr_p025"        ,"nbr_p050"        ,"nbr_p075"        ,"nbr_p090"        ,"ndmi_p010"      
#        ,"ndmi_p025"       ,"ndmi_p050"       ,"ndmi_p075"       ,"ndmi_p090"       ,"ndsi_p010"      ,"ndsi_p025"       ,"ndsi_p050"       ,"ndsi_p075"       
#        ,"ndsi_p090"       ,"ndvi_p010"       ,"ndvi_p025"       ,"ndvi_p050"       ,"ndvi_p075"       ,"ndvi_p090"       ,"ndwi_p010"      
#        ,"ndwi_p025"       ,"ndwi_p050"       ,"ndwi_p075"       ,"ndwi_p090"       ,"nir_p010"       ,"nir_p025"        ,"nir_p050"        ,"nir_p075"        
#        ,"nir_p090"        ,"pdsi"            ,"pet"             ,"pr"              ,"randSel"         ,"red_p010"        ,"red_p025"        ,"red_p050"        
#        ,"red_p075"        ,"red_p090"        ,"ro"              ,"soil"            ,"srad"            ,"swe"             ,"swir1_p010"      ,"swir1_p025"     
#        ,"swir1_p050"      ,"swir1_p075"      ,"swir1_p090"      ,"swir2_p010"      ,"swir2_p025"     
#        ,"swir2_p050"      ,"swir2_p075"      ,"swir2_p090"      ,"tmmn"            ,"tmmx"           ,"vap"             ,"vpd"             ,"vs"              )


## Make models using all preds

##OLD
## Test out selecting just one response variable, adding it to the predictor object
## Unit test passes
##train_data_pred<-paste(resp_names[5],"_pred", sep="")
#train_data<-eval(parse(text =input$lich_data[21])) %>% colnames()
#dplyr::select(pred,resp_names[21]) %>%
#  subset(randSel<0.8) %>% 
#  dplyr::select(-randSel)
##colnames(train_data)
##assign(paste("rf_",resp_names[1], sep=""), randomForest(eval(parse(text =lich_rf_formula[1])), data=train_data, localImp = TRUE, ntree=5000)) 
#assign(paste("rf_",resp_names[21], sep=""), ranger(eval(parse(text =paste(input$lich[21],"~.", sep=""))), data=train_data, local.importance =TRUE, num.trees =15000, importance = "impurity_corrected" )) 
### Pass Unit test with both 2 and 4 as input
##### 

#Build random forest models for each response variable using the trainig 80% split
lich_col_rf_ranger<-function(x)
{
  #NEW RF
  temp_df<- lich_vol_df#_train %>% filter(randSel<0.8)
  temp_resp<- temp_df %>% dplyr::select(resp_names[x]) #%>% colnames()
  temp_pred<- temp_df %>% dplyr::select(pred_names) #%>% dim()
  temp_df_rf<-cbind(temp_resp, temp_pred)
  assign(paste("rf_",resp_names[x], sep=""), ranger(eval(parse(text =paste(resp_names[x],"~.", sep=""))), data=temp_df_rf, local.importance =TRUE, num.trees =200, importance = "impurity_corrected" )) 
  
  #OLD RF
  #train_data_rf<-eval(parse(text =input$lich_data[x])) %>% 
  #  dplyr::select(pred,resp_names[x]) %>% 
  #  subset(randSel<0.8) %>% 
  #  dplyr::select(-randSel);
  ##str(train_data_rf) #This runs
  ##assign(paste("rf_",resp_names[x], sep=""), randomForest(eval(parse(text =paste(input$lich[x],"~.", sep=""))), data=train_data_rf, localImp = TRUE, ntree=5000)) 
  #
  #assign(paste("rf_",resp_names[x], sep=""), ranger(eval(parse(text =paste(input$lich[x],"~.", sep=""))), data=train_data_rf, local.importance =TRUE, num.trees =15000, importance = "impurity_corrected")) 
  
}
## Apply function 
lich_col_rf_run<-lapply(1:length(resp_names),lich_col_rf_ranger)

#Generate models stats for export
lich_col_df_stat<-lapply(1:length(lich_col_rf_run), function(x) {lich_col_rf_run[[x]]$r.squared}) %>% 
  unlist() %>%
  cbind(resp_full_names) %>% 
  as.data.frame()


##Get obs vs pred correlation

##### 
##OLD
##Correlation between observed vs predicted values for 20% reserved validation data
##Unit test PASS
#validation_data_rf<-eval(parse(text =input$lich_data[5])) %>% 
#  dplyr::select(pred,resp_names[5]) %>% 
#  subset(randSel>0.8) %>% 
#  dplyr::select(-randSel)
##str(validation_data_rf$lich_tot)
##str(lich_col_rf_pred_run[5])
#
#cor(unlist(lich_col_rf_pred_run[5]),validation_data_rf[,75])
##### 

## Calculate correlation between obs vs predicted values for validation subset
#UNIT TEST 
obs_input<-lich_vol_df %>% 
  dplyr::select(resp_names[21]) 
valid_pred_input<-lich_col_rf_run[[21]]$predictions %>% as.data.frame()
round((cor(valid_pred_input,obs_input[[1]])^2),2)
Metrics::rmse(valid_pred_input$., obs_input[[1]])
##PASS


cor_valid<- function(x) 
{
  obs_input<-lich_vol_df %>% 
    dplyr::select(resp_names[x]) 
  valid_pred_input<-lich_col_rf_run[[x]]$predictions %>% 
    as.data.frame() #%>% 
    cor_out<-round((cor(valid_pred_input,obs_input[[1]])^2),2)
    mae_out<-Metrics::mae(valid_pred_input$.,obs_input[[1]])
    rmse_out<-Metrics::rmse(valid_pred_input$.,obs_input[[1]])
    sse_out<-Metrics::sse(valid_pred_input$.,obs_input[[1]])
    stats_out<-c(cor_out, mae_out, rmse_out, sse_out)
    return(stats_out)
}

##### 
#OLD FUNCTION
#cor_valid<- function(x) 
#{
#  obs_input<-eval(parse(text =input$lich_data[x])) %>% 
#    dplyr::select(pred,resp_names[x]) %>% 
#    subset(randSel>0.8) %>% 
#    dplyr::select(-randSel) %>% as.data.frame()
#  valid_pred_input<-lich_col_rf_pred_run[[x]] %>% 
#    unlist(recursive=F) %>% 
#    as.data.frame() %>% 
#    select(predictions) %>% as.data.frame()
#  #print(nrow(validation_data_rf))
#  #str(validation_data_rf$lich_tot)
#  #str(lich_col_rf_pred_run[5])
#  round((cor(valid_pred_input,as.numeric(obs_input[,75]))^2),2)
#}
##### 
cor_valid_out<-lapply(1:length(resp_names), cor_valid) %>% as.data.frame() %>% t()
cor_valid_out<-cbind(resp_full_names,cor_valid_out) %>% as.data.frame()
rownames(cor_valid_out)<-NULL
colnames(cor_valid_out)<-c("Lichen Group","Obs vs Pred correlation", "MAE","RMSE","SSE")
write.csv(cor_valid_out,"./Output/rf_stats_lichen_color_groups_v_CCDC.csv")

##UNIT TEST for plotting obs vs pred
valid_pred_input<-lich_col_rf_run[[1]]$predictions %>% as.data.frame()  
obs_input_plot<-cbind(obs_input,valid_pred_input) %>% rename(obs = resp_names[1], pred = ".")

##UNIT TEST on plotting obs vs pred
ggplot(data=obs_input_plot, mapping=aes(x=resp_names[1],y=pred))+#, xmax=100, ymax=100))+
  #geom_abline()
  #geom_hex(bins=15, aes(fill = stat(log(count))))+
  #labs(x=input$lich[x], y=input$lich_pred[x])+
  labs(x=resp_full_names[1], y=paste("Predicted",resp_full_names[1]), subtitle=paste("R2=",cor_valid_out[1,2]), title = paste(resp_full_names[1],"Ranger observed vs predicted"))+
  #theme_minimal()+
  theme(panel.background = element_blank())+
  geom_abline(aes(slope=1,intercept=0))+
  stat_smooth(method = "lm", col = "red")+
  scale_fill_viridis()#+
#annotate("text", main = paste("Obs vs Pred R2=",cor_valid_out[x,2]), colour = "black");

#geom_smooth(method="lm")
#geom_line(method='lm', formula= input$lich[x]~input$lich_pred[x])
#obs_vs_pred_plot


##For each respose variable and associated data set, make a hexbin figure of observed vs predicted values with 1:1 line and sensible axis labels
obs_vs_pred<- function(x) 
{
  obs_input<-lich_vol_df %>% dplyr::select(resp_names[x]) 
  valid_pred_input<-lich_col_rf_run[[x]]$predictions %>% as.data.frame()
  obs_input_plot<-cbind(obs_input,valid_pred_input) %>% rename(obs = resp_names[x], pred = ".")

  obs_vs_pred_plot<-ggplot(data=obs_input_plot, mapping=aes(x=obs,y=pred))+#, xmax=100, ymax=100))+
  geom_hex(bins=15, aes(fill = stat(log(count))))+
  labs(x=resp_full_names[x], y=paste("Predicted",resp_full_names[x]), subtitle=paste("R2=",cor_valid_out[x,2]), title = paste(resp_full_names[x],"Ranger observed vs predicted"))+
  theme(panel.background = element_blank())+
  geom_abline(aes(slope=1,intercept=0))+
  stat_smooth(method = "lm", col = "red")+
  scale_fill_viridis()+
  annotate("text", main = paste("Obs vs Pred R2=",cor_valid_out[x,2]), colour = "black");

obs_vs_pred_plot};
obs_vs_pred_est<-lapply(1:length(resp_names), obs_vs_pred)

obs_vs_pred_est[2]

## Make a blank pdf
pdf("./Output/obs_vs_pred_hexbin_all.pdf")
## Apply obs vs pred function to each element in the list of responses
lapply(1:length(resp_names), obs_vs_pred)
dev.off()

##### 
##OLD
#obs_vs_pred<- function(x) 
#{obs_input<-eval(parse(text =input$lich_data[x])) %>% 
#  subset(randSel>0.8) %>% 
#  dplyr::select(-randSel);
#obs_vs_pred_plot<-ggplot(data=obs_input, mapping=aes(x=eval(parse(text =input$lich[x])),y=eval(parse(text =input$lich_pred[x]))))+#, xmax=100, ymax=100))+
#  geom_hex(bins=15, aes(fill = stat(log(count))))+
#  #labs(x=input$lich[x], y=input$lich_pred[x])+
#  labs(x=resp_full_names[x], y=paste("Predicted",resp_full_names[x]), subtitle=paste("R2=",cor_valid_out[x,2]), title = paste(resp_full_names[x],"GEE smileRF validation observed vs predicted"))+
#  #theme_minimal()+
#  theme(panel.background = element_blank())+
#  geom_abline(aes(slope=1,intercept=0))+
#  stat_smooth(method = "lm", col = "red")+
#  scale_fill_viridis()#+
##annotate("text", main = paste("Obs vs Pred R2=",cor_valid_out[x,2]), colour = "black");
#
##geom_smooth(method="lm")
##geom_line(method='lm', formula= input$lich[x]~input$lich_pred[x])
#obs_vs_pred_plot};
#obs_vs_pred(21)
### Make a blank pdf
#pdf("obs_vs_pred_hexbin_all.pdf")
### Apply obs vs pred function to each element in the list of responses
#lapply(1:length(resp_names), obs_vs_pred)
#dev.off()
##### 

## Find  and remove intercorrelated variables and rerun ranger random forests      
corrMatrix <- #eval(parse(text =input$lich_data[1])) %>% 
  dplyr::select(pred) %>% 
  subset(randSel<0.8) %>% 
  dplyr::select(-randSel) %>% #dim()
  cor()
caret_findCorr <- findCorrelation(corrMatrix, cutoff = 0.97, verbose=TRUE, names=TRUE, exact=TRUE)
caret_findCorr  %>% sort()

eval(parse(text =input$lich_data[1])) %>% 
  dplyr::select(pred) %>% 
  subset(randSel<0.8) %>% 
  dplyr::select(-randSel,-caret_findCorr) %>% ncol()

lich_col_rf_ranger_53var<-function(x)
{
  train_data_rf<-eval(parse(text =input$lich_data[x])) %>% 
    dplyr::select(pred,resp_names[x]) %>% 
    subset(randSel<0.8) %>% 
    dplyr::select(-randSel,-caret_findCorr);
  assign(paste("rf_53pred",resp_names[x], sep=""),ranger(eval(parse(text =paste(input$lich[x],"~.", sep=""))), data=train_data_rf, local.importance =TRUE, num.trees =15000, importance = "impurity_corrected")) 
}  

lich_col_rf_53var_run<-lapply(1:length(resp_names),lich_col_rf_ranger_53pred)


##Find important variables
rf_AIR<-lapply(1:length(resp_names), function (x)
{
  tst<-lich_col_rf_53var_run[x] %>% unlist(recursive = F, use.names=T) #%>% as.data.frame()
  tst_rf<-enframe(tst$variable.importance, name="predictor", value="importance_air") %>% 
    arrange(desc(importance_air))
  tst_rf$resp<-resp_names[x];
  tst_rf$rsq<-tst$r.squared
  tst_rf$error<-tst$prediction.error
  assign(paste("rf_AIR",resp_names[x], sep=""), tst_rf) %>% 
    as.data.frame()
  #write.csv(tst_rf,file = paste(resp_full_names[x],"_53pred_AIR.csv"));
  return(tst_rf)
}
);

rf_AIR_tall<-do.call(rbind, rf_AIR)


##Select fit and error and combine with variable names
rf_fit<-lapply(1:length(resp_names), function (x)
{
tst<-lich_col_rf_53var_run[x] %>% unlist(recursive = F, use.names=T) #%>% as.data.frame()
tst_rf<-enframe(tst$r.squared, name="predictor", value="rsq") %>% 
  arrange(desc(rsq))
tst_rf$resp<-resp_names[x];
#tst_rf$rsq<-tst$r.squared
tst_rf$error<-tst$prediction.error
assign(paste("rf_fit",resp_names[x], sep=""), tst_rf) %>% 
  as.data.frame()
return(tst_rf)
}
)

rf_fit_tall<-do.call(rbind, rf_fit) %>% 
  as.data.frame() %>%
  cbind(cor_valid_out) %>% #colnames()#This object becomes a list here and won't export
  as.data.frame() %>% #colnames() %>% #View()
  select(resp, resp_full_names, everything()) %>%
  rename(obs_vs_pred_GEE = cor_valid_out) %>%
  mutate(rsq=round(rsq,2), error=round(error,2)) %>%
  select(-predictor) # %>% as.data.frame()
    rf_fit_tall <- apply(rf_fit_tall,2,as.character)
      write.csv(rf_fit_tall, file ="rf_53pred_Fit.csv")

rf_AIR_stats<-rf_AIR_tall %>% 
  as.data.frame() %>%
  ##remove models that are too poor to believe
  filter(rsq>=0.4) %>% #select(resp) %>% unique()
  group_by(resp) %>% 
  mutate(max_air= max(importance_air), rel_air = round(importance_air/max_air, 2), avg_rel_air = round(mean(rel_air),2)) %>% as.data.frame() %>% arrange(-rel_air) %>% #str()#View()
  
          #jpeg("WAH_AIR_53vars_100dpi.jpg", res=100, width=1200, height=1200)
          #par(las=2)
          #boxplot(rel_air~predictor, data=rf_AIR_stats, xlab="", ylab="ranger random forest AIR score relativized by maximum", main="AIR score by predictor for lichen cover models with r2 >0.4") 
          #dev.off()
  
  select(predictor, resp, rel_air) %>%  
  ungroup() %>% 
  spread(key=predictor, value=rel_air) %>% 
  select(resp, ro, everything()) %>%
  t() %>% #View()
  as.data.frame() %>% 
  mutate(preds = row.names(.)) %>% #View()
  select(preds, everything())
View(rf_AIR_stats)   
WriteXLS::WriteXLS(rf_AIR_stats, ExcelFileName ="rf_53pred_AIR.xls", col.names=F)

tst<-lich_col_rf_run[2] %>% unlist(recursive = F, use.names=T) #%>% as.data.frame()
tst$prediction.error
tst$r.squared

tst<-lich_col_rf_53var_run[2] %>% unlist(recursive = F, use.names=T) #%>% as.data.frame()
tst$prediction.error
tst$r.squared

##Assess model accuracy by predicting validation subset

##NEW
lich_col_rf_pred<- function(x) 
{
  validation_data_rf<-lich_vol_df_train %>% 
    subset(randSel>0.8) %>%
    dplyr::select(pred_names) #%>%
    
  assign(paste("rf_pred",resp_names[x], sep=""), predict(lich_col_rf_run[x], data=validation_data_rf)) 
  #cor(lich_col_rf_pred_run[x],)
}

# Make a collected output of randomForest models


##OLD
#lich_col_rf_pred<- function(x) 
#{
#  validation_data_rf<-eval(parse(text =input$lich_data[x])) %>% 
#    dplyr::select(pred,resp_names[x]) %>% 
#    subset(randSel>0.8) %>% 
#    #dim(validation_data_rf) #79 76 rows cols
#    dplyr::select(-randSel)
#  assign(paste("rf_pred",resp_names[x], sep=""), predict(lich_col_rf_run[x], data=validation_data_rf)) 
#  #cor(lich_col_rf_pred_run[x],)
#}
#
## Make a collected output of randomForest models
lich_col_rf_pred_run<-lapply(1:length(resp_names),lich_col_rf_pred)

lich_col_rf_pred_run[[2]] %>% 
  unlist(recursive=F) %>% 
  as.data.frame() %>% 
  select(predictions)

lich_col_rf_53pred_run


##NEW Add correlation of obs vs pred for the validation dataset only
    obs_input<-lich_vol_df_train %>% 
      subset(randSel>0.8) %>%
      dplyr::select(resp_names[1]) %>% as.list()
    valid_pred_input<-lich_col_rf_pred_run[[1]] %>% 
      unlist(recursive = FALSE) %>%
      as.data.frame() %>% 
      dplyr::select(predictions)
      #  dim(valid_pred_input)
    round((cor(valid_pred_input,as.numeric(obs_input[[1]]))^2),2)

cor_valid_pred<- function(x) 
{
  obs_input<-lich_vol_df_train %>% 
    subset(randSel>0.8) %>%
    dplyr::select(resp_names[x]) %>% as.list()
  valid_pred_input<-lich_col_rf_pred_run[[x]] %>% 
    unlist(recursive = FALSE) %>%
    as.data.frame() %>% 
    dplyr::select(predictions) 
  round((cor(valid_pred_input,as.numeric(obs_input[[1]]))^2),2)
}

cor_valid_out_pred<-lapply(1:length(resp_names), cor_valid_pred) 
cor_valid_out_pred<-cbind(resp_full_names,cor_valid_out_pred) %>% as.data.frame()
cor_valid_out_pred

##NEW
obs_vs_pred_valid<-function(x)
{
  obs_input<-lich_vol_df_train %>% 
    subset(randSel>0.8) %>%
    dplyr::select(resp_names[x]) #%>%
  valid_pred_input<-lich_col_rf_pred_run[[x]] %>% 
    unlist(recursive=F) %>% 
    as.data.frame() %>% 
    select(predictions)
  obs_input_plot<-cbind(obs_input,valid_pred_input) %>% rename(obs = resp_names[x], pred = predictions)
  
  obs_vs_pred_plot<-ggplot(data=obs_input_plot, mapping=aes(x=obs,y=pred))+#, xmax=100, ymax=100))+
    #labs(x=input$lich[x], y=input$lich_pred[x])+
    geom_hex(bins=10, aes(fill = stat(log(count))))+
    labs(x=resp_full_names[x], y=paste("Predicted",resp_full_names[x]), size=12)+
    theme_minimal()+
    theme(panel.background = element_blank())+
    geom_abline(aes(slope=1,intercept=0))+
    stat_smooth(method = "lm", col = "red")+
    scale_fill_viridis()+
    annotate("text", label = paste("Obs vs Pred R2=",cor_valid_out_pred[x,2]), x = 40, y = 90, size = 5, colour = "black");
  obs_vs_pred_plot
  #text(x=max(eval(parse(text =input$lich[x])))*0.1,y=max(eval(parse(text =input$lich_pred[x])))*0.9, paste(cor_valid_out[x,2]))
  #max(eval(parse(text =input$lich[2])))*0.1
  #geom_smooth(method="lm")
  #geom_line(method='lm', formula= input$lich[x]~input$lich_pred[x])
  #obs_vs_pred_plot
}
##### 
##OLD
#obs_vs_pred_valid<-function(x)
#{
#  obs_input<-eval(parse(text =input$lich_data[x])) %>% 
#    subset(randSel>0.8) %>% 
#    dplyr::select(-randSel);
#  valid_pred_input<-lich_col_rf_pred_run[[x]] %>% 
#    unlist(recursive=F) %>% 
#    as.data.frame() %>% 
#    select(predictions)
#  obs_vs_pred_plot<-ggplot(data=obs_input, mapping=aes(x=eval(parse(text =input$lich[x])),y=eval(parse(text =valid_pred_input))))+ ##, xmax=60, ymax=60))+
#    geom_hex(bins=10, aes(fill = stat(log(count))))+
#    #labs(x=input$lich[x], y=input$lich_pred[x])+
#    labs(x=resp_full_names[x], y=paste("Predicted",resp_full_names[x]), size=12)+
#    #theme_minimal()+
#    theme(panel.background = element_blank())+
#    geom_abline(aes(slope=1,intercept=0))+
#    stat_smooth(method = "lm", col = "red")+
#    scale_fill_viridis()+
#    annotate("text", label = paste("Obs vs Pred R2=",cor_valid_out[x,2]), x = 40, y = 90, size = 5, colour = "black");
#  obs_vs_pred_plot
#  #text(x=max(eval(parse(text =input$lich[x])))*0.1,y=max(eval(parse(text =input$lich_pred[x])))*0.9, paste(cor_valid_out[x,2]))
#  #max(eval(parse(text =input$lich[2])))*0.1
#  #geom_smooth(method="lm")
#  #geom_line(method='lm', formula= input$lich[x]~input$lich_pred[x])
#  #obs_vs_pred_plot
#}
##### 
## Make a blank pdf
pdf("./Output/obs_vs_pred_hexbin_all_validation.pdf")
#pdf("obs_vs_pred_hexbin_all_validation.pdf")
## Apply obs vs pred function to each element in the list of responses
lapply(1:length(resp_names), obs_vs_pred_valid)
dev.off()



###Random Forest explainer
##Unit test 1-4 wrk
#min_depth_frame <- min_depth_distribution(rf_yel_l8_tot)
#save(min_depth_frame, file = "min_depth_frame_rf_yel_l8.rda")
#min_depth_distribution(eval(parse(text=paste("rf_",resp_names[19],sep=""))))
##4
#min_depth_distribution(lich_col_rf_run[[1]])
#assign(paste("min_depth_frame_dummy_",resp_names[5], sep=""),min_depth_distribution(lich_col_rf_run[[1]]))
###
#jpeg("lich_tot_min_depth_dist.jpeg")
#plot_min_depth_distribution(min_depth_frame_dummy_lich_tot)
#dev.off()
##Apply a function to make all the min_depth objects that are saved
## This takes many hours to run
#min_depth_all<-lapply(1:length(resp_names), 
#                      function(x) {assign(paste("min_depth_frame_fnc_",resp_names[x], sep=""), 
#                                          min_depth_distribution(lich_col_rf_run[[x]]))} )
#
#save(min_depth_all, file = "min_depth_all.rda")
#
#load("min_depth_all.rda")
#
#pdf("min_depth_distribution_all.pdf")
#lapply(1:20, function(x) {plot_min_depth_distribution(min_depth_all[[x]], main=paste("Minimum depth distribution for ", resp_full_names[x], sep=""))})
#dev.off()
#
### Make an importance frame for one rf mod
##This takes many hourse to run
#importance_frame_all <- lapply(1:20, function(x) {measure_importance(lich_col_rf_run[[x]])})
#save(importance_frame_all, file = "importance_frame_all.rda")
#
##load("variable_importance/importance_frame.rda")
##importance_frame
#
#pdf("multiway_way_importance_all.pdf")
#lapply(1:20, function(x) {plot_multi_way_importance(importance_frame_all[[x]], size_measure = "no_of_nodes", main=paste("Variable importance for ", resp_full_names[x], sep=""))})
#dev.off()
#
#pdf("importance_ggpairs_all.pdf")
#lapply(1:20, function(x) {plot_importance_ggpairs(importance_frame_all[[x]], main=paste("Importance pairs for  ", resp_full_names[x], sep=""))})
#dev.off()
#
#pdf("importance_rankings.pdf")
#lapply(1:20, function(x) {plot_importance_rankings(importance_frame_all[[x]], main=paste("Importance ranks for  ", resp_full_names[x], sep=""))})
#dev.off()
#
#(vars <- important_variables(importance_frame, k = 10, measures = c("mean_min_depth", "no_of_trees")))
#
##interactions_frame <- min_depth_interactions(rf_yel_l8_tot, vars)
##save(interactions_frame, file = "interactions_frame_rf_yel_l8.rda")
#
##load("interactions_frame_rf_yel_l8.rda")
##head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])
#
#plot_min_depth_interactions(interactions_frame)


