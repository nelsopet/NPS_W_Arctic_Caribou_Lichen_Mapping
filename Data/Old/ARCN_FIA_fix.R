
#Read in data
GAAR_ENV<-read.csv("/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/GAAR_Plots.csv", header=T, sep=",", stringsAsFactors = F)
XY_ENV<-read.csv("/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/XYEnvironmental_Data.csv", sep=",", header=T, stringsAsFactors = F)
WAH_ENV<-read.table("/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/ENV_ARCN_Apr5_2017.txt", header=T, sep=",", stringsAsFactors = F)

#Find plots not in list of ARCN FIA plot used in the lichen depth analysis
WAH_ENV %>% select(plot) %>% anti_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% anti_join(XY_ENV, by="plot") %>% View()

#Change the names of the plots to match those from GAAR and the rest of ARCN
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="6L "       ,"6L",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JPALISADE1","Jpalisad",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="DUNE2"     ,"Dune2",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="DUNES1"    ,"Dunes1",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JLICHEN1"  ,"JLichen1",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JLICHEN2"  ,"JLichen2",WAH_ENV$plot);
WAH_ENV$plot<-ifelse(WAH_ENV$plot=="JMIXED1"   ,"JMixed1",WAH_ENV$plot);

#Select the plots and needed columns from both the Gates and other ARCN parks

tmp1<-WAH_ENV %>% 
  select(plot) %>% 
  inner_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% 
  mutate(samp_date = as.Date(GPStrmDate, "%m/%d/%y")) %>%
  select(plot, GPScorDD84, GPScorDD_1, samp_date) %>%
  rename(lat = GPScorDD_1, long = GPScorDD84)

tmp2<-WAH_ENV %>% 
  select(plot) %>% 
  inner_join(XY_ENV, by="plot") %>% 
  mutate(samp_date = as.Date(GPS_Date, "%m/%d/%y")) %>%
  select(plot, lat, long, samp_date)
  

#Merge Gates and other ARCN parks
tmp3<-rbind(tmp1,tmp2)

#Add source of data as per the shapefile template
tmp3$src_data = "ARCN FIA plot"

#Change the names of the plots to match those in curated plant cover data
tmp3$plot<-ifelse(tmp3$plot=="Jpalisad","JPalisad",tmp3$plot);
tmp3$plot<-ifelse(tmp3$plot=="Lowerc88","Lowrc88",tmp3$plot);

write.csv(tmp3, "/Users/peternelson 1/Documents/UMFK/Grants/NPS WAH/Data/2018/ARCN_FIA_cover_19092019.csv")

rm(tmp1,tmp2,tmp3)



