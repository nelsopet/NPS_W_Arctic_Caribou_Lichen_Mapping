library(tidyverse)
# library(caret)
# library(randomForest)
# library(hexbin)
# library(pdp)
# #library(randomForestExplainer)
# library(ranger)
library(sf)
# library(viridis)
# library(Metrics)

#Read in the old data

#79 rows of env data with Tall, Midtall, Low and Dwarf shrubs
GAAR_ENV<-read_csv("Data/Old/GAAR_Plots.csv")

#446 rows of env data with shrub and subshrub
XY_ENV<-read_csv("Data/Old/XYEnvironmental_Data.csv")

#435 rows but no shrubs
WAH_ENV<-read.table("Data/Old/ENV_ARCN_Apr5_2017.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)

#402 rows of env data but no shrubs. These are the data Emily used in the final manuscript on lichen depth 
Full_Comm<-read_csv("Data/full_community_14Jul20.csv")

Env<-read.table("Data/Old/ENV_ARCN_June4_2019.txt",sep=",", header=TRUE, stringsAsFactors = F) 

#402 rows of env data but no shrubs. 
Env<- Env %>% dplyr::select(plot, colnames(Full_Comm[,2:11]))

#Find plots not in list of ARCN FIA plot used in the lichen depth analysis
#WAH_ENV %>% dplyr::select(plot) %>% anti_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% anti_join(XY_ENV, by="plot") %>% View()
Env %>%  # anti_join(XY_ENV, by="plot") 
  dplyr::select(plot) %>% 
  left_join(GAAR_ENV, by=c("plot"="Veg_Plot")) %>% 
  left_join(XY_ENV, by="plot") %>% 
  colnames()

Env$plot<-ifelse(Env$plot=="6L "       ,"6L",Env$plot);
Env$plot<-ifelse(Env$plot=="JPALISADE1","Jpalisad",Env$plot);
Env$plot<-ifelse(Env$plot=="DUNE2"     ,"Dune2",Env$plot);
Env$plot<-ifelse(Env$plot=="DUNES1"    ,"Dunes1",Env$plot);
Env$plot<-ifelse(Env$plot=="JLICHEN1"  ,"JLichen1",Env$plot);
Env$plot<-ifelse(Env$plot=="JLICHEN2"  ,"JLichen2",Env$plot);
Env$plot<-ifelse(Env$plot=="JMIXED1"   ,"JMixed1",Env$plot);

XY_ENV$plot<-ifelse(XY_ENV$plot=="6L "       ,"6L",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="JPALISADE1","Jpalisad",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="DUNE2"     ,"Dune2",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="DUNES1"    ,"Dunes1",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="JLICHEN1"  ,"JLichen1",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="JLICHEN2"  ,"JLichen2",XY_ENV$plot);
XY_ENV$plot<-ifelse(XY_ENV$plot=="JMIXED1"   ,"JMixed1",XY_ENV$plot);

##Add shrubs

Env_p1<-GAAR_ENV %>%
  select(Veg_Plot,Tree:Water) %>% 
  # mutate(shrub = Tall_Shr+Mid_Tall+Low_Shru, 
  #        subshrub = Dwarf_Sh) %>% 
  # select(-Tall_Shr:-Dwarf_Sh) %>% 
  rename(plot = Veg_Plot, tree = Tree, gram = Graminoi, forb = Forbs, bryo = Bryophyt, lichen = Lichens, soil = Soil, duff = Duff, rock = Rock, water = Water, rock = Rock) %>% 
  select(plot, sort(colnames(.))) %>%
  pivot_longer(bryo:water, names_to="pft", values_to="cover") %>%
  mutate(shrub_method = "GAAR")
  
Env_p2<-  XY_ENV %>% 
  select(plot,tree:water) %>% #View()
  select(plot, sort(colnames(.))) %>%
  pivot_longer(bryo:water, names_to="pft", values_to="cover") %>%
  mutate(shrub_method = "non GAAR")

Env_full_long <- bind_rows(Env_p1, Env_p2) %>%
  semi_join(Env, by="plot")

pfts <- Env_full_long %>%
  group_by(pft, shrub_method) %>%
  summarize(n = n(),
            meanCover = mean(cover))

coverByPlot <- Env_full_long %>%
  group_by(plot) %>%
  summarize(total_cover = sum(cover))

write_csv(pfts, 'Data/pfts_to_xwalk.csv')

# Env_full<- Env %>% select(plot) %>% inner_join(Env_full)
# 
# write_csv(Env_full,"./Output/env_data_for_lichen_volume_cover_by_color_group_NAD83.csv")
