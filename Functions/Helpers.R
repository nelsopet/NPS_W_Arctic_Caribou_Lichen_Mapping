#Functions
read_csv_batch<-function(path) 
{
  files <- list.files(path=path, pattern="*.csv")
  for(file in files)
  {
    perpos <- which(strsplit(file, "")[[1]]==".")
    assign(
      gsub(" ","",substr(file, 1, perpos-1)), 
      read.csv(paste(path,file,sep="")))
    print(file)
  }
}

install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
  
  #I know I should be using purr here, but this is before the Tidyverse is loaded. I know you Tidyverse trend setters will have me here.
  
}

Make_Inputs<- function(lich) 
{ ## Function lists of GEE output objects
  fnc1<- function(x) {paste("pred_rf_",x,"_20200312_74pred_oob_500trees_resamp", sep="")}
  #fnc1(resp_names)
  #fnc1<- function(x) {paste("pred_rf_",x,"_20200412_74pred_oob_500trees", sep="")}
  ## Apply function that creates the list of objects
  lich_data<- unlist(lapply(lich, fnc1))
  #length(unlist(lapply(resp_names, fnc1))) #21 elements
  #return(lich_data)
  ## Function lists predicted response variable names
  fnc2<-function(y) {paste(y,"_pred", sep="")}
  ## Apply function to make list of predicted response variables
  lich_pred <- unlist(lapply(lich, fnc2))
  #length(unlist(lapply(resp_names, fnc2))) #21 elements
  lich_resp<-lich
  ## Make formulas to compare observed vs predicted vaoues
  lich_mods<- paste(lich,lich_pred, sep="~")
  #length(paste(resp_names,unlist(lapply(resp_names, fnc2)), sep="~")) #21 elements
  ## Turn strings into formula object
  lich_form<-lapply(lich_mods, as.formula)
  #lapply(paste(resp_names,unlist(lapply(resp_names, fnc2)), sep="~"), as.formula) #21 elements
  ## Lichen hexbin for obs vs pred
  ##This pastes the equation for hexbin          
  #fnc3<- function(z) {paste("pred_rf_", z, "_20200412_74pred_oob_500trees$", z, "~pred_rf_", z, "_20200412_74pred_oob_500trees$", z, "_pred", sep="")} 
  #hex_form<-lapply(lich, fnc3) %>% unlist()
  ## This makes the hexbin plots for each group
  #fnc4<-function(z) {hexbin(eval(parse(text =hex_form[z])))}#, xbins=10)}
  #hex_lich<-lapply(1:length(resp_names), fnc4)
  ## Combine formulas, observed and predicted names and data set namtes
  fnc5<-function(a) {paste(resp_names[a],lich_pred[a], sep=",")}
  lich_aes<-lapply(1:length(lich), fnc5)
  lich_out<-cbind(lich, lich_pred, lich_mods, lich_data, lich_form, lich_aes) %>% as.data.frame()#, lich_form) %>% as.data.frame()
  ##Save output for plotting
  return(lich_out)
}

