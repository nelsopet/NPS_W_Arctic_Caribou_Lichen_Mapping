##import final dataset##
library(party)
setwd("C:/Users/Emily.Holt/Desktop/Height")
height<-read.csv("HeightFinal_16Nov18.csv", na.strings = "")
height.complete<-na.omit(height)
height.complete$grazing=as.factor(height.complete$grazing) 
height.complete$fire=as.factor(height.complete$fire)
str(height.complete)

##kitchen sink cRF model ##
height.crf<-cforest(AdjHeight~.,data=height.complete, controls=cforest_unbiased(ntree=5000))
height.crf

## calculation of r2 ##
height.pred<-predict(height.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-height.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-21
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2

## variable importance plot for cForest models ##
varimp(height.crf)
cforestImpPlot <- function(height.crf) {
  cforest_importance <<- v <- varimp(height.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(height.crf)

## veg predictors only CRF model##
heightVeg.crf<-cforest(AdjHeight~tree+forb+graminoid+bryophyte+lichen+soil+duff+rock+water+richness,data=height.complete, controls=cforest_unbiased(ntree=5000))
heightVeg.crf
heightV.pred<-predict(heightVeg.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-heightV.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-10
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2
varimp(heightVeg.crf)
cforestImpPlot <- function(heightVeg.crf) {
  cforest_importance <<- v <- varimp(heightVeg.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(heightVeg.crf)

## climate predictors only CRF model##
heightCli.crf<-cforest(AdjHeight~temperature+season+active.layer+permafrost+precipitation+radiation_May+radiation_Aug+VPD.May+VPD_Aug,data=height.complete, controls=cforest_unbiased(ntree=5000))
heightCli.crf
heightCli.pred<-predict(heightCli.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-heightCli.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-9
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2
varimp(heightCli.crf)
cforestImpPlot <- function(heightCli.crf) {
  cforest_importance <<- v <- varimp(heightCli.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(heightCli.crf)

## partial dependence plots in cforest ##
library(pdp)
partial(height.crf, pred.var = "bryophyte", plot=TRUE, rug=T)


####Below this point are analyses prior to 15 Sep 18 #######

##import full dataset##
library (randomForest)
library(party)
setwd("C:/Users/Emily.Holt/Desktop/Height")
height<-read.csv("HeightFull_17Jul17.csv", na.strings = "")
height.complete<-na.omit(height)
str(height.complete)

## import dataset with no proxies (slope, aspect, lat/long)###
library (randomForest)
library(party)
setwd("C:/Users/Emily.Holt/Desktop/Height")
heightNP<-read.csv("Height_noProxies_17Jul17.csv", na.strings = "")
heightNP.complete<-na.omit(heightNP)
str(heightNP.complete)
summary(heightNP.complete, na.rm = T)

##import dataset with no proxies and no (sub)shrub ##
library (randomForest)
library(party)
setwd("C:/Users/Emily.Holt/Desktop/Height")
heightNP<-read.csv("Height_noShrub_20Jul17.csv", na.strings = "")
heightNP.complete<-na.omit(heightNP)
str(heightNP.complete)

##import dataset with no proxies, no (sub)shrub, with fire variable ##
library (randomForest)
library(party)
setwd("C:/Users/Emily.Holt/Desktop/Height")
height<-read.csv("Height_Fire_10Jan18.csv", na.strings = "")
height.complete<-na.omit(height)
height.complete$grazing=as.factor(height.complete$grazing) 
height.complete$fire=as.factor(height.complete$fire)
str(height.complete)

##kitchen sink cRF model ##
height.crf<-cforest(AdjHeight~.,data=height.complete, controls=cforest_unbiased(ntree=5000))
height.crf

## calculation of r2 ##
height.pred<-predict(height.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-height.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-19
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2

## variable importance plot for cForest models ##
varimp(height.crf)
cforestImpPlot <- function(height.crf) {
  cforest_importance <<- v <- varimp(height.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(height.crf)

## veg predictors only CRF model##
heightVeg.crf<-cforest(AdjHeight~tree+forb+graminoid+bryophyte+lichen+soil+duff+rock+water+richness,data=height.complete, controls=cforest_unbiased(ntree=5000))
heightVeg.crf
heightV.pred<-predict(heightVeg.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-heightV.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-10
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2
varimp(heightVeg.crf)
cforestImpPlot <- function(heightVeg.crf) {
  cforest_importance <<- v <- varimp(heightVeg.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(heightVeg.crf)

## climate predictors only CRF model##
heightCli.crf<-cforest(AdjHeight~temperature+season+active.layer+permafrost+precipitation+radiation+VPD,data=height.complete, controls=cforest_unbiased(ntree=5000))
heightCli.crf
heightCli.pred<-predict(heightCli.crf, type="response", OOB=TRUE)
residual<-height.complete$AdjHeight-heightCli.pred
mse<-sum(residual^2)/length(height.complete$AdjHeight)
pseudo.R2<-1-mse/var(height.complete$AdjHeight)
n<-7
adj.R2<-1-(1-pseudo.R2)*((length(height.complete$AdjHeight)-1)/(length(height.complete$AdjHeight)-n-1))
adj.R2
varimp(heightCli.crf)
cforestImpPlot <- function(heightCli.crf) {
  cforest_importance <<- v <- varimp(heightCli.crf)
  dotchart(v[order(v)])
}
cforestImpPlot(heightCli.crf)

## partial dependence plots in cforest ##
library(pdp)
partial(height.crf, pred.var = "radiation", plot=TRUE, rug=T)
partial(height.crf, pred.var = "tree", plot=TRUE, rug=T)
partial(height.crf, pred.var = "forb", plot=TRUE, rug=T)
partial(height.crf, pred.var = "graminoid", plot=TRUE, rug=T)
partial(height.crf, pred.var = "bryophyte", plot=TRUE, rug=T)
partial(height.crf, pred.var = "lichen", plot=TRUE, rug=T)
partial(height.crf, pred.var = "soil", plot=TRUE, rug=T)
partial(height.crf, pred.var = "duff", plot=TRUE, rug=T)
partial(height.crf, pred.var = "rock", plot=TRUE, rug=T)
partial(height.crf, pred.var = "water", plot=TRUE, rug=T)
partial(height.crf, pred.var = "richness", plot=TRUE, rug=T)
partial(height.crf, pred.var = "temperature", plot=TRUE, rug=T)
partial(height.crf, pred.var = "season", plot=TRUE, rug=T)
partial(height.crf, pred.var = "active.layer", plot=TRUE, rug=T)
partial(height.crf, pred.var = "permafrost", plot=TRUE, rug=T)
partial(height.crf, pred.var = "precipitation", plot=TRUE, rug=T)
partial(height.crf, pred.var = "VPD", plot=TRUE, rug=T)
partial(height.crf, pred.var = "fire", plot=TRUE, rug=T)
partial(height.crf, pred.var = "grazing", plot=TRUE, rug=T)


plot(height.complete$radiation, height.complete$AdjHeight, xlab="Radiation", ylab="Adjusted Height")


## kitchen sink RF model ##
heightNP.rf<-randomForest(AdjHeight~.,data=heightNP.complete, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
heightNP.rf
varImpPlot(heightNP.rf)
partialPlot(heightNP.rf,heightNP.complete,radiation)

## veg predictors only RF model##
heightVeg.rf<-randomForest(AdjHeight~tree+forb+graminoid+bryophyte+lichen+soil+duff+rock+water+richness,data=heightNP.complete, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
heightVeg.rf
varImpPlot(heightVeg.rf)

## climate predictors only RF model##
heightCli.rf<-randomForest(AdjHeight~temperature+season+active.layer+permafrost+precipitation+radiation+VPD,data=heightNP.complete, importance = TRUE, proximity = TRUE, bias.corr = TRUE)
heightCli.rf
varImpPlot(heightCli.rf)

## gg RF plots = var importance, min depth, matrix var dependence, matrix partial ##
library(ggRandomForests)
plot(gg_vimp(heightNP.rf))
varsel_height<-var.select(rfsrc(AdjHeight~., data = heightNP.complete))
gg_md_height<-gg_minimal_depth(varsel_height)
plot(gg_md_height)

gg_v_height<-gg_variable(heightNP.rf)
xvarHeight<-gg_md_height$topvars
plot(gg_v_height, xvar=xvarHeight, panel = T, alpha=.4)
plot(gg_v_height, xvar="water", alpha =.4)

partial_height<-plot.variable(rfsrc(AdjHeight~., data = heightNP.complete), xvar=gg_md_height$topvars, partial = T, sorted = F, show.plots = F)
gg_part_height<-gg_partial(partial_height)
plot(gg_part_height, panel = T, se=F)

##random forest bivariate partial plot
##run the below code for the surface plot of 2 var's in full (no substitutions needed!)##

bivarpartialPlot.randomForest <-
  function (x, pred.data, x1.var, x2.var, which.class, w,
            n1.pt = min(length(unique(pred.data[, x1name])), 51),
            n2.pt = min(length(unique(pred.data[, x2name])), 51),
            x1lab=deparse(substitute(x1.var)),
            x2lab=deparse(substitute(x2.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x1.var)),"and",deparse(substitute(x2.var))),
            ...)
  {
    classRF <- x$type != "regression"
    if (is.null(x$forest)) stop("The randomForest object must contain the forest.\\n")
    x1.var <- substitute(x1.var)
    x2.var <- substitute(x2.var)
    x1name <- if (is.character(x1.var)) x1.var else {
      if (is.name(x1.var)) deparse(x1.var) else {
        eval(x1.var)
      }
    }
    x2name <- if (is.character(x2.var)) x2.var else {
      if (is.name(x2.var)) deparse(x2.var) else {
        eval(x2.var)
      }
    }
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    # the first predictor variable
    xv1 <- pred.data[, x1name]
    x1.pt <- seq(min(xv1), max(xv1), length = n1.pt)
    # the second predictor variable
    xv2 <- pred.data[, x2name]
    x2.pt <- seq(min(xv2), max(xv2), length = n2.pt)
    # y is big!
    y.pt <- matrix(0, nrow=n1.pt, ncol=n2.pt)
    for (i in 1:n1.pt) {
      for (j in 1:n2.pt) {
        x.data <- pred.data
        x.data[, x1name] <- rep(x1.pt[i], n)
        x.data[, x2name] <- rep(x2.pt[j], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i,j] <- weighted.mean(log(ifelse(pr[, focus] == 0, 1, pr[, focus]))
                                     - rowMeans(log(ifelse(pr == 0, 1, pr))), w, na.rm=TRUE)
        } else {
          y.pt[i,j] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
    }
    # output is ready for persp
    persp(y.pt, xlab=x1lab, ylab=x2lab, zlab="",main=main,...)
  }

## once you have defined the bivariate plot function (with above code), use this code to create one.  it takes 5-10 mins, so be patient:
nump = 10
bpp.out = bivarpartialPlot.randomForest(heightNP.rf, heightNP.complete, lichen, shrub, ylab="rating", n1.pt=nump, n2.pt=nump, theta=320)
#change theta on this one, can't use factors

setwd("C:/Users/Emily.Holt/Desktop/Height")
Rawheight<-read.csv("RawHeights.csv", na.strings = "")
Rawheight2<-na.omit(Rawheight)
str(Rawheight2)
boxplot(height~Species, data=Rawheight2, ylab="Height (cm)", las=2)
