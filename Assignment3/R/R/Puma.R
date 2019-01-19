## Creating 3D prints of species distributions using GBIF data
## Ben Raymond
## Last-Modified: <2015-02-27 14:12:25>

## change these paths to suit your needs
setwd("D://Erasmus//3eJaar//DV//Taak1/")
cache_directory="D://Erasmus//3eJaar//DV//Taak1//cache/"

## required packages
library(rgbif)
library(plyr)
library(R.cache)
library(ggplot2)
library(randomForest)
library(raster)
library(r2stl)
library(dismo)

## We use the `R.cache` package to cache our web calls to the GBIF API. This just means that, having run a certain query once, we can re-run it without needing an internet connection. Specify the cache directory and create a cached version of the `occ_search` function:
setCacheRootPath(cache_directory)             
cached_occ_search=addMemoization(occ_search)

## Specify the taxa we are interested in
parentKey=9703 ## Puma

this=cached_occ_search(taxonKey=parentKey,continent="north_america",limit=10000)$data

## quick check
md=map_data("world")
p=ggplot(data=subset(this,grepl("Puma (c)",name)),aes(x=decimalLongitude,y=decimalLatitude))+geom_point(aes(color=name))
p=p+geom_path(aes(x=long,y=lat,group=group),colour="black",data=md)+coord_map()+xlim(c(-175,-50))+ylim(c(0,65))

p

## Download the bioclimatic and altitude layers from http://www.worldclim.org/current
## http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio_5m_bil.zip
## and 
## http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_5m_bil.zip
##
## These layers are:
## BIO1 = Annual Mean Temperature
## BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
## BIO3 = Isothermality (BIO2/BIO7) (* 100)
## BIO4 = Temperature Seasonality (standard deviation *100)
## BIO5 = Max Temperature of Warmest Month
## BIO6 = Min Temperature of Coldest Month
## BIO7 = Temperature Annual Range (BIO5-BIO6)
## BIO8 = Mean Temperature of Wettest Quarter
## BIO9 = Mean Temperature of Driest Quarter
## BIO10 = Mean Temperature of Warmest Quarter
## BIO11 = Mean Temperature of Coldest Quarter
## BIO12 = Annual Precipitation
## BIO13 = Precipitation of Wettest Month
## BIO14 = Precipitation of Driest Month
## BIO15 = Precipitation Seasonality (Coefficient of Variation)
## BIO16 = Precipitation of Wettest Quarter
## BIO17 = Precipitation of Driest Quarter
## BIO18 = Precipitation of Warmest Quarter
## BIO19 = Precipitation of Coldest Quarter


## We somewhat-arbitraily select seven of these layers to use in the modelling

## adjust this path to wherever you put the data files
envfiles=file.path("D://Erasmus//3eJaar//DV//Taak1//data//worldclim//",c("alt.bil","bio1.bil","bio5.bil","bio6.bil","bio12.bil","bio16.bil","bio17.bil"))
envstack=stack(envfiles)

## extract environmental values at data locations
trainenv=extract(envstack,this[,c("decimalLongitude","decimalLatitude")])
trainenv=as.data.frame(trainenv)

## fit random forest classification models
trainenv$ov=as.factor(grepl("Puma concolor",this$name))
trainenv$oh=as.factor(grepl("Puma concolor",this$name))
fit_oh=randomForest(oh~alt+bio1+bio5+bio6+bio12+bio16+bio17,data=na.omit(trainenv),importance=TRUE)
fit_ov=randomForest(ov~alt+bio1+bio5+bio6+bio12+bio16+bio17,data=na.omit(trainenv),importance=TRUE)

## create grid for predictions
gridlon=seq(-175,-50,by=0.5)
gridlat=seq(10,80,by=0.5)
grid_data=expand.grid(gridlon,gridlat)
names(grid_data)=c("lon","lat")
## extract environmental covariates across grid
grid_data=cbind(grid_data,extract(envstack,grid_data))

## use models to predict over this region
grid_data$oh=predict(fit_oh,newdata=grid_data,type="prob",norm.votes=TRUE)[,2]
grid_data$ov=predict(fit_ov,newdata=grid_data,type="prob",norm.votes=TRUE)[,2]

# some manual adjustments of the results, mainly to remove some predicted areas that have resulted from extrapolations beyond our training data range
tempidx=grid_data$lon>-85 & grid_data$lat>35## & !is.na(grid_data$oh)
grid_data$oh[tempidx]=grid_data$oh[tempidx]*(1-(grid_data$lon[tempidx] + 85)/5)
grid_data$oh[grid_data$oh<0]=0
grid_data$oh[grid_data$lat>65 & !is.na(grid_data$oh)]=0
grid_data$oh[grid_data$lon< -140 & grid_data$lat<30 & !is.na(grid_data$oh)]=0
grid_data$oh[grid_data$lon>-79.5 & grid_data$lon< -64.6737 & grid_data$lat>13.5996 & grid_data$lat<28 & !is.na(grid_data$oh)]=0
grid_data$oh[grid_data$lon>-63.6737 & grid_data$lon< -59.8794 & grid_data$lat>12.0571 & grid_data$lat<18.7044 & !is.na(grid_data$oh)]=0
grid_data$oh[grid_data$lon>-85.0864 & grid_data$lon< -78.7038 & grid_data$lat>20.2836 & grid_data$lat<24.3601 & !is.na(grid_data$oh)]=0
grid_data$ov[grid_data$lon< -140 & grid_data$lat<30 & !is.na(grid_data$ov)]=0
grid_data$ov[grid_data$lon>-79.5 & grid_data$lon< -64.6737 & grid_data$lat>13.5996 & grid_data$lat<28 & !is.na(grid_data$ov)]=0
grid_data$ov[grid_data$lon>-63.6737 & grid_data$lon< -59.8794 & grid_data$lat>12.0571 & grid_data$lat<18.7044 & !is.na(grid_data$ov)]=0
grid_data$ov[grid_data$lon>-85.0864 & grid_data$lon< -78.7038 & grid_data$lat>20.2836 & grid_data$lat<24.3601 & !is.na(grid_data$ov)]=0
## check
p=ggplot(data=grid_data,aes(x=lon,y=lat))+geom_tile(aes(fill=oh))
p=p+geom_path(aes(x=long,y=lat,group=group),colour="black",data=md)+xlim(c(-175,-50))+ylim(c(0,65))##+coord_map()+
p=p+scale_fill_gradientn(limit=c(0,1),na.value="transparent",colours=c("#5E4EA1","#3287BD","#66C1A5","#ABDDA4","#E5F498","#FFFFBF","#FEDF8B","#FDAD60","#F36C43","#D43E4E","#9E0041"))
p

## create STL files for 3D printing
offset_height=0.1 ## step height of landmasses above baseplate, and of zero-probability above landmasses
## remember that the distributions themselves range from 0-1, so 0.1 is 1/10th of the full height of that

temp=matrix(grid_data$ov,nrow=length(gridlon))+offset_height
temp[is.na(temp)]=0
temp=temp+offset_height
r2stl(gridlon,gridlat,temp,filename="Puma_concolor.stl")

temp=matrix(grid_data$oh,nrow=length(gridlon))+offset_height
temp[is.na(temp)]=0
temp=temp+offset_height
r2stl(gridlon,gridlat,temp,filename="Puma_concolor2.stl")


