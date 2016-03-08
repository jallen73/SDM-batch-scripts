library(spThin)
library(ENMeval)
library(dismo)
library(rgdal)

###Separate one large csv with many species into a separate csv for each species
###Input CSV file with all species and data in three columns: species, LAT, LONG
points<-read.csv("E:/MACP_modeling/MACP_speciesover20.csv")

for (name in levels (points$SPEC)){
  tmp=subset(points,SPEC==name)
  fn=paste('E:/MACP_modeling/BySpecies/',gsub(' ','',name),'.csv',sep='')
  write.csv(tmp,fn,row.names=FALSE)
}


###Use spThin to thin each csv and write the data to a new folder 
###make sure to set the working directory to the folder with the files to be thinned
files <- list.files(path="E:/MACP_modeling/BySpecies", pattern="*.csv", full.names=FALSE, recursive=FALSE)
setwd("E:/MACP_modeling/BySpecies")

for (i in 1:length(files)){
  points<-read.csv(files[i],header=TRUE)
  thin(points, lat.col = "LAT", long.col = "LONG", spec.col = "SPEC",
     10, 1000, locs.thinned.list.return = TRUE, write.files = TRUE,
     max.files = 1,  out.dir="E:/MACP_modeling/Thin", out.base = paste(files[i]),
     write.log.file = TRUE, log.file = "spatial_thin_log.txt",
     verbose = TRUE)}


mymergeddata <- 
  do.call(rbind,
          lapply(list.files("E:/MACP_SLR/Thin"),read.csv))

write.csv(mymergeddata, file="allspecies.csv")



###RUNNING ENMEVAL
###LOOPING

##set working directory to folder with environmnetal variables as raster files
setwd("E:/MACP_SLR/EnvVars")
files <- list.files(pattern='\\.asc$')
env <- stack(files)

##set working directory to folder with seperate csv file for each species
setwd("E:/MACP_SLR/Test2")
files <- list.files(path="E:/MACP_SLR/Test2", pattern="*.csv", full.names=FALSE, recursive=FALSE)

##Change the enmeval settings as appropriate
for (i in 1:length(files)){
  points<-read.csv(files[i], header=TRUE)
  DataPoints <- points[,-1]
  enmeval_test_results <- ENMevaluate(occ=DataPoints, env=env, RMvalues = seq(0.5, 1, 0.5),
                                    fc = c("L"),
                                    categoricals = NULL, n.bg = 10000, method = "block",
                                    overlap = FALSE, kfolds = NA, bin.output = TRUE, clamp = TRUE)

  write.table(enmeval_test_results@results, file=(paste(files[i],"-EVAL.txt",sep="")))}

  
###ONE SPECIES TEST ### 
#Points
points<-read.csv(file.choose())
DataPoints <- points[,-1]

#Environmental Variables
setwd("D:/Fissurina_Alligatorensis")
files <- list.files(pattern='\\.asc$')
env <- stack(files)

setwd("E:/MACP_SLR/ENMeval_1")

enmeval_test_results <- ENMevaluate(DataPoints, env, RMvalues = seq(1, 2, 0.5),
                                    fc = c("L"),
                                    categoricals = NULL, n.bg = 10000, method = "block",
                                    overlap = FALSE, kfolds = NA, bin.output = TRUE, clamp = TRUE)

best <- which(enmeval_test_results@results$delta.AICc == 0)
best.mod <- enmeval_test_results@results[best,]
plot(enmeval_test_results@predictions[[best]])
writeRaster(enmeval_test_results@predictions[[best]], "best_model.asc")


write.table(enmeval_test_results@results, file=(paste(points[1,-1])))


)ENMevaluate(occ, env, bg.coords = NULL, occ.grp = NULL, 
            bg.grp = NULL, RMvalues = seq(0.5, 4, 0.5), 
            fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
            categoricals = NULL, n.bg = 10000, method = NULL, 
            overlap = FALSE, aggregation.factor = c(2, 2), 
            kfolds = NA, bin.output = FALSE, clamp = TRUE)


