# Sets working dir (mac)
setwd("/Users/cskoven/Google Drive/PhD/DRCMR/Lab/LabNotes/PreClinLabs/SAI_data")
identifier <- "20170906"
suffix <- "_h"
setwd(identifier)

#READ -> txt-file
my_txt <- readLines(paste(identifier, suffix, ".txt", sep=""))

#make csv-readble
#my_csv_txt  <- gsub(pattern = ", ", replace = ";", x = mytxt)
my_csv_txt  <- gsub(pattern = ",", replace = ".", x = my_txt)

#remove nonsense-hdr (in this case, line 1,2 and 4)
#my_csv_txt_nonons <- my_csv_txt[-c(1,2,4)] 

#write content into csv_file
writeLines(my_csv_txt, con=paste(identifier, suffix, ".csv", sep=""))

#load csv-data into dataframe

mydata <- read.csv(paste(identifier, suffix, ".csv", sep=""), header = TRUE, sep=";")

mydata <- subset(mydata, select = c("Time", "Resp.Rate", "ECG.Rate", "Temp.1"))
colnames(mydata) <- c("Time", "Resp", "ECG", "Temp")

mydata$Time[1]

#figure out what happens here: https://stackoverflow.com/questions/30359427/calculate-the-mean-of-every-13-rows-in-data-frame-in-r
n <- 60
mydata <- aggregate(mydata,list(rep(1:(nrow(mydata)%/%n+1),each=n,len=nrow(mydata))),mean)[-1];

mydata_hr <- mydata
mydata_hr$Time <- mydata$Time/(60*60)

library(ggplot2)
library(reshape2)


# filter for meaningful temps
mydata_hr_filtered <- mydata_hr

#mydata_hr_filtered$Resp[mydata_hr$Resp<0 | mydata_hr$Resp>150] <- NA
#mydata_hr_filtered$ECG[mydata_hr$ECG<50 | mydata_hr$ECG>450] <- NA
#mydata_hr_filtered$Temp[mydata_hr$Temp<30 | mydata_hr$Temp>45] <- NA


mydata_hr_filtered_long <- melt(mydata_hr_filtered, id.vars=c("Time"))
#str(mydata_hr_long)

#mydata_hr_temp_long$value[mydata_hr_temp_long$variable=="Temp"]

# 
# Temp_plot <- ggplot(mydata_hr, aes(x=Time, y=Temp)) +
#   #geom_point(size=3) +
#   geom_line() + theme_bw() +
#   
#   coord_cartesian(ylim = c(0, ceiling(max(mydata_hr$Temp)/10))*10) + scale_y_continuous(breaks=seq(0,50,5)) +  scale_x_continuous(breaks=seq(0,50,1)) +
#   xlab("") + ylab("Temp")
# Temp_plot <- Temp_plot + theme(legend.position="right", legend.box="vertical", legend.direction="vertical", axis.text = element_text(size=16), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=16), legend.title=element_text(size=16, face="bold"))
# Temp_plot
# 
# plotmatrix(mydata)

#?colSums
#?aggregate
#cut(mydata$Time, breaks="5")
#?as.POSIXct
#start_date <- "09/05/17"
#start_time <- "08:14:25"
#as.POSIXct(mydata$Time, origin = strptime(paste(start_date, start_time), "%m/%d/%y %H:%M:%S")) 
#as.POSIXct(mydata$Time,)

#install.packages("xts")
#library(xts)

#isoflurane <- ()

#rect_center <- c(0.5,1.5,1.9,3.7)
#events <- c()
#rectangles <- data.frame(
 # xmin = rect_center - 0.1,
#  xmax = rect_center + 0.1,
#  ymin = 0,
#  ymax = Inf
#)

plot <- ggplot(mydata_hr_filtered_long, aes(x=Time, y=value)) + facet_grid(variable ~ ., scales="free_y") +
  scale_x_continuous(breaks=seq(0,50,1)) +
  geom_line() + theme_bw() + ylab("") + 
  ggtitle(paste(identifier)) +
  #geom_rect(data=rectangles, inherit.aes=FALSE, aes(xmin=xmin, xmax=xmax, 
  #                                 ymin=ymin, ymax=ymax, 
  #                               alpha=0.1,fill="red"))+
  theme(plot.title=element_text(size=16, face="bold"), legend.position="none", legend.box="horizontal", legend.direction="horizontal", axis.text = element_text(size=16), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=16), legend.title=element_text(size=16, face="bold"))
plot


