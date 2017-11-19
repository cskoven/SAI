# Sets working dir (mac)
setwd("/Users/cskoven/Google Drive/PhD/DRCMR/Lab/LabNotes/PreClinLabs/SAI_data")
identifier <- "20170907"
lab_date <- "2017-09-07"
suffix <- "_hhmmss"
setwd(identifier)

#READ -> txt-file
my_txt <- readLines(paste(identifier, suffix, ".txt", sep=""))
#header <- readLines(paste(identifier, suffix, ".txt", sep=""), n=1)
#headerdata <- read.csv(text=header, header = FALSE, sep=";")
#headerdata_h <- as.matrix(headerdata[,seq(1,ncol(headerdata), by=2)])
#headerdata_d <- as.matrix(headerdata[,seq(2,ncol(headerdata), by=2)])
#colnames(headerdata_d) <- headerdata_h
#headerdata_d <- as.data.frame(headerdata_d)

#make csv-readble
#my_csv_txt  <- gsub(pattern = ", ", replace = ";", x = mytxt)
my_csv_txt  <- gsub(pattern = ",", replace = ".", x = my_txt)

#remove nonsense-hdr (in this case, line 1,2 and 4)
#my_csv_txt_nonons <- my_csv_txt[-c(1,2,4)] 

#write content into csv_file
#writeLines(my_csv_txt_nonons, con=paste(identifier, suffix, ".csv", sep=""))
writeLines(my_csv_txt, con=paste(identifier, suffix, ".csv", sep=""))

#load csv-data into dataframe

mydata <- read.csv(paste(identifier, suffix, ".csv", sep=""), header = TRUE, sep=";")
anaest_iso <- read.csv(paste(identifier, "_iso", ".csv", sep=""), header = TRUE, sep=";")
anaest_dex <- read.csv(paste(identifier, "_dex", ".csv", sep=""), header = TRUE, sep=";")

mydata <- subset(mydata, select = c("Time", "Resp.Rate", "ECG.Rate", "Temp.1"))
colnames(mydata) <- c("Time", "Resp", "ECG", "Temp")

#mydata$Time <- as.POSIXct(paste(headerdata_d$Date, mydata$Time[1], sep=" "), format="%m/%d/%Y %H:%M:%S", tz="Europe/Copenhagen")
mydata$Time <- as.POSIXct(paste(lab_date, mydata$Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="Europe/Copenhagen")
anaest_iso$Time <- as.POSIXct(paste(lab_date, anaest_iso$Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="Europe/Copenhagen")
anaest_dex$Time <- as.POSIXct(paste(lab_date, anaest_dex$Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="Europe/Copenhagen")

library(ggplot2)
library(reshape2)

# filter for meaningful temps
mydata_filtered <- mydata

mydata_filtered$Resp[mydata$Resp<1 | mydata$Resp>150] <- NA
mydata_filtered$ECG[mydata$ECG<50 | mydata$ECG>450] <- NA
mydata_filtered$Temp[mydata$Temp<35 | mydata$Temp>45] <- NA

# library(xts)
# xts.Resp <- xts(mydata_filtered$Resp, mydata_filtered$Time)
# xts.ECG <- xts(mydata_filtered$ECG, mydata_filtered$Time)
# xts.Temp <- xts(mydata_filtered$Temp, mydata_filtered$Time)
# 
# ends.Resp <- endpoints(xts.Temp,'mins',1)
# ends.ECG <- endpoints(xts.ECG,'mins',1)
# ends.Temp <- endpoints(xts.Temp,'mins',1)
# xts.Resp.mean <- period.apply(xts.Resp,ends,mean)

library("dplyr")
mean.Resp <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(Resp = mean(Resp))

mean.ECG <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(ECG = mean(ECG))

mean.Temp <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(Temp = mean(Temp))

meandf_filtered <- cbind (mean.Resp, mean.ECG$ECG, mean.Temp$Temp)

meandf_filtered_long <- melt(meandf_filtered, id.vars=c("Time"))
anaest_iso_long <- melt(anaest_iso, id.vars=c("Time"))
anaest_dex_long <- melt(anaest_dex, id.vars=c("Time"))
#str(mydata_hr_long)

#str(mydata_hr_filtered_long)
alldata_verylong <- rbind(meandf_filtered_long, anaest_iso_long, anaest_dex_long)

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

#rect_center <- c(0.5,1.5,1.9,3.7)
#events <- c()
#rectangles <- data.frame(
 # xmin = rect_center - 0.1,
#  xmax = rect_center + 0.1,
#  ymin = 0,
#  ymax = Inf
#)

plot <- ggplot(alldata_verylong, aes(x=Time, y=value)) + facet_grid(variable ~ ., scales="free_y") +
  #scale_x_continuous(breaks=seq(0,50,1)) +
  geom_line() + theme_bw() + ylab("") + 
  ggtitle(paste(identifier)) +
  #geom_rect(data=rectangles, inherit.aes=FALSE, aes(xmin=xmin, xmax=xmax, 
  #                                 ymin=ymin, ymax=ymax, 
  #                               alpha=0.1,fill="red"))+
  theme(plot.title=element_text(size=16, face="bold"), legend.position="none", legend.box="horizontal", legend.direction="horizontal", axis.text = element_text(size=16), axis.title=element_text(size=16, face="bold"), legend.text=element_text(size=16), legend.title=element_text(size=16, face="bold"))
plot


