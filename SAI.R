# Sets working dir (mac)
setwd("/Users/cskoven/Google Drive/PhD/DRCMR/Lab/LabNotes/PreClinLabs/SAI_data")
identifier <- "20171025"
lab_date <- "2017-10-25"
suffix <- "" #"_hhmmss"
setwd(identifier)

#READ -> txt-file
my_txt <- readLines(paste(identifier, suffix, ".txt", sep=""))
header <- my_txt[1] #readLines(paste(identifier, suffix, ".txt", sep=""), n=1)
my_txt <- my_txt[-1]
#remove nonsense-hdr (in this case, line 1,2 and 4)
#my_csv_txt_nonons <- my_csv_txt[-c(1,2,4)] 

#make csv-readble
#my_csv_txt  <- gsub(pattern = ", ", replace = ";", x = mytxt)
my_txt  <- gsub(pattern = "-1", replace = "", x = my_txt)
my_csv_txt  <- gsub(pattern = ",", replace = ".", x = my_txt)

#write content into csv_file
#writeLines(my_csv_txt_nonons, con=paste(identifier, suffix, ".csv", sep=""))
writeLines(my_csv_txt, con=paste(identifier, suffix, ".csv", sep=""))

#load csv-data into dataframe

mydata <- read.csv(paste(identifier, suffix, ".csv", sep=""), header = TRUE, sep=";")
anaest_iso <- read.csv(paste(identifier, "_iso", ".csv", sep=""), header = TRUE, sep=";")
anaest_dex <- read.csv(paste(identifier, "_dex", ".csv", sep=""), header = TRUE, sep=";")
events <- read.csv(paste(identifier, "_events", ".csv", sep=""), header = TRUE, sep=";")

mydata <- subset(mydata, select = c("Time", "Resp.Rate", "ECG.Rate", "Temp.1"))
colnames(mydata) <- c("Time", "Resp", "ECG", "Temp")

#mydata$Time <- as.POSIXct(paste(headerdata_d$Date, mydata$Time[1], sep=" "), format="%m/%d/%Y %H:%M:%S", tz="Europe/Copenhagen")
mydata$Time <- as.POSIXct(paste(lab_date, mydata$Time, sep=" "),
                          format="%Y-%m-%d %H:%M:%S",
                          tz="Europe/Copenhagen")
anaest_iso$Time <- as.POSIXct(paste(lab_date, anaest_iso$Time, sep=" "),
                              format="%Y-%m-%d %H:%M:%S",
                              tz="Europe/Copenhagen")
anaest_dex$Time <- as.POSIXct(paste(lab_date, anaest_dex$Time, sep=" "),
                              format="%Y-%m-%d %H:%M:%S",
                              tz="Europe/Copenhagen")
events$Time <- as.POSIXct(paste(lab_date, events$Time, sep=" "),
                          format="%Y-%m-%d %H:%M:%S",
                          tz="Europe/Copenhagen")

library(ggplot2)
library(reshape2)

# filter for meaningful temps
mydata_filtered <- mydata

mydata_filtered$Resp[mydata$Resp<1 | mydata$Resp>150] <- NA
mydata_filtered$ECG[mydata$ECG<50 | mydata$ECG>450] <- NA
mydata_filtered$Temp[mydata$Temp<35 | mydata$Temp>45] <- NA

library("dplyr")
mean.Resp <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(Resp = mean(Resp, na.rm=TRUE))

mean.ECG <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(ECG = mean(ECG, na.rm=TRUE))

mean.Temp <- mydata_filtered %>%
  group_by(Time = cut(Time, breaks="1 min")) %>%
  summarize(Temp = mean(Temp, na.rm=TRUE))

meandf_filtered <- cbind (mean.Resp, mean.ECG$ECG, mean.Temp$Temp)
colnames(meandf_filtered) <- c("Time", "Resp", "ECG", "Temp")

meandf_filtered_long <- melt(meandf_filtered, id.vars=c("Time"))
anaest_iso_long <- melt(anaest_iso, id.vars=c("Time"))
anaest_dex_long <- melt(anaest_dex, id.vars=c("Time"))

meandf_filtered_long$Time <- as.POSIXct(meandf_filtered_long$Time, format="%Y-%m-%d %H:%M:%S", tz="Europe/Copenhagen")

alldata_verylong <- rbind(meandf_filtered_long, anaest_iso_long, anaest_dex_long)

r <- as.POSIXct(round(range(alldata_verylong$Time), "hours"))

### plot
plot <- ggplot(alldata_verylong, aes(x=Time, y=value)) + facet_grid(variable ~ ., scales="free_y") +
  geom_line() + theme_bw() + ylab("") + 
  ggtitle(paste(lab_date)) +
  geom_vline(data=events, aes(xintercept=as.numeric(as.POSIXct(Time))), colour="red", size=.2) +
  labs(caption=paste(apply(cbind(format(events$Time, "%H:%M"), paste(events$Event)), 1, paste, collapse =": "), collapse="\n"))+
  #axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H") + 
  theme(
    legend.text=element_text(size=16),
    plot.title=element_text(size=16, face="bold"),
    legend.position="none", legend.box="horizontal",
    legend.direction="horizontal",
    axis.text = element_text(size=16),
    axis.title=element_text(size=16, face="bold"),
    legend.title=element_text(size=16, face="bold"),
    plot.caption=element_text(hjust=0.0, vjust=0.5)
  )
plot
