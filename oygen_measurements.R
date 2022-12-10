library(data.table)
library(ggplot2)

#Take in the data
O2_recalibrated <- read.delim("C:/Users/c7701233/Nextcloud/Column-Experiment/oxygen_measurements/Experiment_DO_Dataprep/data/presense/SELIN_O2_recalibrated.txt", header=T)
data = setDT(O2_recalibrated[,c("Date", "Time", "Value", "Temp", "Column_no","Sample_Name", "New_Calibration")])

#Set the classes, this is important for the dates as well as the factors (especially in plotting facets this would come handy)
data[,"Date"] = as.Date(data$Date , format= "%m/%d/%Y")
data[,"Time"]= as.ITime(data$Time)
data[, c("Sample_Name", "Column_no")]=lapply (data[, c("Sample_Name", "Column_no")], as.factor)
levels(data$Sample_Name) = list(S00="S00", S02="S02", S4.5="S4.5", S06="S06", S6.5="S6.5",  S6.7="S6.7", S08= "S08", 
                                   S11="S11", S13="S13", S13.5="S13.5", S15.5="S15.5", S15.7="S15.7", S17.5="S17.5", S18.5="S18.5", S19="S19")
data[, c("year", "month", "day") := tstrsplit(Date, "-")]

#Calculate the median or the mean
data_sum=data[,.(Oxygen=median(Value), Temp=median(Temp)), by=c("Column_no",  "day", "month", "Date", "Sample_Name")]
data_sum[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_excluded=data_sum[!(Sample_Name == "S06"| Sample_Name=="S6.5"| Sample_Name == "S15.7")]
data_excluded$replicate= factor(data_excluded$replicate, levels = c("F", "C", "B"))

ggplot(data_excluded)+
  geom_line(aes(x=Column_Number, y=Oxygen, group=replicate, col=replicate), lwd=2)+
  geom_point(aes(x=Column_Number, y=Oxygen, group=replicate), size=2)+
  facet_grid(~Sample_Name)+
  theme_bw()+ scale_color_manual(values=c("yellow1", "magenta", "green1"))
