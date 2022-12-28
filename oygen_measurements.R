library(data.table)
library(ggplot2)

#Take in the data
O2_recalibrated <- read.delim("data/presense/SELIN_O2_recalibrated.txt", header=T, , fileEncoding="latin1")
data = setDT(O2_recalibrated[,c("Date", "Time", "Value", "Temp", "Pressure","Column_no","Sample_Name", "New_Calibration")])

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
data_sum$replicate= factor(data_sum$replicate, levels = c("F", "C", "B")) #To change th order of plotting so they are not on top of each other in a bad way

ggplot(data_sum)+
  geom_line(aes(x=Column_Number, y=Oxygen, group=replicate, col=replicate), lwd=2)+
  geom_point(aes(x=Column_Number, y=Oxygen, group=replicate), size=2)+
  geom_hline(yintercept = c(10.6, 11.1), color="red", linetype="dashed")+
  facet_grid(~Sample_Name)+
  theme_bw()+ scale_color_manual(values=c("yellow1", "magenta", "green1"))

# Sample plot with the new calculated F measurements from S11 until S19. 
#the calculation is done using the Presens software but can also be manually calculated provided that the calibration equation is known.
# In this case only the C2 values of the F replicate should show different results 
#(all the F replicate values are recalcualted but C1 and C3 should be quite similar to original values since these had the correct calibration when measuring)
# To exclude a holw set of sampling date use data_sum[!(Sample_Name == "S06"| Sample_Name=="S6.5"| Sample_Name == "S15.7")]

data_sum=data[,.(Oxygen=median(New_Calibration), Temp=median(Temp), Pressure=median(Pressure)), by=c("Column_no",  "day", "month", "Date", "Sample_Name")]
data_sum[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_sum$replicate= factor(data_sum$replicate, levels = c("F", "C", "B"))

ggplot(data_sum)+
  geom_line(aes(x=Column_Number, y=Oxygen, group=replicate, col=replicate), lwd=2)+
  geom_point(aes(x=Column_Number, y=Oxygen, group=replicate), size=2)+
  geom_hline(yintercept = c(10.6, 11.1), color="red", linetype="dashed")+
  facet_grid(~Sample_Name)+
  theme_bw()+ scale_color_manual(values=c("yellow1", "magenta", "green1"))

# The plots are very similar, so we keep the re-calculated measurements
# The S11 F_C3 is still an outlier since this was measured after opening the column. So we're excluding that
# Can also exclude others by putting in the list

data_excluded = data_sum[!(Sample_Name=="S11" & Column_no =="F_C3")]
data_excluded[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_excluded$replicate= factor(data_excluded$replicate, levels = c("F", "C", "B"))

ggplot(data_excluded)+
  geom_line(aes(x=Column_Number, y=Oxygen, group=replicate, col=replicate), lwd=2)+
  geom_point(aes(x=Column_Number, y=Oxygen, group=replicate), size=2)+
  geom_hline(yintercept = c(10.6, 11.1), color="red", linetype="dashed")+
  facet_grid(~Sample_Name)+
  theme_bw()+ scale_color_manual(values=c("yellow1", "magenta", "green1"))

write.csv2(file="oxygen_sample_data.csv",x=data_excluded[Sample_Name%in%c("S08", "S11","S02","S14", "S19", "S13")])

