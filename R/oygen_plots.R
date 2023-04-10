library(data.table)
library(ggplot2)

#Take in the data
O2_recalibrated <- fread("data/presense/SELIN_O2_recalibrated.txt", header=T, skip = 1)
data <- setDT(O2_recalibrated[,c("Date", "Time", "Value", "Temp", "Pressure","Column_no","Sample_Name", "New_Calibration")])

#Set the classes, this is important for the dates as well as the factors (especially in plotting facets this would come handy)
data[,"Date"] <- as.Date(data$Date , format = "%m/%d/%Y")
data[,"Time"] <- as.ITime(data$Time)
data[, c("Sample_Name", "Column_no")] <- lapply (data[, c("Sample_Name", "Column_no")], as.factor)
levels(data$Sample_Name) <- list(S00 = "S00", S02 = "S02", S4.5 = "S4.5", S06 = "S06", S6.5 = "S6.5",  S6.7 = "S6.7", S08 = "S08", 
                                   S11 = "S11", S13 = "S13", S13.5 = "S13.5", S14.5 = "S14.5", S15 = "S15", S17.5 = "S17.5", S18.5 = "S18.5", S19 = "S19")
data[, c("year", "month", "day") := tstrsplit(Date, "-")]

#Calculate the median or the mean
data_sum <- data[,.(Oxygen = median(Value), Temp = median(Temp)), by = c("Column_no",  "day", "month", "Date", "Sample_Name")]
data_sum[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_sum$replicate <- factor(data_sum$replicate, levels = c("F", "C", "B")) #To change th order of plotting so they are not on top of each other in a bad way

ggplot(data_sum)+
  geom_line(aes(x = Column_Number, y = Oxygen, group = replicate, col = replicate), lwd = 2) +
  geom_point(aes(x = Column_Number, y = Oxygen, group = replicate), size = 2) +
  geom_hline(yintercept = c(10.6, 11.1), color = "red", linetype = "dashed") +
  facet_grid(~Sample_Name) +
  theme_bw() + scale_color_manual(values = c("yellow1", "magenta", "green1"))

# Sample plot with the new calculated F measurements from S11 until S19. 
#the calculation is done using the Presens software but can also be manually calculated provided that the calibration equation is known.
# In this case only the C2 values of the F replicate should show different results 
#(all the F replicate values are recalcualted but C1 and C3 should be quite similar to original values since these had the correct calibration when measuring)
# To exclude a holw set of sampling date use data_sum[!(Sample_Name == "S06"| Sample_Name=="S6.5"| Sample_Name == "S15.7")]

data_sum <- data[,.(Oxygen = median(New_Calibration), Temp = median(Temp), Pressure = median(Pressure)), by = c("Column_no",  "day", "month", "Date", "Sample_Name")]
data_sum[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_sum$replicate <- factor(data_sum$replicate, levels = c("F", "C", "B"))
data_subset <- data_sum[Sample_Name %in% c("S08", "S11","S13", "S14.5","S15","S17.5", "S18.5","S19")]

ggplot(data_subset)+
  geom_line(aes(x = Column_Number, y = Oxygen, group = replicate, col = replicate), lwd = 2)+
  geom_point(aes(x = Column_Number, y = Oxygen, group = replicate), size = 2)+
  geom_hline(yintercept = c(10.6, 11.1), color = "red", linetype = "dashed")+
  facet_grid(~Sample_Name)+
  theme_bw() + scale_color_manual(values = c("yellow1", "magenta", "green1"))

# The plots are very similar, so we keep the re-calculated measurements
# The S11 F_C3 is still an outlier since this was measured after opening the column. So we're excluding that
# Can also exclude others by putting in the list

write.csv2(file = "oxygen_sample_data.csv",x = data_excluded[Sample_Name %in% c("S08",  "S11","S02","S14", "S19", "S13")])

# Reverse plots sample_date~col_no 
source("R/plotting_functinos.R")
data_subset[, lapply(.SD, mean), .SDcols="Oxygen", by = c("Sample_Name", "Column_Number")]
data_subset <- set_coloring_column(data_subset) # write a  new column for the differing color scheme
facet_names = c(C1 = "Column 1", C2 = "Column 2", C3 = "Column 3")



ggplot(data_subset, aes(x = Sample_Name, y = Oxygen))+
 geom_boxplot(aes(x = Sample_Name, y = Oxygen, fill = highlight, color = highlight), lwd=0.5)+
  observation_numbers +
  facet_grid(~Column_Number, labeller = as_labeller(facet_names)) +
  geom_hline(yintercept = c(10.6, 11.1), color = "red", linetype = "dashed")+
  oxygen_plots_theme + fill_col_no +color_col_no +
  label_manual + ylab("Oxygen (mg/L)") + xlab ("Days")

