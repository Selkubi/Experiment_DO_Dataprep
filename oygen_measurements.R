library(data.table)
library(ggplot2)

data = data.table(read_delim("final_data.txt", "\t", escape_double = FALSE, 
                  col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                   Time = col_time(format = "%H:%M:%S"), Temp = col_double(), Oxygen = col_double(),
                                   Sample_Name= col_factor((levels=c("S00", "S02", "S4.5", "S06", "S6.5", "S6.7", "S08", "S11", "S13", "S13.5", "S15.5", "S15.7", "S17.5", "S18.5", "S19"))),
                                   Column_no = col_factor(levels = c("B_C1","B_C2", "B_C3", "C_C1", "C_C2", "C_C3", "F_C1", "F_C2", "F_C3")))))
                  
data[, c("year", "month", "day") := tstrsplit(Date, "-")]
data_sum=data[,.(Oxygen=median(Oxygen), Temp=median(Temp)), by=c("Column_no",  "day", "month", "Date", "Sample_Name")]
data_sum[, c("replicate", "Column_Number") := tstrsplit(Column_no, "_")]
data_excluded=data_sum[!(Sample_Name == "S06"| Sample_Name=="S6.5"| Sample_Name == "S15.7")]
data_excluded$replicate= factor(data_excluded$replicate, levels = c("F", "C", "B"))
data_sum[!(Sample_Name)]

ggplot(data_excluded[!(Sample_Name %in% c("S11", "S13", "S13.5", "S15.5", "S17.5", "S18.5", "S19") & Column_no %in% c("F_C1", "F_C3"))])+
  geom_line(aes(x=Column_Number, y=Oxygen, group=replicate, col=replicate), lwd=2)+
  geom_point(aes(x=Column_Number, y=Oxygen, group=replicate), size=2)+
  facet_grid(~Sample_Name)+
  theme_bw()+ scale_color_manual(values=c("yellow1", "magenta", "green1"))
