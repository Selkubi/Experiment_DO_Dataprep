set_coloring_column = function(data) {
  data$highlight = factor(ifelse(data$Sample_Name=="S08" & data$Column_Number =="C1", "before C1", 
                                 ifelse(data$Sample_Name=="S08" & data$Column_Number =="C2", "before C2",
                                        ifelse(data$Sample_Name=="S08" & data$Column_Number =="C3", "before C3", 
                                            
                                                                    ifelse(data$Sample_Name != c("S08") & data$Column_Number =="C1", "after C1",
                                                                           ifelse(data$Sample_Name != c("S08") & data$Column_Number =="C2", "after C2",
                                                                                  ifelse(data$Sample_Name != c("S08") & data$Column_Number =="C3", "after C3",
                                                                                         "Reservoirs")))))))
  return(data)
}



fill_col_no = function(){
  scale_fill_manual( name =  "Column Location",
                     #labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                     values=c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                     guide="legend")
} 

color_col_no = function(){
  scale_color_manual( name =  "Column Location",
                      #labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                      values=c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                      guide="legend")
} 

label_manual = function () {
  scale_x_discrete(labels=c("S08" = "Day0", "S11" = "Day1", "S13" = "Day3", 
                            "S14.5" = "Day8", "S15" = "Day9",  "S17.5" = "Day13", "S18.5" = "Day15",
                            "S19" = "Day17"))
}

oxygen_plots_theme = function(){
  theme_bw()+
    theme(axis.text = element_text(size=10), 
          axis.title = element_text(size=12), 
          text =  element_text(size=10),
          axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1))
  
  
}

n_fun = function(x){
  return(data.frame(y = max(x, na.rm=T), label = paste0(length(x))))
}                                                      

observation_numbers = function (x) {
  stat_summary(fun.data = n_fun, geom = "text", na.rm=T, aes(vjust=0))
}

