
set_coloring_column <- function(data) {
  data$highlight = factor(paste0(ifelse(data$Sample_Name == "S08", "before", "after"), " C", substr(data$Column_Number, 2, 2)))
  return(data)
}



fill_col_no <- scale_fill_manual(name =  "Column Location",
                     #labels = c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                     values = c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                     guide = "legend")

color_col_no <- scale_color_manual(name =  "Column Location",
                      #labels = c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                      values = c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                      guide = "legend")

label_manual <- scale_x_discrete(labels = c("S08" = "0", "S11" = "1", "S13" = "3", 
                            "S14.5" = "8", "S15" = "9",  "S17.5" = "13", "S18.5" = "15",
                            "S19" = "17"))

oxygen_plots_theme <- theme_bw() +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          text =  element_text(size = 12),
          axis.text.x = element_text(size = 12),
          panel.grid = element_blank(), 
          strip.text = element_text(size = 12),
          strip.background = element_blank())

n_fun <- function(x){
  return(data.frame(y = max(x, na.rm = T), label = paste0(length(x))))
}                                                      

observation_numbers <- stat_summary(fun.data = n_fun, geom = "text", na.rm = T, aes(vjust = 0))

