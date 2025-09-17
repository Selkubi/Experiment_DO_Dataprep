
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

label_manual <- scale_x_discrete(labels = c("S08" = "0", "S11" = "1", "S13" = "3", "S13.5" = "6",
                            "S14.5" = "8", "S15" = "9",  "S17.5" = "13", "S18.5" = "15",
                            "S19" = "17"))

color_column <- function() {
  ggplot2::scale_color_manual(name =  "Column Position",
                              values = c("#164C6B", "#F3A712", "#E4572E"),
                              guide = "legend")
}

n_fun <- function(x){
  return(data.frame(y = max(x, na.rm = T), label = paste0(length(x))))
}                                                      

observation_numbers <- stat_summary(fun.data = n_fun, geom = "text", na.rm = T, aes(vjust = 0))

theme_boxplot <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      # General text settings
      text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 11),
      # X-axis text settings
      axis.text.x = ggplot2::element_text(color = "black", size = 10),
      # Y-axis text settings
      axis.text.y = ggplot2::element_text(color = "black", size = 10),
      
      # Legend settings
      legend.position = "right",
      
      # Strip settings
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, color = "black"),
      
      # Panel and axis line settings
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      panel.border = ggplot2::element_rect(color = "black", size = 0.5),  # Add panel border
      axis.line.y.right = ggplot2::element_line(color = "black", linewidth = 0.5),
      
      # Grid settings
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      # Plot background
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      
      # Boxplot specific settings
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)  # Adjust margins as needed
    )
}

