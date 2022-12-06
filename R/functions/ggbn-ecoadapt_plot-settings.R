# updated: 2022-12-02 ----
# ========================================================== -----
# PLOT COLORS ----
# Three futures
colors_scenarios_3 <- c("#a0cf7c",  ## CCSM4	
                        "#5ab3b5",  ## CNRM-CM5
                        "#eead5e")  ## HadGEM2-CC

# Three futures and recent 
colors_scenarios_4 <- c("#a0cf7c",  ## CCSM4	
                        "#5ab3b5",  ## CNRM-CM5
                        "#eead5e",  ## HadGEM2-CC
                        "#5f5f5f")  ## Recent

# AVG, DJF, JJA (for TMP)
colors_metrics_3 <- c("#a6a6a6",  ## AVG	
                      "#2d77a7",  ## DJF
                      "#ba6000")  ## JJA

# DJF, JJA (for PPT)
colors_metrics_2  <- c("#2d77a7",  ## DJF
                       "#ba6000")  ## JJA
# Deuteranopia friendly ----
# Three futures and recent 
# colors_scenarios_4_deuter <- c("#34b8a0",  ## CCSM4	
#                                "#0b9bce",  ## CNRM-CM5
#                                "#ffbc43",  ## HadGEM2-CC
#                                "#5f5f5f")  ## Recent
# Three futures
# colors_scenarios_3_deuter <- c("#34b8a0",  ## CCSM4	
#                                "#0b9bce",  ## CNRM-CM5
#                                "#ffbc43")  ## HadGEM2-CC

# ========================================================== -----

# PLOT THEMES ----
#   theme_bcm_abundance_by_variable ----
theme_bcm_abundance_by_variable <- function(){
  theme_minimal() +
    theme( 
      
      # Format the plot area
      panel.border = element_rect(fill = NA, color = NA), 
      panel.background = element_rect(color = "white"),
      panel.grid = element_line(color = "white", linewidth = 2),

      # Format plot frame
      axis.line.x = element_line(color = "gray50", linewidth = 0.5),
      axis.line.y = element_line(color = "gray50", linewidth = 0.5),
      
      # Format the y-axis text 
      axis.title.y = element_text(size = 18, vjust = 1.5),
      axis.text.y = element_text(size = 16, color = "gray50"),
      
      # Format x-axis 
      axis.title.x = element_text(size = 18, vjust = 1.5),
      axis.text.x = element_text(size = 16, color = "gray30"),
      axis.ticks.x = element_blank(),
      
      # Increase margins around plot to accommodate x-axis title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
      
      # Format the title text 
      plot.title = element_text(size = 20, vjust = 1.5))
      
      # # Format the legend
      # legend.position = "right", 
      # legend.box.spacing = unit(1.2, "lines"), 
      # legend.title = element_text(size = 18),
      # legend.text = element_text(size = 16, color = "gray30"), 
      # legend.spacing.y = unit(0.5, 'cm')
      
}
#   theme_bcm_abundance_by_variable_metric_stacked ----
theme_bcm_abundance_by_variable_metric_stacked <- function(){
  theme_minimal() +
    theme( 
      
      # Format the facet strip
      panel.spacing = unit(1.2, "lines"),
      panel.border = element_rect(fill = NA, color = NA), 
      panel.background = element_rect(color = "white"),
      panel.grid = element_line(color = "white", size = 2),
      # panel.grid = element_blank(),      
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "gray97", 
                                      color = "gray50",
                                      linewidth = 0.5),
      
      # Format plot frame
      # axis.line.x = element_line(color = "gray91", size = 1),
      # axis.line.y = element_line(color = "gray91", size = 1),
      axis.line.x = element_line(color = "gray50", linewidth = 0.5),
      axis.line.y = element_line(color = "gray50", linewidth = 0.5),
      
      # Increase margins around plot to accommodate x-axis title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
      
      # Format the title text 
      plot.title = element_text(size = 20, vjust = 1.5),
      
      # Format the y-axis text 
      axis.title.y = element_text(size = 18, vjust = 1.5),
      axis.text.y = element_text(size = 16, color = "gray50"),
      
      # Format x-axis 
      axis.title.x = element_text(size = 18, vjust = 1.5),
      axis.text.x = element_text(size = 16, color = "gray30"),
      axis.ticks.x = element_blank(),
      
      # Format the legend
      legend.position = "right", 
      legend.box.spacing = unit(1.2, "lines"), 
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16, color = "gray30"), 
      legend.spacing.y = unit(0.5, 'cm')
   
    )  
}
