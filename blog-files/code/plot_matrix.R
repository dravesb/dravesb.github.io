#------------------------------
#
#   Create Plotting Function 
#
#------------------------------

plot_matrix = function(A,...){
  #input A - a square matrix
  require(ggplot2)
  require(reshape2)
  
  #columns get mapped to rows in ascending orde
 
  #map rows to melted data frame
  stack_A = melt(t(A))

  #reorder the rows to descend
  stack_A$Var2 = -1*(stack_A$Var2 - (max(stack_A$Var2) + 1))
  
  #set x & y labels
  colnames(stack_A) = c("Column_ID", "Row_ID", "Value")
  
  #build gg object
  p = ggplot(aes(x = Column_ID, y = Row_ID, fill = Value), data = stack_A) + 
      geom_raster() +
      scale_fill_gradient(low="white", high="red") +
      theme_bw() +
      theme(axis.line = element_line(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+ 
    labs(title = "Heatmap of given matrix")
      
  #output - a ggplot2 object
  return(p)
}

