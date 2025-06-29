plot_a <- function(datamatrix, mean_datamatrix){
  par(mgp = c(1.5, 0.5, 0)) 
  
  data <- datamatrix
  
  mean_data <- mean_datamatrix
  
  class_colors <- palette()[as.numeric(data$Class)]
  
  plot(data$X1,
       data$X2,
       type = 'n',
       xlim = c(-1,1),
       ylim = c(-1,1),
       xlab = "X1", 
       ylab = "X2",
       cex.axis = 0.8,
       col.axis="gray30",
       cex.lab = 1)
  abline(v = 0, h = 0, col = "gray95")
  
  points(data$X1,
         data$X2,
         pch = 16,
         cex = 0.5,
         col = scales::alpha(class_colors, 0.5))
  
  for (i in 1:nrow(mean_data)){
    segments(x0=rep(mean_data$X1[mean_data$Sample_ID==i], each = n_replicates),
             y0=rep(mean_data$X2[mean_data$Sample_ID==i], each = n_replicates),
             x1=data$X1[data$Sample_ID==i],
             y1=data$X2[data$Sample_ID==i],
             col = scales::alpha(palette()[as.numeric(mean_data$Class)[i]], 0.3),
             lwd = 1)
  }
  
  points(mean_data$X1,
         mean_data$X2,
         pch = 24,
         cex = 1.5,
         col = "gray10",
         bg = mean_data$Class)
  #box(lwd = 1.5)
  
}
