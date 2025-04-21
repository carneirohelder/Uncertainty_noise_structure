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
# 
# plot_b <- function(local_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   data <- local_noise_output$Train_data
#   
#   mean_data <- local_noise_output$Sample_means
#   
#   noise_matrix <- local_noise_output$Noise_matrix
#   
#   class_colors <- palette()[as.numeric(noise_matrix$Class)]
#   
#   model <- local_noise_output$Model
#   
#   model_slope <- -model$finalModel$coefficients[1,1,] / model$finalModel$coefficients[2,1,]
#   
#  
#   
#   plot(noise_matrix$X1,
#        noise_matrix$X2,
#        type = 'n',
#        xlim = c(-1,1),
#        ylim = c(-1,1),
#        xlab = "X1", 
#        ylab = "X2",
#        cex.axis = 1,
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   abline(v = 0, h = 0, col = "gray95")
#   abline(a=0,b=model_slope, col = "black", lwd = 1)
#   points(noise_matrix$X1,
#          noise_matrix$X2,
#          pch = 16,
#          cex = 0.2,
#          col = scales::alpha(class_colors, 0.5))
#   
#   for (i in 1:nrow(mean_data)){
#     segments(x0=rep(mean_data$X1[i], each = 100),
#              y0=rep(mean_data$X2[i], each = 100),
#              x1=noise_matrix$X1[noise_matrix$Sample_ID==i],
#              y1=noise_matrix$X2[noise_matrix$Sample_ID==i],
#              col = scales::alpha(palette()[sample_class_index[i]], 0.2),
#              lwd = 0.6)
#   }
# 
#   points(mean_data$X1,
#          mean_data$X2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg =  sample_class_index)
#  
#   
# }
# 
# plot_c <- function(local_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   n_samples <- length(sample_class_index)
#   
#   n_samples_class <- as.numeric(table(sample_class_index))
#   
#   sample_distribution <- local_noise_output$Sample_model_distribution
#   
#   mean_data <- local_noise_output$Sample_means
#   
#   model <- local_noise_output$Model
#   
#   
#   
#   plot(1:n_samples,
#        c(1:n_samples),
#        type = "n",
#        xlab = expression(italic(hat(y)), family = "Helvetica-Oblique"),
#        ylab = "Samples",
#        ylim= c(0,max(n_samples_class)*1.2),
#        xlim = c(-1,1),
#       # yaxt = c(1:max(n_samples_class)),
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1
#   )
#   
#   abline(v = 0, col = "black")
#   
#     up_lim <- sample_distribution$sd_fitted*2 + sample_distribution$mean_fitted
#     bot_lim <- -sample_distribution$sd_fitted*2 + sample_distribution$mean_fitted
# 
#      for (i in 1:length(n_samples_class)) {
#     arrows(x0 = bot_lim[sample_class_index==i],
#            x1 = up_lim[sample_class_index==i],
#            y0 = c(1:n_samples_class[i]),
#            y1 = c(1:n_samples_class[i]),
#            col = scales::alpha(palette()[i], 1),
#            code = 3,
#            angle = 90,
#            length = 0.05)
#      }
#   
#  for (i in 1:length(n_samples_class)) {
#     points(sample_distribution$mean_fitted[sample_class_index==i],
#            c(1:n_samples_class[i]),
#            pch = 24,
#            cex = 1.5,
#            col = "gray10",
#            bg = i)
#   }
#   
# }
# 
# plot_d <- function(global_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0))  
#   
#   data <- global_noise_output$Train_data
#   
#   mean_data <- global_noise_output$Sample_means
#   
#   noise_matrix <- global_noise_output$Global_Noise_matrix
#   
#     model <- global_noise_output$Model
#   
#   model_slope <- -model$finalModel$coefficients[1,1,] / model$finalModel$coefficients[2,1,]
#   
#   noise_proj <-   global_noise_output$Generated_noise_projection
#   
#   noise_proj_sd <-  global_noise_output$Noise_model_distribution$sd_fitted
#   
#  
#   plot(mean_data$X1,
#        mean_data$X2,
#        type = 'n',
#        xlim = c(-1,1),
#        ylim = c(-1,1),
#        xlab = "X1", 
#        ylab = "X2",
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   abline(v = 0, h = 0, col = "gray95")
#   
#   polygon(x = c(1+noise_proj_sd*2 ,1-noise_proj_sd*2 ,-1-noise_proj_sd*2 ,-1+noise_proj_sd*2),
#           y = c( model_slope , model_slope ,-model_slope ,-model_slope ),
#           col = "gray90", border = NA)
#   
#   points(noise_matrix$X1,
#          noise_matrix$X2,
#          pch = 16,
#          cex = 0.2,
#          col = scales::alpha("gray50", 0.5))
#   
#   abline(a=0,b=model_slope, col = "black", lwd = 1)
#   
#   
#   
#   points(mean_data$X1,
#          mean_data$X2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg =  sample_class_index)
#   box(lwd = 1)
#   
# }
# 
# 
# plot_e <- function(global_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   n_samples <- length(sample_class_index)
#   
#   n_samples_class <- as.numeric(table(sample_class_index))
#   
#   sample_distribution <- global_noise_output$Sample_model_distribution
#   
#   noise_proj <- global_noise_output$Generated_noise_projection
#   
#   noise_proj_sd <-  global_noise_output$Noise_model_distribution$sd_fitted
#   mean_data <- global_noise_output$Samples_projection
#   model <- global_noise_output$Model
#   
#   plot(1:n_samples,
#        c(1:n_samples),
#        type = "n",
#        xlab = expression(italic(hat(y)), family = "Helvetica-Oblique"),
#        ylab = "Samples",
#        ylim= c(0,max(n_samples_class)*1.2),
#        xlim = c(-1,1),
#        #yaxt = "n",
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1
#   )
#   polygon(x = c(noise_proj_sd*2 ,-noise_proj_sd*2 ,-noise_proj_sd*2 ,noise_proj_sd*2),
#           y = c(80,80,-1,-1),
#           col = "gray90", border = NA)
#   
#   abline(v = 0, col = "black")
#   
#   
#   
#  
#    box(lwd = 1)
#   
#    for (i in 1:length(n_samples_class)) {
#      points(mean_data[sample_class_index==i],
#             c(1:n_samples_class[i]),
#             pch = 24,
#             cex = 1.5,
#             col = "gray10",
#             bg = i)
#    }
#   
#    points(noise_proj,
#             rep(0,length(noise_proj)),
#             col = scales::alpha("gray50", 0.5),  # Color
#             pch = 16, 
#             cex = 0.2)
#    
#    #lines(density(noise_proj))
#    
#    curve(dnorm(x, mean = 0, sd = noise_proj_sd)/
#            max(dnorm(x, mean = 0, sd = noise_proj_sd)),
#          from = -1, to = 1,
#          col = scales::alpha("gray50", 0.5),
#          add = T)
#  
# }
# 
# plot_b_svm <- function(local_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   data <- local_noise_output$Train_data
#   
#   mean_data <- local_noise_output$Sample_means
#   
#   noise_matrix <- local_noise_output$Noise_matrix
#   
#   class_colors <- palette()[as.numeric(noise_matrix$Class)]
#   
#   model <- local_noise_output$Model
#   
#   x_grid <- seq(min(data$X1) - 2, max(data$X1) + 2, length.out = 1000)
#   y_grid <- seq(min(data$X2) - 2, max(data$X2) + 2, length.out = 1000)
#   
#   grid <- expand.grid(X1 = x_grid, X2 = y_grid)
#   
#   grid$Decision <- predict(model, newdata = grid)
#   grid_numeric <- ifelse(grid$Decision == "Class 1", 1, 2)
#   
#   plot(noise_matrix$X1,
#        noise_matrix$X2,
#        type = 'n',
#        xlim = c(-1,1),
#        ylim = c(-1,1),
#        xlab = "X1", 
#        ylab = "X2",
#        cex.axis = 1,
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   points(grid$X1,
#          grid$X2,
#          pch = 16,
#          cex = 0.2,
#          col = grid_numeric)
#   points(0,
#          0,
#          pch = 16,
#          cex = 100,
#          col = scales::alpha("white",0.8))
#   
#   abline(v = 0, h = 0, col = "gray95")
#   
#   points(noise_matrix$X1,
#          noise_matrix$X2,
#          pch = 16,
#          cex = 0.2,
#          col = scales::alpha(class_colors, 1))
#   
#   
#   
#   
#   for (i in 1:nrow(mean_data)){
#     segments(x0=rep(mean_data$X1[i], each = 100),
#              y0=rep(mean_data$X2[i], each = 100),
#              x1=noise_matrix$X1[noise_matrix$Sample_ID==i],
#              y1=noise_matrix$X2[noise_matrix$Sample_ID==i],
#              col = scales::alpha(palette()[sample_class_index[i]], 0.2),
#              lwd = 0.6)
#   }
#   
#   points(mean_data$X1,
#          mean_data$X2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg =  sample_class_index)
#   
#   box(lwd = 1)
# }
# 
# plot_c_svm <- function(local_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   n_samples <- length(sample_class_index)
#   
#   n_samples_class <- as.numeric(table(sample_class_index))
#   
#   sample_distribution <- local_noise_output$Sample_model_distribution
#   
#   mean_data <- local_noise_output$Sample_means
#   
#   model <- local_noise_output$Model
#   
#   
#   
#   plot(1:n_samples,
#        c(1:n_samples),
#        type = "n",
#        xlab = expression(italic(hat(y)), family = "Helvetica-Oblique"),
#        ylab = "Samples",
#        ylim= c(0,max(n_samples_class)*1.2),
#        xlim = c(-1.5,1.5),
#        # yaxt = c(1:max(n_samples_class)),
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1
#   )
#   
#   abline(v = 0, col = "black")
#   
#   up_lim <- sample_distribution$sd_fitted*2 + sample_distribution$mean_fitted
#   bot_lim <- -sample_distribution$sd_fitted*2 + sample_distribution$mean_fitted
#   
#   for (i in 1:length(n_samples_class)) {
#     arrows(x0 = bot_lim[sample_class_index==i],
#            x1 = up_lim[sample_class_index==i],
#            y0 = c(1:n_samples_class[i]),
#            y1 = c(1:n_samples_class[i]),
#            col = scales::alpha(palette()[i], ),
#            code = 3,
#            angle = 90,
#            length = 0.05)
#   }
#   
#   for (i in 1:length(n_samples_class)) {
#     points(sample_distribution$mean_fitted[sample_class_index==i],
#            c(1:n_samples_class[i]),
#            pch = 24,
#            cex = 1.5,
#            col = "gray10",
#            bg = i)
#   }
#   
# }
# 
# plot_d_svm <- function(global_noise_output, sample_class_index){
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   data <- local_noise_output$Train_data
#   
#   mean_data <- local_noise_output$Sample_means
#   
#   noise_matrix <- local_noise_output$Noise_matrix
#   
#   class_colors <- palette()[as.numeric(noise_matrix$Class)]
#   
#   model <- local_noise_output$Model
#   
#   x_grid <- seq(min(data$X1) - 2, max(data$X1) + 2, length.out = 1000)
#   y_grid <- seq(min(data$X2) - 2, max(data$X2) + 2, length.out = 1000)
#   
#   grid <- expand.grid(X1 = x_grid, X2 = y_grid)
#   
#   grid$Decision <- compute_f_x(model, as.matrix(global_noise_gen))
#   grid_numeric <- ifelse(grid$Decision == "Class 1", 1, 2)
#   
#   plot(noise_matrix$X1,
#        noise_matrix$X2,
#        type = 'n',
#        xlim = c(-1,1),
#        ylim = c(-1,1),
#        xlab = "X1", 
#        ylab = "X2",
#        cex.axis = 1,
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   points(grid$X1,
#          grid$X2,
#          pch = 16,
#          cex = 0.2,
#          col = grid_numeric)
#   points(0,
#          0,
#          pch = 16,
#          cex = 100,
#          col = scales::alpha("white",0.8))
#   
#   abline(v = 0, h = 0, col = "gray95")
#   
#   points(noise_matrix$X1,
#          noise_matrix$X2,
#          pch = 16,
#          cex = 0.2,
#          col = scales::alpha(class_colors, 1))
#   
#   
#   
#   
#   for (i in 1:nrow(mean_data)){
#     segments(x0=rep(mean_data$X1[i], each = 100),
#              y0=rep(mean_data$X2[i], each = 100),
#              x1=noise_matrix$X1[noise_matrix$Sample_ID==i],
#              y1=noise_matrix$X2[noise_matrix$Sample_ID==i],
#              col = scales::alpha(palette()[sample_class_index[i]], 0.2),
#              lwd = 0.6)
#   }
#   
#   points(mean_data$X1,
#          mean_data$X2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg =  sample_class_index)
#   
#   box(lwd = 1)
# }
# 
# 
# plot_pca_1 <- function(model, replicate_index) {
#   # 
#   # model<- pls_1
#   # replicate_index<- rep(1:10, each = 5)
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   class_index <- as.numeric(model$trainingData$.outcome)
#   
#   train_data <- model$trainingData[,-ncol(model$trainingData)]
#   
#   num_samples <- length(unique(replicate_index))
#   
#   pca <- prcomp(train_data)
#   
#   eigenvalues <- pca$sdev^2
#   explained_variance <- eigenvalues / sum(eigenvalues) * 100
#   
#   scores <- data.frame(class_index,replicate_index,pca$x)
#   
#   pca_mean <- scores %>%
#     group_by(replicate_index) %>%
#     summarise(across(everything(), mean))
#   
#   class_colors <- palette()[as.numeric(as.factor(class_index))]
#   
#   class_mean_color <- palette()[pca_mean$class_index]
#   
#   axis_lim <- max(abs(pca$x[1,]))*1.5
#   
#   plot(pca$x[,1],
#        pca$x[,2],
#        type = 'n',
#        xlim = c(-axis_lim,axis_lim),
#        ylim = c(-axis_lim,axis_lim),
#        xlab = paste0("PC 1 - ", round(explained_variance[1], digits = 2),"%"),
#        ylab = paste0("PC 2 - ", round(explained_variance[2], digits = 2),"%"),
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   abline(v = 0, h = 0, col = "gray95")
#   
#   points(pca$x[,1],
#          pca$x[,2],
#          pch = 16,
#          cex = 0.5,
#          col = scales::alpha(class_colors, 0.5))
#   
#   for (i in 1:nrow(pca_mean)){
#     segments(x0=rep(pca_mean$PC1[pca_mean$replicate_index==i], each = num_samples),
#              y0=rep(pca_mean$PC2[pca_mean$replicate_index==i], each = num_samples),
#              x1=scores$PC1[scores$replicate_index==i],
#              y1=scores$PC2[scores$replicate_index==i],
#              col = scales::alpha(palette()[pca_mean$class_index[i]], 0.3),
#              lwd = 1)
#   }
#   
#   points(pca_mean$PC1,
#          pca_mean$PC2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg = pca_mean$class_index)
# }
# 
# plot_pca_2 <- function(a, replicate_index) {
#   # 
#   # model<- pls_1
#   # replicate_index<- rep(1:10, each = 5)
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   model <- a$Model
#   
#   class_index <- as.numeric(model$trainingData$.outcome)
#   
#   train_data <- model$trainingData[,-ncol(model$trainingData)]
#   
#   num_samples <- length(unique(replicate_index))
#   
#   pca <- prcomp(train_data)
#   
#   eigenvalues <- pca$sdev^2
#   explained_variance <- eigenvalues / sum(eigenvalues) * 100
#   
#   scores <- data.frame("Class"=as.numeric(as.factor(df_center$Class)),
#                        "Sample"=df_center$Sample,
#                        pca$x, check.names = F)
#   
#   pca_mean <- scores %>%
#     group_by(Sample) %>%
#     summarise(across(everything(), mean))
#   
#   noise_pca <- data.frame(PC1 = as.matrix(pca_mean[,-c(1:2)]) %*% as.matrix(pca$rotation[,1]),
#                           PC2 = as.matrix(pca_mean[,-c(1:2)]) %*% as.matrix(pca$rotation[,2]),
#                           Sample = scores$Sample,
#                           Class = scores$Class)
#   
#   
#   class_colors <- palette()[noise_pca$Class]
#   
#   class_mean_color <- palette()[pca_mean$class_index]
#   
#   axis_lim <- max(abs(pca$x[1,]))*1.5
#   
#   plot(noise_pca$PC1,
#        noise_pca$PC2,
#        type = 'n',
#        xlim = c(-axis_lim,axis_lim),
#        ylim = c(-axis_lim,axis_lim),
#        xlab = paste0("PC 1 - ", round(explained_variance[1], digits = 2),"%"),
#        ylab = paste0("PC 2 - ", round(explained_variance[2], digits = 2),"%"),
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   abline(v = 0, h = 0, col = "gray95")
#   
#   points(noise_pca$PC1,
#          noise_pca$PC2,
#          pch = 16,
#          cex = 0.5,
#          col = scales::alpha(class_colors, 0.5))
#   
#   for (i in 1:nrow(pca_mean)){
#     segments(x0=rep(pca_mean$PC1[pca_mean$replicate_index==i], each = num_samples),
#              y0=rep(pca_mean$PC2[pca_mean$replicate_index==i], each = num_samples),
#              x1=noise_pca$PC1[noise_pca$Sample_ID==i],
#              y1=noise_pca$PC2[noise_pca$Sample_ID==i],
#              col = scales::alpha(palette()[pca_mean$class_index[i]], 0.3),
#              lwd = 1)
#   }
#   
#   points(pca_mean$PC1,
#          pca_mean$PC2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg = pca_mean$class_index)
# }
# 
# plot_pca_3 <- function(b, replicate_index) {
#   # 
#   # model<- pls_1
#   # replicate_index<- rep(1:10, each = 5)
#   par(mgp = c(1.5, 0.5, 0)) 
#   
#   model <- a$Model
#   
#   class_index <- as.numeric(model$trainingData$.outcome)
#   
#   train_data <- model$trainingData[,-ncol(model$trainingData)]
#   
#   num_samples <- length(unique(replicate_index))
#   
#   pca <- prcomp(train_data)
#   
#   eigenvalues <- pca$sdev^2
#   explained_variance <- eigenvalues / sum(eigenvalues) * 100
#   
#   scores <- data.frame(class_index,replicate_index,pca$x)
#   
#   pca_mean <- scores %>%
#     group_by(replicate_index) %>%
#     summarise(across(everything(), mean))
#   
#   noise_pca <- data.frame(PC1 = as.matrix(b$Global_Noise_matrix) %*% as.matrix(pca$rotation[,1]),
#                           PC2 = as.matrix(b$Global_Noise_matrix) %*% as.matrix(pca$rotation[,2]))
#   
#   
#   class_colors <- palette()[noise_pca$Class]
#   
#   class_mean_color <- palette()[pca_mean$class_index]
#   
#   axis_lim <- max(abs(pca$x[1,]))*1.5
#   
#   plot(noise_pca$PC1,
#        noise_pca$PC2,
#        type = 'n',
#        xlim = c(-axis_lim,axis_lim),
#        ylim = c(-axis_lim,axis_lim),
#        xlab = paste0("PC 1 - ", round(explained_variance[1], digits = 2),"%"),
#        ylab = paste0("PC 2 - ", round(explained_variance[2], digits = 2),"%"),
#        cex.axis = 0.8,
#        col.axis="gray30",
#        cex.lab = 1)
#   abline(v = 0, h = 0, col = "gray95")
#   
#   points(noise_pca$PC1,
#          noise_pca$PC2,
#          pch = 16,
#          cex = 0.5,
#          col = scales::alpha("gray50", 0.5))
#   
#   
#   points(pca_mean$PC1,
#          pca_mean$PC2,
#          pch = 24,
#          cex = 1.5,
#          col = "gray10",
#          bg = pca_mean$class_index)
# }
# 
