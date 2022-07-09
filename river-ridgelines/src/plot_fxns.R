plot_linemap <- function(background_poly, line_data, line_data_no_streams,
                         line_data_so3, line_data_so4,
                         line_data_so5, line_data_so6, line_data_so7,
                         bkgd_color, poly_color, line_fill_color, line_border_color, 
                         width, height, outfile) {
  

  
  jpeg(file=outfile, width = width, height = height, quality=100, res=300)
  
  opar <- par(mar=c(0,0,0,0), bg = bkgd_color)
  plot(st_geometry(background_poly), col=poly_color, border = NA, bg = bkgd_color,
       xlim = c(min(line_data$X), max(line_data$X)), ylim= c(min(line_data$Y), max(line_data$Y)))
  # linemap(x = line_data, var = "value", k = 5, threshold = 1,
  #         aes(col = "value"), lwd=0.8, border = line_border_color, add = TRUE)
  # linemap(x = line_data, var = "value", k = 5, threshold = 1,
  #         col = line_fill_color, lwd=0.8, border = line_border_color, add = TRUE)
  # add_linemap(x = line_data, var = "value", k = 5, threshold = 1,
  #         col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.1, add = TRUE)
  # add_linemap(x = line_data_so3, var = "value_so3", k = 5, threshold = 1,
  #                  col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.1, add = TRUE)
  # add_linemap(x = line_data_so4, var = "value_so4", k = 5, threshold = 1,
  #                  col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.3, add = TRUE)
  # add_linemap(x = line_data_so5, var = "value_so5", k = 5, threshold = 1,
  #                  col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.6, add = TRUE)
  # add_linemap(x = line_data_so6, var = "value_so6", k = 5, threshold = 1,
  #                  col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.8, add = TRUE)
  # add_linemap(x = line_data_so7, var = "value_so7", k = 5, threshold = 1,
  #                  col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 1, add = TRUE)
  add_linemap(x = line_data_so7, var = "value", k = 5, threshold = 1,
                   col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 1, add = TRUE)
  add_linemap(x = line_data_so6, var = "value", k = 5, threshold = 1,
              col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.8, add = TRUE)
  add_linemap(x = line_data_so5, var = "value", k = 5, threshold = 1,
              col = line_fill_color, lwd=0.8, border = line_border_color, alpha = 0.6, add = TRUE)
  add_linemap(x = line_data_so4, var = "value", k = 5, threshold = 1,
              col = line_fill_color, lwd=0.6, border = line_border_color, alpha = 0.3, add = TRUE)
  add_linemap(x = line_data_so3, var = "value", k = 5, threshold = 1,
              col = line_fill_color, lwd=0.6, border = line_border_color, alpha = 0.1, add = TRUE)
  add_linemap(x = line_data_no_streams, var = "value", k = 5, threshold = 1,
              col = line_fill_color, lwd=0.6, border = line_border_color, alpha = 0.1, add = TRUE)
  par(opar)
  
  dev.off()
}

### Modified from https://github.com/riatelab/linemap/blob/master/R/linemap.R
add_linemap <- function(x, var, k = 2, threshold = 1, col = "white",
                             border = "black", lwd = 0.5, alpha=1, add = FALSE){
  x[is.na(x[var]),var] <- 0
  lat <- unique(x[,2])
  lon <- unique(x[,1])
  
  if(!add){
    graphics::plot(1:10, type = "n", axes = F,
                   xlab = "", ylab="", asp = 1,
                   xlim = c(min(x[,1]), max(x[,1])),
                   ylim = c(min(x[,2]), max(x[,2])))
  }
  for (i in length(lat):1){
    ly <- x[x[,2]==lat[i],]
    ly[ly[var] < threshold, var] <- 0
    yVals <- ly[,2] + ly[,var] * k
    xVals <- ly[,1]
    yVals[is.na(yVals)] <- lat[i]
    yVals[1] <- lat[i] + min(ly[,var] * k)
    yVals[length(yVals)] <- yVals[1]
    graphics::polygon(xVals, yVals, border = NA, col = col)
    if(length(yVals)>1){
      for(j in 1:(length(yVals) - 1)){
        if ((ly[j,var] > 0) | (ly[j+1,var] > 0)){
          graphics::segments(xVals[j], yVals[j],
                             xVals[j+1], yVals[j+1],
                             col=alpha(border, alpha), lwd=lwd)
        }
      }
    }
  }
}