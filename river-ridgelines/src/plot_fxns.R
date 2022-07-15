#' @title plot linemap
#' @description generate a line map of the streams in the U.S., with the
#' height of peaks proportional to the value set for each stream order in
#' `get_grided_data()`
#' @param line_data - data.frame of X and Y coordinates, values for those coordinates,
#' and the latitude group `Y_group` used to dictate the order in which the coordinates
#' should be plotted
#' @param vertical_exag_factor - Factor by which to vertically exaggerate
#' the height values assigned to each cell
#' @param line_thickness - thickness of each horizontal line
#' @param gradient_low - color to set as low end of color gradient
#' @param gradient_high - color to set as high end of color gradient
#' @param states_poly - polygon sf object of US states
#' @param poly_color - color to be used to map `states_poly`
#' @param states_mask - polygon sf object that is inverse of US states,
#' used to mask grid lines extending beyond US borders
#' @param bkgd_color - background color for visual
#' @param google_font - font to use for title
#' @param text_color - color to use for title and USGS logo
#' @param usgs_logo_filepath - filepath for USGS logo
#' @param width - widht of exported plot
#' @param height - height of exported plot
#' @param dim_units - units for with and height
#' @param res - resolution (dpi) of exported plot
#' @param outfile - filepath of exported plot
#' @return filepath of exported plot
plot_linemap <- function(line_data, vertical_exag_factor,line_thickness, gradient_low, gradient_high,
                         states_poly, poly_color, states_mask, bkgd_color, google_font,
                         text_color, usgs_logo_filepath, width, height, dim_units, res, outfile) {
  # import font
  font_add_google(google_font)
  showtext_opts(dpi = 300, regular.wt = 400)
  showtext_auto(enable = TRUE)

  # usgs logo
  usgs_logo <- magick::image_read(usgs_logo_filepath) %>%
    magick::image_colorize(100, text_color)
  
  # set plot margin
  plot_margin <- 0.025
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = bkgd_color, alpha = 1, col = bkgd_color)
  )
  
  # Generate line plot
  line_plot <- ggplot() +
    # Add polygons for US states
    geom_sf(data=states_poly, color=poly_color, fill=poly_color) +
    # Add lines, by group, which begins w/ highest Y (latidude) value
    geom_path(data =line_data, 
              aes(x = X, y = Y+(value*vertical_exag_factor), color=(value*vertical_exag_factor), group=Y_group), 
              size=line_thickness) +
    # Color lines according to their vale
    scale_colour_gradient(low=gradient_low, high=gradient_high) +
    # mask grid lines extending beyond us borders (necessary if low gradient color differs from bkgd color)
    geom_sf(data=states_mask, fill=bkgd_color, color=NA) + 
    theme_void() +
    theme(legend.position="none",
          strip.text = element_blank())
  
  # Compose the final visual
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 9, width = 16,
              hjust = 0, vjust = 1) +
    # line plot
    draw_plot(line_plot,
              x = 0.01,
              y = -0.05,
              height = 1,
              width = 1) +
    # title
    draw_label('If Rivers were Mountains',
               x = plot_margin, y = 1-(plot_margin*1.5), 
               size = 16, 
               hjust = 0, 
               vjust = 1,
               fontfamily = google_font,
               color = text_color,
               lineheight = 1)  +
    # add logo
    draw_image(usgs_logo, x = plot_margin, y = plot_margin, width = 0.1, hjust = 0, vjust = 0, halign = 0, valign = 0)
    
  # save plot
  ggsave(filename=outfile, width = width, height = height, dpi=res, units=dim_units)
  return(outfile)
}
