plot_linemap <- function(line_data, vertical_exag_factor,line_thickness, palette_low, palette_high,
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
  
  line_plot <- ggplot() +
    geom_sf(data=states_poly, color=poly_color, fill=poly_color) +
    geom_path(data =line_data, 
              aes(x = X, y = Y+(value*vertical_exag_factor), color=(value*vertical_exag_factor), group=Y_group), 
              size=line_thickness) +
    scale_colour_gradient(low=palette_low, high=palette_high) +
    geom_sf(data=states_mask, fill=bkgd_color, color=NA) +
    theme_void() +
    theme(legend.position="none",
          strip.text = element_blank())
  
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
    
  
  ggsave(filename=outfile, width = width, height = height, dpi=res, units=dim_units)
  return(outfile)
}

# to try:
# geom polygon with gradient fill - top border = line w/ peaks

