#' Plot map
#' 
#' Plot static map for given year (single frame in animation).
#' 
#' @param basemap Basemap raster.
#' @param fire_pts Fire occurrence points.
#' @param year Year of fire occurences to plot.
#' @param col_fire Color of the fire points.
#' @param font_year Font name for the printed year.
#' 
build_map <- function(basemap, fire_pts, year, col_fire, font_year){
  
  # Prep fonts
  showtext_opts(dpi = 300, regular.wt = 400, bold.wt = 800)
  showtext_auto(enable = TRUE)
  
  # Load basemap
  basemap <- rast(basemap)
  
  # Filter fire points to given year
  fire_pts_year <- fire_pts %>%
    #filter(Year == year)
    filter(Year == ifelse(year %% 1 < 0.75, year %/% 1, year %/% 1 + 0.5))
  
  # Filter fire points to all years prior to given year
  fire_pts_past <- fire_pts %>%
    filter(Year < year)
  
  # Plotting
  ggplot()+
    # Plot basemap
    geom_spatraster_rgb(data = basemap) +
    
    # Plot dark glowpoints for previously burned areas
    geom_glowpoint(data = fire_pts_past,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc),
                   alpha = .1,
                   color = "#45220f",
                   shadowcolour = "#45220f",
                   shadowalpha = .05,
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.05, 0.5)) +
    new_scale("size") +
    
    # Plot glowpoints for current year
    geom_glowpoint(data = fire_pts_year,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc),
                   alpha = .8,
                   color = col_fire,
                   shadowcolour = col_fire,
                   shadowalpha = .1,
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.08, 0.6)) +
    new_scale("size") +
    geom_glowpoint(data = fire_pts_year,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc), 
                   alpha = .6,
                   shadowalpha = .05,
                   color = "#ffffff",
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.01, .2)) +
    
    # Styling
    theme_void() 
}


#' Plot chart
#' 
#' Plot static graph for given year
#' 
#' @param chart_data Vata frame for the chart data.
#' @param col_lines Vector (length = 2) of colors for the graph lines.
#' @param year Year to highlight in chart.
#' @param font_chart_titles Font name for the chart titles (facet strip text)
#' @param font_chart_axes Font name for axis lables
#' 
build_graph <- function(chart_data, col_lines, year, font_chart_titles, font_chart_axes){
  
  # Prep fonts
  showtext_opts(dpi = 300, regular.wt = 400, bold.wt = 500)
  showtext_auto(enable = TRUE)
  
  # Filter fire points to given year
  chart_data_line <- chart_data 
  
  chart_data_point <- chart_data %>%
    filter(Year == year)
  
  # Plotting
  ggplot() +
    # Plot line graph
    geom_glowline(data = chart_data, aes(x = Year, y = value, color = name), size = 0.4) +
    # Plot points with alternated Year column (so entire lines are static and only point moves)
    geom_glowpoint(data = chart_data_point, 
                   aes(x = Year, y = value, color = name), size = 1) +
    
    # Styling
    facet_wrap(~ name_f, ncol = 1, scales = "free_y") +
    scale_color_manual(values = col_lines) +
    ylab(NULL) +
    xlab(NULL) +
    theme(plot.background = element_rect(fill = "#262626", color = NA),
          panel.background = element_rect(fill = "#262626", color = NA),
          strip.background = element_blank(),
          strip.text = element_text(color = "gray70", size = 5, family = font_chart_titles, face = "bold"),
          strip.placement = "outside",
          panel.spacing = unit(1/8, "in", data = NULL),
          legend.position = "none",
          axis.title.x = element_text(color = "gray70"),
          panel.grid = element_line(color = "gray40"),
          axis.text = element_text(color = "gray40", family = font_chart_axes, face = "plain", size = 5))
}

#' Combine plots
#' 
#' Combine static map and static chart for given year to create single animation
#' frame.
#' 
#' @param chart_data Data frame for the chart data.
#' @param col_lines Vector (length = 2) of colors for the graph lines.
#' @param font_chart_titles Font name for the chart titles (facet strip text)
#' @param font_chart_axes Font name for axis lables
#' @param basemap Basemap raster.
#' @param fire_pts Fire occurrence points.
#' @param col_fire Color of the fire points.
#' @param font_year Font name for the printed year.
#' 
#' @param font_main_title Font name for the main title.
#' @param year Year from which to plot data.
#' @param col_bg Color of the image background.
#' @param height Height of the image in inches.
#' @param width Width of the image in inches.
#' @param file_out The output filename with extension.
#' 
combine_plots <- function(chart_data, col_lines, font_chart_titles, font_chart_axes,
                          basemap, fire_pts, col_fire, font_year, 
                          font_main_title, year, col_bg, height, width, file_out){
  # Build output path
  out_path <- sprintf("3_visualize/out/anim_frames/%s", file_out)
  
  # Plot constituent plots 
  plot_left <- build_map(basemap = basemap, fire_pts = fire_pts, year = year,
                         col_fire = col_fire, font_year)
  plot_right <- build_graph(chart_data = chart_data, 
                            year = year,
                            font_chart_titles = font_chart_titles, 
                            font_chart_axes = font_chart_axes, 
                            col_lines = col_lines)
  
  # Combine plots
  plot_grid(plot_left, plot_right, nrow = 1) + 
    
    # Add year
    draw_label(label = floor(year), x = 0.03, y = 0.1, hjust = 0,
               size = 20, color = "gray70", fontfamily = font_year, fontface = "bold")

  # Export data
  ggsave(filename = out_path,
         bg = col_bg, height = height, width = width, units = "in", dpi = 300)
  
  return(out_path)
}

#' Animate frames into a gif
#' 
#' Using input frames, animate into a gif, interpolating as necessary.
#' 
#' @param in_frames Filepaths to frames to animate.
#' @param out_file File path, name, and extension of animated gif.
#' @param inter_frames Number of interpolated frames between frames.
#' @param reduce Reduce file size by using only 256 colors. Must have gifsicle 
#'   installed (can be installed with NodeJs (https://www.npmjs.com/package/gifsicle).
#' @param frame_delay_cs Delay after each frame in 1/100 seconds.
#' @param frame_rate Frames per second.
#' 
animate_plots <- function(in_frames, out_file, inter_frames, reduce = TRUE, frame_delay_cs, frame_rate){
  in_frames %>%
    image_read() %>%
    image_resize("65x65%") %>%
    image_join() %>%
    image_morph(frames = inter_frames) %>%
    image_animate(
      delay = frame_delay_cs,
      optimize = TRUE,
      fps = frame_rate
    )%>%
    image_write(out_file)
  
  if(reduce == TRUE){
    optimize_gif(out_file, frame_delay_cs)
  }
}

optimize_gif <- function(out_file, frame_delay_cs) {
  
  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 -d %s --colors 256 %s',
                              frame_delay_cs, out_file)
  system(gifsicle_command)
  
  return(out_file)
}
