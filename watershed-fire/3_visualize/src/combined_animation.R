# Build static map for given year
build_map <- function(basemap, fire_pts, year, col_fire){
  # Load basemap
  basemap <- rast(basemap)
  
  # Filter fire points to given year
  fire_pts_year <- fire_pts %>%
    filter(Year == year)
  
  # Fiter fire points to all years prior to given year
  fire_pts_past <- fire_pts %>%
    filter(Year < year)
  
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
    scale_size(range = c(.1, 1)) +
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
    scale_size(range = c(.1, 1.5)) +
    new_scale("size") +
    geom_glowpoint(data = fire_pts_year,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc), 
                   alpha = .6,
                   shadowalpha = .05,
                   color = "#ffffff",
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.01, .7)) +
    
    # Styling
    theme_void() +
    
    # Print year text
    geom_text(aes(x= -Inf, y = -Inf, hjust = -0.5, vjust = -1.2,
                  label = ifelse(year %% 1 == 0, year, "")),
              size = 10, color = "gray70", fontface = "bold")
}


# Build static map for given year
build_graph <- function(chart_data, col_lines, year){
  # Filter fire points to given year
  chart_data_line <- chart_data 
  
  chart_data_point <- chart_data %>%
    filter(Year == year)
  
  # Build charts
  ggplot() +
    # Plot line graph
    geom_glowline(data = chart_data, aes(x = Year, y = y, color = name)) +
    # Plot points with alternated Year column (so entire lines are static and only point moves)
    geom_glowpoint(data = chart_data_point, 
                   aes(x = Year, y = y, color = name), size = 2) +
    
    # Styling
    facet_wrap(~ name_f, ncol = 1, scales = "free_y") +
    scale_color_manual(values = col_lines) +
    ylab(NULL) +
    theme(plot.background = element_rect(fill = "#262626", color = NA),
          panel.background = element_rect(fill = "#262626", color = NA),
          strip.background = element_blank(),
          strip.text = element_text(color = "gray70", size = 10, face = "bold"),
          strip.placement = "outside",
          panel.spacing = unit(1/8, "in", data = NULL),
          legend.position = "none",
          axis.title.x = element_text(color = "gray70", size = 10),
          panel.grid = element_line(color = "gray40"),
          axis.text = element_text(color = "gray40"))
}

combine_plots <- function(chart_data, col_lines,
                          basemap, fire_pts, col_fire,
                          year, col_bg, height, width, file_out){
  out_path <- sprintf("3_visualize/out/anim_frames/%s", file_out)
  
  plot_left <- build_map(basemap = basemap, fire_pts = fire_pts, year = year,
                         col_fire = col_fire)
  
  plot_right <- build_graph(chart_data = chart_data, year = year,
                            col_lines = col_lines)
  
  plot_grid(plot_left, plot_right, nrow = 1)
  
  # Export data
  ggsave(filename = out_path,
         bg = col_bg, height = height, width = width, units = "in", dpi = 300)
  
  return(out_path)
}
