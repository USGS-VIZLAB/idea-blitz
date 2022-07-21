source("3_visualize/src/chart_animation.R")
source("3_visualize/src/map_animation.R")

# Create animations which include:
#   1. A chart that illustrates fire effects on municipal watersheds over time (years)
#   2. A map that shows fire locations over time (years)
p3_targets <- list(
  
  # Build chart animation for graphs 
  tar_target(chart_animation_gif,
             build_graph(chart_data = chart_data, 
                         col_lines = c("#0abdc6", "#ea00d9"), 
                         file_out = "3_visualize/out/chart_animation.gif"),
             format = "file"),
  
  # Build frames for map animation
  tar_target(map_animation_png,
             build_map_png(basemap = basemap, fire_pts = map_data, year = Years_0.5,
                       col_fire = "#c94b10", col_bg = "#262626",
                       file_out = sprintf("map_%s.png",Years_0.5)),
             pattern = map(Years_0.5),
             format = "file")

  # Morph images into animation
  
  # Use image_append(c(map_animation_gif, chart_animation_gif)) to stitch gifs together
)