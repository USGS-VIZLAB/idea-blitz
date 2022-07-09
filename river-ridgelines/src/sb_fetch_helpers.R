###################################################################################################
# PULLED FROM: 
# https://code.usgs.gov/wma/proxies/habs/wq-data-prep/-/blob/main/1_fetch/src/sb_fetch_helpers.R 
###################################################################################################

# Helper functions for finding and downloading SB data

#' @title Download files from ScienceBase
#' @description Download specific ScienceBase files from one ScienceBase item to a local
#' directory. Set up to retry at least 3 times if there is an error to handle network flakiness.
#' @param sb_id character string of the ScienceBase item to inventory
#' @param sb_files_to_download character vector of the filenames on ScienceBase. Designed to work
#' with output from `inventory_sb_pcode_files()`.
#' @param dest_dir character string of the local directory (which should exist already) to
#' download the ScienceBase files to.
#' @param sb_secret_exists logical indicating whether or not the user has local
#' ScienceBase credentials stored for `login_with_local_sb_credentials()` to use.
download_sb_files <- function(sb_id, sb_files_to_download, dest_dir, sb_secret_exists) {
  
  # Login to SB first first. Need to do this for each task that calls
  stopifnot(sb_secret_exists)
  login_with_local_sb_credentials()
  
  # Download the files and save in the local directory
  files_local_name <- file.path(dest_dir, sb_files_to_download)
  files_out <- retry(
    sbtools::item_file_download(
      sb_id,
      names = sb_files_to_download,
      destinations = files_local_name,
      overwrite_file = TRUE),
    when = "Error:",
    max_tries = 3)
  
  # `item_file_download()` returns the local filepaths
  # so pass these on to the user
  return(files_out)
}
