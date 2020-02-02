##************************************##
##        Main pipeline function      ##
##************************************##


# Argument to save intermediate marker files?
BVA_to_R <- function(path, # path to folder with .vmrk files
                     ERP,
                     var_labs,
                     samp_freq_Hz,
                     full_window_bounds,
                     filename_returned = NULL) {

  #' Pipeline from BVA exports to R dataframe.
  #'
  #' @description Take BVA exports and return large csv file
  #' with eveyrthing summarised.
  #' @param path Full path to all .vmrk and .dat files.
  #' @param ERP List of ERP components and their lower/upper bounds.
  #' @param var_labs Variable labels to assign to filename
  #' parts separated by underscores e.g. c("subject", "condition").
  #' @param samp_freq_Hz Sampling frequency in Hz.
  #' @param full_window_bounds Bounds of time window is ms e.g. c(-200, 1198).
  #' @param filename_returned "filename.csv" if you want a .csv returned/
  #' @importFrom magrittr %>%
  #' @importFrom  pbapply pblapply
  #' @import tidyverse
  #' @import data.table
  #' @import stringr
  #' @return Formatted dataframe.
  #'
  #' @export

  cat("Converting .vmrks to .csv ... \n")

  # First convert .vmrk files - make a check here
  invisible(
    change_file_endings(path = path,
                        ending_detect = ".vmrk",
                        ending_replace = ".csv")
  )

  cat("Processing marker files ... \n")


  # List and convert marker files
  marker_files <- list.files(path = path,
                             pattern =  ".csv",
                             full.names = T)

  list_marker_files <- pblapply(marker_files, function(file) {

    get_markers(file, var_labs = var_labs)
  })

  big_marker_file <- rbindlist(list_marker_files)


  # List and convert voltage files (.dat)
  cat("\n Processing voltage files ... \n")


  voltage_files <- list.files(path = path,
                              pattern =  ".dat",
                              full.names = T)

  list_voltage_files <- pblapply(voltage_files, function(file) {

    get_voltages(file, var_labs = var_labs,
                 ERP = ERP, samp_freq_Hz = samp_freq_Hz,
                 full_window_bounds = full_window_bounds)
  })

  big_voltage_file <- rbindlist(list_voltage_files)


  # Merge both together in big file
  cat("\n Merging voltage and marker files ... \n")

  result_file <- big_voltage_file %>%
    left_join(big_marker_file) %>%
    fill(marker_num:marker_order)

  if (is.null(filename_returned)) {
    return(result_file)
  } else {

    cat("'\n Writing file to .csv...")

    fwrite(result_file, file = filename_returned,
           sep = ",", row.names = F)

    cat("'\n Complete!")
  }
}
