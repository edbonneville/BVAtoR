##************************************##
##        Main pipeline function      ##
##************************************##



# Argument to save intermediate marker files?
BVA_to_R <- function(path,
                     ERP,
                     var_labs,
                     samp_freq_Hz,
                     full_window_bounds,
                     save_markers = NULL,
                     save_voltages = NULL,
                     filename_returned = NULL) {

  #' Pipeline from BVA exports to R dataframe.
  #'
  #' @description Take BVA exports and return large csv file
  #' with eveyrthing summarised.
  #'
  #' @param path Full path to all .vmrk and .dat files.
  #' @param ERP Named list of ERP components and their lower/upper bounds.
  #' @param var_labs Variable labels to assign to filename
  #' parts separated by underscores e.g. c("subject", "condition").
  #' @param samp_freq_Hz Sampling frequency in Hz.
  #' @param full_window_bounds Bounds of time window is ms e.g. c(-200, 1198).
  #' @param filename_returned "filename.csv" if you want a .csv returned/
  #' @param save_markers (Optional) filename for marker csv file
  #' @param save_voltages (Optional) filename for voltages csv file
  #'
  #' @importFrom magrittr %>%
  #' @importFrom  pbapply pblapply
  #' @import dplyr
  #' @importFrom data.table rbindlist fwrite fread
  #' @import stringr
  #' @import tidyr
  #' @importFrom tibble rownames_to_column
  #' @importFrom utils read.csv read.table
  #' @importFrom rlang .data
  #'
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

  if (!is.null(save_markers)) {
    fwrite(big_marker_file, file = save_markers,
           sep = ",", row.names = F)
  }

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

  if (!is.null(save_voltages)) {
    fwrite(big_voltage_file, file = save_voltages,
           sep = ",", row.names = F)
  }


  # Merge both together in big file
  cat("\n Merging voltage and marker files ... \n")

  result_file <- big_voltage_file %>%
    left_join(big_marker_file) %>%
    fill_(marker_num:marker_order)

  # Write .csv, or just return df
  if (is.null(filename_returned)) {

    return(result_file)
  } else {

    cat("'\n Writing file to .csv...")

    fwrite(result_file, file = filename_returned,
           sep = ",", row.names = F)

    cat("'\n Complete!")
  }
}
