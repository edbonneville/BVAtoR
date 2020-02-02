##************************************##
## Support functions for EEG pipeline ##
##************************************##


# Change format of many files by changing file endings eg. .csv
change_file_endings <- function(path,
                                ending_detect,
                                ending_replace) {

  #' Mass change file endings.
  #'
  #' @description Changes file types by editing endings.
  #' @param path A number.
  #' @param ending_detect File ending to change e.g. ".vmrk"
  #' @param ending_replace File ending to change e.g. ".csv"
  #' @return Nothing in console, but files will be renamed.
  #' @examples
  #' change_file_endings("project_files/", ".vmrk", ".csv")
  #'
  #' @export

  # Get character vector of all file names
  old_files <- list.files(path = path,
                          pattern =  paste0("*", ending_detect),
                          full.names = T)

  # Convert to .csv
  new_files <- gsub(paste0(ending_detect, "$"),
                    ending_replace, old_files)

  file.rename(old_files, new_files)
}


get_voltages <- function(filename,
                         var_labs,
                         ERP,
                         samp_freq_Hz,
                         full_window_bounds) {

  #' Format .dat voltage file.
  #'
  #' @description Format .dat voltage file.
  #' @param filename Voltage filename, full path.
  #' @param var_labs Variable labels to assign to filename
  #' parts separated by underscores e.g. c("subject", "condition").
  #' @param ERP List of ERP components and their lower/upper bounds.
  #' @param samp_freq_Hz Sampling frequency in Hz.
  #' @param full_window_bounds Bounds of time window is ms e.g. c(-200, 1198).
  #' @importFrom magrittr %>%
  #' @import tidyverse
  #' @return Formatted voltage dataframe.
  #'
  #' @export

  dat_vol <- read.table(filename,
                        sep = ";", header = T)

  # separate from .csv ending
  name <- str_match(filename, "[^/]+(?=\\.\\w+$)")[1]

  # Split rest based on underscore and name columns
  name_split <- data.frame(str_split(name, "_", simplify = T), stringsAsFactors = F)

  colnames(name_split) <- var_labs

  obj_datapoints <- cbind.data.frame(dat_vol, name_split) %>%
    mutate(posit_datapoints = 1:n())

  obj <- append_times_ERP(voltage_dat = obj_datapoints,
                          ERP = ERP,
                          samp_freq_Hz = samp_freq_Hz,
                          full_window_bounds = full_window_bounds)

  return(obj)
}

get_markers <- function(filename,
                        var_labs) {

  #' Format .vmrk marker file.
  #'
  #' @description Format .vmrk marker file.
  #' @param filename Marker filename, full path.
  #' @param var_labs Variable labels to assign to filename
  #' parts separated by underscores e.g. c("subject", "condition").
  #' @importFrom magrittr %>%
  #' @import tidyverse
  #' @return Formatted marker dataframe.
  #'
  #' @export

  # Get triggers
  triggers <- read.csv(filename, sep = "\n") %>%

    # rename single column as 'main'
    dplyr::rename(main = names(.)) %>%

    # Subset marker lines, ^ means at beginning
    filter(str_detect(main, "^Mk")) %>%

    # Separate and label columns
    separate(main, c("marker", "descrip", "posit_datapoints",
                     "size_datapoints", "channel_num", "dat"),
             sep = ",", fill = "right", convert = T) %>%

    # Separate marker columns
    separate("marker", c("marker_num", "marker_type"), sep = "=") %>%

    # Select S markers
    #select(descrip) %>%
    filter(str_detect(descrip, "S")) %>%
    mutate(marker_order = 1:n()) %>%
    mutate(posit_onset = posit_datapoints,
           posit_datapoints = 1 + posit_datapoints - posit_datapoints[1])

  # separate from .csv ending
  name <- str_match(filename, "[^/]+(?=\\.\\w+$)")[1]

  # Split rest based on underscore and name columns
  name_split <- data.frame(str_split(name, "_", simplify = T),
                           stringsAsFactors = F)
  colnames(name_split) <- var_labs

  # Final marker data
  obj <- cbind.data.frame(triggers, name_split)

  return(obj)
}



append_times_ERP <- function(voltage_dat,
                             ERP,
                             samp_freq_Hz,
                             full_window_bounds) {

  #' Append ERPs to voltage file.
  #'
  #' @description Append ERPs to voltage file.
  #' @param voltage_dat Formatted voltage dataframe.
  #' @param ERP List of ERP components and their lower/upper bounds.
  #' @param samp_freq_Hz Sampling frequency in Hz.
  #' @param full_window_bounds Bounds of time window is ms e.g. c(-200, 1198).
  #' @importFrom magrittr %>%
  #' @import tidyverse
  #' @import data.table
  #' @return Voltage with ERPs appropriately appendended (dataframe).
  #'
  #' @export

  sample_step <- 1000 / samp_freq_Hz

  time_samples <- seq(full_window_bounds[1],
                      full_window_bounds[2],
                      by = sample_step)

  rep_ind <- nrow(voltage_dat) / length(time_samples)

  # if is.null ERP

  ERP_dat <- ERP_to_times(ERP, t = time_samples)

  dat_appended <- setDT(voltage_dat) %>%
    cbind(., ERP_dat[rep(seq_len(nrow(ERP_dat)), rep_ind), ])

  return(dat_appended)
}


ERP_to_times <- function(ERP_list, t) {

  #' Convert ERP list to dataframe with times.
  #'
  #' @description Convert ERP list to dataframe with times.
  #' @param ERP_list List of ERP components and their lower/upper bounds.
  #' @param t Vector of times.
  #' @importFrom magrittr %>%
  #' @import tidyverse
  #' @return Dataframe with ERP and times.
  #'
  #' @export

  # Collapse list
  ERP_dat <- as.data.frame(do.call(rbind, ERP))
  colnames(ERP_dat) <- c("low", "high")

  ERP_dat <- ERP_dat %>%
    rownames_to_column(var= "ERP") %>%
    gather(bound, t, low, high) %>%
    select(-bound) %>%
    right_join(data.frame(t = t), by = "t") %>%
    fill(ERP)

  return(ERP_dat)
}
