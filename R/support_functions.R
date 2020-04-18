##************************************##
## Support functions for EEG pipeline ##
##************************************##

#utils::globalVariables(c("."))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE



# Small helpers -----------------------------------------------------------



# Read vhdr meta file
get_vhdr <- function(filename,
                     path) {
  
  # Read in common infos part
  infos_vhdr <- ini::read.ini(filename)$`Common Infos`
  
  # Extract data
  samp_freq <- 1000000 / as.numeric(infos_vhdr$SamplingInterval)
  #total_datpoints <- as.numeric(test_vhdr$DataPoints)
  
  # Markerfile
  filename_markers <- paste0(path, infos_vhdr$MarkerFile)
  filename_voltages <- paste0(path, infos_vhdr$DataFile)
  
  # Put all in list
  list_vhdr <- list("samp_freq" = samp_freq, 
                    "filename_markers" = filename_markers,
                    "filename_voltages" = filename_voltages)
  
  return(list_vhdr)
}


get_metadata <- function(filename,
                         var_labs,
                         sep) {
  
  # Extract part of filenames without end
  name <- stringr::str_extract(filename, "[^/]+(?=\\.\\w+$)")
  
  # Split rest based on underscore and name columns
  name_split <- data.frame(
    stringr::str_split(name, sep, simplify = T),
    stringsAsFactors = F
  )
  
  # Make a check
  if (ncol(name_split) != length(var_labs))
    stop(paste0("var_labs should be of length ", ncol(name_split)))
  
  colnames(name_split) <- var_labs
  
  return(name_split)
}




get_markers <- function(filename) {

  #' Format .vmrk marker file.
  #'
  #' @description Format .vmrk marker file.
  #'
  #' @param filename Marker filename, full path.
  #'
  #' @return Formatted marker dataframe.
  #'
  #' @export

  
  # Separation of marker columns later
  column_seps <- c("marker", "descrip", "posit_datapoints", 
                   "size_datapoints", "channel_num", "date")
  
  # Get triggers
  triggers <- data.table::fread(
    filename, 
    fill = TRUE, 
    header = F, 
    sep = ";"
  ) %>%

    # Rename single column as 'main'
    data.table::setnames("main") %>%

    # Subset marker lines
    .data[, .SD[stringr::str_detect(.data$main, "^Mk")]] %>% 

    # Separate and label columns
    .data[, (column_seps) := data.table::tstrsplit(
      .data$main, split = ",", fixed = T
    )] %>% 
    
    # Separate marker column
    .data[, c("marker_num", "marker_type") := data.table::tstrsplit(
      .data$marker, split = "=", fixed = T
    )] %>% 

    # Keep only S markers for now
    .data[, .SD[stringr::str_detect(.data$descrip, "S")]] %>% 
    .data[, c("descrip", "posit_datapoints", "marker_num")] %>% 
    .data[, "posit_datapoints" := as.numeric(.data$posit_datapoints)] %>% 
    
    # We move posit data points so entire window is associated
    # with an S marker; and for merging with voltage file
    .data[, ':=' (
      "posit_onset" = .data$posit_datapoints,
      "posit_datapoints" = 1 + .data$posit_datapoints - .data$posit_datapoints[1]
    )]

  return(triggers)
}




ERP_to_times <- function(ERP_list, time_vec) {

  #' Convert ERP list to dataframe with times.
  #'
  #' @description Convert ERP list to dataframe with times.
  #'
  #' @param ERP_list List of ERP components and their lower/upper bounds.
  #' @param time_vec Vector of times.
  #'
  #' @return Dataframe with ERP and times.
  #'
  #' @export
  

  # Make t into a data.table
  time_dat <- data.table::data.table("time" = time_vec)
  
  # Collapse list
  ERP_dat <- do.call(rbind, ERP_list) %>%
    as.data.frame() %>% 
    
    # Change names and make long format
    data.table::setDT(keep.rownames = T) %>% 
    data.table::setnames(old = c("ERP", "low", "upp")) %>%
    data.table::melt.data.table(
      id.vars = "ERP",
      measure.vars = c("low", "upp"),
      variable.name = "bounds",
      value.name = "time"
    ) %>% 
    
    # Merge and label
    .data[order(.data$time), !"bounds"] %>% 
    data.table::merge.data.table(time_dat, all.y = T) %>% 
    .data[, "ERP" := zoo::na.locf(.data$ERP)] 

  return(ERP_dat)
}


# Main func, to apply across vhdr files -----------------------------------



one_vhdr <- function(path, 
                     filename, # of single vhdr filr
                     var_labs,
                     sep,
                     ERP_list,
                     full_window_bounds) {
  
  #' BVA_to_R function but for only one file
  #' 
  #' @param filename Vhdr filename, full path
  #' @inheritParams BVA_to_R
  #'
  #' @export
                  
  # Extract vhdr info
  vhdr_info <- get_vhdr(filename, path)
  
  # Read in voltage file and and datapoints cols
  voltages <- data.table::fread(
    file = vhdr_info$filename_voltages,
    sep = ";", 
    header = T
  ) %>% .data[, .data$posit_datapoints := 1:.N]
  
  # Read markers file
  markers <- BVAtoR::get_markers(vhdr_info$filename_markers)
  
  # Get meta data from filename - check first if double seps
  if (grepl(filename, pattern = paste(rep(sep, 2), collapse = ""))) {
    filename <- gsub(
      filename, pattern = paste(rep(sep, 2), collapse = ""), replacement = sep
    )
  }
  
  metadat <- get_metadata(
    filename,
    var_labs = var_labs, 
    sep = sep
  )
  
  # Make time vector
  time_vec <- seq(
    full_window_bounds[1], 
    full_window_bounds[2], 
    by = 1000 / vhdr_info$samp_freq
  )
  
  # Make ERP data.table
  ERP_dat <- ERP_to_times(ERP_list, time_vec)
  
  # Specify columns for indexing after
  cols <- c("descrip", "marker_num", "posit_onset")
  
  # Bind everything
  merged_dat <- data.table::merge.data.table(
    x = voltages,
    y = markers, 
    all.x = T
  ) %>% 
    data.table::setDT() %>% 
    .data[, (cols) := lapply(.SD, zoo::na.locf), .SDcols = cols] %>% 
    .data[, "time" := rep(time_vec, .N / length(time_vec))] %>% 
    #.[, time_onset := time[which(posit_datapoints == posit_onset)][1]]
    
    # Add ERPs and sort
    data.table::merge.data.table(y = ERP_dat, by = "time") %>% 
    .data[order(.data$posit_datapoints)] %>% 
    cbind(.data, metadat)
  
  return(merged_dat)
}



