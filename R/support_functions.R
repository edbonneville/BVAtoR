##************************************##
## Support functions for EEG pipeline ##
##************************************##

# For package checks
#utils::globalVariables(c("."))
#posit_datapoints <- descrip <- marker <- main <- ERP <- time <- NULL
.datatable.aware = TRUE



# Small helpers -----------------------------------------------------------



# Read vhdr meta file
get_vhdr <- function(filename, path) {

  # Read in common infos part
  infos_vhdr <- ini::read.ini(filename)[["Common Infos"]]

  # Extract data
  samp_freq <- 1000000 / as.numeric(infos_vhdr$SamplingInterval)
  #total_datpoints <- as.numeric(test_vhdr$DataPoints)

  # Markerfile
  filename_markers <- paste0(path, infos_vhdr[["MarkerFile"]])
  filename_voltages <- paste0(path, infos_vhdr[["DataFile"]])

  # Put all in list
  list_vhdr <- list(
    "samp_freq" = samp_freq,
    "filename_markers" = filename_markers,
    "filename_voltages" = filename_voltages
  )

  return(list_vhdr)
}


get_metadata <- function(filename,
                         var_labs,
                         sep) {

  # Extract part of filenames without end
  name <- regmatches(
    x = filename,
    m = regexpr(pattern = "[^/]+(?=\\.\\w+$)", text = filename, perl = TRUE)
  )

  # Split rest based on underscore and name columns
  name_split <- unlist(strsplit(name, split = "_"))

  # Make a check
  if (length(name_split) != length(var_labs))
    stop(paste0("var_labs should be of length ", ncol(name_split)))

  names(name_split) <- var_labs

  return(data.frame(t(name_split)))
}


get_markers <- function(filename, markers_exclude = NULL) {

  #' Format .vmrk marker file.
  #'
  #' @description Format .vmrk marker file.
  #'
  #' @param filename Marker filename, full path.
  #' @param markers_exclude Character vector of (response) markers to exclude e.g.
  #' "S240" or if all >200 then paste0("S", 201:300)
  #'
  #' @return Formatted marker dataframe.
  #'
  #' @export

  # For R check
  posit_datapoints <- main <- marker <- descrip <- NULL

  # Separation of marker columns later
  column_seps <- c("marker", "descrip", "posit_datapoints",
                   "size_datapoints", "channel_num", "date")

  # Get triggers
  triggers <- data.table::fread(
    filename,
    fill = TRUE,
    header = F,
    sep = ";"
  )

  # Rename single column as 'main'
  data.table::setnames(x = triggers, "main")
  triggers_subset <- triggers[grepl(pattern = "^Mk", main)]

  # Separate and label columns
  triggers_subset[, (column_seps) := data.table::tstrsplit(
    main, split = ",", fixed = T
  )]

  # Separate marker column
  triggers_subset[, c("marker_num", "marker_type") := data.table::tstrsplit(
    marker, split = "=", fixed = T
  )]

  triggers_subset[, setdiff(names(triggers_subset), c("descrip", "posit_datapoints", "marker_num")) := NULL]
  triggers_subset[, "posit_datapoints" := as.numeric(posit_datapoints)]
  triggers_markers_S <- triggers_subset[grepl(pattern = "^S", x = descrip)]

  # Also exclude any extra response markers
  if (!is.null(markers_exclude)) {
    triggers_markers_S <- triggers_markers_S[!(descrip %in% markers_exclude)]
  }

  triggers_markers_S[, ':=' (
    "posit_onset" = posit_datapoints,
    "posit_datapoints" = 1 + posit_datapoints - posit_datapoints[1]
  )]

  return(triggers_markers_S)
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

  # For R check
  ERP <- NULL

  # Make t into a data.table
  time_dat <- data.table::data.table("time" = time_vec)

  # Collapse list
  ERP_dat <- as.data.frame(do.call(rbind, ERP_list))

  # Change names and make long format
  data.table::setDT(ERP_dat, keep.rownames = T)
  data.table::setnames(ERP_dat, old = c("ERP", "low", "upp"))

  erp_long <- data.table::melt.data.table(
    data = ERP_dat,
    id.vars = "ERP",
    measure.vars = c("low", "upp"),
    variable.name = "bounds",
    value.name = "time"
  )

  times_merged <- data.table::merge.data.table(erp_long, time_dat, all.y = T)
  data.table::setorder(times_merged, "time")
  times_merged[, ':=' (
    "ERP" = zoo::na.locf(ERP),
    "bounds" = NULL
  )]

  return(times_merged)
}


# Main func, to apply across vhdr files -----------------------------------



one_vhdr <- function(path,
                     filename, # of single vhdr filr
                     var_labs,
                     sep,
                     ERP_list,
                     full_window_bounds,
                     markers_exclude = NULL) {

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
    file = vhdr_info[["filename_voltages"]],
    sep = ";",
    header = T
  )

  voltages[, "posit_datapoints" := seq_len(.N)]

  # Read markers file
  markers <- BVAtoR::get_markers(
    filename = vhdr_info[["filename_markers"]],
    markers_exclude = markers_exclude
  )

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
  time_vec <- seq(full_window_bounds[1], full_window_bounds[2], by = 1000 / vhdr_info[["samp_freq"]])

  # Make ERP data.table
  ERP_dat <- ERP_to_times(ERP_list, time_vec)

  # Specify columns for indexing after
  cols <- c("descrip", "marker_num", "posit_onset")

  # Bind everything
  merged_dat <- data.table::merge.data.table(
    x = voltages,
    y = markers,
    all.x = T
  )

  data.table::setDT(merged_dat)
  merged_dat[, (cols) := lapply(.SD, zoo::na.locf), .SDcols = cols]
  merged_dat[, "time" := rep(time_vec, .N / length(time_vec))]
    #.[, time_onset := time[which(posit_datapoints == posit_onset)][1]]

  # Add ERPs and sort
  merged_erp <- data.table::merge.data.table(x = merged_dat, y = ERP_dat, by = "time")
  data.table::setorder(merged_erp, "posit_datapoints")


  return(cbind(merged_erp, metadat))
}

