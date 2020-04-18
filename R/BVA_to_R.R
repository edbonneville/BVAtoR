##************************************##
##        Main pipeline function      ##
##************************************##



#' Pipeline from BVA exports to R dataframe.
#'
#' @description Take BVA exports and return large csv file
#' with eveyrthing summarised.
#'
#' @param path Full path to all .vhdr, .vmrk and .dat files.
#' Of the form "path/to/file/" with the final forward slash.
#' If unspecified, defaults to current working directory.
#' @param sep Separation in the filename e.g. "_"
#' @param ERP_list Named list of ERP components and their lower/upper bounds.
#' @param var_labs Variable labels to assign to filename
#' parts separated by underscores e.g. c("subject", "condition").
#' @param full_window_bounds Bounds of time window is ms e.g. c(-200, 1198).
#' @param filename_returned "filename.csv" if you want a .csv returned/
#'
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @return Formatted data.table
#' 
#' @examples
#' \dontrun{
#' path <- "path/to/file/"
#' 
#' ERPs <- list(
#' "baseline" = c(-200, -2), 
#' "test_ERP" = c(0, 498),
#' "ERP1" = c(500, 1198)
#' )
#' 
#' obj <- BVA_to_R(
#' path = path,
#' sep = "_",
#' ERP_list = ERPs,
#' var_labs = c("subjID", "comp", "base", "cogn", "congr", "viol"),
#' full_window_bounds = c(-200, 1198)
#' )
#' }
#'
#' @export

BVA_to_R <- function(path,
                     ERP_list,
                     var_labs,
                     sep,
                     full_window_bounds,
                     filename_returned = NULL) {
  
  # Set path if missing
  if (missing(path)) path <- "" # assume all files are in current working 
  
  # List vhdr files in path
  vhdr_files <- list.files(
    path = path, 
    pattern = ".vhdr", 
    full.names = T
  )
  
  # Checks
  if (length(vhdr_files) == 0)
    stop("There are no vhdr files in the supplied path!")
  
  ERP_vec <- unlist(ERP_list)
  if (min(ERP_vec) != full_window_bounds[1] | 
      max(ERP_vec) != full_window_bounds[2])
    stop("Smallest/largest values of ERP bounds should = full_window_bounds")
  
  # Apply vhdr processing
  list_processed_dats <- lapply(vhdr_files, function(vhdr_filename) {
    
    BVAtoR::one_vhdr(
      path = path,
      filename = vhdr_filename, 
      sep = sep,
      ERP_list = ERP_list,
      var_labs = var_labs,
      full_window_bounds = full_window_bounds
    )
  })
  
  # Bind them together
  result_file <- data.table::rbindlist(list_processed_dats)

  # Write .csv, or just return df
  if (is.null(filename_returned)) {

    return(result_file)
  } else {

    data.table::fwrite(
      result_file, 
      file = filename_returned,
      sep = ",", 
      row.names = F
    )
  }
}
