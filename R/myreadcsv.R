#' My function to read data
#'
#' More detailed description.
#'
#' @param dird directory
#' @param csv csv file
#'
#' @importFrom utils read.table
#'
#' @export
myreadcsv = function(dird, csv) {
  if (missing(dird)) {
    fl=paste(getwd(),csv,sep="/")
    read.table(file = fl, header = TRUE, sep = ",")
  } else {
    fl=paste(dird,csv,sep="/")
    read.table(file = fl, header = TRUE, sep = ",")
  }

}
