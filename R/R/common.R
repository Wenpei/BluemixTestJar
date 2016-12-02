setOldClass("jobj")

SPSSModel <-
  setClass("SPSSModel", slots = list(jobj = "jobj"))

#'
#'
#'
#' @param object A fitted MLlib model
#' @param newData a SparkDataFrame for testing.
#' @return a List with Two Step Model summary.
#' @rdname spss.summary
#' @export
#'
setMethod("spss.summary.xml", signature(object = "SPSSModel"),
          function(object){
            spss.summary_internal(object)
          })


#'
#' @param object A fitted SPSS Model
#' @param newData a DataFrame for testing.
#' @return returns a DataFrame containing predicted column
#' @name spss.predict
#' @export
setMethod("spss.predict", signature(object = "SPSSModel"),
          function(object, newData) {
            spss.predict_internal(object, newData)
          })

#'
#' @param object A fitted SPSS Model
#' @param path a folder path for mode export.
#' @param overwrite Overwrites or not if the output path already exists. Default is FALSE
#'                  which means throw exception if the output path exists.
#' @name spss.save
#' @export
setMethod("spss.save", signature(object = "SPSSModel"),
          function(object, path, overwrite = FALSE) {
            spss.write_internal(object, path, overwrite)
          })

#'
#' @param path a folder path for mode export.
#' @name spss.load
#' @export
spss.load <- function(path) {
            spss.load_internal(path)
          }
