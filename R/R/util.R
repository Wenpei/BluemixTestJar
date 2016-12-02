
#' Automatic switch input value to required format
#' To DO ...
#'  1. switch for scala list?
#'  2. switch for other scala format
#'
#'
#'
spss.util.parameterSwitch <- function(
  parameterMap,
  parameterName,
  parameterValue
){
  parameterType <- parameterMap[parameterName]
  if(parameterType == "StringArrayParam"){
    return(as.array(parameterValue))
  }else if(parameterType == "IntParam"){
    return(as.integer(parameterValue))
  }else if(parameterType == "DoubleParam"){
    return(parameterValue)
  }else if(parameterType == "Param[String]"){
    return(as.character(parameterValue))
  }
  return(parameterValue)
}

# Should we add support for parameter set for score method
spss.predict_internal <- function(object, newData) {
  SparkR:::dataFrame(SparkR:::callJMethod(object@jobj, "transform", newData@sdf))
}

spss.summary_internal <- function(object){
  jobj <- object@jobj
  pmml <- SparkR:::callJMethod(jobj, "pmml")
  statxml <- SparkR:::callJMethod(jobj, "statxml")
  list(pmml = pmml, statxml = statxml)
}

spss.write_internal <- function(object, path, overwrite = FALSE) {
  writer <- SparkR:::callJMethod(object@jobj, "write")
  if (overwrite) {
    writer <- SparkR:::callJMethod(writer, "overwrite")
  }
  invisible(SparkR:::callJMethod(writer, "save", path))
}

spss.load_internal <- function(path) {
  path <- suppressWarnings(normalizePath(path))
  jobj <- SparkR:::callJStatic("com.ibm.spss.ml.r.RWrapper", "load", path)
  SPSSModel(jobj = jobj)
}
