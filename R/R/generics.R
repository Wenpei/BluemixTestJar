
#' @rdname clustering.twostep
#' @export
setGeneric("clustering.twostep", function(data, ...){ standardGeneric("clustering.twostep") })


#' @rdname classificationandregression.linearregression
#' @export
setGeneric("classificationandregression.linearregression", function(data, ...){ standardGeneric("classificationandregression.linearregression") })

#' @rdname spss.predict
#' @export
setGeneric("spss.predict", function(object, ...) { standardGeneric("spss.predict") })

#' @param object a fitted ML model object.
#' @rdname spss.summary
#' @export
setGeneric("spss.summary", function(object) { standardGeneric("spss.summary") })

#' @param object a fitted ML model object.
#' @param path a folder path for mode export.
#' @rdname spss.save
#' @export
setGeneric("spss.save", function(object, path, ...) { standardGeneric("spss.save") })

#' @param path a folder path for mode load.
#' @rdname spss.load
#' @export
setGeneric("spss.load", function(path) { standardGeneric("spss.load") })
