setOldClass("jobj")

#' S4 class that represents an TwoStep Model
#'
#' @param jobj a Java object reference to the Wrapper
#' @export
#' @note TwoStep for SPSS on SPARK
TwoStepModel <-
  setClass("TwoStepModel", slots = c(name = "character"), contains = "SPSSModel")
#setClass("TwoStepModel", representation(jobj = "jobj"))

#' Two-Step Cluster
#'
#'
#' Scalable Two-Step is based on the familiar two-step clustering algorithm but extends both
#' its functionality and performance in several directions.
#' First, it can effectively work with large and distributed data supported by Spark that
#' provides the Map-Reduce computing paradigm.
#' Second, the algorithm provides mechanisms for selecting the most relevant features for
#' clustering the given data, as well as detecting rare outlier points. Moreover, it provides
#' an enhanced set of evaluation and diagnostic features for enabling insight.
#' The algorithm of two-step clustering first performs a pre-clustering step by scanning the
#' entire dataset and storing the dense regions of data cases in terms of summary statistics
#' called cluster features. The cluster features are stored in memory in a data structure
#' called the CF-tree. Finally, an agglomerative hierarchical clustering algorithm is applied
#' to cluster the set of cluster features.

#' @param inputFieldList Param for a list of input fields.
#' @param categoryMajorityThreshold Categorical fields with a percentage of cases in a single category
#'                        greater than the specified value are excluded from the analysis. The value
#'                        must be greater than 0 and less than 1. Default: 0.95.
#' @param outlierHandling Param for outlier handling option. Default: true.
#' @param maxClusterNum Param for the upper range of cluster number if it's RANGE. Must be >
#'                        0. Default: 15.
#' @param autoClusteringMethod If you set autoClustering = True, choose from the following
#'                        clustering methods used to automatically determine the number of clusters: -
#'                        "CRITERION": Information criteria convergence is the ratio of information
#'                        criteria corresponding to two current cluster solutions and the first cluster
#'                        solution. The criterion used is the one selected in the Clustering Criterion
#'                        group. - "DISTANCEJUMP": Distance jump is the ratio of distances corresponding
#'                        to two consecutive cluster solutions. - "MAXIMUM": Combine results from the
#'                        information criteria convergence method and the distance jump method to
#'                        produce the number of clusters corresponding to the second jump. - "MINIMUM":
#'                        Combine results from the information criteria convergence method and the
#'                        distance jump method to produce the number of clusters corresponding to the
#'                        first jump Default: MINIMUM
#' @param standardizeFieldList Parameter for specifying one set of input continuous fields that
#'                        needs to be standardized. Every field in this set must have two statistics
#'                        "mean" and "variance" in the input data model, otherwise this field will be
#'                        not standardized. For example about data model: <Field measure="continuous"
#'                        name="Cost" role="input" storage="real"> <ContinuousStatistics mean="54.909"
#'                        variance="806.252" sumCompWts="200" sumFreqWts="200"/> </Field>
#' @param featureImportanceMethod Feature Importance Method determines how important the features
#'                        (fields) are in the cluster solution. The output includes information about
#'                        overall feature importance and the importance of each feature field in each
#'                        cluster. Features that do not meet a minimum threshold are excluded: -
#'                        "CRITERION": Based on the criterion that is selected in the Clustering
#'                        Criterion group. - "EFFECTSIZE": Feature importance is based on effect size
#'                        instead of significance values. Default: CRITERION
#' @param featureSelection Param for doing adaptive feature selection or not. Default: true. The
#'                        adaptive feature selection is a heavyweight operation,so it's time-consuming;
#'                        the required time is proportional to the number of fields.
#' @param maxEntNonLeaf Param for the max number of entries that a non leaf node can hold.
#'                        Must be > 0. Default: 8.
#' @param missingValueThreshold Param for the portion of missing value in any feature. Must be >= 0
#'                        and <= 1. Default: 0.7. If the missing value portion larger than this value,
#'                        exclude the feature.
#' @param outlierClusterNum Parameter for specifying the number of the most extreme outlier
#'                        clusters. Must be > 0. Default: 20. Use to specify the number outliers to
#'                        export, used only on outlierHandling=true.
#' @param outlierThreshold Param for leaf entry which contains less than this value will
#'                        consider as outlier. Must be >= 2. Default: 10.
#' @param maxEntLeaf Param for the max number of entries that a leaf node can hold. Must
#'                        be > 1. Default: 8.
#' @param minFrequency Param for the minimum portion of feature frequency in Feature
#'                        Selection. Must be > 0 and < 1. Default: 0.5. This parameter is used for
#'                        adaptive feature selection.
#' @param autoClustering If set to True, the algorithm will automatically determine the best
#'                        number of clusters, within the specified range. Default: true.
#' @param sigLevel Param for significance level for computing feature importance. Must
#'                        be > 0 and < 1. Default: 0.05. This is used only when
#'                        featureImpoMethod=EFFECTSIZE.
#' @param featureFiltering Param for doing feature filtering or not. Default: true.
#' @param coefficientThreshold Param for the limit of coefficient of variation of a continuous
#'                        feature. Must be >= 0 and <= 1. Default: 0.05. If the coefficient of variation
#'                        of a continuous feature is smaller than this value, exclude the feature.
#' @param categoryCountThreshold Categorical fields with more than the specified number of categories
#'                        are excluded from the analysis. The value must be a positive integer greater
#'                        than 0. Default: 24.
#' @param distMeasure This selection determines how the similarity between two clusters is
#'                        computed: - "LOGLIKELIHOOD": The likelihood measure places a probability
#'                        distribution on the fields. Continuous fields are assumed to be normally
#'                        distributed, while categorical fields are assumed to be multinomial. All
#'                        fields are assumed to be independent. - "EUCLIDEAN": The Euclidean measure is
#'                        the "straight line" distance between two clusters. Squared Euclidean measure
#'                        and the Ward method are used to compute similarity between clusters. It can be
#'                        used only when all of the fields are continuous. Default: LOGLIKELIHOOD
#' @param maxTreeHeight Param for maximum height of the Tree. Must be > 0. Default: 3.
#' @param minClusterNum Param for the lower range of cluster number if it's RANGE. Must be >
#'                        1. Default: 2. This value should smaller or equal with maxClusterNum.
#' @param fixedClusterNum Param for the number of cluster if it's FIXED. Must be > 1. Default:
#'                        5.
#' @param informationCriterion This selection controls how the automatic clustering algorithm
#'                        determines the number of clusters: - "BIC": A measure for selecting and
#'                        comparing models based on the -2 log likelihood. Smaller values indicate
#'                        better models. The BIC also "penalizes" overparameterized models (complex
#'                        models with a large number of inputs, for example), but more strictly than the
#'                        AIC. - "AIC": A measure for selecting and comparing models based on the -2 log
#'                        likelihood. Smaller values indicate better models. The AIC "penalizes"
#'                        overparameterized models (complex models with a large number of inputs, for
#'                        example). Default: BIC
#' @name clustering.twostep
#' @export
#' @examples
#' \dontrun{
#' sc <- sparkR.init()
#' data(iris)
#' sqlContext <- sparkRSQL.init(sc)
#' df <- createDataFrame(sqlContext, iris)
#' twostepmodel <- clustering.twostep(df,~Sepal_Length+Sepal_Width)
#'
#' scoredata <- spss.predict(twostepmodel,df)
#' head(scoredata)
#' }
setMethod("clustering.twostep", signature(data = "DataFrame"),
          function(
            data,
            formula = NULL,
            inputFieldList = c(),
            standardizeFieldList = c(),
            autoClustering = TRUE,
            featureSelection = TRUE,
            outlierHandling = TRUE,
            ...){
            TwoStepModel(
                SPSSModel(jobj = spss.TwoStep.fit(data, formula, inputFieldList, standardizeFieldList,
                                                  autoClustering = autoClustering,
                                                  featureSelection = featureSelection,
                                                  outlierHandling = outlierHandling,
                                                  ...)),
                name = "TwoStepModel")
          })

# Major function call for twostep
spss.TwoStep.fit <- function(
  data,
  formula,
  inputFieldList,
  standardizeFieldList,
  ...
){
  twostep <- SparkR:::callJStatic("com.ibm.spss.ml.clustering.TwoStep","apply")
  advanceArgs = list(...)
  lapply(seq_along(advanceArgs), function(i){
    parameterName <- names(advanceArgs[i])
    head <- toupper(substr(parameterName,0,1))
    tril <- substring(parameterName,2)
    functionName <- paste("set",head,tril,sep="")
    #print(functionName)
    value <- spss.util.parameterSwitch(spss.TwoStep.parameterType, parameterName, advanceArgs[[i]])
    print(paste(functionName, "(", value, ")"))
    SparkR:::callJMethod(twostep,functionName,value)
  })
  print("Done Parameter set")
  if(!is.null(formula)){
    formula <- paste(deparse(formula), collapse = "")
    rmodel <- SparkR:::callJStatic("com.ibm.spss.ml.r.RWrapper","fitWithRFormula", twostep, formula, df@sdf)
  }else{
    SparkR:::callJMethod(twostep,"setInputFieldList",as.array(inputFieldList))
    if(!is.null(standardizeFieldList))
      SparkR:::callJMethod(twostep,"setStandardizeFieldList",as.array(standardizeFieldList))
    rmodel <- SparkR:::callJStatic("com.ibm.spss.ml.r.RWrapper","fit", twostep, df@sdf)
  }
  rmodel
}

setMethod("spss.summary", signature(object = "TwoStepModel"),
          function(object){
            summary = SparkR:::callJMethod(object@jobj,"summary")
            list(
              modelgoodness = SparkR:::callJMethod(summary,"get","modelgoodness"),
              modelcriterion = SparkR:::callJMethod(summary,"get","modelcriterion"))
          })

spss.TwoStep.parameterType <- list(
  categoryMajorityThreshold = "DoubleParam",
  outlierHandling = "BooleanParam",
  maxClusterNum = "IntParam",
  autoClusteringMethod = "Param[String]",
  standardizeFieldList = "StringArrayParam",
  featureImportanceMethod = "Param[String]",
  featureSelection = "BooleanParam",
  maxEntNonLeaf = "IntParam",
  missingValueThreshold = "DoubleParam",
  outlierClusterNum = "IntParam",
  outlierThreshold = "IntParam",
  maxEntLeaf = "IntParam",
  minFrequency = "DoubleParam",
  autoClustering = "BooleanParam",
  sigLevel = "DoubleParam",
  featureFiltering = "BooleanParam",
  coefficientThreshold = "DoubleParam",
  categoryCountThreshold = "IntParam",
  distMeasure = "Param[String]",
  maxTreeHeight = "IntParam",
  minClusterNum = "IntParam",
  fixedClusterNum = "IntParam",
  informationCriterion = "Param[String]"
)
