setOldClass("jobj")

#' S4 class that represents an LinearRegressionModel Model
#'
#' @param jobj a Java object reference to the Wrapper
#' @export
#' @note LinearRegressionModel for SPSS on SPARK
LinearRegressionModel <-
  setClass("LinearRegressionModel", slots = c(name = "character"), contains = "SPSSModel")
#' The Linear Regression Model
#'
#' The linear regression model analyzes the predictive relationship between a continuous target and one
#'  or more predictors which can be continuous or categorical.
#'
#' Features of the linear regression model include automatic interaction effect detection, forward
#' stepwise model selection, diagnostic checking, and unusual category detection based on EMMEANS.
#'
#' @param randomSeed Param for specifying the random seed value for ASE as a method. Must
#'                        be > 0. Default: 54752075.
#' @param PIn Param for significance level for F,,enter,, value. Must be > 0 and <
#'                        1. Default: 0.05.
#' @param maxParamsAllEffects Param for specifying the maximum number of parameters the system can
#'                        handle. Must be > 0. Default: 5000.
#' @param numOfExportedOutliers Param for number of exported influential outliers sorted with
#'                        descending Cook's distances. Must be a positive number. Default: 100. This
#'                        parameter is active when detectInfluentialOutlier = true.
#' @param criteria4ForwardStepwise Param for criteria used to enter or remove an effect in each step in
#'                        forward stepwise: - "adjustedRSquare" - "AICC" - "ASE" - "FStatistics"
#'                        Default: adjustedRSquare This parameter is active when varSelectionMethod =
#'                        forwardStepwise.
#' @param CIn Param for specifying a confidence interval level. Must be > 0.0 and <
#'                        1.00. Default: 0.95.
#' @param factorSortOrder Param for specifying the sorting order for factors: - "ASCENDING" -
#'                        "DESCENDING" Default:ASCENDING
#' @param detectInfluentialOutliers Param for detecting influential outliers and exporting the related
#'                        statistics. Default: true.
#' @param detectTwoWayInteraction Param for two-way interaction detection. It is a sub-option under
#'                        variable selection. Detect squared term of each covariate will be created and
#'                        included in the design matrix. Detect each pair of two factors should be
#'                        included in the design matrix. Detect each pair of one covariate and one
#'                        factor should be included in the design matrix. Default:true.
#' @param varSelectionMethod Param for specifying a model selection method: - "bestSubsets" -
#'                        "forwardStepwise" - "none" Default: forwardStepwise
#' @param maxSteps Param for maximum number of steps for forwardStepwise as model
#'                        selection method. Must be a positive number. Default: Three times maxEffects.
#' @param maxEffects4AllPossibleSubsets Param for the maximum number of effects for all possible subsets in
#'                        "Best subsets" method. Must be a positive number. Default: 20. This parameter
#'                        is active when varSelectionMethod = bestSubsets.
#' @param detectInteractionMaxIter Param for specifying the maximum number of iterations in an iterative
#'                        process of detecting interaction of two factors. Must be > 0. Default: 10.
#' @param forwardStepwiseRule Param for specifying a rule for entering or removing with interaction
#'                        effects in forward stepwise: - "none" - "single" - "sFactor" - "containment"
#'                        Default:single
#' @param freqField Param for specifying the frequency weight. The frequency weight must
#'                        be a numeric variable.
#' @param m1InteractionEffects Param for specifying threshold value to conduct interaction effect
#'                        detection. Must be > 0. Default: 100.
#' @param detectInteractionSig Param for specifying significance level for F statistics in automatic
#'                        interaction effect detection. Must be > 0 and < 1. Default: 0.05.
#' @param criteria4BestSubsets Param for criteria used to enter or remove an effect in each step in
#'                        best subsets: - "adjustedRSquare" - "AICC" - "ASE" Default: adjustedRSquare
#'                        This parameter is active when varSelectionMethod = bestSubsets.
#' @param recordIDFieldList Param for specifying the record ID fields.
#' @param diagnosticPlots Param for exporting statistics for diagnostic plots. Default: true.
#' @param effectSizeType4Coefficients Param for specifying computation of effect size for coefficients: -
#'                        "None" - "PartialEtaSquared" Default: None
#' @param regrWeightField Param for specifying the regression weight. The regression weight
#'                        must be a numeric variable.
#' @param sigLevel Param for specifying a significance level for all tests . Must be > 0
#'                        and < 1. Default: 0.05.
#' @param factorsCombinationsThreshold Param for the maximum number of factors and category combinations
#'                        with incomplete design while Linear computes EMMEANS. Must be >= 0. Default:
#'                        10,000,000L. If the number of levels of factors and categories are larger than
#'                        this threshold, a warning will be issued and EMMEANS information will not be
#'                        output.
#' @param plotCutPointNum Param for specifying the number of cut points for scatter plots. Must
#'                        be >= 5 and <= 100. Default: 19.
#' @param intercept Param for including the intercept in model. Default: true.
#' @param detectInteractionTol Param for specifying the tolerance value for stopping criterion in an
#'                        iterative process of detecting interaction of two factors. Must be > 10^-12^
#'                        and < 1. Default: 1.0e-6.
#' @param detectUnusualCategories Param for detecting unusual categories or category combinations based
#'                        on EMMEANS and exporting statistics for the Grouping and Unusual Categories
#'                        tables. Default: true. If false, unusual categories will not be detected.
#' @param useCustomMaxSteps Param for specifying a custom maximum number of steps for
#'                        forwardStepwise as model selection method. Default: false.
#' @param effectSizeType4ModelEffects Param for specifying computation of effect size for model effects: -
#'                        "None" - "PartialEtaSquared" - "EtaSquared" - "All" Default:None
#' @param m2SelectedMainEffects Param for specifying threshold value to select main effects for
#'                        interaction effect detection. Must be > 0. Default: 50.
#' @param POut Param for Significance level for F,,remove,, value. Must be > 0 and <
#'                        1. Default: 0.1. POut should be larger than PIn.
#' @param homoTest Param for conducting a Homoskedasticity test. Default: true.
#' @param robustEstimator Param for specifying a robust estimator: - "HC0" - "HC1" - "HC2" -
#'                        "HC3" Default: HC0
#' @param effectEntryTol Param for specifying the singularity tolerance value for checking
#'                        effect entry conditions. Must be >= 10^-12^ and <= 1. Default: 0.0001.
#' @param useCustomMaxEffects Param for user specifying a custom max effects in the final model.
#'                        Default: false.
#' @param plotBinThreshold Param for maximum data size for scatter plot binning. Must be >= 20.
#'                        Default: 20.
#' @param maxEffects Param for maximum number of effects in the final model. Must be a
#'                        positive number. Default: The maximum number of candidate model effects.
#' @param numOfModelsToExport Param for specifying the number of models from model selection for
#'                        which to export information for display. Must be > 0 and < 100. Default: 10.
#' @param turnOnFactorSortOrder Param for turning on / off sorting order for factors. Default: true.
#' @name classificationandregression.linearregression
#' @export
#' @examples
#' \dontrun{
#' sc <- sparkR.init()
#' data(iris)
#' sqlContext <- sparkRSQL.init(sc)
#' df <- createDataFrame(sqlContext, iris)
#' twostepmodel <- classificationandregression.linearregression(df,Species~Sepal_Length+Sepal_Width)
#'
#' scoredata <- spss.predict(twostepmodel,df)
#' head(scoredata)
#' }
setMethod("classificationandregression.linearregression", signature(data = "DataFrame"),
          function(
            data,
            formula = NULL,
            inputFieldList = NULL,
            targetField = NULL,
            freqField = NULL,
            recordIDFieldList = NULL,
            regrWeightField = NULL,
            factorSortOrder = c("ASCENDING","DESCENDING"),
            intercept = TRUE,
            plotCutPointNum = 19,
            turnOnFactorSortOrder = TRUE,
            useCustomMaxEffects = FALSE,
            varSelectionMethod = c("forwardStepwise", "bestSubsets", "lasso", "ridge", "elasticNet", "none"),
            ...){
            factorSortOrder <- match.arg(factorSortOrder)
            varSelectionMethod <- match.arg(varSelectionMethod)
            LinearRegressionModel(
              SPSSModel(jobj = spss.LinearRegression.fit(data, formula, inputFieldList, targetField,
                                                    freqField = freqField,
                                                    recordIDFieldList = recordIDFieldList,
                                                    regrWeightField = regrWeightField,
                                                    factorSortOrder = factorSortOrder,
                                                    intercept = intercept,
                                                    plotCutPointNum = plotCutPointNum,
                                                    turnOnFactorSortOrder = turnOnFactorSortOrder,
                                                    useCustomMaxEffects = useCustomMaxEffects,
                                                    varSelectionMethod = varSelectionMethod,
                                                ...)),
              name = "LinearRegressionModel")
          })

# Major function call for linear
spss.LinearRegression.fit <- function(
  data,
  formula,
  inputFieldList,
  targetField,
  ...
){
  linear <- SparkR:::callJStatic("com.ibm.spss.ml.classificationandregression.LinearRegression","apply")
  advanceArgs = list(...)
  lapply(seq_along(advanceArgs), function(i){
    if(!is.null(advanceArgs[[i]])){
      parameterName <- names(advanceArgs[i])
      head <- toupper(substr(parameterName,0,1))
      tril <- substring(parameterName,2)
      functionName <- paste("set",head,tril,sep="")
      value <- spss.util.parameterSwitch(spss.LinearRegression.parameterType, parameterName, advanceArgs[[i]])
      print(paste(functionName, "(", value, ")"))
      SparkR:::callJMethod(linear,functionName,value)
    }
  })
  print("Done Parameter set")
  if(!is.null(formula)){
    formula <- paste(deparse(formula), collapse = "")
    rmodel <- SparkR:::callJStatic("com.ibm.spss.ml.r.RWrapper","fitWithRFormula", linear, formula, df@sdf)
  }else{
    SparkR:::callJMethod(linear,"setInputFieldList",as.array(inputFieldList))
    SparkR:::callJMethod(linear,"setTargetField",targetField)
    rmodel <- SparkR:::callJStatic("com.ibm.spss.ml.r.RWrapper","fit", linear, df@sdf)
  }
  rmodel
}



spss.LinearRegression.parameterType <- list(
  randomSeed = "IntParam",
  PIn = "DoubleParam",
  maxParamsAllEffects = "IntParam",
  numOfExportedOutliers = "IntParam",
  criteria4ForwardStepwise = "Param[String]",
  CIn = "DoubleParam",
  factorSortOrder = "Param[String]",
  detectInfluentialOutliers = "BooleanParam",
  detectTwoWayInteraction = "BooleanParam",
  varSelectionMethod = "Param[String]",
  maxSteps = "IntParam",
  maxEffects4AllPossibleSubsets = "IntParam",
  detectInteractionMaxIter = "IntParam",
  forwardStepwiseRule = "Param[String]",
  freqField = "Param[String]",
  m1InteractionEffects = "IntParam",
  detectInteractionSig = "DoubleParam",
  criteria4BestSubsets = "Param[String]",
  recordIDFieldList = "StringArrayParam",
  diagnosticPlots = "BooleanParam",
  effectSizeType4Coefficients = "Param[String]",
  regrWeightField = "Param[String]",
  sigLevel = "DoubleParam",
  factorsCombinationsThreshold = "IntParam",
  plotCutPointNum = "IntParam",
  intercept = "BooleanParam",
  detectInteractionTol = "DoubleParam",
  detectUnusualCategories = "BooleanParam",
  useCustomMaxSteps = "BooleanParam",
  effectSizeType4ModelEffects = "Param[String]",
  m2SelectedMainEffects = "IntParam",
  POut = "DoubleParam",
  homoTest = "BooleanParam",
  robustEstimator = "Param[String]",
  effectEntryTol = "DoubleParam",
  useCustomMaxEffects = "BooleanParam",
  plotBinThreshold = "IntParam",
  maxEffects = "IntParam",
  numOfModelsToExport = "IntParam",
  turnOnFactorSortOrder = "BooleanParam"
)
