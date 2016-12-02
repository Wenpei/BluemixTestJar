spssonspark.demo <- function(){
  Sys.setenv(SPARK_HOME = "C:\\aWorkFolder\\spark\\spark-1.6.0-bin-hadoop2.6\\spark-1.6.0-bin-hadoop2.6")
  sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="2g"))
  data(iris)
  sqlContext <- sparkRSQL.init(sc)
  df <- createDataFrame(sqlContext, iris)
  twostep <- SparkR:::callJStatic("com.ibm.spss.ml.clustering.TwoStep","apply")
  SparkR:::callJMethod(twostep,"setInputFieldList",as.array(c("Sepal_Length","Sepal_Width")))
  twostpmodel <- SparkR:::callJMethod(twostep,"fit",df@sdf)
  SparkR:::callJMethod(twostpmodel,"toPMML")
  #SparkR:::callJMethod(twostpmodel,"statXML")
  SparkR:::callJMethod(twostpmodel,"uid")
}

spssonspark.demo2 <- function(){
  Sys.setenv(SPARK_HOME = "C:\\aWorkFolder\\spark\\spark-1.6.0-bin-hadoop2.6\\spark-1.6.0-bin-hadoop2.6")
  sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="2g"))
  data(iris)
  sqlContext <- sparkRSQL.init(sc)
  df <- createDataFrame(sqlContext, iris)
  twostepwrapper <- SparkR:::callJStatic("com.ibm.spss.ml.r.TwoStepWrapper","fit",
                                         df@sdf,as.array(c("Sepal_Length","Sepal_Width")),as.array(c("")),TRUE,TRUE,TRUE)
  scoredf <- SparkR:::callJMethod(twostepwrapper,"transform",df@sdf)
  scoredf <- SparkR:::dataFrame(scoredf)
  head(scoredf)
}

twostep <- function(
  #  formula,
  data,
  inputFieldList = c(""),
  standardizeFieldList = c(""),
  autoClustering = TRUE,
  featureSelection = TRUE,
  outlierHandling = TRUE,
  ...
){
  twostep <- SparkR:::callJStatic("com.ibm.spss.ml.clustering.TwoStep","apply")
  SparkR:::callJMethod(twostep,"setInputFieldList",as.array(inputFieldList))
  SparkR:::callJMethod(twostep,"setStandardizeFieldList",as.array(standardizeFieldList))
  SparkR:::callJMethod(twostep,"setAutoClustering",autoClustering)
  SparkR:::callJMethod(twostep,"setFeatureSelection",featureSelection)
  SparkR:::callJMethod(twostep,"setOutlierHandling",outlierHandling)
  advanceArgs = list(...)
  print(advanceArgs)
  lapply(seq_along(advanceArgs), function(i){
    name <- names(advanceArgs[i])
    #value <- advanceArgs[[i]]
    head <- toupper(substr(name,0,1))
    tril <- substring(name,2)
    name <- paste("set",head,tril,sep="")
    print(name)
    print(advanceArgs[[i]])
    SparkR:::callJMethod(twostep,name,advanceArgs[[i]])
  })

  twostepmodel <- SparkR:::callJMethod(twostep,"fit",df@sdf)

  return(new("PipelineModel", model = twostepmodel))
}

twostep <- function(){
  library(SparkR)
  Sys.setenv(SPARK_HOME = "C:\\aWorkFolder\\spark\\spark-1.6.0-bin-hadoop2.6\\spark-1.6.0-bin-hadoop2.6")
  sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="2g"))
  data(iris)
  sqlContext <- sparkRSQL.init(sc)
  df <- createDataFrame(sqlContext, iris)
  twostepmodel <- clustering.twostep(df,Species~.)
  scoredata <- spss.predict(twostepmodel,df)
  head(scoredata)
}

linear <- function(){
  library(SparkR)
  Sys.setenv(SPARK_HOME = "C:\\aWorkFolder\\spark\\spark-1.6.0-bin-hadoop2.6\\spark-1.6.0-bin-hadoop2.6")
  sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="2g"))
  data(iris)
  sqlContext <- sparkRSQL.init(sc)
  df <- createDataFrame(sqlContext, iris)
  linearmodel <- classificationandregression.linearregression(df,Species~.)
  scoredata <- spss.predict(linearmodel,df)
  head(scoredata)
}
