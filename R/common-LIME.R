#' @title
#' Build a LIME explainer for a trained classification model
#'
#' @description Build a LIME explainer for a trained classification model 
#' using the trained model and the training set used to train the model
#' @param trainingSet The data set on which the model was trained
#' @param trainedModel The trained model
#' @param binContinuous
#' @param bins The number of bins to use for continuous variables if 
#' binContinuous is TRUE

buildLIMEExplainer = function(trainingSet, 
                              trainedModel,
                              binContinuous = TRUE,
                              bins = 5) {
  # build explainer usin LIME
  explainer <- lime::lime(x = trainingSet, 
                          model = trainedModel, 
                          bin_continuous = binContinuous,
                          n_bins = bins)
  return(explainer)
}

#' @title
#' Use LIME explainer to explain new classification predictions
#'
#' @description Use a LIME explainer built using buildLIMEExplainer to explain 
#' new predictions
#' @param explainer A LIME explainer built using buildLIMEExplainer
#' @param testSet New data on which predictions have been made
#' @param numLabels The number of response class predictions to explain, 
#' ordered from highest to lowest probability (set to 1 or 2 for binary 
#' classification, more for multiclass)
#' @param numFeatures The number of features to use when explaining a prediction

getLIMEExplanations = function(explainer,
                               testSet, 
                               numLabels,
                               numFeatures = 3, 
                               grainCol = NULL, 
                               predictedCol = NULL) {
  
  # Remove predictedCol and grainCol from testSet
  cleanData <- testSet[!(colnames(testSet) %in% c(grainCol, predictedCol))]
  
  # build explainer using LIME
  explanations <- explainer(cases = cleanData, 
                            n_labels = numLabels,
                            n_features = numFeatures)
  
  # Label rows using the grain column if it exists
  if (!is.null(predictedCol)) {
    explanations$case <- testSet[explanations$case, ][[grainCol]]
  }
  
  return(explanations)
}
