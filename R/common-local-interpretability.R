#' @title
#' Get a dataframe consisting of rows "near" a given row
#'
#' @description Build a dataframe consisting of rows which are perturbations 
#' of a given row.
#' @param baseRow a row of data (in a dataframe) to perturb
#' @param modifiableCols a vector of column names corresponding to the columns
#' which should be perturbed
#' @param info a list which indexed by the column names in modifiableCols 
#' containing the standard deviation (for numeric columns) or the factor levels
#' (for factor columns)
#' @param size the number of rows in
#' @param spread
#' @param grainCol
#' @param predictedCol
#' 
#' @return a dataframe consisting whose rows are perturbations of baseRow
#' 
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
localPerturbations = function(baseRow, 
                              modifiableCols,
                              info,
                              size = 1000,
                              spread = 1/4,
                              grainCol = NA, 
                              predictedCol = NA) {
  # dummy column to set number of rows for dataframe
  df <- data.frame(temp_column_to_drop = rep(0, times = size))
  for (col in names(baseRow)) {
    if (is.na(grainCol) || col != grainCol) {
      mu <- baseRow[[col]]
      # if modifiable, fill with random values near mu
      if (col %in% modifiableCols) {
        # if numeric, sample using a normal distribution
        if (is.numeric(mu)) {
          sd <- info[[col]]
          df[[col]] <- rnorm(size, mean = mu, sd = spread*sd)
        } else {# not numeric
          levels <- factor(info[[col]], ordered = F)
          df[[col]] <- sample(levels, replace = T, size = size)
        }
      } else {# not modifiable -> repeat mu throughout
        df[[col]] <- rep(mu, times = size)
      }
    }
  }
  # drop temp column
  df$temp_column_to_drop <- NULL
  return(df)
}

localLinearApproximation = function(fitObj, 
                                    localDf) {
  # get model output probs on localDf
  predictions <- predict.train(object = fitObj,
                               newdata = localDf,
                               type = "prob")
  
  testDf <- cbind(localDf, predictions[,2])
  # fit linear model
  linearApproximation <- lm(predictions~., data = testDf)
  return(linearApproximation)
}

getLinearCoeffs = function(linearModel, orderByMagnitude = FALSE) {
  coefs <- linearModel$coefficients
  # peel off intercept
  intercept <- coefs[1]
  coefs <- coefs[2:length(coefs)]
  
  # remove NAs
  coefs <- coefs[!is.na(coefs)]
  
  # reorder by absolute value
  if (orderByMagnitude) {
    coefs <- coefs[order(abs(coefs), decreasing = T)] 
  } else {
    coefs <- coefs[order(coefs, decreasing = T)] 
  }
  attr(coefs, "intercept") <- intercept
  
  return(coefs)
}
