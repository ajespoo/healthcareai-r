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

localLinearApproximation = function(baseRow, 
                                    fitObj, 
                                    localDf) {
  # get model output probs on localDf
  predictions <- predict.train(object = fitObj,
                               newdata = localDf,
                               type = "prob")
  yesProbs <- data.frame(predictions = predictions[,2])
  
  for (col in names(localDf)) {
    if (is.factor(localDf[[col]])) {
      if (length(unique(localDf[[col]])) == 1) {
        # remove factor columns with no variance
        localDf[[col]] <- NULL
      } else {
        # Set base level for dummy variables
        localDf[[col]] <- relevel(localDf[[col]], as.character(baseRow[[col]]))
      }
    }
  }
  testDf <- cbind(localDf, yesProbs)
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

plotVariableEffects = function(baseRow, 
                               modifiableCols,
                               info,
                               fitObj,
                               spread = 1/2, 
                               grid = NULL) {
  # By default, display at most 8 graphs
  if (is.null(grid)) {
    numberOfPlots <- length(modifiableCols)
    displayRows <- min(2, ceiling(numberOfPlots/4))
    displayCols <- min(4, numberOfPlots)
    grid <- c(displayRows, displayCols)
  }
  oldGraphicalParams <- par()$mfrow
  par(mfrow = grid)
  
  for (col in modifiableCols) {
    # Build dataframe with variation in a single column
    if (is.numeric(baseRow[[col]])) {
      # Add column with variation to df
      sd <- info[[col]]
      singleVarDf <- data.frame(seq(from = baseRow[[col]] - sd*spread,
                                    to = baseRow[[col]] + sd*spread,
                                    length.out = 100))
    } else {
      singleVarDf <- data.frame(info[[col]])
    }
    names(singleVarDf) <- c(col)
    
    # Add the other columns
    for (col2 in names(baseRow)) {
      if (col2 != col) singleVarDf[[col2]] <- baseRow[[col2]]
    }
    # Get prediction probabilities
    predictions <- predict.train(object = fitObj,
                                 newdata = singleVarDf,
                                 type = "prob")
    yesProbs <- data.frame(predictions = predictions[,2])
    
    # plot the effect of changing the variable
    plot(singleVarDf[[col]], yesProbs$predictions, type = "l",
         ylim = c(0, 1), xlab = col, ylab = "Probability")

    rowPred <- predict.train(object = fitObj,
                             newdata = baseRow,
                             type = "prob")[2]
    abline(h = rowPred, lty = "dashed")
    points(baseRow[[col]], rowPred, pch = 16, cex = 1.5)
  }
  par(mfrow = oldGraphicalParams)
}
