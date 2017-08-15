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
#' @param spread a number representing the size of the perturbations
#' @param grainCol the name of the grain column in baseRow, if present
#' @param predictedCol the name of the predicted column in baseRow, if present
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
          levels <- factor(info[[col]], levels = info[[col]], ordered = F)
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

#' @title
#' Build a linear model approximating another model
#'
#' @description Build a linear model approximating another model at a point
#' @param baseRow A row in a dataframe containing a data point at which to make 
#' the linear approximation.
#' @param fitObj The model to approximate
#' @param localDf A dataframe of data near baseRow on which to approximate the 
#' model
#' 
#' @return The linear model approximating the original model
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
localLinearApproximation = function(baseRow, 
                                    predictFunction, 
                                    localDf) {
  # get model output probs/values on localDf
  labels <- predictFunction(localDf)
  
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
  testDf <- cbind(localDf, labels)
  # fit linear model
  linearApproximation <- lm(predictions~., data = testDf)
  return(linearApproximation)
}

#' @title
#' Extract an ordered list of coefficients from a linear model
#'
#' @description Extract an ordered list of coefficients from a linear model in 
#' decreasing order (by value or by magnitude)
#' @param linearModel The linear model whose coefficients you want to extract
#' @param orderByMagnitude A boolean, determining whether or not to order the 
#' coefficients by magnitude rather than by value
#' 
#' @return A vector containing the ordered coefficients of the linear model 
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
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

#' @title
#' Plot the output of a model as each coefficient changes individually
#'
#' @description Plot the output of a model as each coefficient changes 
#' individually
#' @param baseRow a row in a data frame, used as the reference from which to 
#' vary the variables
#' @param modifiableCols a vector of column names corresponding to the columns
#' which should be perturbed
#' @param info a list which indexed by the column names in modifiableCols 
#' containing the standard deviation (for numeric columns) or the factor levels
#' (for factor columns)
#' @param fitObj the model for which we are plotting changes in variables
#' @param type the type of fitObj: "classification" or "regression"
#' @param spread a number representing how widely to vary the variables
#' @param modifiableDfRow the row in ModifiableFactorsDf corresponding to 
#' baseRow
#' @param grid An ordered pair of integers (rows, columns) which determines the 
#' number of plots to display at once. The first entry determines the number of
#' rows and the second determines the number of columns.
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
plotVariableEffects = function(baseRow, 
                               modifiableCols,
                               info,
                               predictFunction,
                               type,
                               spread = 1/2,
                               modifiableDfRow = NULL,
                               grid = NULL) {
  # By default, display at most 8 graphs
  if (is.null(grid)) {
    numberOfPlots <- length(modifiableCols)
    displayRows <- min(2, ceiling(numberOfPlots/4))
    displayCols <- min(4, numberOfPlots)
    grid <- c(displayRows, displayCols)
  }
  oldGraphicalParams1 <- par()$mfrow
  oldGraphicalParams2 <- par()$oma
  tryCatch({
    par(mfrow = grid, oma = c(2,2,2,2))
    
    for (col in modifiableCols) {
      # Build dataframe with variation in a single column
      if (is.numeric(baseRow[[col]])) {
        # Add column with variation to df
        sd <- info[[col]]
        singleVarDf <- data.frame(seq(from = baseRow[[col]] - sd*spread,
                                      to = baseRow[[col]] + sd*spread,
                                      length.out = 100))
      } else {
        singleVarDf <- data.frame(factor(info[[col]], levels = info[[col]]))
      }
      names(singleVarDf) <- c(col)
      
      # Add the other columns
      for (col2 in names(baseRow)) {
        if (col2 != col) singleVarDf[[col2]] <- baseRow[[col2]]
      }
      
      labels <- predictFunction(singleVarDf)
      rowPred <- predictFunction(baseRow)
      if (!is.null(modifiableDfRow)) {
        LMrowPred <- modifiableDfRow$LMPrediction
      }
      
      # plot the effect of changing the variable
      if (type == "classification") {
        yAxisLabel <- "Probability"
        yAxisLimits <- c(0,1)
      } else {
        yAxisLabel <- "Response"
        if (!is.null(modifiableDfRow)) {
          diff <- as.numeric(abs(rowPred - LMrowPred))
          topWT <- 1.2*max(abs(modifiableDfRow[grepl("Modify[0123456789]+WT", 
                                           names(modifiableDfRow))]))
          yAxisLimits <- c(as.numeric(rowPred) - topWT - diff, 
                           as.numeric(rowPred) + topWT + diff)
        } else {
          yAxisLimits <- 1.2*c(min(as.numeric(labels), max(as.numeric(labels))))
        }
      }
      
      plot(singleVarDf[[col]], labels$predictions, type = "l",
           ylim = yAxisLimits, xlab = col, ylab = yAxisLabel)
      
      abline(h = rowPred, lty = "dashed")
      points(baseRow[[col]], rowPred, pch = 16, cex = 1.5)
      
      if (!is.null(modifiableDfRow)) {
        points(baseRow[[col]],modifiableDfRow$LMPrediction, 
               pch = 8, col = "red")
        if (is.numeric(baseRow[[col]])) {
          numIndex <- which(modifiableDfRow == col) + 1
          slope <- as.numeric(modifiableDfRow[numIndex])
          xCoords <- baseRow[[col]] + 0.5*sd*spread*c(-1, 1)
          yCoords <- slope*(xCoords - baseRow[[col]]) + LMrowPred
          lines(xCoords, yCoords, col = "red")
        } else {
          for (level in info[[col]]) {
            if (level != baseRow[[col]]) {
              factorIndex <- which(modifiableDfRow == paste0(col, level)) + 1
              slope <- as.numeric(modifiableDfRow[factorIndex])
              xCoord <- factor(level, levels = info[[col]])
              yCoord <- slope + modifiableDfRow$LMPrediction
              points(xCoord, y = yCoord, pch = 1, col = "red")
            }
          }
        }
      mtext(paste("Grain Column ID:", modifiableDfRow$GrainID), 
            outer = TRUE, cex = 1)
      }
    }
  }, error = function(e) {
    message(e)
  }, finally =  {
    # Reset the graphics parameters even if an error was raised
    par(mfrow = oldGraphicalParams1, oma = oldGraphicalParams2)
  })
}

#' @title
#' Get a percentile interval.
#'
#' @description 
#' Returns the lower and upper ends of an interval roughly \code{range} 
#' percentiles above and below \code{x}.
#' 
#' We assume that the vector of percentiles was computed using sample data so
#' that the minimum and maximum values may not truly represent the minimum and
#' maximum values of the underlying distribution. If \code{x} lies in the k-th 
#' percentile and k - \code{range} < 0 then a -1% "percentile" value is 
#' computed from 0% - (1% - 0%) and the range (-1%, (2*\code{range} - 1)%) is 
#' returned. Similarly, if k + \code{range} > 100, then a 101% "percentile" is 
#' computed.
#' 
#' @param x the value (of a continuous variable) around which to build the 
#' interval
#' @param percentiles a vector of percentiles of length 101, with one entry 
#' for each percentile 0%, 1%, 2%, ..., 99%, 100%
#' @param range an integer represententing how many percentiles to include in
#' each direction, so if \code{x} lies in the k-th percentile, then the
#' interval will range from the (k - \code{range})-th percentile to the (k + 
#' \code{range})-th percentile.
#'
#' @return a 2-dimensional vector consisting of the lower and upper endpoints
#' of the interval
#'
#' @export
#' @references \url{http://healthcare.ai}
#' @seealso \code{\link{healthcareai}}
#' @examples 
#' # A sample from a normal distribution
#' normal_data <- rnorm(10000, mean = 0, sd = 1)
#'
#' # A vector of percentiles 0%, 1%, 2%, ..., 99%, 100% for the data
#' sample_percentiles <- quantile(normal_data, seq(0, 1, 0.01))
#'
#' # Check that ~68% of a normal distribution's mass is within 1 standard
#' # deviation of the mean.
#' percentileInterval(0, sample_percentiles, range = 34)
#' 
#' # Check that ~95% of a normal distribution's mass is within 2 standard
#' # deviations of the mean.
#' percentileInterval(0, sample_percentiles, range = 48)
#'
#' # Get an interval around 1, extending 10 percentiles in each direction
#' percentileInterval(1, sample_percentiles, range = 10)
#' 
#' # If x is near one end of the distribution, a placeholder -1% or 101% 
#' # "percentile" value is created
#' percentileInterval(2, sample_percentiles, range = 10)
percentileInterval = function(x, percentiles, range = 20) {
  # if x is very small, make fake -1% percentile
  if (x < percentiles[1 + range]) {
    lower <- 2*percentiles[1] - percentiles[2]
    upper <- percentiles[2*range]
    names(lower) <- "-1%"
  # if x is very large, make fake 101% percentile
  } else if (x > percentiles[101 - range]) {
    lower <- percentiles[101 - 2*range]
    upper <- 2*percentiles[101] - percentiles[100]
    names(upper) <- "101%"
  # otherwise, use the expected percentiles
  } else {
    index <-  which.min(abs(percentiles - x)) # closest percentile to x
    lower <- percentiles[index - range]
    upper <- percentiles[index + range]
  }
  return(c(lower, upper))
}
