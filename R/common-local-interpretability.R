#' @title
#' Get a dataframe consisting of rows "near" a given row
#' 
#' @importFrom stats rnorm
#' 
#' @description Build a dataframe consisting of rows which are perturbations 
#' of a given row.
#' @usage localPerturbations(baseRow,
#'                    modifiableCols,
#'                    info,
#'                    size = 1000,
#'                    spread = 1/4,
#'                    grainCol = NA,
#'                    predictedCol = NA)
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
#' @references \url{http://healthcareai-r.readthedocs.io}
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
#' @importFrom stats relevel
#' @importFrom stats lm
#' 
#' @description Build a linear model approximating another model at a point
#' @usage localLinearApproximation(baseRow, predictFunction, localDf)
#' @param baseRow A row in a dataframe containing a data point at which to make 
#' the linear approximation.
#' @param predictFunction A function with which to make predictions for the
#' model to approximate
#' @param localDf A dataframe of data near baseRow on which to approximate the 
#' model
#' 
#' @return The linear model approximating the original model
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
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
#' Extract an ordered list of scaled coefficients from a linear model
#'
#' @description Extract an ordered list of coefficients from a linear model in 
#' decreasing order (by value or by magnitude). These coefficients are scaled
#' by the standard deviation of the variable.
#' @usage getScaledCoeffs(linearModel, info, orderByMagnitude = FALSE)
#' @param linearModel The linear model whose coefficients you want to extract
#' @param info a list indexed containing the standard deviations of all of the
#' numeric variables
#' @param orderByMagnitude A boolean, determining whether or not to order the 
#' coefficients by magnitude rather than by value
#' 
#' @return A vector containing the ordered coefficients of the linear model 
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
getScaledCoeffs = function(linearModel, 
                           info, 
                           orderByMagnitude = FALSE) {
  coefs <- linearModel$coefficients
  # peel off intercept
  intercept <- coefs[1]
  coefs <- coefs[2:length(coefs)]
  
  # remove NAs
  coefs <- coefs[!is.na(coefs)]
  
  # Scale numeric variables by the standard deviation
  for (variable in names(coefs)) {
    sd <- info[[variable]]
    if (!is.null(sd)) {
      coefs[[variable]] <- coefs[[variable]]*sd
    }
  }
  
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
#' @importFrom graphics abline
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics points
#' 
#' @description Plot the output of a model as each coefficient changes 
#' individually
#' @usage plotVariableEffects(baseRow, 
#'                     modifiableCols, 
#'                     info,
#'                     info2,
#'                     predictFunction,
#'                     type,
#'                     spread = 2,
#'                     numberOfPercentiles = 12,
#'                     modifiableDfRow = NULL,
#'                     grid = NULL)
#' @param baseRow a row in a data frame, used as the reference from which to 
#' vary the variables
#' @param modifiableCols a vector of column names corresponding to the columns
#' which should be perturbed
#' @param info a list which indexed by the column names in modifiableCols 
#' containing the standard deviation (for numeric columns) or the factor levels
#' (for factor columns)
#' @param info2 list of percentiles TODO: expand
#' @param predictFunction a function with which to make new predictions for 
#' the model
#' @param type the type of fitObj: "classification" or "regression"
#' @param spread a number representing how widely to vary the variables
#' @param numberOfPercentiles the number of percentiles to use TODO:expand
#' @param modifiableDfRow the row in ModifiableFactorsDf corresponding to 
#' baseRow
#' @param grid An ordered pair of integers (rows, columns) which determines the 
#' number of plots to display at once. The first entry determines the number of
#' rows and the second determines the number of columns.
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
plotVariableEffects = function(baseRow, 
                               modifiableCols,
                               info,
                               info2,
                               predictFunction,
                               type,
                               spread = 2, 
                               numberOfPercentiles = 12,
                               modifiableDfRow = NULL,
                               grid = NULL) {
  # By default, display at most 8 graphs
  if (is.null(grid)) {
    numberOfPlots <- length(modifiableCols)
    displayRows <- min(2, ceiling(numberOfPlots/4))
    displayCols <- min(4, numberOfPlots)
    grid <- c(displayRows, displayCols)
  }
  
  # Compute modifiable factors using univariate method
  univariate <- modifiableFactors1Row(baseRow = baseRow, 
                                      modifiableCols = modifiableCols,
                                      info2 = info2, 
                                      predictFunction = predictFunction,
                                      lowerProbGoal = TRUE, 
                                      numberOfPercentiles = numberOfPercentiles)
  
  oldGraphicalParams1 <- par()$mfrow
  oldGraphicalParams2 <- par()$oma
  tryCatch({
    par(mfrow = grid, oma = c(2,2,2,2))
    
    for (col in modifiableCols) {
      # Build dataframe with variation in a single column
      if (is.numeric(baseRow[[col]])) {
        # Add column with variation to df
        sd <- info[[col]]
        singleVarDf <- data.frame(seq(from = info2[[col]][1],
                                      to = info2[[col]][101],
                                      length.out = 1000))  
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
      
      # Set axes labels, limits
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
      
      # Plot model values
      lineWidth <- ifelse(is.numeric(baseRow[[col]]), 2, 1)
      
      plot(singleVarDf[[col]], labels$predictions, type = "l",
           ylim = yAxisLimits, xlab = col, ylab = yAxisLabel, lwd = lineWidth)
      
      abline(h = rowPred, lty = "dashed")
      points(baseRow[[col]], rowPred, pch = 16, cex = 1.5)
      
      # Plot univariate approximation
      if (is.numeric(baseRow[[col]])) {
        uniRow <- univariate[univariate$variable == col, ]
        currentValue <- as.numeric(uniRow$currentValue)
        altValue <- as.numeric(uniRow$altValue)
        xCoords <- c(1.25*currentValue - 0.25*altValue, altValue)
        yCoords <- uniRow$slope*xCoords + uniRow$intercept
        lines(xCoords, yCoords, col = "blue", lwd = lineWidth)
        
        points(currentValue, uniRow$slope*currentValue + uniRow$intercept, 
               pch = 8, col = "blue")
      }

      # Plot multivariate approximation
      if (!is.null(modifiableDfRow)) {
        points(baseRow[[col]],attr(modifiableDfRow, "currentProbLM"), 
               pch = 8, col = "red")
               
        if (is.numeric(baseRow[[col]])) {
          xCoords <- as.numeric(modifiableDfRow$altValue[modifiableDfRow$variable == col])
          yCoords <- as.numeric(modifiableDfRow$altProbLM[modifiableDfRow$variable == col])

          lines(xCoords, yCoords, col = "red", lwd = lineWidth)
        } else {
          for (level in info[[col]]) {
            if (level != baseRow[[col]]) {
              xCoord <- factor(level, levels = info[[col]])
              yCoord <- as.numeric(modifiableDfRow$altProbLM[(modifiableDfRow$variable == col) 
                                                             & (modifiableDfRow$altValue == level)])
              points(xCoord, y = yCoord, pch = 1, col = "red")
            }
          }
        }
      mtext(paste("Grain Column ID:", attr(modifiableDfRow, "grainID")), 
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


plotVariableEffects2 = function(baseRow,
                                modifiableCols,
                                nonConstantCols,
                                info,
                                info2,
                                predictFunction,
                                spread = 2, 
                                grid = NULL, 
                                lowerProbGoal = TRUE) {
  # By default, display at most 8 graphs
  if (is.null(grid)) {
    numberOfPlots <- length(modifiableCols)
    displayRows <- min(2, ceiling(numberOfPlots/4))
    displayCols <- min(4, numberOfPlots)
    grid <- c(displayRows, displayCols)
  }
  
  
  
  # Compute modifiable factors using univariate method
  univariate <- modifiableFactors1Row2(baseRow = baseRow,
                                       modifiableCols = modifiableCols,
                                       nonConstantCols = nonConstantCols,
                                       info = info,
                                       percentiles = info2,
                                       scale = 1/2,
                                       predictFunction = predictFunction,
                                       lowerProbGoal = lowerProbGoal)
  
  oldGraphicalParams1 <- par()$mfrow
  oldGraphicalParams2 <- par()$oma
  tryCatch({
    par(mfrow = grid, oma = c(2,2,2,2))
    
    for (col in modifiableCols) {
      # Build dataframe with variation in a single column
      if (is.numeric(baseRow[[col]])) {
        # Add column with variation to df
        sd <- info[[col]]
        singleVarDf <- data.frame(seq(from = info2[[col]][1],
                                      to = info2[[col]][101],
                                      length.out = 1000))  
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
      
      # Set axes labels, limits
      yAxisLabel <- "Probability"
      yAxisLimits <- c(0,1)
      
      # Plot model values
      lineWidth <- ifelse(is.numeric(baseRow[[col]]), 1.75, 1)
      
      plot(singleVarDf[[col]], labels$predictions, type = "l",
           ylim = yAxisLimits, xlab = col, ylab = yAxisLabel, lwd = lineWidth)
      
      abline(h = rowPred, lty = "dashed")
      points(baseRow[[col]], rowPred, pch = 16, cex = 1.5)
      
      # Plot univariate approximation
      if (is.numeric(baseRow[[col]])) {
        uniRow <- univariate[univariate$variable == col, ]
        currentValue <- as.numeric(uniRow$currentValue)
        altValue <- as.numeric(uniRow$altValue)
        xCoords <- c(1.25*currentValue - 0.25*altValue, altValue)
        yCoords <- uniRow$slope*xCoords + uniRow$intercept
        lines(xCoords, yCoords, col = "red", lwd = lineWidth)
        
        points(currentValue, uniRow$slope*currentValue + uniRow$intercept, 
               pch = 8, col = "red")
      } else {
        factorDf <- univariate[univariate$variable == col, ]
        for (level in info[[col]]) {
          uniRow <- factorDf[factorDf$altValue == level, ]
          if (level == uniRow$currentValue) {
            plotch <- 8
          } else {
            plotch <- 16
          }
          points(x = factor(uniRow$altValue, levels = info[[col]]), 
                 y = uniRow$altProb, 
                 pch = plotch, col = "red")
        }
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
#' @description Returns the lower and upper ends of an interval roughly 
#' \code{range} percentiles above and below \code{x}.
#' 
#' We assume that the vector of percentiles was computed using sample data so
#' that the minimum and maximum values may not truly represent the minimum and
#' maximum values of the underlying distribution. If \code{x} lies in the k-th 
#' percentile and k - \code{range} < 0 then a -1\% "percentile" value is 
#' computed from 0\% - (1\% - 0\%) and the range (-1\%, (2*\code{range} - 1)\%) 
#' is returned. Similarly, if k + \code{range} > 100, then a 101\% "percentile" 
#' is computed.
#' @usage percentileInterval(x, percentiles, range = 20)
#' @param x the value (of a continuous variable) around which to build the 
#' interval
#' @param percentiles a vector of percentiles of length 101, with one entry 
#' for each percentile 0\%, 1\%, 2\%, ..., 99\%, 100\%
#' @param range an integer represententing how many percentiles to include in
#' each direction, so if \code{x} lies in the k-th percentile, then the
#' interval will range from the (k - \code{range})-th percentile to the (k + 
#' \code{range})-th percentile.
#'
#' @return a 2-dimensional vector consisting of the lower and upper endpoints
#' of the interval
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
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
    lower <- percentiles[1] - 1.0*(percentiles[2] - percentiles[1])
    upper <- percentiles[2*range]
    names(lower) <- "-1%"
  # if x is very large, make fake 101% percentile
  } else if (x > percentiles[101 - range]) {
    lower <- percentiles[101 - 2*range]
    upper <- percentiles[101] + 1.0*(percentiles[101] - percentiles[100])
    names(upper) <- "101%"
  # otherwise, use the expected percentiles
  } else {
    index <-  which.min(abs(percentiles - x)) # closest percentile to x
    lower <- percentiles[index - range]
    upper <- percentiles[index + range]
  }
  return(c(lower, upper))
}

#' @title
#' Build a dataframe whose rows all agree except in one numeric column.
#' 
#' @description Build a dataframe whose rows all agree except in one numeric 
#' column whose values are evenly spread out along a range of values.
#' 
#' @param baseRow a row of data (in a dataframe) to perturb
#' @param variable the name of the numeric column which will take on multiple
#' values
#' @param interval a 2-dimensional vector consisting of the endpoints of an 
#' interval along which to vary the value of \code{variable}
#' @param size the number of rows the dataframe should have
#' @param center a optional value to use as the center of the interval; if a 
#' value is specified, half of the values of the \code{variable} column will be 
#' less than \code{center} and the other half will be greater than \code{center}
#' 
#' @return a dataframe dataframe whose rows all agree except in the
#' \code{variable}column whose values are evenly spread out along a range of values
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
singleNumericVariableDf = function(baseRow, 
                                   variable,
                                   interval,
                                   size = 50,
                                   center = NULL) {
  # dummy column to set number of rows for dataframe
  df <- data.frame(temp_column_to_drop = rep(0, times = size))
  for (col in names(baseRow)) {
    baseValue <- baseRow[[col]]
    # if modifiable, fill with ...
    if (col  == variable) {
      if (is.null(center)) {
        df[[col]] <- seq(interval[1], interval[2], length.out = size)
      } else {
        leftHalf <- seq(interval[1], center, length.out = size/2)
        rightHalf <- seq(center, interval[2], length.out = size/2)
        df[[col]] <- c(leftHalf, rightHalf)
      }
    } else {# not modifiable -> repeat baseValue throughout
      df[[col]] <- rep(baseValue, times = size)
    }
  }
  # drop temp column
  df$temp_column_to_drop <- NULL
  return(df)
}

singleNumericVariableDf2 = function(baseRow, 
                                    modifiableVariable,
                                    standardDeviations,
                                    nonConstantVariables,
                                    scale = 1/2,
                                    size = 500,
                                    skew = NULL) {
  # dummy column to set number of rows for dataframe
  df <- rbind(baseRow[rep(1, times = size), ])
  for (col in names(baseRow)) {
    baseValue <- baseRow[[col]]
    # modify modifiable and nonConstant
    if (col  == modifiableVariable) {
      # Set range of values for modifiable
      sd <- standardDeviations[[col]]
      lowerScale <- scale*sd
      upperScale <- scale*sd
      if (!is.null(skew)) {
        if (skew == "positive") {
          lowerScale <- lowerScale*1/4
        } else if (skew == "negative") {
          upperScale <- upperScale*1/4
        }
      }
      
      df[[col]] <- seq(baseValue - lowerScale, 
                       baseValue + upperScale, 
                       length.out = size)
    } else if (col %in% nonConstantVariables) {
      # Add noise to nonConstant numeric
      if (is.numeric(baseRow[[col]])) {
        noiseSd <- 0.1*standardDeviations[[col]]*scale
        df[[col]] <- addNoise(df[[col]], noiseSd)
      }
    }
  }
  return(df)
}

addNoise = function(column, noiseSd) {
  rowCount <- length(column)
  return(column + rnorm(n = rowCount, sd = noiseSd))
}

#' @title
#' Build a dataframe whose rows all agree except in one categorical column.
#' 
#' @description Build a dataframe whose rows all agree except in one categorical 
#' column which has a unique category in each row.
#' 
#' @param baseRow a row of data (in a dataframe) to perturb
#' @param variable the name of the categorical column which will take on
#' multiple values
#' @param factorLevels a list of factor levels or categories which the
#' categorical column can take
#' 
#' @return a dataframe whose rows all agree except in the \code{variable}
#' column which takes each level in \code{factorLevels} exactly once.
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
singleFactorVariableDf = function(baseRow,
                                  variable,
                                  factorLevels) {
  # dummy column to set number of rows for dataframe
  size <- length(factorLevels)
  df <- data.frame(temp_column_to_drop = rep(0, times = size))
  for (col in names(baseRow)) {
    baseValue <- baseRow[[col]]
    # if modifiable, fill with ...
    if (col  == variable) {
      df[[col]] <- factor(factorLevels, levels = factorLevels)
    } else {# not modifiable -> repeat baseValue throughout
      df[[col]] <- rep(baseValue, times = size)
    }
  }
  # drop temp column
  df$temp_column_to_drop <- NULL
  return(df)
}

singleFactorVariableDf2 = function(baseRow,
                                   modifiableVariable,
                                   factorLevels,
                                   standardDeviations,
                                   nonConstantVariables, 
                                   scale = 1/2,
                                   size = 500) {
  # dummy column to set number of rows for dataframe
  df <- rbind(baseRow[rep(1, times = size), ])
  for (col in names(baseRow)) {
    if (col %in% nonConstantVariables & is.numeric(baseRow[[col]])) {
      noiseSd <- 0.1*standardDeviations[[col]]*scale
      df[[col]] <- addNoise(df[[col]], noiseSd)
    }
  }
  df <- do.call(rbind, lapply(factorLevels, function(level) {
    levelDf <- df
    levelDf[[modifiableVariable]] <- factor(level, levels = factorLevels)
    return(levelDf)
  }))
  return(df)
}

#' @title
#' Build a dataframe with the modifiable factors information for 1 row
#' 
#' @importFrom stats lm
#' 
#' @description Build a dataframe with the modifiable factors information for 1 
#' row. This dataframe contains the 
#' 
#' @param baseRow the row of data for which to determine the modifiable factors
#' @param modifiableCols a list of names of the modifiable columns
#' @param info2 percentiles and frequencies TODO: expand
#' @param predictFunction a function used to make new predictions using the 
#' model we wish to study
#' @param lowerProbGoal a boolean specifying whether the goal is to reduce the
#' predicted probability (i.e., Y corresponds to negative outcome)
#' @param numberOfPercentiles the number of percentiles to use when
#' approximating the model
#' 
#' @return a dataframe whose rows all agree except in the \code{variable}
#' column which takes each level in \code{factorLevels} exactly once.
#'
#' @export
#' @references \url{http://healthcareai-r.readthedocs.io}
#' @seealso \code{\link{healthcareai}}
modifiableFactors1Row = function(baseRow,
                                 modifiableCols,
                                 info2,
                                 predictFunction,
                                 lowerProbGoal = TRUE, 
                                 numberOfPercentiles = 12) {
  currentProb <- predictFunction(baseRow)$predictions
  
  featureList <- list()
  for (col in modifiableCols) {
    # Save current value of the variable
    currentValue <- baseRow[[col]]
    
    # Compute alternate values and probabilities for numeric variables
    if (is.numeric(baseRow[[col]])) {
      
      # Build first linear model
      # Build dataframe with variable ranging within quantile range
      variableRange <- percentileInterval(x = baseRow[[col]], 
                                          percentiles = info2[[col]], 
                                          range = numberOfPercentiles)
      balancedDf <- singleNumericVariableDf(baseRow = baseRow, 
                                            variable = col, 
                                            interval = variableRange, 
                                            center = baseRow[[col]],
                                            size = 50)
      # Make predictions and train a linear model on these
      balancedPredictions <- predictFunction(newData = balancedDf)
      balancedLM <- lm(balancedPredictions$predictions ~ balancedDf[[col]])
      # Extract linear model coefficients
      balancedSlope <- balancedLM$coefficients[2]
      
      # Build second linear model
      # Case 1: want to shift to the right
      if ((balancedSlope < 0 & lowerProbGoal) 
          || (balancedSlope > 0 & !lowerProbGoal)) {
        # skew interval to the right
        newLow <- baseRow[[col]] - 0.25*(variableRange[2] - baseRow[[col]])
        altValue <- variableRange[2]
        skewedInterval <- c(newLow, altValue)
        # Case 2: want to shift to the left
      } else {
        # skew interval to the left
        newHigh <- baseRow[[col]] + 0.25*(baseRow[[col]] - variableRange[1])
        altValue <- variableRange[1]
        skewedInterval <- c(altValue, newHigh)
      }
      skewedDf <- singleNumericVariableDf(baseRow = baseRow, 
                                          variable = col, 
                                          interval = skewedInterval, 
                                          size = 50)
      
      skewedPredictions <- predictFunction(newData = skewedDf)
      skewedLM <- lm(skewedPredictions$predictions ~ skewedDf[[col]])
      # Extract model coefficients
      skewedSlope <- skewedLM$coefficients[2]
      skewedIntercept <- skewedLM$coefficients[1]
      
      altProb <- round(skewedSlope*altValue + skewedIntercept, 4)
      
      summaryDf <- data.frame(variable = col, 
                              currentValue = signif(currentValue, 4),
                              altValue = signif(altValue, 4),
                              altProb = altProb,
                              delta = round(altProb - currentProb, 4), 
                              intercept = skewedIntercept,
                              slope = skewedSlope)
      featureList[[col]] <- summaryDf
      
      # Compute alternate values and probabilities for categorical variables
    } else {
      levels <- names(info2[[col]])
      for (level in levels) {
        altRow <- baseRow
        altRow[[col]] <- factor(level, levels = levels)
        altProb <- predictFunction(altRow)$predictions
        summaryDf <- data.frame(variable = col, 
                                currentValue = as.character(currentValue),
                                altValue = as.character(level),
                                altProb = round(altProb, 4),
                                delta = round(altProb - currentProb, 4), 
                                intercept = NA,
                                slope = NA,
                                stringsAsFactors = FALSE)
        featureList[[paste0(col, '.', level)]] <- summaryDf
      }
    }
  }
  
  # Arrange the featureList into a dataframe
  tempDf <- do.call(rbind, featureList)
  # Order by delta
  return(tempDf[order(tempDf$delta, decreasing = !lowerProbGoal), ])
}

buildTopModifiableFactorsDf = function(df,
                                       modifiableCols,
                                       info2,
                                       predictFunction,
                                       numTopFactors = 3,
                                       lowerProbGoal = TRUE,
                                       repeatedFactors = FALSE, 
                                       numberOfPercentiles = 12) {
  # Build the full list
  modFactorsList <- lapply(1:nrow(df), function(i) {
    modifiableFactors1Row(baseRow = df[i, ], 
                          modifiableCols = modifiableCols,
                          info2 = info2, 
                          predictFunction = predictFunction,
                          lowerProbGoal = lowerProbGoal, 
                          numberOfPercentiles = numberOfPercentiles)})
  
  # Drop slope and intercept columns and drop repeated top variables if 
  # appropriate
  if (!repeatedFactors) {
    modFactorList <- lapply(modFactorsList, function(rowDf) {
      rowDf <- rowDf[!duplicated(rowDf[, 1]), ]
      return(rowDf[, 1:5])
    })
  } else {
    modFactorList <- lapply(modFactorsList, function(rowDf) {
      return(rowDf[, 1:5])
    })
  }
  
  # Aetermine the maximum number of modifiable factors
  numTopFactors <- min(numTopFactors, nrow(modFactorsList[1]))
  # Combine into dataframe
  modFactorsDf <- do.call(rbind, lapply(modFactorList, function(rowDf){
    do.call(cbind, lapply(1:numTopFactors, function(i) {
      row <- rowDf[i, ]
      names(row) <- c(paste0("Modify", i, "TXT"),
                      paste0("Modify", i, "Current"), 
                      paste0("Modify", i, "AltValue"),
                      paste0("Modify", i, "AltProb"),
                      paste0("Modify", i, "Delta"))
      return(row)
    }))
  }))
  row.names(modFactorsDf) <- NULL
  
  return(modFactorsDf)
}

modifiableFactors1Row2 = function(baseRow, 
                                  modifiableCols,
                                  nonConstantCols,
                                  info,
                                  percentiles, 
                                  predictFunction,
                                  scale = 1/2,
                                  lowerProbGoal = TRUE) {
  currentProbModel <- predictFunction(baseRow)$predictions
  
  featureList <- list()
  for (col in modifiableCols) {
    # Save current value of the variable
    currentValue <- baseRow[[col]]
    
    # Compute alternate values and probabilities for numeric variables
    if (is.numeric(baseRow[[col]])) {
      
      global_min <- percentiles[[col]][1]
      global_max <- percentiles[[col]][101]
      
      # Build first linear model
      # Build dataframe with variable ranging within quantile range
      balancedDf <- singleNumericVariableDf2(baseRow = baseRow,
                                             variable = col, 
                                             info = info, 
                                             nonConstant = nonConstantCols, 
                                             skew = NULL,
                                             #size = 200, 
                                             scale = scale)
      # Make predictions and train a linear model on these
      balancedPredictions <- predictFunction(newData = balancedDf)
      balancedLM <- lm(balancedPredictions$predictions ~ balancedDf[[col]])
      # Extract linear model coefficients
      balancedSlope <- balancedLM$coefficients[2]
      
      # Build second linear model
      
      shiftAmount <- scale*info[[col]]
      # Case 1: want to shift to the right
      if (wantRightSkew(slope = balancedSlope,
                        currentValue = currentValue,
                        globalMin = global_min, 
                        globalMax = global_max,
                        lowerProbGoal = lowerProbGoal)) {
        # skew interval to the right
        altValue <- min(currentValue + shiftAmount, global_max)
        skew <- "positive" # for new dataframe
      } else {# Case 2: want to shift to the left
        # skew interval to the left
        altValue <- max(currentValue - shiftAmount, global_min)
        skew <- "negative"
      }
      skewedDf <- singleNumericVariableDf2(baseRow = baseRow,
                                           variable = col,
                                           info = info, 
                                           nonConstant = nonConstantCols,
                                           skew = skew,
                                           #size = 200, 
                                           scale = scale)
      skewedPredictions <- predictFunction(newData = skewedDf)
      skewedLM <- lm(skewedPredictions$predictions ~ skewedDf[[col]])
      
      # Extract model coefficients
      skewedSlope <- skewedLM$coefficients[2]
      skewedIntercept <- skewedLM$coefficients[1]
      
      currentProb <- skewedSlope*currentValue + skewedIntercept
      altProb <- skewedSlope*altValue + skewedIntercept
      delta <- altProb - currentProb
      
      summaryDf <- data.frame(variable = col, 
                              currentValue = currentValue,
                              altValue = signif(altValue, 4),
                              altProb = signif(currentProbModel + delta, 4),
                              delta = signif(delta, 4),
                              intercept = skewedIntercept,
                              slope = skewedSlope)
      featureList[[col]] <- summaryDf
      
      # Compute alternate values and probabilities for categorical variables
    } else {
      levels <- info[[col]]
      perturbedDf <- singleFactorVariableDf2(baseRow = baseRow, 
                                             variable = col, 
                                             factorLevels = levels,
                                             info = info, 
                                             nonConstant = nonConstantCols)
      predictions <- predictFunction(newData = perturbedDf)$predictions
      currentProb <- mean(predictions[perturbedDf[[col]] == baseRow[[col]]])
      for (level in levels) {
        altProb <- mean(predictions[perturbedDf[[col]] == level])
        delta <- altProb - currentProb
        summaryDf <- data.frame(variable = col, 
                                currentValue = as.character(currentValue),
                                altValue = as.character(level),
                                altProb = signif(currentProbModel + delta, 4),
                                delta = signif(delta, 4), 
                                intercept = NA,
                                slope = NA,
                                stringsAsFactors = FALSE)
        featureList[[paste0(col, '.', level)]] <- summaryDf
      }
    }
  }
  
  # Arrange the featureList into a dataframe
  tempDf <- do.call(rbind, featureList)
  # Order by delta
  return(tempDf[order(tempDf$delta, decreasing = !lowerProbGoal), ])
}


buildTopModifiableFactorsDf2 = function(modFactorsList,
                                        repeatedFactors = FALSE,
                                        numTopFactors = 3) {
   # Drop slope and intercept columns and drop repeated top variables if 
  # appropriate
  if (!repeatedFactors) {
    modFactorList <- lapply(modFactorsList, function(rowDf) {
      rowDf <- rowDf[!duplicated(rowDf[, 1]), ]
      return(rowDf[, 1:5])
    })
  } else {
    modFactorList <- lapply(modFactorsList, function(rowDf) {
      return(rowDf[, 1:5])
    })
  }
  
  # Aetermine the maximum number of modifiable factors
  numTopFactors <- min(numTopFactors, nrow(modFactorsList[1]))
  # Combine into dataframe
  modFactorsDf <- do.call(rbind, lapply(modFactorList, function(rowDf){
    do.call(cbind, lapply(1:numTopFactors, function(i) {
      row <- rowDf[i, ]
      names(row) <- c(paste0("Modify", i, "TXT"),
                      paste0("Modify", i, "Current"), 
                      paste0("Modify", i, "AltValue"),
                      paste0("Modify", i, "AltProb"),
                      paste0("Modify", i, "Delta"))
      return(row)
    }))
  }))
  row.names(modFactorsDf) <- NULL
  
  return(modFactorsDf)
}

wantRightSkew <- function(slope,
                          currentValue, 
                          globalMin, 
                          globalMax,
                          lowerProbGoal) {
  # First check slopes
  if ((slope < 0 & lowerProbGoal)
      | (slope > 0 & !lowerProbGoal)) {
    shiftRight <- TRUE
  } else {
    shiftRight <- FALSE
  }
  
  # Overwrite if value is too extreme
  if (currentValue <= globalMin) {
    shiftRight <- TRUE
  }
  if (currentValue >= globalMax) {
    shiftRight <- FALSE
  }
  return(shiftRight)
}