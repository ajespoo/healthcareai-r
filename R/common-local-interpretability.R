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


singleNumericVariableDf = function(baseRow, 
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

singleFactorVariableDf = function(baseRow,
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

modifiableFactors1Row = function(baseRow, 
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
      balancedDf <- singleNumericVariableDf(baseRow = baseRow,
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
      skewedDf <- singleNumericVariableDf(baseRow = baseRow,
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


buildTopModifiableFactorsDf = function(modFactorsList,
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