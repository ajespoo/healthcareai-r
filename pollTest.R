# Script for HAS Polls

library(healthcareai)
library(dplyr)
library(RODBC)
setwd('~/repos/healthcareai-r')
source('common-sql-queries.R')
source('common-profile-cleaner.R')
source('./.credentials.R')


# Profile vs a single poll question id
singlePollVariance <- function(df1, measureColName, questionID) {
	glued <- data.frame()
	res <- list()

	catVarList <- names(Filter(is.factor, df1))

	for (q in catVarList) { 
		res[[q]] <- variationAcrossGroups(df1, 
							  categoricalCols = q,
							  measureCol = measureColName,
							  sigLevel = .05,
							  plotGroupDifferences = FALSE,
							  returnGroupStats = TRUE)
		res[[q]]$`Multiple-comparison-adjusted p-values for each pair of groups`$profileQuestionNumber <- q
		glued <- rbind(glued, res[[q]]$`Multiple-comparison-adjusted p-values for each pair of groups`)
	}

	glued <- glued[order(glued$`Adjusted p-value`),]
	glued$pollQuestionID <- questionID
	return(glued)
}

singlePollCategorical <- function(df1, measureColName, questionID) {
  
  require(tidyverse)
  # ID profile question column numbers (to loop over)
  profileQs <- which(!names(df1) %in% c("UserID", "UserFullNM", "FavoriteAgeDSC"))
  
  responseCol <- grep(measureColName, names(df1)) 
  profileQs <- profileQs[!profileQs %in% responseCol]
  out <- 
    map_df(profileQs, function(Q) {
      
      answers <- as.character(unique(df1[[Q]]))
      answers <- answers[!is.na(answers)]
      # Get all the pairs of answers, then remove duplicates
      pairs <- expand.grid(answer1 = answers, answer2 = answers, stringsAsFactors = FALSE)
      pairs <- pairs[pairs$answer1 < pairs$answer2, ]
      
      
      map_df(seq_len(nrow(pairs)), function(i) {
        
        fTab <- table(df1[df1[[Q]] %in% pairs[i, ], c(Q, responseCol)])
        # Remove rows not in the two answers we're interested in
        fTab <- fTab[rowSums(fTab) > 1, ]
        if (nrow(fTab) > 2)
          stop("There should only be two answers here, but there are more.")
        
        # Run chi-squared
        chiSq <- suppressWarnings(chisq.test(fTab))
        
        data_frame(
          Groups = paste(pairs[i, ], collapse = "-"),
          "Mean Difference" = 0,
          "Adjusted p-value" = chiSq$p.value,
          profileQuestionNumber = names(df1)[Q],
          pollQuestionID = questionID
        ) 
        
      })
    }) 
  out <- arrange(out, `Adjusted p-value`)
  return(as.data.frame(out))
}


findVariance <- function(questionID){
	# setup azure connection from sourced file
	# questionID = 980
	tempCredentials = credentials()
	conn <- odbcDriverConnect(connection = tempCredentials)

	# Get data
	query <- pollQuestionSQL(questionID)
	df <- sqlQuery(conn, query)
	# head(df)

	# Clean the profile data
	dfClean <- cleanProfiles(df)

	print(paste('number of missing rows:',sum(is.na(dfClean$AnswerNBR)))) # Count of missing answers
	print(paste('missing rows ratio:', sum(is.na(dfClean$AnswerNBR)) / nrow(dfClean)))  # Proportion of missing answers
	
	# Clean the data (and filter out anyone who hasn't answered the questions - NULLS)
	dfClean <- dfClean[!is.na(dfClean$AnswerNBR), ]
	print(paste('Found',length(dfClean$UserID), 'answers'))

	# fake up some data
	# dfClean$AnswerNBR <- sample(1:5, nrow(dfClean), replace=TRUE)

	# png("../../Desktop/tmp.png", width = 400000, height = 40000)

	# Get variance for either ordinal or categorical
	if (is.numeric(dfClean$AnswerNBR)){
		variances <- singlePollVariance(dfClean, 'AnswerNBR', questionID)
		
	} else {
		variances <- singlePollCategorical(dfClean, 'AnswerNBR', questionID)
	}

  # dev.off()
	return(variances)
}

# Run this bit here
q = 978
results <- findVariance(q)
View(results)


# lapply(dfClean, class)
# names(dfClean)
# lapply(dfClean, levels)