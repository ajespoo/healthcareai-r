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
  
  map_df(profileQs, function(Q) {
  
    answers <- as.character(unique(df1[[Q]]))
    answers <- answers[!is.na(answers)]
    # Get all the pairs of answers, then remove duplicates
    pairs <- expand.grid(answer1 = answers, answer2 = answers, stringsAsFactors = FALSE)
    pairs <- pairs[pairs$answer1 < pairs$answer2, ]
    
    
    map_df(seq_len(nrow(pairs)), function(i) {
      
      message(i, " - ", Q)
      
      fTab <- table(df1[df1[[Q]] %in% pairs[i, ], c(Q, responseCol)])
      # Remove rows not in the two answers we're interested in
      fTab <- fTab[rowSums(fTab) > 1, ]
      if (nrow(fTab) > 2)
        stop("There should only be two answers here, but there are more.")
      
      # Run chi-squared
      chiSq <- suppressWarnings(chisq.test(fTab))
      
      data.frame(
        Groups = paste(pairs[i, ], collapse = "-"),
        `Mean Difference` = 0,
        `Adjusted p-value` = chiSq$p.value,
        profileQuestionNumber = names(df1)[Q],
        pollQuestionID = questionID
      )
      
    })
  })
}
singlePollCategorical(dfClean, "AnswerNBR", 977)

# # # setup azure connection from sourced file
# tempCredentials = credentials()
# conn <- odbcDriverConnect(connection = tempCredentials)
# 
# # # Get data
# questionID = 977
# query <- pollQuestionSQL(questionID)
# df <- sqlQuery(conn, query)
# head(df)
# 
# # # Clean the profile data
# dfClean <- cleanProfiles(df)
# # names(dfClean)
# length(dfClean$UserID)

# fake up some data
# dfClean$AnswerNBR <- sample(1:5, nrow(dfClean), replace=TRUE)

# Get variance
# variances <- singlePollVariance(dfClean, 'AnswerNBR', questionID)


# findVariance <- function(questionID){
	# setup azure connection from sourced file
	questionID = 977
	tempCredentials = credentials()
	conn <- odbcDriverConnect(connection = tempCredentials)

	# Get data
	query <- pollQuestionSQL(questionID)
	df <- sqlQuery(conn, query)
	# head(df)

	# Clean the profile data
	dfClean <- cleanProfiles(df)

	print(sum(is.na(dfClean$AnswerNBR))) # Count of missing answers
	print(paste('missing rows percentage:', sum(is.na(dfClean$AnswerNBR)) / nrow(dfClean)))  # Proportion of missing answers
	
	# Ceal
	dfClean <- dfClean[!is.na(dfClean$AnswerNBR), ]

	# names(dfClean)
	print(length(dfClean$UserID))

	# fake up some data
	# dfClean$AnswerNBR <- sample(1:5, nrow(dfClean), replace=TRUE)

	# Get variance
	# png("../../Desktop/tmp.png", width = 400000, height = 40000)
	variances <- singlePollVariance(dfClean, 'AnswerNBR', questionID)
	str(variances)
  # dev.off()
# 	return(variances)
# }

# Run this bit here
q = 978
results <- findVariance(q)
View(results)


# lapply(dfClean, class)
# names(dfClean)
# lapply(dfClean, levels)