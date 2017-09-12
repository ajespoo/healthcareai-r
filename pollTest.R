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



# setup azure connection from sourced file
tempCredentials = credentials()
conn <- odbcDriverConnect(connection = tempCredentials)

# Get data
questionID = 309

query <- pollQuestionSQL(questionID)
df <- sqlQuery(conn, query)
head(df)

# Clean the profile data
dfClean <- cleanProfiles(df)
dfClean$FavoriteAgeDSC
names(dfClean)

# fake up some data
dfClean$AnswerNBR <- sample(1:5, nrow(dfClean), replace=TRUE)

# Get variance
variances <- singlePollVariance(dfClean, 'AnswerNBR', questionID)

# lapply(dfClean, class)
# names(dfClean)
# lapply(dfClean, levels)