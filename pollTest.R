# Script for HAS Polls

library(healthcareai)
library(dplyr)
library(RODBC)
source('common-sql-queries.R')
source('common-profile-cleaner.R')
source('.credentials.R')

# Profile vs a single poll question id
singlePollVariance <- function(df1, catVarList, measureColName, questionID) {
	glued <- data.frame()
	res <- list()
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

res <- singlePollVariance(df1, catVars, 'pollAnswer', 343)


# setup azure connection from sourced file
conn <- odbcDriverConnect(connection = credentials()

# Get data
query <- pollQuestionSQL(309)
df <- sqlQuery(conn, query)
head(df)

dfClean <- cleanProfiles(df)

names(dfClean)
lapply(dfClean, class)
dfClean$FavoriteAgeDSC