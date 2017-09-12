# Script for HAS Polls

library(healthcareai)
library(dplyr)
library(RODBC)
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
foo = credentials()
conn <- odbcDriverConnect(connection = foo)

# Get data
query <- pollQuestionSQL(309)
df <- sqlQuery(conn, query)
head(df)

# Clean the thing
dfClean <- cleanProfiles(df)
dfClean$FavoriteAgeDSC

# Get variance
res <- singlePollVariance(dfClean, 'FavoriteAgeDSC', 343)



names(dfClean)
lapply(dfClean, class)
dfClean$FavoriteAgeDSC


df1 <- data.frame(
  Profile_q1 = sample(c("kermit", "animal", "elmo", "gonzo", "beaker"), 20, replace = TRUE),
  Profile_q2 = sample(c("Midazolam", "Propofolasdfasdf", "Ketamine", "Thiamylal", "Diazepam"), 20, replace = TRUE),
  Profile_q3 = sample(c("creamy", "mild", "medium", "hot", "muyCaliente"), 20, replace = TRUE),
  Profile_q4 = sample(1:5, 20, replace = TRUE),
  pollAnswer = sample(1:5, 20, replace = TRUE),  # poll question ID 343
  pollAnswer2 = rnorm(20) ) # poll question ID 343
df1$Profile_q3 <- factor(df1$Profile_q3, levels=c("creamy", "mild", "medium", "hot", "muyCaliente"), ordered=TRUE)                     
df1$Profile_q4 <- factor(df1$Profile_q4, ordered=TRUE) 

singlePollVariance(df1, 'pollAnswer2', 234)