# Script for HAS Polls
# Authors: Taylor(s), Mike(s)
#
# Notes:
# 	- There are lots of TODOs sprinkled throughout that should be turned into issues and dealt with for 2018
# 	- This was built with a slightly hacked version of healthcareai 1.0
# 		+ The hacks were entirely in the findingVariation, so after a refactor, 
#			this will need to be fixed.
# 	- The credentials file contains a simple connection string returning function called 'credentials()'
#		- example: server=<server dns>;database=<database>;uid=<user>;pwd=<password>;Port=1433;driver=FreeTDS;TDS_Version=8.0;
#			* note the driver here was for macOS. For windows switch to something like {SQL Server 11}
#	- Note that for speed of hacks the categorical dataframe returns all zeros in the median difference
#		to make sorting the entire dataframe easier. This should probably be fixed next year.

library(healthcareai)
library(dplyr)
library(RODBC)
# This needs to be adjusted to your copy of the hacked 1.0 healthcareai TODO remove this hack for 2018
setwd("~/repos/healthcareai-r")
source("common-sql-queries.R")
source("common-profile-cleaner.R")
source("./.credentials.R")

# Find variance for an ordinal poll question compared to all profile questions
single_poll_variance <- function(df1, measure_column_name, question_id) {
    glued <- data.frame()
    raw_results <- list()
    
    categorical_columns <- names(Filter(is.factor, df1))
    
    for (q in categorical_columns) {
        raw_results[[q]] <- variationAcrossGroups(df1, categoricalCols = q, measureCol = measure_column_name, sigLevel = 0.05, 
            plotGroupDifferences = FALSE, returnGroupStats = TRUE)
        # Note the heinously long column name that comes from healthcareai 1.0 TODO change this column after the refactor next year
        raw_results[[q]]$`Multiple-comparison-adjusted p-values for each pair of groups`$profileQuestionNumber <- q
        glued <- rbind(glued, raw_results[[q]]$`Multiple-comparison-adjusted p-values for each pair of groups`)
    }
    
    glued <- glued[order(glued$`Adjusted p-value`), ]
    glued$pollquestion_id <- question_id
    return(glued)
}

# Find variance for a categorical poll question compared to all profile questions
single_poll_categorical <- function(df1, measure_column_name, question_id) {
    require(tidyverse)
    # ID profile question column numbers (to loop over)
    profile_questions <- which(!names(df1) %in% c("UserID", "UserFullNM", "FavoriteAgeDSC"))
    
    responseCol <- grep(measure_column_name, names(df1))
    profile_questions <- profile_questions[!profile_questions %in% responseCol]
    out <- map_df(profile_questions, function(Q) {
        
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
            
            data_frame(Groups = paste(pairs[i, ], collapse = "-"), `Mean Difference` = 0, `Adjusted p-value` = chiSq$p.value, 
                profileQuestionNumber = names(df1)[Q], pollquestion_id = question_id)
        })
    })
    out <- arrange(out, `Adjusted p-value`)
    return(as.data.frame(out))
}


find_variance <- function(question_id) {
    # setup azure connection from credentials file question_id = 980
    connection_string = credentials()
    conn <- odbcDriverConnect(connection = connection_string)
    
    # Get data
    query <- pollQuestionSQL(question_id)
    df <- sqlQuery(conn, query)
    
    # Clean the profile data
    dfClean <- clean_profiles(df)
    
    print(paste("number of missing rows:", sum(is.na(dfClean$AnswerNBR))))  # Count of missing answers
    print(paste("missing rows ratio:", sum(is.na(dfClean$AnswerNBR))/nrow(dfClean)))  # Proportion of missing answers
    
    # Clean the data (and filter out anyone who hasn't answered the questions - NULLS)
    dfClean <- dfClean[!is.na(dfClean$AnswerNBR), ]
    print(paste("Found", length(dfClean$UserID), "answers"))
    
    # Create some fake data for testing dfClean$AnswerNBR <- sample(1:5, nrow(dfClean), replace=TRUE)
    
    # hack for base r box plot in current version findingVariation TODO clean up next year png('../../Desktop/tmp.png', width =
    # 400000, height = 40000)
    
    # Get variance for either ordinal or categorical
    if (is.numeric(dfClean$AnswerNBR)) {
        variances <- single_poll_variance(dfClean, "AnswerNBR", question_id)
    } else {
        variances <- single_poll_categorical(dfClean, "AnswerNBR", question_id)
    }
    
    # dev.off()
    return(variances)
}

# In session running a single poll question
q = 992
results <- find_variance(q)
View(results)

# All poll questions for day 1 bulk analyis
all_day_one_poll_questions <- c(1156, 912, 977, 978, 979, 1080, 1091, 980, 1083, 1084, 913, 914, 915, 916, 917, 918, 919, 920, 
    921, 922, 923, 924, 925, 926, 927, 928, 929, 930, 981, 982, 983, 984, 985, 986, 987, 988, 989)

all_results <- map_df(all_day_one_poll_questions, find_variance)
all_results <- arrange(all_results, `Adjusted p-value`)