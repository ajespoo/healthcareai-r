# This function takes in the free text field that asked for an integer age which of course
# people put all sorts of stuff into. This should be sanitized on the mobile end, but
# we can easily deal with dirty data here.
sanitize_integers_in_free_text <- function(ages) {
    cleanAges <- c()
    
    for (age in ages) {
        # extract out integers from garbage data, may see NAs here
        trimmed_age <- as.numeric(regmatches(age, regexpr("[0-9]*", age)))
        
        # Note append is not inplace
        cleanAges <- append(cleanAges, trimmed_age)
    }
    
    # fill in all nas with the mean
    cleanAges[is.na(cleanAges)] <- mean(cleanAges, na.rm = TRUE)
    
    return(cleanAges)
}


remove_dashes_from_strings <- function(df) {
    # cleaning out dashes which are used in the findingVariation in r 1.0 release
    for (num in 1:ncol(df)) {
        options <- levels(df[, num])
        newLevels <- c()
        for (o in options) {
            o <- gsub("-", "_", o)
            newLevels <- append(newLevels, o)
        }
        levels(df[, num]) <- newLevels
    }
    return(df)
}


clean_profiles <- function(df){
	# Clean all 2017 profile data
	#
	# note that there is a lot of hard coded values that set the ordinal order. There
	# likely isn't a way around this.
	# TODO next year talk to mobile team about ordering in the app - there MUST be an order value

	# Nuke a few columns
	columnsToRemove <- c(
		"UserID",
		"UserFullNM",
		"QuestionDSC",
		"AnswerDSC"
		)
	for (column in columnsToRemove){
		if (!(is.null(df[[column]]))) {
			df[[column]] <- as.character(df[[column]])
		}
	}
	df$QuestionID <- NULL
	df$QuestionDSC <- NULL
	df$AnswerDSC <- NULL
	df$AnswerOrderNBR <- NULL

	# 2017 Profile question healthcare: experience
	df[,4] <- factor(df[,4], levels=c(
		"Not applicable",
		"Over 30 years", 
		"21-30 years", 
		"11-20 years", 
		"6-10 years", 
		"0-5 years"
		),
	ordered=TRUE)

	# 2017 Profile question Position:
	df[,5] <- factor(df[,5], levels=c(
		"Other or not applicable",
		"Front line (e.g., analyst, engineer)",
		"Mid-level (e.g., manager)",
		"Senior (VP, Director)",
		"Executive (e.g., C-Level, SVP)"
		),
		ordered=TRUE)

	# 2017 Profile question EDW: Success
	df[,8] <- factor(df[,8], levels=c(
		"Unsure or not applicable",
		"No",
		"Yes - although it has not been very successful",
		"Yes - a successful data warehouse is in place"
		),
		ordered=TRUE)

	# 2017 Profile question EDW: Success
	df[,9] <- factor(df[,9], levels=c(
		"Unsure or not applicable",
		"3 - Advanced stage - we have been using analytics for a long time and are increasingly using advanced and even predictive analytics across a variety of departments",
		"2 - Middle stage - we have been using analytics and data for a while and are beginning to see results in a few select areas",
		"1 - Beginning stage - we are recognizing the importance of analytics but in the early stages of determining what to do and how to do it"
		),
		ordered=TRUE)

	# 2017 Profile question Analyst: involvement
	df[,10] <- factor(df[,10], levels=c(
		"Unsure or not applicable",
		"1 - Not involved",
		"2 - Involved in initial reporting",
		"3 - Very involved throughout"
		),
		ordered=TRUE)

	# 2017 Profile question Free text age field needs cleaning
	df$FavoriteAgeDSC <- sanitize_integers_in_free_text(df[,15])

	# 2017 Profile question Something: about being weak
	df[,16] <- factor(df[,16], levels=c(
		"Not applicable",
		"1 - Weak or below average",
		"2 - Average to good",
		"3 - Strong or very strong"
		),
		ordered=TRUE)

	# 2017 Profile question Something: about being weak
	df[,17] <- factor(df[,17], levels=c(
		"Unsure or not applicable",
		"1 - Not successful",
		"2 - Moderately successful",
		"3 - Very successful"
		),
		ordered=TRUE)

	# 2017 Profile question 18: needs to be ordered. Long names
	num <- 18
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 4, 1, 2)], ordered = TRUE)

	# 2017 Profile question 19: is fine

	# 2017 Profile question 20: needs to be ordered. Long names
	num <- 20
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(5, 1, 4, 2, 3)], ordered = TRUE)

	# 2017 Profile question 21: needs to be ordered. Long names
	num <- 21
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(4, 2, 3, 1)], ordered = TRUE)

	# 2017 Profile question 22: needs to be ordered. Long names
	num <- 22
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 1, 2)], ordered = TRUE)

	# 2017 Profile question 23: needs to be ordered. Long names
	num <- 23
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 2, 1, 4)], ordered = TRUE)

	# 2017 Profile question 24: needs to be ordered. Long names
	num <- 24
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(4, 5, 3, 2, 1)], ordered = TRUE)

	# 2017 Profile question 25: is fine

	df <- remove_dashes_from_strings(df)

	return(df)
}
