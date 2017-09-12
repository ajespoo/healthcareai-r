ageCleaner <- function(ages) {
	cleanAges <- c()

	for (age in ages){
		# extract out integers from garbage data, may see NAs here
		trimmedAge <- as.numeric(regmatches(age, regexpr("[0-9]*", age)))

		# Note append is not inplace
		cleanAges <- append(cleanAges, trimmedAge)
	}

	# fill in all nas with the mean
	cleanAges[is.na(cleanAges)] <- mean(cleanAges, na.rm=TRUE)

	return(cleanAges)
}


cleanProfiles <- function(df){

	columnsToRemove <- c(
		"UserID",
		"UserFullNM",
		"QuestionDSC",
		"AnswerDSC"
		)

	for (column in columnsToRemove){
		df[[column]] <- as.character(df[[column]])
	}

	# healthcare experience
	df[,4] <- factor(df[,4], levels=c(
		"Not applicable",
		"Over 30 years", 
		"21-30 years", 
		"11-20 years", 
		"6-10 years", 
		"0-5 years"
		),
	ordered=TRUE)

	# Position
	df[,5] <- factor(df[,5], levels=c(
		"Other or not applicable",
		"Front line (e.g., analyst, engineer)",
		"Mid-level (e.g., manager)",
		"Senior (VP, Director)",
		"Executive (e.g., C-Level, SVP)"
		),
		ordered=TRUE)

	# EDW Success
	df[,8] <- factor(df[,8], levels=c(
		"Unsure or not applicable",
		"No",
		"Yes - although it has not been very successful",
		"Yes - a successful data warehouse is in place"
		),
		ordered=TRUE)

	# EDW Success
	df[,9] <- factor(df[,9], levels=c(
		"Unsure or not applicable",
		"3 - Advanced stage - we have been using analytics for a long time and are increasingly using advanced and even predictive analytics across a variety of departments",
		"2 - Middle stage - we have been using analytics and data for a while and are beginning to see results in a few select areas",
		"1 - Beginning stage - we are recognizing the importance of analytics but in the early stages of determining what to do and how to do it"
		),
		ordered=TRUE)

	# Analyst involvement
	df[,10] <- factor(df[,10], levels=c(
		"Unsure or not applicable",
		"1 - Not involved",
		"2 - Involved in initial reporting",
		"3 - Very involved throughout"
		),
		ordered=TRUE)

	# Free text age field needs cleaning
	df$FavoriteAgeDSC <- ageCleaner(df[,15])

	# Something about being weak
	df[,16] <- factor(df[,16], levels=c(
		"Not applicable",
		"1 - Weak or below average",
		"2 - Average to good",
		"3 - Strong or very strong"
		),
		ordered=TRUE)

	# Something about being weak
	df[,17] <- factor(df[,17], levels=c(
		"Unsure or not applicable",
		"1 - Not successful",
		"2 - Moderately successful",
		"3 - Very successful"
		),
		ordered=TRUE)

	# 18 needs to be ordered. Long names
	num <- 18
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 4, 1, 2)], ordered = TRUE)

	# 19 is fine

	# 20 needs to be ordered. Long names
	num <- 20
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(5, 1, 4, 2, 3)], ordered = TRUE)

	# 21 needs to be ordered. Long names
	num <- 21
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(4, 2, 3, 1)], ordered = TRUE)

	# 22 needs to be ordered. Long names
	num <- 22
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 1, 2)], ordered = TRUE)

	# 23 needs to be ordered. Long names
	num <- 23
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(3, 2, 1, 4)], ordered = TRUE)

	# 24 needs to be ordered. Long names
	num <- 24
	names(df)[num]
	levels(df[,num])
	choices <- levels(df[,num])
	df[,num] <- factor(df[,num], levels = choices[c(4, 5, 3, 2, 1)], ordered = TRUE)

	# 25 is fine

	return(df)
}