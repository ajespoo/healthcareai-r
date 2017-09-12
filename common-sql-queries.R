pollQuestionSQL <- function(questionID) {

	query <- paste0("WITH Step1CTE AS (
	SELECT
		 DISTINCT
		 sr.UserID
		,sr.QuestionID
		,sr.QuestionDisplay AS QuestionDSC
		,sr.AnswerDisplay AS AnswerDSC
		,sr.AnswerValue AS AnswerNBR
		,sr.OrderNumber AS AnswerOrderNBR
	FROM [Eventalytics_db].[dbo].Sessions s
	INNER JOIN [Eventalytics_db].[dbo].[SessionSurveys] ss
		ON s.ID = ss.SessionID
	INNER JOIN [Eventalytics_db].[dbo].[SurveyResults] sr
		ON ss.SessionID = sr.SessionID
	WHERE 1=1
		AND s.SessionCapacity = 1100
		AND sr.EventID = 29
		AND sr.QuestionID IN (",questionID,") --this is the QuestionID that we'd want to parameterize
		AND ISNUMERIC(sr.[AnswerValue]) = 1
		)

	,Step2CTE AS (
	SELECT
		 UserID
		,UserFullNM
		,[905] AS CurrentRoleDSC
		,[906] AS HealthcareExperienceDSC
		,[907] AS SeniorityLevelDSC
		,[908] AS CharacterizeOrganizationDSC
		,[909] AS ProviderTypeDSC
		,[931] AS HaveDataWarehouseDSC
		,[932] AS AdoptingAnalyticsDSC
		,[933] AS AnalystsQualityImprovementDSC
		,[935] AS FavoritySuperHeroDSC
		,[939] AS SuperPowerDSC
		,[940] AS DiscoverUseOrPreventDSC
		,[941] AS EatOrSleepDSC
		,[942] AS FavoriteAgeDSC
		,[943] AS QualityImprovementCultureDSC
		,[944] AS OrganizationsPopHealthDSC
		,[945] AS ImpactValueBasedOnCareDSC
		,[946] AS MissingDataSetsDSC
		,[947] AS YearsForMLIntegrationDSC
		,[948] AS TransparentPricingDSC
		,[949] AS MonitoringPatientHealthDSC
		,[950] AS TrueCostingImportanceDSC
		,[951] AS FeelingsOnAnalyticsDSC
		,[952] AS GrandPrizeChoiceDSC
		,[936] AS NetworkingTypeDSC
	FROM 
		(SELECT
			 DISTINCT
			 ui.ID AS UserID
			,CONCAT(ui.LastName,', ',ui.FirstName) AS UserFullNM
			,QuestionID AS QuestionID
			,AnswerDisplay AS AnswerDSC
		FROM Eventalytics_db.dbo.SurveyResults sr
		INNER JOIN Eventalytics_db.dbo.UserInformations ui
			ON sr.UserID = ui.ID
		WHERE sr.SurveyID = 318 --HAS 2017 profile survey
		) p
		PIVOT
		(
			MAX(AnswerDSC)
				FOR QuestionID IN ([905],[906],[907],[908],[909],[931],[932],[933],[935],[939],[940],[941],[942],[943],[944],[945],[946],[947],[948],[949],[950],[951],[952],[936])
		) AS pvt
	GROUP BY
		 UserID
		,UserFullNM
		,[905]
		,[906]
		,[907]
		,[908]
		,[909]
		,[931]
		,[932]
		,[933]
		,[935]
		,[939]
		,[940]
		,[941]
		,[942]
		,[943]
		,[944]
		,[945]
		,[946]
		,[947]
		,[948]
		,[949]
		,[950]
		,[951]
		,[952]
		,[936]
	)

	SELECT
		 DISTINCT
		 s2.UserID
		,s2.UserFullNM
		,s2.CurrentRoleDSC
		,s2.HealthcareExperienceDSC
		,s2.SeniorityLevelDSC
		,s2.CharacterizeOrganizationDSC
		,s2.ProviderTypeDSC
		,s2.HaveDataWarehouseDSC
		,s2.AdoptingAnalyticsDSC
		,s2.AnalystsQualityImprovementDSC
		,s2.FavoritySuperHeroDSC
		,s2.SuperPowerDSC
		,s2.DiscoverUseOrPreventDSC
		,s2.EatOrSleepDSC
		,s2.FavoriteAgeDSC
		,s2.QualityImprovementCultureDSC
		,s2.OrganizationsPopHealthDSC
		,s2.ImpactValueBasedOnCareDSC
		,s2.MissingDataSetsDSC
		,s2.YearsForMLIntegrationDSC
		,s2.TransparentPricingDSC
		,s2.MonitoringPatientHealthDSC
		,s2.TrueCostingImportanceDSC
		,s2.FeelingsOnAnalyticsDSC
		,s2.GrandPrizeChoiceDSC
		,s1.QuestionID
		,s1.QuestionDSC
		,s1.AnswerDSC
		,s1.AnswerNBR
		,s1.AnswerOrderNBR
	FROM Step2CTE s2
	LEFT OUTER JOIN Step1CTE s1
		ON s2.UserID = s1.UserID")

	return(query)
}

pollQuestionSQL(88)