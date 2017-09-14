library(GGally)
library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(DBI)
library(odbc)
library(tidyverse)

source('common-sql-queries.R')
source('common-profile-cleaner.R')
source('./.credentials.R')
tempCredentials = credentials()
conn <- odbcDriverConnect(connection = tempCredentials)
quer <- "SELECT
     DISTINCT
                  sr.UserID AS FromUserID
                  ,uu.ToUserID
                  FROM dbo.SurveyResults sr
                  LEFT OUTER JOIN [dbo].[UserToUserConnections] uu
                  ON sr.UserID = uu.FromUserID
                  AND uu.FromUserID <> uu.ToUserID
                  WHERE 1=1
                  AND sr.EventId IN (29/*has 2017*/,32/*hcu 2017*/)
                  AND sr.SurveyID IN (318)/*not including pre-summit profile data*/"
dat <- sqlQuery(conn, quer)
dat <- dat[complete.cases(dat), ]

query <- "SELECT
   UserID
,[905] AS CurrentRoleDSC
,[906] AS HealthcareExperienceDSC
,[907] AS SeniorityLevelDSC
,ISNULL([909],9) AS ProviderTypeDSC
FROM
(SELECT
DISTINCT
ui.ID AS UserID
,CONCAT(ui.LastName,', ',ui.FirstName) AS UserFullNM
,QuestionID AS QuestionID
,AnswerDisplay AS AnswerValueDSC
FROM Eventalytics_db.dbo.SurveyResults sr
INNER JOIN Eventalytics_db.dbo.UserInformations ui
ON sr.UserID = ui.ID
WHERE (sr.SurveyID = 318 OR (sr.SurveyID = 339 AND sr.QuestionID = 936))
AND ui.username NOT LIKE '%@healthcatalyst.com'
) p
PIVOT
(
 MAX(AnswerValueDSC)
 FOR QuestionID IN ([905],[906],[907],[909])
) AS pvt
 GROUP BY
 UserID
 ,UserFullNM
 ,[905]
 ,[906]
 ,[907]
 ,[909]"
df <- sqlQuery(conn, query)
atts <- select(df, UserID, SeniorityLevelDSC, CurrentRoleDSC)


net <- 
  mutate_all(dat, as.character) %>%
  as.matrix() %>% 
  igraph::graph_from_edgelist(directed = FALSE) %>%
  intergraph::asNetwork()

deg <- sna::degree(net, gmode = "graph")
net %v% "degree" <- deg

atts$UserID <- as.character(atts$UserID)
atts$senLevel <- as.character(atts$SeniorityLevelDSC)
ids <- data.frame(UserID = net %v% "vertex.names")
atts <- left_join(ids, atts)
# sum(!complete.cases(atts))
atts$senLevel[is.na(atts$senLevel)] <- "Other or not applicable"
net %v% "SeniorityLevel" <- atts$senLevel

vcols <- c("#00A859",
           "#6E53A3",
           "#F8961D",
           "#F05323",
           "#F5ED56"
           # "#EF4767"
           )
names(vcols) <- levels(factor(net %v% "SeniorityLevel"))
vcols <- vcols[c(1, 5, 3, 2, 4)]
vertexCols <- vcols[factor(net %v% "SeniorityLevel")]
png("../../Desktop/HAS_Net.png", width = 9, height = 9, units = "in", res = 300)
par(bg = '#006D9A')
plot(net 
     , vertex.col = vertexCols
     , vertex.cex = log10(net %v% "degree" + 1)
     , edge.col = adjustcolor("white", .1)  #  "#5E676F"
     , vertex.border = adjustcolor("white", .1)  #  "#5E676F")
)
legend(x = "bottomleft", legend = names(vcols), fill = vcols, 
       text.col = "white", box.col = "white")
dev.off()
