# Prototype / testing stuff for HAS



# set.seed(35)

# Sample data
df1 <- data.frame(
  Profile_q1 = sample(c("kermit", "animal", "elmo", "gonzo", "beaker"), 20, replace = TRUE),
  Profile_q2 = sample(c("Midazolam", "Propofolasdfasdf", "Ketamine", "Thiamylal", "Diazepam"), 20, replace = TRUE),
  Profile_q3 = sample(c("creamy", "mild", "medium", "hot", "muyCaliente"), 20, replace = TRUE),
  Profile_q4 = sample(1:5, 20, replace = TRUE),
  pollAnswer = sample(1:5, 20, replace = TRUE))  # poll question ID 343
df1$Profile_q3 <- factor(df1$Profile_q3, levels=c("creamy", "mild", "medium", "hot", "muyCaliente"), ordered=TRUE)                     
df1$Profile_q4 <- factor(df1$Profile_q4, ordered=TRUE) 
head(df1)
is.ordered(df1$Profile_q3)


# There are 3 profile questions
# there is one current poll question with 5 answers

catVars <- c('Profile_q1', 'Profile_q2', 'Profile_q3', 'Profile_q4')
