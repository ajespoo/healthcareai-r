df <- read.csv('~/Box Sync/Michael Mastanduno/healthcareai.working/levels.csv')
df$X <- NULL

ncol(df)
names(df)

num <- 24
names(df)[num]
levels(df[,num])



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
df[,num] <- factor(df[,num], levels = choices[c(3, 2, 2, 4)], ordered = TRUE)

# 24 needs to be ordered. Long names
num <- 24
names(df)[num]
levels(df[,num])
choices <- levels(df[,num])
df[,num] <- factor(df[,num], levels = choices[c(4, 5, 3, 2, 1)], ordered = TRUE)

# 25 is fine