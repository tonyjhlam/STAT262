
library(ggplot2)
library(dplyr)

# load data 
df <- read.csv('dataset_diabetes/diabetic_data.csv')
map <- read.csv('dataset_diabetes/IDs_mapping.csv')
df[df == '?'] = NA
df[df == 'None'] = NA

# remove columns with NAs
df <- df[,colSums(is.na(df)) == 0]

# relabel readmitted
df$readmitted = ifelse(df$readmitted=='NO',0,1)

# remove columns
df <- df[ , !(names(df) %in% c('discharge_disposition_id', 'admission_source_id'))]

#delete invalid gender
df <- df[- grep("Unknown/Invalid", df$gender),]

# turn treatment into binomial
df[16:38] <- ifelse(df[16:38] == 'No', 0, 1)

# rename 
#write.csv(df, file='dataset_diabetes/diabetic_data.filtered.csv')

colnames(df)

head(df)

install.packages('bang')

library(bang)

df.new <- df[c('insulin','age')]
df.new$age <- as.numeric(factor(df.new$age))

df.new$age <- as.numeric(factor(df.new$age))
df.new$insulin <- as.numeric(df.new$insulin)

colnames(df.new) <- c('y','n')

df.new$y[df.new$y == 0] <- 2

df.new$n <- df.new$n + 10

success.df <- df.new %>% filter(y == 1) %>% group_by(n) %>% summarise(success=n())

count.df <- df.new %>% group_by(n) %>% summarise(count=n())

summary.df <- merge(success.df, count.df)

dim(df.new)
summary.2.df

summary.2.df <- summary.df[,2:3]


bb_model <- hef(model = "beta_binom", data = summary.2.df, n = 1000)

user_prior <- function(x)
{
    return(-5 * log(x[1] + x[2]))
} 

plot(bb_model, ru_scale = TRUE)
