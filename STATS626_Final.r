
library(ggplot2)
library(dplyr)

# load data 
df <- read.csv('dataset_diabetes/diabetic_data.csv')
map <- read.csv('dataset_diabetes/IDs_mapping.csv')
df[df == '?'] = NA
df[df == 'None'] = NA

# remove columns with NAs
#df <- df[,colSums(is.na(df)) == 0]

# relabel readmitted
df$readmitted = ifelse(df$readmitted=='NO',0,1)

# turn treatment into binomial
df[22:47] <- ifelse(df[22:47] == 'No', 0, 1)

# process insulin dataframe
df.insulin <- df[c('insulin','age')] #subset dataframe
df.insulin$insulin <- ifelse(df.insulin$insulin == 0, 'No', 'Yes') #relabel
df.insulin$insulin <- as.factor(df.insulin$insulin) 

# edit age labels
df.insulin$age <- gsub("[^0-9-]","",df.insulin$age) # remove [ )
df.insulin$age <- gsub("-","_",df.insulin$age)
df.insulin$age <- paste("age" , df.insulin$age, sep='_')

#plot histogram
ggplot(df.insulin, aes(x=insulin))+
    stat_count(width = 0.5)+
    facet_wrap(~age,nrow = 2)+
    xlab('Insulin Usage')+
    geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
    theme_bw()

#split dataset by age group
df.insulin.split <- split(df.insulin, df.insulin$age)

table(df.insulin.split$age_0_10)

# summarize splits
for (i in 1: length(df.insulin.split)){
    #df.insulin.split[[i]] <- table(df.insulin.split[[i]])
    s <- table(df.insulin.split[[i]])[2] #yes (success)
    f <- table(df.insulin.split[[i]])[1] #no (failure)
    n <- s + f
    bernoulli_p <- s/(s + f)
}
