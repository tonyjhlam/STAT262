#require(ggplot2)

# load data 
df <- read.csv('dataset_diabetes/diabetic_data.csv')
df[df == '?'] = NA
df[df == 'None'] = NA

# remove columns with NAs
df <- df[,colSums(is.na(df)) == 0]

# relabel readmitted
df$readmitted = ifelse(df$readmitted=='NO',0,1)

write.csv(df, file='dataset_diabetes/diabetic_data.filtered.csv')
