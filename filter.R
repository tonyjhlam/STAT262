#require(ggplot2)

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

# rename 
write.csv(df, file='dataset_diabetes/diabetic_data.filtered.csv')
