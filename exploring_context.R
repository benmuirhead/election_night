library(ggplot2)
library(ggcorrplot)
#library(class)
library(neighbr)
library(caret)
library(tictoc)
library(doParallel)
library(foreach)
library(tidyverse)

num_cores = detectCores()
registerDoParallel(num_cores)

context = read.csv("election-context-2018.txt",header = TRUE,sep = ",")


na_count = function(input_df) {
  col_count = ncol(input_df)
  if (is.na(col_count) || col_count == 0) {
    stop("No Columns")
  }
  data.frame(sapply(input_df, function(y) sum(length(which(is.na(y))))))
 }

na_count(context)

#Bad Rows
context[is.na(context["cvap"]),]

#remove Bedford VA
context = context[context$fips != 51515,]

#Set KC and Ogala to have all zeros in NA fields
context[context$fips == 36000 | context$fips == 46113 ,]
context[is.na(context["cvap"]),][is.na(context[is.na(context["cvap"]),])] = 0

#Cols to keep
cols_list = colnames(context[,23:39])
cols_list = cols_list[!cols_list %in% c("nonwhite_pct", "lesshs_whites_pct", "lesscollege_whites_pct", "ruralurban_cc","clf_unemploy_pct","cvap")]

#Keep fips and cols_list
cleaned_context = context[!is.na(context["cvap"]),c("fips",cols_list)]

#Same thing as above
cleaned_context_numeric = context[!is.na(context["cvap"]),c("fips",cols_list)]

cor_matrix_context = cor(cleaned_context_numeric)
ggcorrplot(cor_matrix_context,type = "upper",hc.order = TRUE)


#set range of cols to be 0 to 1
preProcessValues=preProcess(cleaned_context_numeric[cols_list],method = "range",rangeBounds = c(0,1))
clean_context_numeric_transformed = predict(object = preProcessValues, cleaned_context_numeric)

summary(clean_context_numeric_transformed)

model1 = knn(train_set = clean_context_numeric_transformed[0:50,],
    test_set = clean_context_numeric_transformed[0:100,
        #3000:nrow(cleaned_context_numeric),
                                       !colnames(clean_context_numeric_transformed) %in% "fips"],
    k=50,
    return_ranked_neighbors=3,
    id="fips",
    comparison_measure = "squared_euclidean"
    )

model1$test_set_scores
na_count( cleaned_context_numeric[3000:nrow(cleaned_context_numeric),!colnames(cleaned_context_numeric) %in% "fips"])

similarity(round(clean_context_numeric_transformed[1,-1]),round(clean_context_numeric_transformed[2000,-1]),measure = "simple_matching")

?similarity

#Process
#For every pair of counties:
#sum up differences between values (squared values?). 1-that.
# For each row, rank.


tic()
scores = foreach (i = 1:nrow(clean_context_numeric_transformed), .combine='rbind', .packages = "foreach") %dopar% {
  #scores[[i]] = numeric(nrow(clean_context_numeric_transformed))
  x = clean_context_numeric_transformed[i,-1]
  foreach (j = 1:nrow(clean_context_numeric_transformed), .combine = 'c') %do% {
    
    if(i==j){
      #scores[[i]][j] = 
      NA
      #print(NA)
    } else {
      y = clean_context_numeric_transformed[j,-1]
      #scores[[i]][j] = 
      round((sum((x-y)^2)),2)
      #print(j)
      #print(scores[[i]][j])
      
    }
    
  }
}
toc()


head(scores[1:1000],1000)
hist(scores,breaks = 100)
summary(scores)

str(z)
summary(z)
scores[[i]][1:100]
sort(scores[[i]])

str(scores)

x = clean_context_numeric_transformed[1,-1]
y = clean_context_numeric_transformed[2,-1]
x
y
sum((x-y)^2)

apply()


str(scores)
hist(1/scores[1,],breaks=100)

hist(round(1/scores[1,]/sum(1/scores[1,],na.rm = TRUE),3) [round(1/scores[1,]/sum(1/scores[1,],na.rm = TRUE),3) >0])

cleaned_context[1,]

context[context$fips==1001,]     


scores_df = as.data.frame(scores)

q = 1/scores[1,][order(1/scores[1,],decreasing = TRUE)<10 ]
q  

q = as.data.frame(scores[1:10,])
q
str(scores_df)

1/scores[1,][order(1/scores[1,],decreasing = TRUE)<10 ]

hist(as.numeric(scores_df[1,1:100]))

1/scores_df[1,1:10] %>%

