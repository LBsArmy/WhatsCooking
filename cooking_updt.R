# Load Packages

require(jsonlite)
require(tm)
require(SnowballC)
require(stringi)
require(arules)
require(arulesViz)
require(stringr)
require(tidyr)
require(dplyr)


# Load Data
train <- "C:\\Users\\FISCA028\\Downloads\\train.json\\train.json"

train2 <- fromJSON(train, flatten=TRUE)

####Create Corpus at full ingredient level

#train2 <- json_data[1:2,]

train2$ingredients <- gsub(" ", "_", train2$ingredients)
train2$ingredients <- gsub("[^A-Za-z0-9,_]", "", train2$ingredients)
train2$ingredients <- gsub(",_", " ", train2$ingredients)
train2$ingredients <- substring(train2$ingredients,2)



test <- str_split(train2$ingredients, " ")


test6 <- head(test)


df <- data.frame()
for (i in seq(1:length(test))){
  a <- as.data.frame(t(as.data.frame(unlist(test[[i]], " ", use.names=FALSE)    )))
  df <- rbind.fill(df,a)
  if (i %% 500 == 0){
    print(i)
  }
}










ingredients <- Corpus(VectorSource(train2$ingredients))

#ingredients <- tm_map(ingredients)

ingredientsDTM <- DocumentTermMatrix(ingredients)

tester <- as.data.frame(inspect(ingredientsDTM[1,]))

a<- as.data.frame(names(tester))

sparse <- removeSparseTerms(ingredientsDTM, 0.997)
sparse

ingredientsDTM2 <- as.data.frame(as.matrix(sparse))

####Create Corpus at full ingredient level

#train2 <- json_data[1:2,]

train2 <- fromJSON(train, flatten=TRUE)

ingredients <- Corpus(VectorSource(train2$ingredients))

ingredients <- tm_map(ingredients, stemDocument)

ingredientsDTM <- DocumentTermMatrix(ingredients)

tester <- as.data.frame(inspect(ingredientsDTM[1,]))

a<- as.data.frame(names(tester))

sparse <- removeSparseTerms(ingredientsDTM, 0.997)
sparse

ingredientsDTM_word <- as.data.frame(as.matrix(sparse))
          