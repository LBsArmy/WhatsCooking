library(jsonlite)
library(data.table)

dir <- "/users/adampatisteas/Desktop/Kaggle/What's Cooking/"
train_file <- paste(dir, "train.json", sep="")
test_file <- paste(dir, "test.json", sep="")

train_df <- fromJSON(txt=train_file, flatten = TRUE)
test_df <- fromJSON(txt=test_file, flatten = TRUE)

detach("package:jsonlite", unload=TRUE)

#Count number of ingredients per recipe
count <- rapply(train_df[,3], length)
train_df <- cbind(train_df, count)

#Create a list of all ingredients (in lowercase)
ingredients <- unlist(train_df[,3])
ingredients <- tolower(ingredients)
unique_ingredients <- unique(ingredients)

#create df with 3 cols: id, cuisine, ingredient with 1 ingredient per row
train_long <- train_df[rep(row.names(train_df), train_df$count), 1:2]
train_long <- cbind(train_long, ingredients)

#Do a count of each ingredient across all cuisines
unique_count <- data.table(train_long$ingredients)[, .N, keyby = train_long$ingredients]
#sort by N
unique_count <- unique_count[order(unique_count$N),]

#Histogram of ingredient occurrence
#breaks <- cut(unique_count$N, breaks=c(0:20, 50, 100, 1000, Inf))
#barplot(table(breaks))


#unique_count[like(train_long,"pepper")]


#Create a sparse matrix of all ingredients
#Number recipes 1 to 39774
recipes <- data.frame(c(1:length(train_df[,1])),train_df[,1])
colnames(recipes) <- c("r_index", "id")

#Number ingredients 1 to 6703
ing <- data.frame(c(1:length(unique_ingredients)),unique_ingredients)
colnames(ing) <- c("i_index", "ingredients")

#Do some SQL type matching between train_long and recipes/ing to get i,j coordinates for sparse matrix
a <- merge(x = train_long, y = recipes, by = "id", all.x = TRUE)
a <- merge(x = a, y = ing, by = "ingredients", all.x = TRUE)

#Turn it into a sparse matrix
library("Matrix")
train_sparse <- sparseMatrix(a[,4], a[,5], x = TRUE)

#And into a full matrix
dimnames(train_sparse) <- list(NULL, ing$ingredients)
b <- as.data.frame(as.matrix(train_sparse))
row.names(b) <- recipes$id


#Sparse matrices trivial example for illustration
## simple example
i <- c(1,3:8)
j <- c(2,9,6:10)
x <- sparseMatrix(i, j, x = 1)
summary(x)

dimnames(x) = list(NULL,letters[1:10]) 
as.data.frame(as.matrix(x)) 
