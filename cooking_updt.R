# Load Packages

require(jsonlite)
require(tm)
require(SnowballC)
require(stringi)
require(arules)
require(arulesViz)
require(stringr)
require(tidyr)
require(plyr)
require(dplyr)


# Load Data
#train <- "C:\\Users\\FISCA028\\Downloads\\train.json\\train.json"
train<- train <- "C:\\Users\\AFischler\\Desktop\\Kaggle\\Cooking\\train.json"

train2 <- fromJSON(train, flatten=TRUE)

####Create Corpus at full ingredient level

#train2 <- json_data[1:2,]

train2$ingredients <- gsub(" ", "_", train2$ingredients)
train2$ingredients <- gsub("[^A-Za-z0-9,_]", "", train2$ingredients)
train2$ingredients <- gsub(",_", " ", train2$ingredients)
train2$ingredients <- substring(train2$ingredients,2)



test <- str_split(train2$ingredients, " ")



df <- data.frame()
time <- Sys.time()
for (i in seq(1:length(test))){
    a <- as.data.frame(t(as.data.frame(unlist(test[[i]], " ", use.names=FALSE)    )))
    df <- rbind.fill(df,a)
    if (i %% 1000 == 0){
        print(paste(round(i/length(test),3),"%", round(Sys.time() - time,2), sep=" "))
        time <- Sys.time()
    }
}

#Merge cuisine

df2 <- cbind(train2$cuisine, df)



# Get the rules
rules <- apriori(df2, parameter = list(supp = 0.0001, conf = 0.05, minlen =2, maxlen = 4, target="rules"))

itemFrequencyPlot(df2, topN = 25)

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules)

#coerce into data frame
try1 <- as(rules, "data.frame")

#remove curly brackets from rules
try1$rules <- gsub("[{]", "", try1$rules)
try1$rules <- gsub("[}]", "", try1$rules)

#keep only rules that include a cuisine
try2 <- try1[grepl("cuisine",try1$rules),]

#Separate LHS and RHS
rule_sep <- read.table(text = try2$rules, sep = ">", colClasses = "character")

#Keep rules that have cuisine on RHS
try3 <- cbind(try2,rule_sep)
try3 <- try3[grepl("cuisine",try3[,6]),]


#Remove 'train2$cuisine' from RHS string
RHS <- read.table(text = try3[,6], sep = "=", colClasses = "character")
try4 <- cbind(try3,RHS[,2])

# Add names and remove trash string pieces
colnames(try4) <- c("rules","support","confidence","lift","trash1","trash2","cuisine")
try4 <- try4[ -c(5,6) ]

#Work on LHS -- first split by commas
LHS <- read.table(text = try4$rules, sep = ">", colClasses = "character")
LHS <- cbind(LHS[,2],LHS[,1])

LHS2 <- str_split(LHS[,2], ",")
LHS3 <- data.frame()
time <- Sys.time()
for (i in seq(1:length(LHS2))){
        a <- as.data.frame(t(as.data.frame(unlist(LHS2[[i]], " ", use.names=FALSE)    )))
        LHS3 <- rbind.fill(LHS3,a)
        if (i %% 1000 == 0){
                print(paste(round(i/length(LHS2),3),"%", round(Sys.time() - time,2), sep=" "))
                time <- Sys.time()
        }
}



####### WIP


train2$ingredients <- gsub("[^A-Za-z0-9,_]", "", train2$ingredients)
train2$ingredients <- gsub(",_", " ", train2$ingredients)
train2$ingredients <- substring(train2$ingredients,2)



test <- str_split(train2$ingredients, " ")





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
          