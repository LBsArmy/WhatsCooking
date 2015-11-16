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
require(qdap)


# Load Data
train <- "C:\\Users\\FISCA028\\Downloads\\train.json\\train.json"
#train<- train <- "C:\\Users\\AFischler\\Desktop\\Kaggle\\Cooking\\train.json"

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

#remove V#s=:
v_string <- paste("V",1:65,"=",sep="")
v_string2 <- paste(v_string, collapse = '|')

LHS_c1 <- gsub(v_string2,"", LHS3[,1])
LHS_c2 <- gsub(v_string2,"", LHS3[,2])
LHS_c3 <- gsub(v_string2,"", LHS3[,3])

#remove =s
LHS_c1 <- gsub(" =","", LHS_c1)
LHS_c2 <- gsub(" =","", LHS_c2)
LHS_c3 <- gsub(" =","", LHS_c3)

# merge LHS back together
LHS_clean <- as.data.frame(cbind(LHS_c1,LHS_c2,LHS_c3))
colnames(LHS_clean) <- c("Piece1","Piece2","Piece3")

# Combine LHS_clean with rest of data:

final_rules <- as.data.frame(cbind(try4,LHS_clean))
attach(final_rules)
final_rules <- data.frame(Piece1, Piece2, Piece3,cuisine,support,confidence,lift)
detach(final_rules)


#create small testing batch

train_small <- train2[1:10,]

# Create rules lists by number of ingredients

one_ing <- final_rules[is.na(final_rules[,2]),]
two_ing <- final_rules[(is.na(final_rules[,3]) & !is.na(final_rules[,2])) ,]
three_ing <- final_rules[!is.na(final_rules[,3]),]



level1 <- 0
scores <-matrix(, nrow = nrow(train_small), ncol = length(cuisines))
time <- Sys.time()
cuisines <- unique(train2$cuisine)



for (g in (1:nrow(train_small))){
    for (i in (1:nrow(one_ing))){
        level1 <- level1 + grepl(one_ing$Piece1[i],train_small$ingredients[g]) * one_ing$lift[i]
        }
    scores1 <- as.data.frame(rbind(scores1,level1))
    if (g %% 1000 == 0){
      print(paste(round(i/nrow(train_small),3),"%", round(Sys.time() - time,2), sep=" "))
      time <- Sys.time()
    }
    level1 <- 0
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
          