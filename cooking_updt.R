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
require(caret)
require(datasets)


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

write.table(df2,file=paste(getwd(),"/df2.csv",sep=""), row.names=FALSE, na="", sep=",")

df3 <- read.transactions(file=paste(getwd(),"/df2.csv",sep=""),format="basket", sep=",", rm.duplicates = TRUE)



# Get the rules
rules <- apriori(df3, parameter = list(supp = 0.0001, conf = 0.05, minlen =2, maxlen = 4, target="rules"))
itemFrequencyPlot(df3, topN = 25)

rules.sub <- subset(rules, subset = rhs %pin% "greek")

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



cuisines <- unique(train2$cuisine)
scores <-matrix(0, nrow = nrow(train_small), ncol = length(cuisines))
colnames(scores) <- cuisines
time <- Sys.time()




for (g in (1:nrow(train_small))){
    for (i in (1:nrow(one_ing))){
        if (grepl(one_ing$Piece1[i],train_small$ingredients[g])){
            scores[g,which( cuisines == one_ing$cuisine[i])] <- scores[g,which( cuisines == one_ing$cuisine[i])] + one_ing$lift[i]
        }
        
    }
    if (g %% 1000 == 0){
      print(paste(round(g/nrow(train_small),3),"%", round(Sys.time() - time,2), sep=" "))
      time <- Sys.time()
    }
}

#Full data


cuisines <- unique(train2$cuisine)
scores_sm <-matrix(0, nrow = nrow(train_small), ncol = length(cuisines))
count_sm <- matrix(0, nrow = nrow(train_small), ncol = length(cuisines))
colnames(scores) <- cuisines
time <- Sys.time()

for (g in (1:nrow(train_small))){
  for (i in (1:nrow(one_ing))){
    if (grepl(one_ing$Piece1[i],train_small$ingredients[g])){
      scores_sm[g,which(cuisines == one_ing$cuisine[i])] <- round(scores_sm[g,which(cuisines == one_ing$cuisine[i])] + (one_ing$lift[i] - 1) * one_ing$confidence[i] / one_ing$support[i] /10000,2) 
      count_sm[g,which(cuisines == one_ing$cuisine[i])] <- count_sm[g,which(cuisines == one_ing$cuisine[i])] + 1
    }
  }
}
  if (g %% 1000 == 0){
    print(paste(round(g/nrow(train2),3),"%", round(Sys.time() - time,2), sep=" "))
    time <- Sys.time()
  }
  if (g == 5000){
      write.csv(scores[1:5000,],file=paste(getwd(),"/scores1.csv",sep=""), row.names=FALSE)}
  else if (g == 10000){
    write.csv(scores[5001:10000,],file=paste(getwd(),"/scores2.csv",sep=""), row.names=FALSE)}
  else if (g == 15000){
    write.csv(scores[10001:15000,],file=paste(getwd(),"/scores3.csv",sep=""), row.names=FALSE)}
  else if (g == 20000){
    write.csv(scores[15001:20000,],file=paste(getwd(),"/scores4.csv",sep=""), row.names=FALSE)}
  else if (g == 25000){
    write.csv(scores[20001:25000,],file=paste(getwd(),"/scores5.csv",sep=""), row.names=FALSE)}
  else if (g == 30000){
    write.csv(scores[25001:30000,],file=paste(getwd(),"/scores6.csv",sep=""), row.names=FALSE)}
  else if (g == 35000){
    write.csv(scores[30001:35000,],file=paste(getwd(),"/scores7.csv",sep=""), row.names=FALSE)}
  else if (g == nrow(train2)){
    write.csv(scores[35001:nrow(train2),],file=paste(getwd(),"/scores8.csv",sep=""), row.names=FALSE)}  
}



df_scores <- as.data.frame(scores)

df_scores <- unlist(colnames(df_scores)[max.col(df_scores,ties.method="random")])


# Confusion matrix
Observations <- train2$cuisine
Predicted <- df_scores
conf <- table(Predicted, Observations)



#Accuracy
mean(ifelse(Observations == Predicted,1,0))








####### WIP

aa <- matrix(c("a","b"," c","d"), nrow = 2, ncol = 2)


write.csv(try1,file=paste(getwd(),"/one_ing2.csv",sep=""), row.names=FALSE)

##### Standardizing Scores by Numbers of Ingredients:

ing_count <- str_count(train2$ingredients, " ") + 1

scores2 <- scores_sm 

#/ count_sm

df_scores2 <- as.data.frame(scores2)

df_scores2 <- unlist(colnames(df_scores2)[max.col(df_scores2,ties.method="random")])


# Confusion matrix
Observations <- train_small$cuisine
Predicted <- df_scores2
conf <- table(Predicted, Observations)



#Accuracy
mean(ifelse(Observations == Predicted,1,0))


(train_small$cuisine[g] == one_ing$cuisine[i])

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
          