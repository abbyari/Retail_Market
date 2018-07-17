#Implementing Market Basket Analysis using Apriori Algorithm
#---------------------1.Data Pro -----------------------------
#read Data
df_shop <- read.csv(file = "D:/R - research/casestudyretail/assoication rule/Totalcsv.csv")

#--------------------2.Data cleaning and manipulations using R-----------------------
#and df_shop is a subset dataframe of the original dataframe
df_shop <- df_shop[,c("INVOICENO", "ITEMCODE")]; 
str(df_shop)

#sort with respect to INVOICENO 
df_sorted <- df_shop[order(df_shop$INVOICENO),]

#convert INVOICENO to numeric
df_sorted$INVOICENO <- as.numeric(df_sorted$INVOICENO) # our target column

str(df_sorted)

#convert dataframe to transaction format using ddply; 

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

#group all the items that were bought together; by the same customer on the same date
library(plyr)

# convert the dataframe into basket format, based on the INVOICENO 
df_itemList <- ddply(df_sorted, c("INVOICENO"), function(df1)paste(df1$ITEMCODE,collapse = ","))

# Here V1 is INVOICENO
#remove member number and date

df_itemList$INVOICENO <- NULL

colnames(df_itemList) <- c("itemList")

#write to csv format
write.csv(df_itemList,"D:/R - research/casestudyretail/assoication rule/ItemList.csv", quote = FALSE, row.names = TRUE)

#-------------------- association rule mining algorithm : apriori -------------------------#

#load package required
library(arules)

#convert csv file to basket format
txn = read.transactions(file="D:/R - research/casestudyretail/assoication rule/ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);


#run apriori algorithm
basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.01, conf = 0.01, target="rules"))


#convert rules to a dataframe and then use Vi
df_basket <- as(basket_rules,"data.frame")
View(df_basket)
df_basket_csv=as.data.frame(df_basket)
write.csv(df_basket_csv,"D:/R - research/casestudyretail/assoication rule/df_basket_csv.csv", quote = FALSE, row.names = TRUE)

#graph view
install.packages("TSP")
install.packages("arulesViz")
install.packages("fpc")

library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

# graph of top 10 element

itemFrequencyPlot(txn, topN = 10)
