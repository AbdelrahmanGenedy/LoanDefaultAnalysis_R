data <- read.csv("C:/Users/pc/Downloads/archive (1)/train.csv")
tdata <- read.csv("C:/Users/pc/Downloads/archive (1)/test.csv")

head(data)
head(tdata)


#----------------------------PREPROCESSING----------------------------


names(data)[names(data) == "Employment.Duration"] <- "Ownership"
names(tdata)[names(tdata) == "Employment.Duration"] <- "Ownership"

names(data)[names(data) == "Home.Ownership"] <- "Salary"
names(tdata)[names(tdata) == "Home.Ownership"] <- "Salary"

data$Verification.Status[data$Verification.Status == "Source Verified"] <- "Verified"
data$Verification.Status[tdata$Verification.Status == "Source Verified"] <- "Verified"

#drop ID and Batch.Enrolled later.

sum(is.na(data))
sum(is.na(tdata)) #nulls are the prediction colmn

for(col in colnames(data)) {
  print(paste(col, "Nulls:", sum(is.na(data[[col]])) ))
}

for(col in colnames(tdata)) {
  print(paste(col, "Nulls:", sum(is.na(tdata[[col]])) ))
}

anyDuplicated(data)
anyDuplicated(tdata)

names(data)

str(data)
str(tdata)

summary(data)

summary(tdata)

#accounts delinquent w payment plan fihom unique value wahda bas

x <- unique(data$Grade)
x

data$Loan.Title <- ifelse(grepl("card", data$Loan.Title), "Cred Card Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("Card", data$Loan.Title), "Cred Card Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("Credit", data$Loan.Title), "Cred Card Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("credit", data$Loan.Title), "Cred Card Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("debt", data$Loan.Title), "Debt Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("Debt", data$Loan.Title), "Debt Payoff", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("home", data$Loan.Title), "Home ++", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("Home", data$Loan.Title), "Home ++", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("CC", data$Loan.Title), "CC Consolidation", data$Loan.Title) #Cash credit not credit card
data$Loan.Title <- ifelse(grepl("medical", data$Loan.Title), "Medical Expenses", data$Loan.Title)
data$Loan.Title <- ifelse(grepl("Medical", data$Loan.Title), "Medical Expenses", data$Loan.Title)

tdata$Loan.Title <- ifelse(grepl("card", tdata$Loan.Title), "Cred Card Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("Card", tdata$Loan.Title), "Cred Card Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("Credit", tdata$Loan.Title), "Cred Card Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("credit", tdata$Loan.Title), "Cred Card Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("debt", tdata$Loan.Title), "Debt Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("Debt", tdata$Loan.Title), "Debt Payoff", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("home", tdata$Loan.Title), "Home ++", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("Home", tdata$Loan.Title), "Home ++", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("CC", tdata$Loan.Title), "CC Consolidation", tdata$Loan.Title) #Cash credit not credit card
tdata$Loan.Title <- ifelse(grepl("medical", tdata$Loan.Title), "Medical Expenses", tdata$Loan.Title)
tdata$Loan.Title <- ifelse(grepl("Medical", tdata$Loan.Title), "Medical Expenses", tdata$Loan.Title)

data$Loan.Title <- ifelse(!grepl("Debt Payoff|Cred Card Payoff|HomeImprov|Medical Expenses", data$Loan.Title), "Other", data$Loan.Title)
tdata$Loan.Title <- ifelse(!grepl("Debt Payoff|Cred Card Payoff|HomeImprov|Medical Expenses", tdata$Loan.Title), "Other", tdata$Loan.Title)


data$Loan.Title <- ifelse(!grepl("Debt Payoff|Cred Card Payoff|HomeImprov", data$Loan.Title), "Other", data$Loan.Title)

tdata$Loan.Title <- ifelse(!grepl("Debt Payoff|Cred Card Payoff|HomeImprov", tdata$Loan.Title), "Other", tdata$Loan.Title)


labelEncoder <- function(data) {
  result <- data
  for (i in 1:ncol(data)) {
    if (is.character(data[[i]])) {
      levels <- unique(data[[i]])
      result[[i]] <- match(data[[i]], levels)
    }
  }
  return(result)
}

data <-labelEncoder(data)

tdata <-labelEncoder(tdata)

data

tdata


anova_result <- aov(Interest.Rate ~ Grade, data = data)
summary(anova_result)


TukeyHSD(anova_result)

anova_result <- aov(`Interest.Rate` ~ `Ownership`, data = data)

summary(anova_result)

anova_result <- aov(Revolving.Utilities ~ Grade, data = data)

summary(anova_result)

anova_result <- aov(Total.Accounts ~ Grade, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Total.Accounts ~ Ownership, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Funded.Amount.Investor ~ Ownership, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Funded.Amount.Investor ~ Grade, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Loan.Amount ~ Ownership, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Funded.Amount ~ Ownership, data = data)

summary(anova_result)

TukeyHSD(anova_result)

anova_result <- aov(Funded.Amount ~ Grade, data = data)

summary(anova_result)

TukeyHSD(anova_result)

#-------------------------PREPROCESSING-------------------------



#------------------------PLOTTING------------------------------
#pie charts visualize different values in each column to understand the distrubution of values
#helps understand which columns should be dropped

pie(table(data$Term), labels = names(table(data$Term)))

pie(table(data$Delinquency...two.years), labels = names(table(data$Delinquency...two.years)))

pie(table(data$Inquires...six.months), labels = names(table(data$Inquires...six.months)))

pie(table(data$Collection.12.months.Medical), labels = names(table(data$Collection.12.months.Medical)))

pie(table(data$Revolving.Balance), labels = names(table(data$Revolving.Balance)))

pie(table(data$Public.Record), labels = names(table(data$Public.Record)))

pie(table(data$Application.Type), labels = names(table(data$Application.Type)))

pie(table(data$Loan.Status), labels = names(table(data$Loan.Status)))

pie(table(data$Grade), labels = c('B','C','F','A','G','E','D'), col = c("lavender", "green", "yellow", "blue", "purple", "orange","maroon"))

pie(table(data$Sub.Grade), labels = names(table(data$Sub.Grade)))

pie(table(data$Initial.List.Status), labels = names(table(data$Initial.List.Status)))

pie(table(data$Loan.Title), labels = names(table(data$Loan.Title)))

pie(table(data$Verification.Status), labels = names(table(data$Verification.Status)))

pie(table(data$Payment.Plan), labels = names(table(data$Payment.Plan)))

pie(table(data$Ownership), labels = names(table(data$Ownership)))

print(unique(data$Initial.List.Status))

print(unique(data$Loan.Title))

print(unique(data$Verification.Status))

print(unique(data$Employment.Duration))

print(unique(data$Sub.Grade))

print(unique(data$Payment.Plan))

print(unique(data$Grade))

pie(table(data$Loan.Title), labels = names(table(data$Loan.Title)))

pie(table(data$Loan.Title), labels = names(table(data$Loan.Title)))

pie(table(data$Loan.Title), labels = names(table(data$Loan.Title)))

numericDF <- subset(data, select = -c(Verification.Status,Loan.Title,Initial.List.Status, Ownership, Sub.Grade, Grade, Batch.Enrolled, ID, Accounts.Delinquent,Application.Type,Payment.Plan))

Three1 <- subset(numericDF, select = c(Loan.Amount, Funded.Amount, Funded.Amount.Investor))

Four <- subset(numericDF, select = c(Total.Accounts,Open.Account, Interest.Rate, Debit.to.Income))

Two <- subset(numericDF, select = c(Salary, Total.Revolving.Credit.Limit))

One <- subset(numericDF, select = c(Revolving.Balance))

Two1 <- subset(numericDF, select = c(Recoveries))

One1 <- subset(numericDF, select = c(Total.Received.Late.Fee))

Two4 <- subset(numericDF, select = c(Total.Accounts, Last.week.Pay))

One2 <- subset(numericDF, select = c(Total.Received.Interest))

Two2 <- subset(numericDF, select = c(Collection.Recovery.Fee, Revolving.Utilities))

Three <- subset(numericDF, select = c(Delinquency...two.years, Inquires...six.months, Public.Record))

dim(numericDF)

head(numericDF)

#boxplots indicate the IQR, 1st quartile, 3rd quartile, and median to help us understand the spread of data in more detail.
#helps get rid of columns with huge amount of outliers

boxplot(numericDF$Total.Collection.Amount, main = "Total Collection Account")

boxplot(numericDF$Total.Current.Balance, main = "Total Current Balance")

boxplot(Two4)

boxplot(One2, main = "Total Received Interest")

boxplot(One1, main = "Total Received Late Fee")

boxplot(Three, main = "Inquiries Six Months")

boxplot(Two2)

boxplot(Two1, main = "Recoveries")

boxplot(One, main = "Revolving Balance")

boxplot(Three1)

boxplot(Four)

boxplot(Two)

table(data$Loan.Status, data$Initial.List.Status)

unique(data$Loan.Status)

unique(data$Initial.List.Status)

#barplots help us visualize data counts (categorized or not) as well as the spread, standard deviation

barplot(table(data$Loan.Status, data$Initial.List.Status), beside = TRUE, col = c("green", "blue"),
        legend.text = TRUE, xlab = "Loan Title",
        ylab = "Count", main = "Counts of 'f' and 'w' by Loan Status",
        names.arg = c("f", "w"))

barplot(table(data$Loan.Status, data$Loan.Title), beside = TRUE, col = c("red", "blue"),
        legend.text = TRUE, xlab = "Loan Title (Medical Expenses)",
        ylab = "Count", main = "Counts of Each Value by Loan Status",
        names.arg = unique(data$Some_Column))

barplot(table(data$Loan.Status, data$Verification.Status), beside = TRUE, col = c("red", "blue"),
        legend.text = TRUE, xlab = "Verification Type",
        ylab = "Count", main = "Counts of Each Value by Loan Status",
        names.arg = unique(data$Some_Column))

barplot(table(data$Grade), col = c("gray", "purple", "turquoise", "brown", "orange", "gold", "lavender"), main = "Frquency of Loans per Grade", xlab = "Grade", ylab = "Frequency")

breaks <- quantile(data$Total.Current.Balance, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

balance_ranges <- cut(data$Total.Current.Balance, breaks = breaks, labels = c("Q1", "Q2", "Q3", "Q4"))

defaulted_counts <- tapply(data$Loan.Status, balance_ranges, function(x) sum(x == 1))

prob_default <- defaulted_counts / sum(defaulted_counts)

barplot(prob_default,
        names.arg = names(prob_default),
        col = "blue",
        main = "Probability of Defaulting by Total Current Balance Quartile",
        xlab = "Total Current Balance Quartile",
        ylab = "Probability of Defaulting")

hist(data$Loan.Amount, col =
       "lightblue", xlab = "Loan Amounts",
     main = "Loan Histogram")

hist(data$Open.Account, col =
       "lightblue", xlab = "Open Account",
     main = "Number of Open Accounts")

hist(data$Total.Current.Balance, col =
       "lightblue", xlab = "Total Current Balance",
     main = "Distribution of Total Current Balance")

data_by_status <- split(data$Interest.Rate, data$Loan.Status)

counts <- lapply(data_by_status, function(x) table(ceiling(x)))

# Extract interest rates and counts for each group
interest_rates <- unique(ceiling(unlist(data_by_status)))
counts_by_group <- sapply(counts, function(x) {
  counts <- as.numeric(x[match(interest_rates, names(x))])
  counts[is.na(counts)] <- 0  # Replace NA with 0 if interest rate not present
  return(counts)
})

# Define colors for loan status 0 and loan status 1
colors <- c("red", "purple")

# Create dot chart
dotchart(unlist(counts_by_group), labels = interest_rates, cex = 0.7, pch = 19,
         col = rep(colors, each = length(interest_rates)),
         xlab = "Count", main = "Counts of Interest Rate by Loan Status")


#----------------------------PLOTTING----------------------------------------


#------------------------------MODEL-------------------------------------

data <- subset(data, select = -c(Verification.Status,Loan.Title,Sub.Grade,Accounts.Delinquent,Application.Type,Payment.Plan, ID, Batch.Enrolled, Collection.12.months.Medical))
tdata <- subset(tdata, select = -c(Verification.Status,Loan.Title,Sub.Grade,Accounts.Delinquent,Application.Type,Payment.Plan, ID, Batch.Enrolled, Collection.12.months.Medical))




correlation_matrix <- cor(data)

heatmap(correlation_matrix,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        symm = TRUE,
        margins = c(10, 10))

library(e1071)
ind<-sample(2,nrow(data),prob=c(0.8,0.2),replace=TRUE)
train.data<-data[ind==1,]
libraClassifier<-naiveBayes(Loan.Status ~. , data=train.data)
output<-predict(Classifier, test.data)
tab<-table(output, test.data$Loan.Status)
true_negatives <- tab[1, 1]
false_positives <- tab[2, 1]
false_negatives <- tab[1, 2]
true_positives <- tab[2, 2] 

accuracy <- (true_negatives + true_positives) / sum(tab)
print(accuracy)

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
f1_score <- 2 * precision * recall / (precision + recall)


print(precision)
print(recall)
print(f1_score)

