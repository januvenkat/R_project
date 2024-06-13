#Select dataset
TMDB_data <- read.csv("C:/Users/mahar/Downloads/R_PROJECT_CLEAN_DATA.csv", stringsAsFactors=TRUE)
print(TMDB_data)

#Verifying Data
# Identify missing values is_missing <- is.na(TMDB_data) # Count missing values sum(is_missing) #0
# Identify duplicate rows
duplicated_rows <- duplicated(TMDB_data) sum (duplicated_rows) #0

#Enriching dataset: adding profit variable
TMDB_data$profit <- TMDB_data$revenue - TMDB_data$budget head (TMDB_data)

# Check the structure of the data
str(TMDB_data)
#print total number of columns and rows
cat("Total Columns: ", ncol(TMDB_data)) #16 columns cat("Total Rows: ", nrow(TMDB_data)) #2583 rows

# Generate summary statistics
summary(TMDB_data)

#Histograms options(scipen = 999) hist(TMDB_data$profit,
main = "Profit Distribution", xlab = "Profit",
ylab = "Frequency", col = "blue",
border = "black", breaks = 30)

options(scipen = 999) hist(TMDB_data$vote_average,
                           main = "Vote Average Distribution", xlab = "Vote Average",
                           ylab = "Frequency", col = "blue",
                           border = "black", breaks = 20)

#Correlation Analyses
cor_matrix <- cor(TMDB_data[, c("budget", "revenue", "profit", "vote_average", "runtime")])
print(cor_matrix) #all are positively correlated

#Average profit on both a monthly and seasonal basis
library(dplyr)
TMDB_data <- TMDB_data %>% mutate(month = format(release_date, "%m"),
                                  season = case_when(
                                    month %in% c("12", "01", "02") ~ "Winter",
                                    month %in% c("03", "04", "05") ~ "Spring",
                                    month %in% c("06", "07", "08") ~ "Summer",
                                    month %in% c("09", "10", "11") ~ "Fall"
                                  ))
mean_profit_by_month <- TMDB_data %>% group_by(month) %>% summarize(mean_profit = mean(profit)) mean_profit_by_season <- TMDB_data %>% group_by(season) %>% summarize(mean_profit = mean(profit)) print(mean_profit_by_month) print(mean_profit_by_season)

# Single-factors regression on profit
PROJECT<- read_csv("C:/Users/20105/Desktop/R PROJECT CLEAN DATA.csv") PROJECT$profit<-(PROJECT$revenue-PROJECT$budget)
generics<-as.factor(PROJECT$genres) generics

PROJECT$genres_Action<-str_detect(PROJECT$genres,"Action") PROJECT$genres_Adventure<-str_detect(PROJECT$genres,"Adventure") PROJECT$genres_ScienceFiction<-str_detect(PROJECT$genres,"Science Fiction") PROJECT$genres_Fantasy<-str_detect(PROJECT$genres,"Fantasy") PROJECT$genres_Comedy<-str_detect(PROJECT$genres,"Comedy") PROJECT$genres_History<-str_detect(PROJECT$genres,"History") PROJECT$genres_Drama<-str_detect(PROJECT$genres,"Drama") PROJECT$genres_TV<-str_detect(PROJECT$genres,"TV Movie") PROJECT$genres_Documentary<-str_detect(PROJECT$genres,"Documentary")


library(stringr) log_data<-
  data.frame(profit=PROJECT$profit,genres_Action=PROJECT$genres_Action,genres_Adven ture=PROJECT$genres_Adventure,genres_Comedy=PROJECT$genres_Comedy,genres_Sci
             
             enceFiction=PROJECT$genres_ScienceFiction,genres_Fantasy=PROJECT$genres_Fantasy, genres_Drama=PROJECT$genres_Drama)
log_data$genres_Action<-str_replace_all(log_data$genres_Action,"TRUE","1") log_data$genres_Action<-str_replace_all(log_data$genres_Action,"FALSE","0") log_data$genres_Adventure<-str_replace_all(log_data$genres_Adventure,"TRUE","1") log_data$genres_Adventure<-str_replace_all(log_data$genres_Adventure,"FALSE","0") log_data$genres_Comedy<-str_replace_all(log_data$genres_Comedy,"TRUE","1") log_data$genres_Comedy<-str_replace_all(log_data$genres_Comedy,"FALSE","0") log_data$genres_ScienceFiction<- str_replace_all(log_data$genres_ScienceFiction,"TRUE","1") log_data$genres_ScienceFiction<- str_replace_all(log_data$genres_ScienceFiction,"FALSE","0") log_data$genres_Fantasy<-str_replace_all(log_data$genres_Fantasy,"TRUE","1") log_data$genres_Fantasy<-str_replace_all(log_data$genres_Fantasy,"FALSE","0") log_data$genres_Drama<-str_replace_all(log_data$genres_Drama,"TRUE","1") log_data$genres_Drama<-str_replace_all(log_data$genres_Drama,"FALSE","0")

log_data summary(log_data)

#Action-profit single factor regression
fit1<-lm(profit~genres_Action,data=log_data) fit1Sum<-summary(fit1)
fit1Sum

#Adventure-profit single factor regression fit2<-lm(profit~genres_Adventure,data=log_data) fit2Sum<-summary(fit2)
fit2Sum

#Comedy-profit single factor regression
fit3<-lm(profit~genres_Comedy,data=log_data) fit3Sum<-summary(fit3)
fit3Sum

#ScienceFiction-profit single factor regression fit4<-lm(profit~genres_ScienceFiction,data=log_data) fit4Sum<-summary(fit4)
fit4Sum

#Fantasy-profit single factor regression fit5<-lm(profit~genres_Fantasy,data=log_data) fit5Sum<-summary(fit5)
fit5Sum

#Drama-profit single factor regression fit6<-lm(profit~genres_Drama,data=log_data) fit6Sum<-summary(fit6)

fit6Sum

log_data1<- data.frame(profit=PROJECT$profit,genres_Documentary=PROJECT$genres_Documentary, genres_TVMovie=PROJECT$genres_TV,genres_History=PROJECT$genres_History) log_data1$genres_Documentary<- str_replace_all(log_data1$genres_Documentary,"TRUE","1") log_data1$genres_Documentary<- str_replace_all(log_data1$genres_Documentary,"FALSE","0") log_data1$genres_TVMovie<-str_replace_all(log_data1$genres_TVMovie,"TRUE","1") log_data1$genres_TVMovie<-str_replace_all(log_data1$genres_TVMovie,"FALSE","0") log_data1$genres_History<-str_replace_all(log_data1$genres_History,"TRUE","1") log_data1$genres_History<-str_replace_all(log_data1$genres_History,"FALSE","0") log_data1

#Documentary-profit single factor regression
fit7<-lm(profit~genres_Documentary,data=log_data1) fit7Sum<-summary(fit7)
fit7Sum

#TVMovie-profit single factor regression
fit8<-lm(profit~genres_TVMovie,data=log_data1) fit8Sum<-summary(fit8)
fit8Sum

#History-profit single factor regression
fit9<-lm(profit~genres_History,data=log_data1) fit9Sum<-summary(fit9)
fit9Sum
