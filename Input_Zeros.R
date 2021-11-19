#Author: Yifan Zhang

library(tidyverse)
library(tidyr)
library(datetime)
library(lubridate)
library(dplyr)
library(reshape2)


df <- read.csv("warehouse_credit_cleansed.csv", header = T)
df <- arrange(df, WAREHOUSE_NAME)
# create new ID
ids <-distinct(data.frame(df$WAREHOUSE_NAME)) %>% rename(WAREHOUSE_NAME =  df.WAREHOUSE_NAME )
ID_CROSSWALK <- ids %>% mutate( NEWID = row_number()) 
df <- full_join(df,ID_CROSSWALK, by ="WAREHOUSE_NAME")
df <- df[,c("NEWID","WAREHOUSE_METERING_START_TIME", "ACTUAL_CREDITS", "EXECUTION_CREDITS", "EFFECTIVE_EXECUTION_CREDITS")]
df <- arrange(df, NEWID, WAREHOUSE_METERING_START_TIME)

# wipe out duplicated data - Note: this may change once we get more information from Bob and Brian
dup.no <- which(duplicated(df[, c("NEWID","WAREHOUSE_METERING_START_TIME")]))
dup.list <- df[, c("NEWID","WAREHOUSE_METERING_START_TIME")][dup.no, ]

dup.check <- data.frame(matrix(nrow = 0 , ncol = 5)) 
colnames(dup.check ) <- colnames(df)
for(i in 1:length(dup.list$NEWID)){
  dup.check <- rbind(dup.check, filter(df, NEWID == dup.list$NEWID[i] & WAREHOUSE_METERING_START_TIME == dup.list$WAREHOUSE_METERING_START_TIME[i]))
}
df <- df[-dup.no, ]
df$WAREHOUSE_METERING_START_TIME <- as.character(as_datetime(df$WAREHOUSE_METERING_START_TIME))
arrange(count(df, NEWID), desc(n))
dup.list = data.frame(matrix(, nrow = 0, ncol = 2))
for(i in 1: dim(dup.check)[1]){
  # i = 1
  dup.list = rbind(filter(ID_CROSSWALK, NEWID == dup.check$NEWID[i]), dup.list)
}
dup.list <- distinct(dup.list)



# create the complete time line from 2019/08/01/00:00:00 to 2021/08/02/05:00:00
diff_hour <- as.numeric(difftime(as_datetime("2021-08-02 04:00:00"), as_datetime("2019-08-01 00:00:00"), units = c("hours")))
out <- ISOdatetime(2019,07,31,20,0,0) + (0:diff_hour) *60*60
attr(out, "tzone") <- "UTC" 
time_index <- as.character(out)
time_index[17573]
# cast ACTUAL, EXECUTION, EFFECTIVE EXECUTION CREDITS of 'df' onto created time index
df_actual <- dcast(df, NEWID ~ WAREHOUSE_METERING_START_TIME, value.var = "ACTUAL_CREDITS")
df_execution <- dcast(df, NEWID ~ WAREHOUSE_METERING_START_TIME, value.var = "EXECUTION_CREDITS")
df_effective_execution <- dcast(df, NEWID ~ WAREHOUSE_METERING_START_TIME, value.var = "EFFECTIVE_EXECUTION_CREDITS")

####################################################################################
add_time <- function(df, timeline){
  # df <- df_actual
  # timeline <- time_index
  output <- data.frame(matrix(, nrow = dim(df)[1], ncol = length(timeline)))
  colnames(output) <- timeline
  for (j in 1: length(timeline)) {
    if(is.element(timeline[j], colnames(df))){
      output[, j] <- df[, match(timeline[j], colnames(df))]
    }else{
      output[, j] <- output[, j]
    }
  }
  output <- cbind(NEWID = df[, c("NEWID")], output)
  return(output)
}
####################################################################################

subset(df_actual, NEWID == 283)
df_actual <- add_time(df_actual, timeline = time_index)
df_execution <- add_time(df_execution, timeline = time_index)
df_effective_execution <- add_time(df_effective_execution, timeline = time_index)
subset(df_actual, NEWID == 283)

# create an index from 2019-08-01 00:00:00 to 2021-08-01 23:00:00
diff_hour1 <- as.numeric(difftime(as_datetime("2021-08-01 23:00:00"), as_datetime("2019-08-01 00:00:00"), units = c("hours")))
out1 <- ISOdatetime(2019,07,31,20,0,0) + (0:diff_hour1) *60*60
attr(out1, "tzone") <- "UTC" 
time_index_1 <- as.character(out1)
time_index_1 <- c("NEWID", time_index_1)

df_actual_1 <- df_actual[, time_index_1] %>% melt(c("NEWID"))
df_execution_1 <- df_execution[, time_index_1] %>% melt(c("NEWID"))
df_effective_execution_1 <- df_effective_execution[, time_index_1] %>% melt(c("NEWID"))

df_actual_1$variable <- as_date(df_actual_1$variable)
df_execution_1$variable <- as_date(df_execution_1$variable)
df_effective_execution_1$variable <- as_date(df_effective_execution_1$variable)

# replace 'NA' with '0'
df_actual_1[is.na(df_actual_1)] <- 0
df_execution_1[is.na(df_execution_1)] <- 0
df_effective_execution_1[is.na(df_effective_execution_1)] <- 0
colnames(df_actual_1) <- c("NEWID", "date", "credits")
colnames(df_execution_1) <- c("NEWID", "date", "credits")
colnames(df_effective_execution_1) <- c("NEWID", "date", "credits")



actual_daily <- df_actual_1 %>% group_by(NEWID, date) %>% summarise(sum = sum(credits)) %>%  add_column(type ="ACTUAL_CREDITS",.after =1)
execution_daily <- df_execution_1 %>% group_by(NEWID, date) %>% summarise(sum = sum(credits)) %>% add_column(type = "EXECUTION_CREDITS",.after =1)
effective_execution_daily <- df_effective_execution_1 %>% group_by(NEWID, date) %>% summarise(sum = sum(credits)) %>% add_column(type ="EFFECTIVE_EXECUTION_CREDITS",.after =1)
