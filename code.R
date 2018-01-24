library(tidyverse)
library(readxl)
library(hms)


data <- read_excel("surveydata.xlsx", sheet = 2 )
head(data)

data_tidy <- data %>% 
  gather(survey, response , -c(1:2))


data_tidy %>%
  select(survey, response) %>%
  filter(grepl("Awa$|AdAw$", survey)) %>% 
  group_by(survey) %>%
  summarise(
    Aware = sum(response == 1),
    Not_Aware = sum(response == 0),
    Not_Answered = sum(response == "N/A"),
    Total = n(),
    Prop_Aware = Aware / Total  )


data_tidy %>%
  select(survey, response) %>%
  filter(grepl("Recmnd$", survey)) %>%
  mutate(response = na_if(response , "N/A"),
         response = as.integer(response)) %>% 
  group_by(survey) %>%
  summarise(
    Total_Score = sum(response , na.rm = TRUE),
    Not_Asked = sum(is.na(response)),
    Total = n() - Not_Asked,
    Prop_Score = Total_Score / Total)


time <-  data %>% 
  transmute(start_date = as.Date(InterviewStart), 
            start_time = times(format(InterviewStart, "%H:%M:%S")),
            end_date = as.Date(InterviewEnd), 
            end_time = times(format(InterviewEnd, "%H:%M:%S")),
            time_dif = end_time - start_time )




time$time_dif[time$time_dif < 0] <- 0
time %>% 
  summarise(avg = mean(time_dif))
