ObesityTop10 <- function(data, yearinput) {

df_Obesity <- data$df_Obesity
obesity <- df_Obesity %>% 
  filter(Question == "Percent of adults aged 18 years and older who have obesity") 

names(obesity)[names(obesity) == "Data_Value"] <- "Obesity_Percentage"

obesity <- obesity %>% replace_na(list(Obesity_Percentage = 0))

USAobesityTop10 <- obesity %>% filter(YearStart == yearinput) %>% group_by(YearStart, LocationDesc) %>% summarize(totalobeesity_percentage = sum(Obesity_Percentage)) %>% arrange(desc(totalobeesity_percentage)) %>% top_n(10,totalobeesity_percentage)

ObeseStatesTop10 <- ggplot(USAobesityTop10, aes(x=reorder(LocationDesc,-totalobeesity_percentage), y=totalobeesity_percentage)) +
  geom_bar(stat="identity", fill="orange") + coord_flip() 

p <- ObeseStatesTop10 + theme_minimal() +xlab("State") + ylab("Obesity Rate (%)") +
  ggtitle("The 10 States with the Highest Rates of Adult Obesity")

return(p)
}

physicalTop10 <- function(data, yearinput) {
  
  df_Physical <-data$df_Physical 
  
  physical <- df_Physical %>% 
    filter(Question == "Percent of adults who engage in no leisure-time physical activity") 
  
  names(physical)[names(physical) == "Data_Value"] <- "PhysicalActivity_Percentage"
  
  physical <- physical %>% replace_na(list(PhysicalActivity_Percentage = 0))
  
  USAInactivityTop10 <-physical %>% filter(YearStart == yearinput)  %>% group_by(YearStart, LocationDesc) %>% summarize(total_percentage = sum(PhysicalActivity_Percentage)) %>% arrange(desc(total_percentage)) %>% top_n(10,total_percentage)
  
  InactiveStatesTop10 <- ggplot(USAInactivityTop10, aes(x=reorder(LocationDesc,-total_percentage), y=total_percentage)) +
    geom_bar(stat="identity", fill="#006633") + coord_flip()  
  
  p1 <- InactiveStatesTop10 + theme_minimal() +xlab("State") + ylab("% of Inactive Adults") +
    ggtitle("10 States with the Highest Physical Inactivity Rates")
  
  return(p1)
}

jointdata <- function(data) {

df_whole <- data$df_whole

df_joindata = df_whole %>% filter(Question == "Percent of adults who engage in no leisure-time physical activity" | Question == "Percent of adults aged 18 years and older who have obesity")

p2 <- ggplot() + geom_col(data = df_joindata, aes(x = as.factor(YearStart), y = Data_Value, fill = Question), position = "dodge")   +  coord_flip() +
  theme(legend.position = "top")

return(p2)
}