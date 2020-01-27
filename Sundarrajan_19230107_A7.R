#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 7								
#	TOPIC - "dplyr relational operation"            STUDEBT ID: 19230107
#----------------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(corrplot)

# Reading the excel sheets from local directory 
d_in = readxl::read_excel('T:\\NUIG DA\\SEM_1\\Programming for DA\\DublinAirport.xlsx',sheet ='DublinInward')
d_out = readxl::read_excel('T:\\NUIG DA\\SEM_1\\Programming for DA\\DublinAirport.xlsx',sheet ='DublinOutward')


d_in[1:10,1:10]

d_out[1:10,1:10]


#check data types of the columns
# Spliting Date column into Year & Month | untidy --> tidy based on key columns other than date column in df_in | Mutating Direction and Monthname 
d_in = d_in %>% separate(Date,into = c('Year','Month'),sep = 'M') %>% gather(key = Airport ,value = Passengers, colnames(d_in)[-1]) %>%
                mutate(Direction = 'Inbound',MonthName = month.abb[as.numeric(Month)],Date = as.Date(ISOdate(Year,Month,'01') ) ) 

# These 2 operations are done to match which the dtype in problem output
d_in$Passengers = as.integer(d_in$Passengers)
d_in$MonthName = as.factor(d_in$MonthName)


#Do we need date column? its like manipulating without knowing actual day with just year and month
#check data types of the columns
# Spliting Date column into Year & Month | untidy --> tidy based on key columns other than date column in df_out | Mutating Direction and Monthname 
d_out =d_out %>% separate(Date,into = c('Year','Month'),sep = 'M') %>% gather(key = Airport ,value = Passengers, colnames(d_out)[-1]) %>%
                 mutate(Direction = 'Outbound',MonthName = month.abb[as.numeric(Month)],Date = as.Date(ISOdate(Year,Month,'01') ))

# These 2 operations are done to match which the dtype in problem output
d_out$Passengers = as.integer(d_out$Passengers)
d_out$MonthName = as.factor(d_out$MonthName)

d_in
d_out


tidy_data = full_join(d_in,d_out)
tidy_data

# Top 10 Airports wrt Total no of passengers across both inbound and outboound 
# Used pull to get that column data out in Base R format(vector) else it returns a tibble by default
top_ten = tidy_data %>% group_by(Airport) %>% summarise( total_passengers = sum(Passengers)) %>%
  arrange(desc(total_passengers)) %>% top_n(10) %>% pull(Airport) 

top_ten

# Total passengers by airport and direction
tidy_data %>% group_by(Year,Direction) %>% summarise( Total = sum(Passengers)) 


# Total passengers by year,month and direction & here aggregated  date using distinct - to match problem output 
tidy_data %>% group_by(Year,Month,Direction) %>% summarise( Total = mean(Passengers), Date = unique(Date)) 

# Total outbound passengers by year,month [excluding year 2019]
total_pass = tidy_data %>% filter(Year != '2019',Direction == 'Outbound') %>% group_by(Year,Month) %>% summarise( Total = sum(Passengers))

total_pass

# Total outbound passengers by year,month [excluding year 2019] --> heatmap using geom_title() and specified colour range using scale_fill_gradient
total_pass %>% ggplot(aes(x=Year,y=Month,fill=Total))+geom_tile() +scale_fill_gradient(low = "blue",high = "red",)


# Split Airport column using multiple separators between '|' --> [preffix '(' code ')' suffix] ex: "Aberdeen(ABZ),Great Britain" = "Aberdeen" + "ABZ" + ",Great Britain"
# After spliting columns used spread to make data untidy so that it can be approopriate for calculatig correlation matrix
spreaded_d_out = d_out %>% separate(Airport,into = c('City','City_code','Country'),sep = '[(]|[)]') %>% 
                           select(Year,Month,City_code,Passengers) %>%spread (City_code,Passengers,convert = T)

#Selecting columns except 'Year' & 'Month' for correlation matrix
correlation_matrix = cor(spreaded_d_out[,-c(1,2)])

#upper half of correlation matrix
corrplot(correlation_matrix,type="upper")
