#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 6								
#	TOPIC - "dplyr"                                 STUDEBT ID: 19230107
#----------------------------------------------------------------------------------

library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

my_flights <- flights
my_flights

#Filtering columns which dont have missing values for both dep_delay and arr_delay
flts = my_flights %>% filter(!is.na(dep_delay),!is.na(arr_delay)) %>% 
                      select(time_hour, origin, dest, carrier, dep_delay, arr_delay, air_time, distance)
flts

#Using lubricate to get month,hour,dayofweek columns 
#Used select statement to incorporate results as per the screenshot in the assignment

flts = flts %>% mutate( Month = month(time_hour,label = T),DayOfWeek = wday(time_hour,label = T),HourOfDay=hour(time_hour))%>% 
                select( time_hour,Month,DayOfWeek,HourOfDay,origin,dest,carrier,everything())
flts

# Group by HourOfDay sort by dep_delay desc
dep_delay_by_hour = flts %>% group_by(HourOfDay) %>% 
                             summarise(AvrDepDelay=mean(dep_delay)) %>% 
                             arrange(desc(AvrDepDelay))
dep_delay_by_hour

# Group by Month sort by dep_delay desc
dep_delay_by_month = flts %>% group_by(Month) %>% 
                              summarise(AvrDepDelay=mean(dep_delay)) %>% 
                              arrange(desc(AvrDepDelay))
dep_delay_by_month

#Group by Carrier sort by dep_delay desc
dep_delay_by_carrier = flts %>% group_by(carrier) %>% 
                                summarise(AvrDepDelay=mean(dep_delay)) %>% 
                                arrange(desc(AvrDepDelay))
dep_delay_by_carrier

# Group by Origin,month sort by dep_delay desc
dep_delay_by_airport_month = flts %>% group_by(origin,Month) %>% 
                                      summarise(AvrDepDelay=mean(dep_delay)) %>% 
                                      arrange(desc(AvrDepDelay))
dep_delay_by_airport_month

#Mutating new column day section to interpret part of the day
day_section = flts %>% mutate(DaySection = case_when((HourOfDay >= 5 & HourOfDay <12) ~ 'Morning',(HourOfDay >= 12 & HourOfDay <18) ~ 'Afternoon',HourOfDay>=18 ~ 'Evening')) %>% 
                       select(time_hour,DaySection,Month,DayOfWeek,HourOfDay,origin,dest,everything())
day_section

#Side by side box plot for each day of week vs dep_delay < 60 by day section
day_section %>% filter(dep_delay < 60) %>% 
                ggplot(aes(x=DayOfWeek,y=dep_delay,colour = DaySection)) + geom_boxplot()