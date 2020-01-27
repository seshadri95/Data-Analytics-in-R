#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 9								
#	TOPIC - "Closures"            					STUDEBT ID: 19230107
#----------------------------------------------------------------------------------

library(tidyverse)


my_timer = function(){
  
  p_name = 'unknown'
  start_time = 0
  end_time = 0
  time_diff = 0
  
  tbl = tibble(Name=character(), StartTime=character(),  FinishTime=character(),Duration= numeric()) 
  
  #Function to write data into tibble
  archive = function()tbl <<- add_row(tbl,Name = p_name,StartTime=as.character(start_time),  FinishTime=as.character(end_time),Duration= time_diff)
  
  list( #Returns a list of functions
    
    start = function(name='unknown'){  # Records start_time @ fn call& store name
      start_time <<- Sys.time() 
      p_name <<- name
    },
    
    finish = function(){ # Records end_time @ fn call, calclates start vs end time diff and calls archive fn 
      end_time <<- Sys.time()
      time_diff <<- end_time - start_time
      archive()
    },
    
    get_time = function() time_diff, #Returns time difference
    summary = function() list(Name = p_name,StartTime = start_time,FinishTime = end_time), # Custtomised list summary
    archive = archive,
    get_all_times = function() tbl #Returns tibble
    
  )
  
  
}


t = my_timer()
str(t)

t$start("Person1")
Sys.sleep(3)
t$finish()
t$get_time()
t$summary()

t$start("Person2")
Sys.sleep(2)
t$finish()
t$get_time()

t$summary()
t$get_all_times()