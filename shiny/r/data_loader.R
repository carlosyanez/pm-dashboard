### Select from different loading sources.

if(app_vars$source_system=="Trello"){
  
    source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)

  ##code to run
    my_token <- trello_token(app_vars$trello_key)
    trello<-trello_retrieve_data(my_token)
    normalised_data <- trello_normalise(trello,app_vars$programme_board,app_vars$tasks_states)
    normalised_data$data_retrieved <- if(app_vars$source_system=="Demo"){app_vars$demo_now}else{lubridate::now()}
  
}else{
  if(app_vars$source_system=="Excel"){
    
    source("./r/excel.R", echo = F, prompt.echo = "", spaced = F)
    
    normalised_data <- excel_normalised(app_vars$excel_files,app_vars$url_value,app_vars$date_last_activity)
    normalised_data$data_retrieved <- if(app_vars$source_system=="Demo"){app_vars$demo_now}else{lubridate::now()}
    
  }else{
    
    source("./r/excel.R", echo = F, prompt.echo = "", spaced = F)
    
    normalised_data <- excel_normalised(app_vars$demo_files,app_vars$url_value,app_vars$date_last_activity)
    normalised_data$data_retrieved <- if(app_vars$source_system=="Demo"){app_vars$demo_now}else{lubridate::now()}
    
  }
}

message("Data Loaded")
