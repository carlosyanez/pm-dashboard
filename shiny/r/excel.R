
get_project_board <- function(file_location,file_name="Projects.xlsx",url_value){
  
  output <-vector(mode = "list", length = 0)
  
  
  projects_file <- paste(file_location,file_name,sep="")
  
  output$Projects <- readxl::read_excel(projects_file, 
                                 sheet = "Projects")#, col_types = c("text", "text", "text",
                                                    #               "text", "text", "numeric", 
                                                    #               "numeric", "text", "text", "text"))
  
  output$Project_Updates <- readxl::read_excel(projects_file, 
                                        sheet = "Updates", col_types = c("text", 
                                                                         "text", "text", "text"))
  
  output$Project_Actions <- readxl::read_excel(projects_file, 
                                        sheet = "Actions")#, col_types = c("text", 
                                                          #        "text", "text", "text","text","text"))
  
  
  file_names <-  output$Projects %>% mutate(name=paste(No," - ",`Project Name`,".xlsx",sep="")) %>%
    select(No,`Project Name`,name)
  
  output$files <- tibble(name=list.files(path = file_location)) %>% 
    filter(name %in% file_names$name) %>% left_join(file_names,by="name")
  
output

}

get_excel_tasks <- function(file_location,files,url_value,date_last_activity){
  
  output <-vector(mode = "list", length = 0)
  i<-1
  
  project_tasks <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                      sheet = "Plan")#,
  #col_types = c("text", "text", "text", "text", 
  #              "text", "text", "text"))
  
  project_tasks <- project_tasks[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                   Project=files[i,]$"Project Name") 
  
  
  if(nrow(files)>1){
    
    for (i in 2:nrow(files)){
      
      project_tasks_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                            sheet = "Plan")#,
      #col_types = c("text", "text", "text", "text", 
      #              "text", "text", "text"))
      
      project_tasks_i <- project_tasks_i[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                           Project=files[i,]$"Project Name") 
     
      project_tasks <- rbind(project_tasks_i,project_tasks)

    }
  }
  
  #tasks
  #"Number","Task","due","State","Group","Project_Name","assignee","id","Project_id","url","dateLastActivity"
  
 tasks<-project_tasks %>% mutate(id=paste("task",ProjectID,No,sep="."),
                                         url=url_value,
                                         start=lubridate::as_date(as.Date(Start),tz=NULL),
                                         due=lubridate::as_date(as.Date(End), tz = NULL),
                                         dateLastActivity=lubridate::as_date(date_last_activity, tz = NULL),
                                         Project_id=as.character(ProjectID)) %>%
    select(Number=No,Task,start=start,due=due,State=Status,Group=Phase,Project_Name=Project,
           assignee=Responsible,id,Project_id=Project_id,url,dateLastActivity)
  
 tasks
}

get_excel_comments <- function(file_location,files,Project_Updates,url_value){
  
  #files  <- board_data$files
  #Project_Updates <- board_data$Project_Updates

  i<-1
  
  project_update <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                       sheet = "Updates")#,
  #col_types = c("text", "text", "text"))
  
  if(nrow(files)>1){
    
    for (i in 2:nrow(files)){
  
  project_update <- project_update[,1:3] %>%  mutate(ProjectID=files[i,]$No,
                                                     Project=files[i,]$"Project Name") 
  
  
  project_update_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                         sheet = "Updates")#,
  # col_types = c("text", "text", "text"))
  
  
  project_update_i <- project_update_i[,1:3] %>%  mutate(ProjectID=files[i,]$No,
                                                         Project=files[i,]$"Project Name") 
  
  
  project_update <- rbind(project_update_i,project_update)
  
  }
}
  
  #comments
  #"","id","card_id","author","comment","date","Done","ToDo"
  
  comments <- rbind( Project_Updates %>% mutate(ToDo="") %>%
                             select(Date,
                                    Done="Update/Comments",
                                    ToDo,ProjectID=No,Project),
                           project_update %>% select(Date,Done,ToDo="To Do",
                                                     ProjectID,Project)) %>% 
    mutate(id = paste("comment",1:n(),sep="-"),
           author="",
           comment=paste(Done,ToDo,sep="-"),
           date=lubridate::as_date(Date, tz = NULL)) %>%
    select(id,card_id=ProjectID,author,comment,date=date,Done,ToDo)
  
  
  comments
}

get_excel_issues <- function(file_location,files,url_value){
  
  i<-1
  project_issues <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                       sheet = "Issues")#,
  # col_types = c("text", "text", "text", "text", 
  #              "text", "text", "text"))
  
  project_issues <- project_issues[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                     Project=files[i,]$"Project Name") 
  
  if(nrow(files)>1){
    
    for (i in 2:nrow(files)){
      
      project_issues_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                             sheet = "Issues")#,
      #col_types = c("text", "text", "text", "text", 
      #               "text", "text", "text"))
      
      project_issues_i <- project_issues_i[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                             Project=files[i,]$"Project Name")
      
      project_issues <- rbind(project_issues_i,project_issues)

    }
  }
  
  #issues
  #"Severity","Project","Title","due","Issue","Impact","Action","State","Assignee","id","Project_id","url"
  
  
  issues <- project_issues %>% mutate(id = paste("issue",1:n(),sep="-"),
                                             url=url_value,
                                             due=lubridate::as_date(Due, tz = NULL),
                                             Project_id=as.character(ProjectID)) %>%
    select(Severity,Project,Title=Issue,due=due,Issue=Issue,Impact,Action,State,Assignee,
           id,Project_id=Project_id,url)
  
  issues
  
} 

get_excel_actions  <- function(Project_Actions,url_value){
  
  #Actions
  #"","action","assignee","due","id","Project_id","Task_id","Project","Replacement","State","url"
  
  actions <- Project_Actions  %>% mutate(id = paste("action",1:n(),sep="-"),
                                                url=url_value,
                                                due=lubridate::as_date(Due, tz = NULL),
                                                No=as.character(No)) %>%
    select(action=Action,assignee=Responsible,due=due,id,Project_id=No,Project,Task_id=id,
           Replacement=State,
           State=State,
           url)
  
  actions
  
}

get_excel_projects <- function(file_location,files,tasks,Projects,url_value){
  i<-1
  ## project details
  project_details <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                        sheet = "Project_Details",
                                        col_types = c("text", 
                                                      "text")) 
  
  project_details <- project_details %>%  mutate(ProjectID=files[i,]$No,
                                                 Project=files[i,]$"Project Name") %>%
    pivot_wider(names_from = a,values_from = b) 
  
  
  # more than one file
  
  if(nrow(files)>1){
    
    for (i in 2:nrow(files)){
      
      ## project details
      project_details_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                              sheet = "Project_Details",
                                              col_types = c("text", "text")) 
      
      project_details_i <- project_details_i %>%  mutate(ProjectID=files[i,]$No,
                                                         Project=files[i,]$"Project Name") %>%
        pivot_wider(names_from = a,values_from = b) 
      
      ##bind together
      
      project_details <- rbind(project_details_i,project_details)
    }
    
  }


  ### Projects
  # Name              State              labels               due                  id    
  #    Scope            Objectives        Project_Lead       Project_Manager     Parameters 
  #   Project_id          card_id              url             updates_id     
  
  dates_task <- tasks %>% group_by(Project_id) %>% 
    summarise(start_t=min(start),due_t=max(due)) %>%
    ungroup() %>% select(No=Project_id,start_t,due_t)
  
  projects <- Projects %>% left_join((project_details %>% mutate(No=ProjectID)),by="No") %>%
    #   left_join(dates_task,by="No") %>%
    mutate(No=as.character(No),
           Scope=ifelse(is.na(Scope.y),Scope.x,Scope.y),
           Objectives=ifelse(is.na(Objectives.y),Objectives.x,Objectives.y),
           Project_Lead=ifelse(is.na(.$"Project Lead.y"),.$"Project Lead.x",.$"Project Lead.x"),
           Project_Manager=ifelse(is.na(.$"Project Manager"),PM,.$"Project Manager"),
           Parameters ="",
           labels="",
           url=Link,
           due=lubridate::as_date(End,tz = NULL)) %>%
    select(Name="Project Name",State=State,labels,
           due=due,id=No,
           Scope,Objectives,Project_Lead,
           Project_Manager,Parameters,
           Project_id=No,card_id=No,url,updates_id=No)
  
  projects

}

excel_normalised <- function(file_location,url_value,date_last_activity){

#file_location <- app_vars$demo_files
#url_value <- app_vars$url_value
#date_last_activity <- app_vars$date_last_activity

board_data <- get_project_board(file_location,"Projects.xlsx",url_value)

output <-vector(mode = "list", length = 0)

output$tasks <- get_excel_tasks(file_location,board_data$files,url_value,date_last_activity)
output$comments <- get_excel_comments(file_location,board_data$files,board_data$Project_Updates,url_value)
output$issues <- get_excel_issues(file_location,board_data$files,url_value)
output$actions <- get_excel_actions(board_data$Project_Actions,url_value)
output$projects <- get_excel_projects(file_location,board_data$files,output$tasks,board_data$Projects,url_value)

output

}
