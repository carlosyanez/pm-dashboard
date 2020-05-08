##### REQUIRED
### normalised_data
###status_colours
#### project_kanban_background

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

#source("./variables.R", echo = F, prompt.echo = "", spaced = F)

#Functions#
##########

#setwd("./")
if(!exists("normalised_data")){
  normalised_data<-readRDS(file=Rdata_file)
  
}


eval_tasks <- function(normalised_data,
                       status_colours,
                       project_kanban_background, 
                       today, t.default_colour){
  
  tasks <- normalised_data$tasks
  
  #Fill end and start dates if not available
  
  if(!(any(names(tasks) == 'end'))){
    tasks$end <- tasks$due
    tasks <- select(tasks,-due)
  }
  
  if(!(any(names(tasks) == 'start'))){
    tasks$start <- tasks$end
  }
  
  # Add metrics
  
  tasks <- tasks %>% mutate(metric_today_minus_end=ifelse(is.na(end),10^6,as.duration(interval(today,end)) / ddays(1)),
                            metric_start_minus_today=ifelse(is.na(start),-10^6,as.duration(interval(start,today)) / ddays(1)),
                            metric_activity_minus_today=ifelse(is.na(dateLastActivity),-10^6,as.duration(interval(dateLastActivity,today)) / ddays(1)))
  
  
  
  #Calculate RAG
  
  tasks <- tasks %>% mutate(RAG="x",RAG_comment="-") %>%
    mutate(eval_no_due_date=is.na(end),
           RAG=ifelse(eval_no_due_date,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_no_due_date,paste(RAG_comment,"No end date"),RAG_comment)) %>% ### No end Date
    mutate(eval_no_start_date=is.na(start),
           RAG=ifelse(eval_no_start_date,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_no_start_date,paste(RAG_comment,"No start date",sep=", "),RAG_comment)) %>% ### No start Date
    mutate(eval_late_start=((State=="To Do") & metric_start_minus_today>0),
           RAG=ifelse(eval_late_start,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_late_start,paste(RAG_comment,"Should have started",sep=", "),RAG_comment)) %>% ### Should have started
    mutate(eval_overdue=(!(State=="Complete") & metric_today_minus_end<0),
           RAG=ifelse(eval_overdue,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_overdue,paste(RAG_comment,"Should have ended",sep=", "),RAG_comment)) %>%  ### Should have ended
    mutate(eval_unassigned=((State=="To Do") & (assignee=="NA")),
           RAG=ifelse(eval_unassigned,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval_unassigned,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%  ### No Assignee while To Do 
    mutate(eval_unassigned2=((State=="In Progress") & (assignee=="NA")),
           RAG=ifelse(eval_unassigned2,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_unassigned2,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>% ### No Assignee while In Progress
    mutate(eval_unassigned=(eval_unassigned | eval_unassigned2)) %>%
    select(-eval_unassigned2) %>%
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","x"))) %>% #consolidate Rs and As
    mutate(RAG=ifelse(RAG=="x",
                      ifelse(State=="In Progress","G",
                             ifelse(State=="Blocked","A", 
                                    "G" )),
                      RAG)) %>%
    mutate(RAG=ifelse(State %in% c("Complete"),"g",RAG)) %>%
    mutate(RAG_comment = gsub(x = RAG_comment, pattern = "-,", replacement = "-")) 
  ####
  ### add colour options
  
  tasks <- tasks %>% left_join(status_colours %>% select(RAG=colour_short,RAG_colour=hex),by="RAG") %>%
    left_join(project_kanban_background %>% select(State=Task,State_colour=hex),by="State")
  
  
  ## add latest comment:
  
  latest_comment <- normalised_data$comments %>% 
    left_join((normalised_data$comments%>% 
                 group_by(card_id) %>% 
                 summarise(latest_date=max(date)) %>%
                 ungroup()),by="card_id") %>%
    mutate(latest=(date==latest_date)) %>% 
    filter(latest) %>% select(card_id,date,comment) %>%
    group_by(card_id,date) %>% 
    mutate(comment = paste0(comment, collapse = ", ")) %>%
    ungroup() %>%
    unique(.) %>% mutate(comment = paste(date, comment, sep = ": ")) %>%
    select(id=card_id,comment)
  
  
  tasks <- tasks %>% left_join(latest_comment, by="id") 
  
  rm(latest_comment)
  
  tasks <- tasks %>%
    mutate(t.RAG=cell_spec(RAG,color=RAG_colour,background = RAG_colour,tooltip=RAG_comment),
           t.Task=cell_spec(Task,link=url,
                            color=ifelse(State=="Complete",State_colour,t.default_colour),
                            tooltip="Click to see source"),
           t.assignee = assignee,
           t.State = cell_spec(State,color="white",background=State_colour,tooltip=comment),
           t.start = cell_spec(ifelse(is.na(strftime(start)),
                                      NA,
                                      paste(day(start),month(start, label=TRUE))),color=ifelse(metric_start_minus_today>0,
                                                  (status_colours %>%
                                                     filter(colour_short=="R") %>% 
                                                     pull(hex)),
                                                  ifelse(metric_start_minus_today>-1,
                                                         (status_colours %>%
                                                            filter(colour_short=="A") %>% 
                                                            pull(hex)),
                                                         t.default_colour))),
           t.end= cell_spec(ifelse(is.na(strftime(end)),
                                   NA,
                                   paste(day(end),month(end, label=TRUE))),color= ifelse(metric_today_minus_end<0,
                                              (status_colours %>%
                                                 filter(colour_short=="R") %>% 
                                                 pull(hex)),
                                              ifelse(metric_today_minus_end<1,
                                                     (status_colours %>%
                                                        filter(colour_short=="A") %>% 
                                                        pull(hex)),
                                                     t.default_colour))
           ),
           
    )
  
}


eval_actions <- function(normalised_data,
                         status_colours,
                         project_kanban_background, 
                         today, t.default_colour){
  
  actions <- normalised_data$actions %>% 
    mutate(metric_today_minus_end=ifelse(is.na(strftime(due)),10^6,as.duration(interval(today,due)) / ddays(1))) %>% 
    mutate(RAG="x",RAG_comment="-") %>%
    mutate(eval_no_due_date=is.na(strftime(due)) ,
           RAG=ifelse(eval_no_due_date,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_no_due_date,paste(RAG_comment,"No due date"),RAG_comment)) %>% ### No Due Date
    mutate(eval_overdue=((State=="Open") & metric_today_minus_end<0),
           RAG=ifelse(eval_overdue,
                      ifelse((metric_today_minus_end<-7),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval_overdue,
                              ifelse((metric_today_minus_end<-7),
                                     paste(RAG_comment,"Should have ended more than a week back",sep=", "),
                                     paste(RAG_comment,"Should have ended",sep=", ")),
                              RAG_comment)) %>%  ### Should have been done
    mutate(eval_unassigned=((State=="Open") & is.na(assignee)),
           RAG=ifelse(eval_unassigned,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_unassigned,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%### No Assignee 
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","G"))) %>%
    mutate(RAG=ifelse(State %in% c("Closed"),"g",RAG)) %>%
    mutate(RAG_comment = gsub(x = RAG_comment, pattern = "-,", replacement = "-")) 
  
  ### add colour options
  
  actions <- actions %>% left_join(status_colours %>% select(RAG=colour_short,RAG_colour=hex),by="RAG") %>%
    left_join((project_kanban_background %>% select(State=Action,State_colour=hex)),by="State")
  
  actions <- actions %>%
    mutate(t.RAG=cell_spec(RAG,color=RAG_colour,background = RAG_colour,tooltip=RAG_comment),
           t.assignee = assignee,
           t.State = cell_spec(State,color="white",background=State_colour),
           t.due = cell_spec(ifelse(is.na(strftime(due)),
                                    NA,
                                    paste(day(due),month(due, label=TRUE))),color= ifelse(metric_today_minus_end<-7,
                                               (status_colours %>%
                                                  filter(colour_short=="R") %>% 
                                                  pull(hex)),
                                               ifelse(metric_today_minus_end<0,
                                                      (status_colours %>%
                                                         filter(colour_short=="A") %>% 
                                                         pull(hex)),
                                                      t.default_colour))
           ),
           t.Action=cell_spec(action,link=url,
                              color=ifelse(State=="Closed",State_colour,t.default_colour),
                              tooltip="Click to see source"),
           t.Project = Project
    )
  
  
}

eval_issues <- function(normalised_data,
                        status_colours,
                        project_kanban_background, 
                        today, t.default_colour){
  
  
  issues <- normalised_data$issues %>%
    mutate(metric_today_minus_end=ifelse(is.na(strftime(due)),10^6,as.duration(interval(today,due)) / ddays(1))) %>%
    mutate(RAG=Severity,RAG_comment="-") %>%
    mutate(eval_no_due_date=is.na(strftime(due)) ,
           RAG=ifelse(eval_no_due_date,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_no_due_date,paste(RAG_comment,"No due date"),RAG_comment)) %>% ### No Due Date
    mutate(eval_overdue=((State=="Open") & metric_today_minus_end<0),
           RAG=ifelse(eval_overdue,
                      ifelse((metric_today_minus_end<-7),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval_overdue,
                              ifelse((metric_today_minus_end<-7),
                                     paste(RAG_comment,"Should have ended more than a week back",sep=", "),
                                     paste(RAG_comment,"Should have ended",sep=", ")),
                              RAG_comment)) %>%  ### Should have been done
    mutate(eval_unassigned=((State=="Open") & (is.na(Assignee)| Assignee=="NA")),
           RAG=ifelse(eval_unassigned,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval_unassigned,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%### No Assignee 
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","G"))) %>%
    mutate(RAG=ifelse(State %in% c("Closed"),"g",RAG)) %>%
    mutate(RAG_comment = gsub(x = RAG_comment, pattern = "-,", replacement = "-")) 
  
  
  issues <- issues %>% left_join(status_colours %>% select(RAG=colour_short,RAG_colour=hex),by="RAG") %>%
    left_join((project_kanban_background %>% select(State=Issue,State_colour=hex)),by="State")
  
  ##add latest comments
  
  latest_comment <- normalised_data$comments %>% 
    left_join((normalised_data$comments%>% 
                 group_by(card_id) %>% 
                 summarise(latest_date=max(date)) %>%
                 ungroup()),by="card_id") %>%
    mutate(latest=(date==latest_date)) %>% 
    filter(latest) %>% select(card_id,date,comment) %>%
    group_by(card_id,date) %>% 
    mutate(comment = paste0(comment, collapse = ", ")) %>%
    ungroup() %>%
    unique(.) %>% mutate(comment = paste(date, comment, sep = ": ")) %>%
    select(id=card_id,comment)
  
  
  issues <- issues %>% left_join(latest_comment, by="id") 
  
  rm(latest_comment)
  
  
  ##table format
  
  issues <- issues %>%
    mutate(t.RAG=cell_spec(RAG,color=RAG_colour,background = RAG_colour,tooltip=RAG_comment),
           t.Project = Project,
           t.Assignee = Assignee,
           t.State = cell_spec(State,color="white",background=State_colour,tooltip=comment),
           t.due = cell_spec(ifelse(is.na(strftime(due)),
                                    NA,
                                    paste(day(due),month(due, label=TRUE))),color= ifelse(metric_today_minus_end<-7,
                                               (status_colours %>%
                                                  filter(colour_short=="R") %>% 
                                                  pull(hex)),
                                               ifelse(metric_today_minus_end<0,
                                                      (status_colours %>%
                                                         filter(colour_short=="A") %>% 
                                                         pull(hex)),
                                                      t.default_colour))
           ),
           t.Title=cell_spec(Title,link=url,
                             color=ifelse(State=="Done",State_colour,t.default_colour),
                             tooltip="Click to see source"),
           t.Issue=cell_spec(Issue,link=url,
                             color=ifelse(State=="Done",State_colour,t.default_colour),
                             tooltip="Click to see source"),
           t.Impact=cell_spec(Impact,
                              color=ifelse(State=="Done",State_colour,t.default_colour)),
           t.Action=cell_spec(Action,
                              color=ifelse(State=="Done",State_colour,t.default_colour),
                              tooltip=comment)
           
    )
  
}

eval_projects <- function(normalised_data,
                          status_colours,
                          project_kanban_background, 
                          today, t.default_colour){
  
  task_start <- presentation_data$tasks %>% 
    filter(!is.na(strftime(start))) %>%
    group_by(Project_id) %>%
    summarise(task.start=min(start)) %>% ungroup()
  
  task_end <- presentation_data$tasks %>% 
    filter(!is.na(strftime(end))) %>%
    group_by(Project_id) %>%
    summarise(task.end=max(end)) %>% ungroup()
  
  
  task_metrics <- presentation_data$tasks %>% group_by(Project_id) %>%
    summarise(task.R=sum(RAG=="R"),
              task.A=sum(RAG=="A"),
              task.G=sum(RAG=="G"),
              task.g=sum(RAG=="g"),
              task.eval_no_due_date = sum(eval_no_due_date),
              task.eval_no_start_date = sum(eval_no_start_date),
              task.eval_late_start = sum(eval_late_start),
              task.eval_overdue = sum(eval_overdue),
              task.eval_overdue_days = sum(-metric_today_minus_end*eval_overdue),
              task.eval_unassigned = sum(eval_unassigned),
    ) %>% ungroup %>%
    mutate(task.total = task.R+task.A+task.G+task.g,
           task.progress = task.g/task.total
    ) %>%
    left_join(task_start,by="Project_id") %>%
    left_join(task_end,by="Project_id")
  
  rm(task_start,task_end)
  
  
  action_due <- presentation_data$actions %>% 
    filter(!is.na(strftime(due))) %>%
    group_by(Project_id) %>%
    summarise(action.due=min(due)) %>% ungroup()
  
  action_metrics <- presentation_data$actions%>% group_by(Project_id) %>%
    summarise(action.R=sum(RAG=="R"),
              action.A=sum(RAG=="A"),
              action.G=sum(RAG=="G"),
              action.g=sum(RAG=="g"),
              action.eval_no_due_date = sum(eval_no_due_date),
              action.eval_overdue = sum(eval_overdue),
              action.eval_overdue_days = sum(-metric_today_minus_end*eval_overdue),
              action.eval_unassigned = sum(eval_unassigned),
    ) %>% ungroup %>%
    mutate(action.open=action.R+action.A+action.G) %>%
    left_join(action_due,by="Project_id")
  
  rm(action_due)
  
  
  issue_due <- presentation_data$issues %>% 
    filter(!is.na(strftime(due))) %>%
    group_by(Project_id) %>%
    summarise(issue.due=min(due)) %>% ungroup()
  
  issue_metrics <- presentation_data$issues%>% group_by(Project_id) %>%
    summarise(issue.R=sum(RAG=="R"),
              issue.A=sum(RAG=="A"),
              issue.G=sum(RAG=="G"),
              issue.g=sum(RAG=="g"),
              issue.eval_no_due_date = sum(eval_no_due_date),
              issue.eval_overdue = sum(eval_overdue),
              issue.eval_overdue_days = sum(-metric_today_minus_end*eval_overdue),
              issue.eval_unassigned = sum(eval_unassigned),
    ) %>% ungroup %>%
    mutate(issue.open=issue.R+issue.A+issue.G) %>%
    left_join(issue_due,by="Project_id")
  
  rm(issue_due)
  
  ## latest comment
  
  latest_comment <- normalised_data$comments %>% 
    left_join((normalised_data$comments%>% 
                 group_by(card_id) %>% 
                 summarise(latest_date=max(date)) %>%
                 ungroup()),by="card_id") %>%
    mutate(latest=(date==latest_date)) %>% 
    filter(latest) %>% select(card_id,date,comment) %>%
    group_by(card_id,date) %>% 
    mutate(comment = paste0(comment, collapse = ", ")) %>%
    ungroup() %>%
    unique(.) %>% mutate(comment = paste(date, comment, sep = ": ")) %>%
    select(id=card_id,comment,comment_updated=date)
  
  
  
  projects <- 
    normalised_data$projects %>% 
    left_join((latest_comment %>% select(updates_id=id,Updates=comment,comment_updated)), by="updates_id") %>%
    left_join(task_metrics,by="Project_id") %>%
    left_join(action_metrics,by="Project_id") %>%
    left_join(issue_metrics,by="Project_id") %>%
    left_join((status_colours%>% select(labels=colour,RAG=colour_short)),by="labels") %>%
    mutate(RAG_comment="-") 
  
  #Fill end and start dates if not available
  
  if(!(any(names(projects) == 'end'))){
    projects$end <- projects$due
    projects <- select(projects,-due)
  }
  
  if(!(any(names(projects) == 'start'))){
    projects$start <- projects$end
  }
  
  projects <- projects %>% mutate(metric_today_minus_end=ifelse(is.na(end),10^6,as.duration(interval(today,end)) / ddays(1)),
                                  metric_start_minus_today=ifelse(is.na(start),-10^6,as.duration(interval(start,today)) / ddays(1)),
                                  metric_activity_minus_today=ifelse(is.na(comment_updated),-10^6,
                                                                     as.duration(interval(comment_updated,today)) / ddays(1))) %>%
    mutate(task.progress=ifelse(is.na(task.progress),0,round(100*task.progress)),
           task.eval_overdue=ifelse(is.na(task.eval_overdue),0,task.eval_overdue),
           issue.open=ifelse(is.na(issue.open),0,issue.open)
           
    )
  
  
  projects <- projects %>% mutate(start=as_date(ifelse(is.na(strftime(task.start)),start,task.start)),
                                  end=as_date(ifelse(is.na(strftime(task.end)),end,task.end))) %>%
    mutate(eval=(State %in% c("Planning", "In Progress")) & is.na(strftime(start)) ,
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No start date"),RAG_comment)) %>% ###No start date R
    mutate(eval=(State %in% c("Planning", "In Progress")) & is.na(strftime(end)),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No due date"),RAG_comment)) %>% ### No end date R
    mutate(eval=((State %in% c("Backlog","Planning")) & metric_start_minus_today>0),
           project.eval_late_start=eval,
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "Should have started",sep=", "),
                              RAG_comment)) %>% ### Should have started R
    mutate(eval=(!(State=="Complete") & metric_today_minus_end<0),
           project.eval_overdue=eval,
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "Should have ended",sep=", "),
                              RAG_comment)) %>% ### Should have ended R
    mutate(eval=((State=="Planning") & (Scope=="NA" | is.na(Scope))),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No scope",sep=", "),
                              RAG_comment)) %>% 
    mutate(eval=((State=="In Progress") & (Scope=="NA" | is.na(Scope))),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No scope",sep=", "),
                              RAG_comment)) %>%  #### No scope in Planning A , Progress R
    mutate(eval=((State=="Planning") & (Objectives=="NA" | is.na(Objectives))),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No objectives",sep=", "),
                              RAG_comment)) %>% 
    mutate(eval=((State=="In Progress") & (Objectives=="NA" | is.na(Objectives))),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No objectives",sep=", "),
                              RAG_comment)) %>% #### No objectives in Planning A, Progress R
    mutate(eval=((State=="Planning") & (Project_Manager=="NA" | is.na(Project_Manager))),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No PM",sep=", "),
                              RAG_comment)) %>% 
    mutate(eval=((State=="In Progress") & (Project_Manager=="NA" | is.na(Project_Manager))),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No PM",sep=", "),
                              RAG_comment)) %>% #### No PM Planning A, Progress R
    mutate(eval=((State=="Planning") & (Project_Lead=="NA" | is.na(Project_Lead))),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No lead",sep=", "),
                              RAG_comment)) %>% 
    mutate(eval=((State=="In Progress") & (Project_Lead=="NA" | is.na(Project_Lead))),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No lead",sep=", "),
                              RAG_comment)) %>%  #### No lead in Planning A , Progress R
    mutate(eval=((State=="Planning") & (Project_id=="NA" | is.na(Project_id) | is.na(strftime(task.end)))),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No project plan",sep=", "),
                              RAG_comment)) %>% 
    mutate(eval=((State=="In Progress") & (Project_id=="NA" | is.na(Project_id) | is.na(strftime(task.end)))),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,
                                         "No project plan",sep=", "),
                              RAG_comment)) %>%  ### No board, Planning R , Progress R
    mutate(eval=((State %in% c("In Progress")) & metric_activity_minus_today>update_date_limit_1),
           RAG=ifelse(eval,
                      ifelse((metric_activity_minus_today>update_date_limit_2),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,
                              ifelse((metric_activity_minus_today>update_date_limit_2),
                                     paste(RAG_comment,"Missing recent status update",sep=", "),
                                     paste(RAG_comment,"Very old update",sep=", ")),
                              RAG_comment)) %>%  ### Last update older than   update_date_limit_1
    ### Last update older than update_date_limit_2
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(task.R),task.R>task.R.threshold.1,FALSE)),
           RAG=ifelse(eval,
                      ifelse((task.R>task.R.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Tasks R:",task.R),sep=", "),
                              RAG_comment)) %>%    ### Task_R treshold 1 -A
    ### Task_R treshold 2 -R
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(task.A),task.A>task.A.threshold.1,FALSE)),
           RAG=ifelse(eval,
                      ifelse((task.A>task.A.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Tasks A:",task.A),sep=", "),
                              RAG_comment)) %>%               ### Task_A treshold 1 -A
    ### Task_A treshold 2 -R
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(issue.R),issue.R>issue.R.threshold.1,
                                                                   FALSE)),
           RAG=ifelse(eval,
                      ifelse((issue.R>issue.R.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Isssues R:",issue.R),sep=", "),
                              RAG_comment)) %>%               ### Issue_R treshold 1 -A
    ### Issue_R treshold 2 -R
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(issue.A),issue.A>issue.A.threshold.1,
                                                                   FALSE)),
           RAG=ifelse(eval,
                      ifelse((issue.A>issue.A.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Issues A:",issue.A),sep=", "),
                              RAG_comment)) %>%               ### Issue_A treshold 1 -A
    ### Issue_A treshold 2 -R
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(action.R),action.R>action.R.threshold.1,
                                                                   FALSE)),
           RAG=ifelse(eval,
                      ifelse((action.R>action.R.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Actions R:",action.R),sep=", "),
                              RAG_comment)) %>%               ### Action_R treshold 1 -A
    ### Action_R treshold 2 -R
    mutate(eval=((State %in% c("Planning","In Progress")) & ifelse(!is.na(action.A),action.A>action.A.threshold.1,
                                                                   FALSE)),
           RAG=ifelse(eval,
                      ifelse((action.A>action.A.threshold.1),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,paste("Actions A:",action.A),sep=", "),
                              RAG_comment)) %>%               ### Action_A treshold 1 -A
    ### Action_A treshold 2 -R
    mutate(RAG=ifelse(RAG=="NA","G",RAG)) %>%
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","G"))) %>%
    mutate(RAG=ifelse(State %in% c("Closed","Complete"),"g",RAG)) %>%
    mutate(RAG_comment = gsub(x = RAG_comment, pattern = "-,", replacement = "-")) %>%
    select(-eval,-task.start,-task.end,-action.due,-issue.due,-labels)                                                           
  
  #add colours
  projects <- projects %>% left_join(status_colours %>% select(RAG=colour_short,RAG_colour=hex),by="RAG") %>%
    left_join(project_kanban_background %>% select(State,State_colour=hex),by="State")
  
  #create table items
  projects <- projects %>%
    mutate(eval=(State %in% c("Closed","Complete")),
           t.RAG=cell_spec(RAG,color=RAG_colour,background = RAG_colour,tooltip=RAG_comment),
           t.Project=cell_spec(Name,link=url,
                               color=ifelse(eval,State_colour,t.default_colour),
                               tooltip="Click to see source",
                               align="left",bold=T),
           t.Project_Lead = cell_spec(Project_Lead,color=ifelse(eval,State_colour,t.default_colour)),
           t.Project_Manager = cell_spec(Project_Manager,color=ifelse(eval,State_colour,t.default_colour)),
           t.State = cell_spec(State,color="white",background=State_colour,tooltip=Updates),
           t.Update=cell_spec(Updates,color=ifelse(eval,State_colour,t.default_colour),align="left"),
           t.start = cell_spec(ifelse(is.na(strftime(start)),
                                      NA,
                                      paste(day(start),month(start, label=TRUE))),color= ifelse(eval,State_colour,
                                                   ifelse(metric_start_minus_today>0,
                                                          (status_colours %>%
                                                             filter(colour_short=="R") %>% 
                                                             pull(hex)),
                                                          ifelse(metric_start_minus_today>-1,
                                                                 (status_colours %>%
                                                                    filter(colour_short=="A") %>% 
                                                                    pull(hex)),
                                                                 t.default_colour)))),
           t.end= cell_spec(ifelse(is.na(strftime(end)),
                                   NA,
                                   paste(day(end),month(end, label=TRUE))),color= ifelse(eval,State_colour,
                                              ifelse(metric_today_minus_end<0,
                                                     (status_colours %>%
                                                        filter(colour_short=="R") %>% 
                                                        pull(hex)),
                                                     ifelse(metric_today_minus_end<1,
                                                            (status_colours %>%
                                                               filter(colour_short=="A") %>% 
                                                               pull(hex)),
                                                            t.default_colour)))),
           t.Progress=ifelse(State %in% c("In Progress"),cell_spec(task.progress,color="white",background = "lightgreen",tooltip=paste("Completion percentage:",task.progress)),""),
           t.Overdue_Tasks = ifelse(!(State %in% c("Complete","Backlog")),cell_spec(task.eval_overdue,color="white",background = "orange",tooltip=paste("Overdue Tasks: R=",task.eval_overdue)),""),
           t.Open_Issues = ifelse(!(State %in% c("Complete","Backlog")),cell_spec(issue.open,color="white",background = "orange",tooltip=paste("Open Issues: R=",issue.R,", A=",issue.A,", G=",issue.G)),""),
           k.Project = cell_spec(Name,color="white",background = RAG_colour,tooltip=Updates,link=url),
           k.Progress=t.Progress,
           k.Overdue_Tasks = t.Overdue_Tasks,
           k.Open_Issues = t.Open_Issues
    ) %>%
    select(-eval)
  
  projects
  
}

eval_stats <- function(presentation_data){
  programme_stats <- presentation_data$projects %>%group_by(State) %>% summarise(by_state=n()) %>% 
  mutate(key="Stats") %>%
  pivot_wider(names_from = State,values_from = by_state) 

  programme_stats$R <- sum(presentation_data$project$RAG =="R")
  programme_stats$A <- sum(presentation_data$project$RAG =="A")
  programme_stats$G <- sum(presentation_data$project$RAG =="G")
  programme_stats$late.tasks <-  sum(presentation_data$project$task.eval_overdue)
  programme_stats$open.issues <- sum(presentation_data$project$issue.open)
  programme_stats$open.actions <- sum(presentation_data$project %>%
                                        filter(!is.na(issue.open)) %>% pull(issue.open))
  programme_stats$late.start <- sum(presentation_data$project %>%
                                      filter(!is.na(project.eval_late_start)) %>% pull(project.eval_late_start))
  programme_stats$overdue <- sum(presentation_data$project%>%
                                   filter(!is.na(project.eval_overdue)) %>% pull(project.eval_overdue))
  
  programme_stats$issue.R <- sum(presentation_data$project%>%
                                   filter(!is.na(issue.R)) %>% pull(issue.R))
  
  
  programme_stats$issue.A <- sum(presentation_data$project%>%
                                   filter(!is.na(issue.A)) %>% pull(issue.A))




programme_stats
}

eval_consolidated_items <- function(presentation_data){
  
  t.consolidated_tasks <- bind_rows(presentation_data$tasks %>% mutate(Type="Task") %>%
                                      select(Type,Project=Project_Name,Item=Task,State,
                                             assignee,due=end,RAG,
                                             t.Project=Project_Name,
                                             t.RAG,t.Item=t.Task,t.assignee,t.State,
                                             t.due=t.end),
                                    presentation_data$actions %>% mutate(Type="Action") %>%
                                      select(Type,Project,Item=action,State,assignee,
                                             due,RAG,t.RAG,t.Item=t.Action,t.assignee,
                                             t.State,t.due),
                                    presentation_data$issues %>% mutate(Type="Issue") %>%
                                      select(Type,Project,Item=Title,State,assignee=Assignee,
                                             due,RAG,t.RAG,t.Item=t.Title,
                                             t.assignee=t.Assignee,
                                             t.State,t.due))
  
  
  t.consolidated_tasks <-  t.consolidated_tasks %>% mutate(f.condition1=(due <= today),
                                                           
                                                           f.condition2=(due < (today + horizon_span1)),
                                                           f.condition3=(due < (today + horizon_span2)),
                                                           f.condition4=is.na(strftime(due)),
                                                           f.condition5=!(State %in% c("Complete","Closed")),
                                                           f.selection=((f.condition1 | f.condition3 |f.condition4)
                                                                        & f.condition5)) %>%
    filter(f.selection) %>%
    mutate(row_colour=ifelse(f.condition4,"X",
                             ifelse(f.condition1,"U",
                                    ifelse(f.condition2,"M",
                                           ifelse(f.condition3,"L","N"))))) %>%
    arrange(due)
  
  t.consolidated_tasks
  
}


eval_items_stats <- function(t.consolidated_tasks,palette){
  
  set.seed(1234)
  
  output <-vector(mode = "list", length = 0)
  
#  cloud1<-t.consolidated_tasks %>% group_by(Project) %>% summarise(freq=n()) %>%
#    ungroup() %>% 
#    mutate(word=as.factor(Project))%>%
#    select(word,freq) %>% arrange(-freq)
  
#  output$wordcloud_projects <- wordcloud(cloud1$word,cloud1$freq,scale=c(3,.1),min.freq=1, random.color=TRUE,colors=brewer.pal(8, palette))
  
 # cloud2<-t.consolidated_tasks %>%
 #   separate_rows(assignee,sep=", ") %>%
#    group_by(assignee) %>% summarise(freq=n()) %>%
#    ungroup() %>% 
 #   mutate(word=as.factor(assignee))%>%
  #  select(word,freq) %>% arrange(-freq)
  
  #output$wordcloud_assignee <- wordcloud(cloud2$word,cloud2$freq,scale=c(3,.1),min.freq=1, random.color=TRUE,colors=brewer.pal(8, palette))
  
  output$person_frequency <- t.consolidated_tasks %>%
    separate_rows(assignee,sep=", ") %>% filter(!is.na(assignee)) %>%
    mutate(assignee=str_trim(assignee))  %>%
    group_by(assignee) %>% summarise(freq=n()) %>%
    ungroup() %>% 
    mutate(assignee=ifelse(is.na(assignee),"unassigned",assignee)) %>%
    arrange(-freq) 
  
  output$person_frequency$nbr <- seq.int(nrow(output$person_frequency))
  
  output$project_frequency <- t.consolidated_tasks %>%
    group_by(Project) %>% summarise(freq=n()) %>%
    ungroup() %>% 
    arrange(-freq) 
  output$project_frequency$nbr <- seq.int(nrow(output$project_frequency))
  
  output$R <- sum(t.consolidated_tasks$RAG=="R")
  output$A <- sum(t.consolidated_tasks$RAG=="A")
  output$G <- sum(t.consolidated_tasks$RAG=="G")
  
  output
  
}

#Run
###########
##Run Functions

presentation_data <-vector(mode = "list", length = 0)

presentation_data$tasks <- eval_tasks(normalised_data,status_colours,
                                      project_kanban_background, 
                                      today, t.default_colour)

presentation_data$actions<- eval_actions(normalised_data,status_colours,
                                         project_kanban_background, 
                                         today, t.default_colour)

presentation_data$issues<- eval_issues(normalised_data,status_colours,
                                       project_kanban_background, 
                                       today, t.default_colour)

presentation_data$projects<- eval_projects(normalised_data,status_colours,
                                           project_kanban_background, 
                                           today, t.default_colour)

programme_stats <- eval_stats(presentation_data)

t.consolidated_tasks <- eval_consolidated_items(presentation_data)
consolidated_stats <- eval_items_stats(t.consolidated_tasks,wordcloud_palette)
