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


today<-Sys.Date()
t.default_colour <- "black"

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
    mutate(eval=is.na(end),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No end date"),RAG_comment)) %>% ### No end Date
    mutate(eval=is.na(start),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No start date",sep=", "),RAG_comment)) %>% ### No start Date
    mutate(eval=((State=="To Do") & metric_start_minus_today>0),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"Should have started",sep=", "),RAG_comment)) %>% ### Should have started
    mutate(eval=(!(State=="Complete") & metric_today_minus_end<0),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"Should have ended",sep=", "),RAG_comment)) %>%  ### Should have ended
    mutate(eval=((State=="To Do") & (assignee=="NA")),
           RAG=ifelse(eval,paste(RAG,"A"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%  ### No Assignee while To Do 
    mutate(eval=((State=="In Progress") & (assignee=="NA")),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%  ### No Assignee while In Progress
    select(-eval) %>%
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","x"))) %>% #consolidate Rs and As
    mutate(RAG=ifelse(RAG=="x",
                      ifelse(State=="In Progress","G",
                             ifelse(State=="Blocked","A", 
                                    "G" )),
                      RAG)) %>%
    mutate(RAG=ifelse(State %in% c("Complete"),"g",RAG))
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
           t.start = cell_spec(start,color=ifelse(metric_start_minus_today>0,
                                                  (status_colours %>%
                                                     filter(colour_short=="R") %>% 
                                                     pull(hex)),
                                                  ifelse(metric_start_minus_today>-1,
                                                         (status_colours %>%
                                                            filter(colour_short=="A") %>% 
                                                            pull(hex)),
                                                         t.default_colour))),
           t.end= cell_spec(end,color= ifelse(metric_today_minus_end<0,
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
    mutate(eval=is.na(strftime(due)) ,
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No due date"),RAG_comment)) %>% ### No Due Date
    mutate(eval=((state=="incomplete") & metric_today_minus_end<0),
           RAG=ifelse(eval,
                      ifelse((metric_today_minus_end<-7),paste(RAG,"R"),paste(RAG,"A")),RAG),
           RAG_comment=ifelse(eval,
                              ifelse((metric_today_minus_end<-7),
                                     paste(RAG_comment,"Should have ended more than a week back",sep=", "),
                                     paste(RAG_comment,"Should have ended",sep=", ")),
                              RAG_comment)) %>%  ### Should have been done
    mutate(eval=((state=="incomplete") & is.na(assignee)),
           RAG=ifelse(eval,paste(RAG,"R"),RAG),
           RAG_comment=ifelse(eval,paste(RAG_comment,"No Assignee",sep=", "),RAG_comment)) %>%### No Assignee 
    mutate(RAG=ifelse(grepl("R",RAG),"R",
                      ifelse(grepl("A",RAG),"A","G"))) %>%
    mutate(RAG=ifelse(state %in% c("complete"),"g",RAG)) %>%
    select(-eval)
  
  ### add colour options
  
  actions <- actions %>% left_join(status_colours %>% select(RAG=colour_short,RAG_colour=hex),by="RAG") %>%
    left_join((project_kanban_background %>% select(state=Action,State_colour=hex)),by="state")
  
  actions <- actions %>%
    mutate(t.RAG=cell_spec(RAG,color=RAG_colour,background = RAG_colour,tooltip=RAG_comment),
           t.assignee = assignee,
           t.State = cell_spec(state,color="white",background=State_colour),
           t.due = cell_spec(due,color= ifelse(metric_today_minus_end<-7,
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
                              color=ifelse(state=="complete",State_colour,t.default_colour),
                              tooltip="Click to see source")
    )
  
  
}