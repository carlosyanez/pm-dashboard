####Libraries
if(!require(trelloR)) install.packages("trelloR", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(trelloR)
library(tidyverse)
library(lubridate)

#####
programme_board <-c("Sample_Programme")
tasks_states <- "tasks_states.csv"
trello_key <- "trello_secret.txt"
####

###get token
trello_token <- function(text_file){
  token_data <- read_delim(text_file,"=",col_names = FALSE)
  
  key<- token_data[which(token_data[,1]=="key"),2] %>% pull(.)
  secret<- token_data[which(token_data[,1]=="secret"),2] %>% pull(.)
  
  my_token <- trello_get_token(key,secret,appname = "my-app")

  my_token
}

###Functions to get data from Trello

get_all_cards <-function(my_boards,token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  
  cards <- get_board_cards(my_boards[1,]$id,my_token)
  
  if(n_boards>1){
    for(i in 2:n_boards){
      cards_i <- get_board_cards(my_boards[i,]$id,my_token)
      if(!is.null(cards_i)){
        cards_i[setdiff(names(cards), names(cards_i))] <- NA
        cards[setdiff(names(cards_i), names(cards))] <- NA
        cards <- rbind(cards,cards_i)
      }
    }
  }
  cards <- cards %>% filter(!is.na(id))
  cards
} 
get_all_lists <- function(my_boards,token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  
  lists <- get_board_lists(my_boards[1,]$id,my_token)
  
  if(n_boards>1){
    for(i in 2:n_boards){
      lists_i <- get_board_lists(my_boards[i,]$id,my_token)
      if(!is.null(lists_i)){
        lists_i[setdiff(names(lists), names(lists_i))] <- NA
        lists[setdiff(names(lists_i), names(lists))] <- NA
        lists <- rbind(lists,lists_i)
      }
    }
  }
  
  lists <- lists %>% filter(!is.na(id)) 
  lists
} 
get_all_checklists <- function(my_boards,my_token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  my_checklists <- get_board_checklists(my_boards[1,]$id,my_token)
  
  i<-2
  if(n_boards>1){
    for(i in 2:n_boards){
      my_checklists_i <- get_board_checklists(my_boards[i,]$id,my_token)
      if(!is.null(my_checklists_i)){
        my_checklists_i[setdiff(names(my_checklists), names(my_checklists_i))] <- NA
        my_checklists[setdiff(names(my_checklists_i), names(my_checklists))] <- NA
        my_checklists <- rbind(my_checklists,my_checklists_i)
      }
    }
  }
  my_checklists <- my_checklists %>% filter(!is.na(id))
  my_checklists
}
get_all_comments <- function(my_boards,my_token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  my_comments <- get_board_comments(my_boards[1,]$id,my_token)
  
  i<-2
  if(n_boards>1){
    for(i in 2:n_boards){
      my_comments_i <- get_board_comments(my_boards[i,]$id,my_token)
      if(!is.null(my_comments_i)){
        my_comments_i[setdiff(names(my_comments), names(my_comments_i))] <- NA
        my_comments[setdiff(names(my_comments_i), names(my_comments))] <- NA
        my_comments <- rbind(my_comments,my_comments_i)
      }
    }
  }
  my_comments <- my_comments %>% filter(!is.na(id))
  my_comments
}
get_all_labels <- function(my_boards,my_token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  my_labels <- get_board_labels(my_boards[1,]$id,my_token)
  
  i<-2
  if(n_boards>1){
    for(i in 2:n_boards){
      my_labels_i <- get_board_labels(my_boards[i,]$id,my_token)
      if(!is.null(my_labels_i)){
        my_labels_i[setdiff(names(my_labels), names(my_labels_i))] <- NA
        my_labels[setdiff(names(my_labels_i), names(my_labels))] <- NA
        my_labels <- rbind(my_labels,my_labels_i)
      }
    }
  }
  my_labels <- my_labels %>% filter(!is.na(id))
  my_labels
}
get_all_members <- function(my_boards,my_token){
  
  n_boards <- nrow(my_boards)
  
  #first one
  my_members <- get_board_members(my_boards[1,]$id,my_token)
  
  i<-2
  if(n_boards>1){
    for(i in 2:n_boards){
      my_members_i <- get_board_members(my_boards[i,]$id,my_token)
      if(!is.null(my_members_i)){
        my_members_i[setdiff(names(my_members), names(my_members_i))] <- NA
        my_members[setdiff(names(my_members_i), names(my_members))] <- NA
        my_members <- rbind(my_members,my_members_i)
      }
    }
  }
  my_members <- my_members %>% filter(!is.na(id)) %>% unique(.)
  my_members
}


trello_retrieve_data <- function(my_token){
  
 # text_file <- "trello_secret.txt"
#  my_token <- trello_token(text_file)
  output <-vector(mode = "list", length = 0)
  

  my_boards<-get_my_boards(my_token)
  
  output$boards <- my_boards
  output$cards <- get_all_cards(my_boards,my_token)
  output$lists <- get_all_lists(my_boards,my_token)
  output$checklists <- get_all_checklists(my_boards,my_token)
  output$comments<- get_all_comments(my_boards,my_token)
  output$labels <- get_all_labels(my_boards,my_token)
  output$members <- get_all_members(my_boards,my_token)
  
  rm(my_boards,my_token)
  output
}

trello_normalise <-function(trello,programme_board,tasks_states_file){
  
  output <-vector(mode = "list", length = 0)
  
  
  #assignees
  n <- nrow(trello$cards)
  assignees <- tibble(card_id=character(),member_id=character())
  
  for (i in 1:n) {
    assignee_i <- trello$cards[i,]  %>%
      mutate(members = toString(idMembers)) %>%
      select(card_id=id,member_id=members)
    
    assignees <- rbind(assignees,assignee_i)
  }
  
  rm(assignee_i,i,n)  
  
  assignees <- assignees %>% mutate(member_id = gsub(x = member_id, pattern = "character\\(0)", replacement = "unassigned")) %>%
    mutate(member_id = gsub(x = member_id, pattern = 'c\\(\\"', replacement = "")) %>%
    mutate(member_id = gsub(x = member_id, pattern = '\\", \\"', replacement = ",")) %>%
    mutate(member_id = gsub(x = member_id, pattern = '\\")', replacement = ""))  %>%
    separate_rows(member_id, sep = ",") %>% 
    left_join((trello$members %>% select(member_id=id,username,fullName)),by="member_id")
  
  
  #labels
  n <- nrow(trello$cards)
  labels <- tibble(card_id=character(),member_id=character())
  
  for (i in 1:n) {
    label_i <- trello$cards[i,]  %>%
      mutate(labels = toString(labels)) %>%
      select(card_id=id,labels)
    
    labels <- rbind(labels,label_i)
  }
  
  rm(label_i,i,n) 
  
  labels <- labels %>% separate_rows(labels, sep = ", id") %>% 
    filter(!grepl("Board =",labels)) %>%
    mutate(labels = gsub(x = labels, pattern = "list\\()", replacement = "none")) %>%
    mutate(labels = gsub(x = labels, pattern = "list\\(id =", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = 'c\\(\\"', replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = '\\", \\"', replacement = ",")) %>%
    mutate(labels = gsub(x = labels, pattern = '\\")', replacement = ""))  %>%
    mutate(labels = gsub(x = labels, pattern = '\\"', replacement = ""))  %>%
    mutate(labels = gsub(x = labels, pattern = ' ', replacement = ""))  %>%
    separate_rows(labels, sep = ",")   %>%
    left_join((trello$labels %>% select(labels=id,name)%>% unique(.)),by="labels")
  
  #collated_cards
  
  collated_cards<- trello$cards %>% left_join((trello$boards %>% 
                                                 transmute(idBoard=id,
                                                           Project_Name=name,
                                                           Project_URL=url)) ,
                                              by="idBoard") %>%
    left_join((trello$lists %>% 
                 transmute(idList=id,
                           List_Name=name,)),
              by="idList")%>%
    select(-labels) %>%
    left_join((labels %>% 
                 select(card_id,name) %>% 
                 group_by(card_id) %>%
                 summarise(labels=toString(name)) %>%
                 ungroup()  %>% select(id=card_id,labels)),
              by="id") %>%
    left_join((assignees %>% select(card_id,username) %>%
                 group_by(card_id) %>%
                 summarise(assignee=toString(username)) %>% 
                 ungroup() %>%
                 select(id=card_id,assignee)),by="id") %>%
    select(id,name,dateLastActivity,desc,due,idBoard,isTemplate,shortUrl,Project_Name,List_Name,labels,assignee)
  
  
  #projects
  
  projects <- collated_cards %>% filter(Project_Name %in% programme_board) %>%
    select(Name=name,State=List_Name,labels,assignee,desc,due) %>%
    left_join((trello$boards %>%
                 left_join( (rbind((collated_cards %>%
                                      filter(List_Name=="Project Details" & isTemplate==FALSE & !(name %in% c("Project Lead","Project Manager")))  %>%
                                      select(idBoard,name,desc)),
                                   collated_cards %>%
                                     filter(List_Name=="Project Details" & isTemplate==FALSE & name %in% c("Project Lead","Project Manager"))  %>%
                                     select(idBoard,name,desc=assignee)) %>%
                               pivot_wider(id_cols=idBoard,names_from=name, values_from=desc) %>%
                               mutate(id=idBoard)),
                            by="id") %>%
                 select(id=idBoard,Name=name,url,Scope,Objectives,"Project Lead","Project Manager",Parameters,card_id=id) %>%
                 filter(!(Name %in% programme_board))),by="Name") %>%
    mutate("Project Lead"=if_else(is.na(.$"Project Lead"),assignee,.$"Project Lead"),
           Scope=if_else(is.na(Scope),desc,Scope)) %>%
    mutate(due=as_date(due)) %>%
    select(-assignee,-desc) %>%
    left_join((collated_cards %>% 
                 filter(List_Name %in% c("Project Details") & name %in% c("Project Updates")) %>%
                 select(id=idBoard,updates_id=id)),
              by="id")
  
  #issues
  
  issues<-collated_cards %>% filter(List_Name=="Issues" & isTemplate==FALSE) %>% separate_rows(desc,sep = "\n") %>%
    mutate(val=if_else(grepl("Issue",desc),"Issue","dummy_column")) %>%
    mutate(val=if_else(grepl("Impact",desc),"Impact",val)) %>%
    mutate(val=if_else(grepl("Action",desc),"Action",val)) %>%
    pivot_wider(names_from=val, values_from=desc) %>%
    mutate(Issue = gsub(x = Issue, pattern = "Issue:", replacement = ""),
           Impact = gsub(x = Impact, pattern = "Impact:", replacement = ""),
           Action = gsub(x = Action, pattern = "Action:", replacement = "")) %>%
    mutate(due=as_date(due)) %>%
    select(Severity=labels,Project=Project_Name,Title=name,due,Issue,Impact,Action,Assignee=assignee,id,Project_id=idBoard)
  
  
  #meetings
  
  meetings <- collated_cards %>% 
    filter(List_Name=="Meeting Notes" & isTemplate==FALSE) %>%
    mutate(dateLastActivity=as_date(dateLastActivity)) %>%
    select(Title=name,type=labels,Project_Name,attendees=assignee,summary=desc,updated=dateLastActivity,Project_id=idBoard,id)
  
  #tasks
  
  tasks_states <- read_csv(tasks_states_file)
  
  tasks <- collated_cards %>% filter(!(id %in% meetings$id) & 
                                       !(id %in% issues$id) & 
                                       !(id %in% projects$card_id) &
                                       isTemplate==FALSE &
                                       !(Project_Name %in% programme_board) &
                                       !(List_Name %in% c("Project Details"))) %>%
    left_join((tasks_states %>% select(List_Name=Original,State=Normalised)),by="List_Name") %>%
    mutate(labels = gsub(x = labels, pattern = "NA", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = ",", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = "green", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = "amber", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = "red", replacement = "")) %>%
    mutate(labels = gsub(x = labels, pattern = " ", replacement = "")) %>%
    mutate(labels = if_else(nchar(labels)==0,"ungrouped",labels)) %>% mutate(due=as_date(due),dateLastActivity=as_date(dateLastActivity)) %>%
    select(Task=name,due,State,Group=labels,Project_Name,assignee,id,Project_id=idBoard,url=shortUrl,dateLastActivity)
  
  
  rm(tasks_states)
  
  #comments
  
  comments <- trello$comments %>% mutate(date=as_date(date)) %>%
    select(id,date,comment=data.text,
           author=memberCreator.username,
           card_id=data.card.id)
  
  #actions
  
  chk_list_temp <-trello$checklists %>% mutate(name_cklst=name) %>%
    select(-id,-name,-pos) %>% unnest(checkItems) %>% select(id,name) %>%
    separate_rows(name,sep = "--") %>%
    mutate(val=if_else(grepl("Due",name),"due","action")) %>%
    mutate(val=if_else(grepl("Resp",name),"assignee",val)) %>%
    pivot_wider(names_from=val, values_from=name) %>%
    mutate(assignee = gsub(x = assignee, pattern = "Resp:", replacement = ""),
           due = gsub(x = due, pattern = "Due:", replacement = "")) %>%
    mutate(due=as_date(due)) 
  
  actions <- trello$checklists %>% mutate(name_cklst=name) %>%
    select(-id,-name,-pos) %>% unnest(checkItems) %>% select(-due) %>%
    left_join(chk_list_temp,by="id") %>%
    select(type=name_cklst,action,assignee,state,due, id,Project_id=idBoard,Task_id=idCard)
  
  rm(chk_list_temp)
  
  
  #output
  output$projects <- projects
  output$tasks <- tasks
  output$comments <- comments
  output$issues <- issues
  output$meetings <- meetings
  output$actions <- actions

  output
}

##code to run

my_token <- trello_token(trello_key)
trello<-trello_retrieve_data(my_token)
normalised_data <- trello_normalise(trello,programme_board,tasks_states)
rm(my_token,trello)
rm(programme_board,tasks_states,trello_key)
  
  
  
 