
soruce_system <- "Trello"
today<-Sys.Date()

Rdata_file <- "./files/normalised_data.rds"
auto_refresh <- dhours(3)

status_colours <- tibble(colour=c("NA","green","amber","red","grey","black"),
                         colour_short=c("NA","G","A","R","g","B"),
                         hex=c("purple","green","orange","red","#dedede","black"))

project_kanban_background <- tibble(State=c("Backlog","Planning","In Progress","Complete"),
                                    Task=c("To Do","Blocked","In Progress","Complete"),
                                    Action=c("dummy1","dummy2","Open","Closed"),
                                    Issue=c("dummy1","dummy2","Open","Closed"),
                                    hex=c("#EAE8FF","#D8D5DB","#a8edc9","#dedede"),
                                    hex2=c("#EAE8FF","#D8D5DB","#ADACB5","#2D3142"))


project_kanban_background <- project_kanban_background %>%
                              mutate(kanban_style=paste("background-color:",hex2,";",sep=""))

action_replace <- tibble(Original=c("complete","incomplete"),
                         Replace=c("Closed","Open"))

tasks_states <- tibble(Original=c("To Do",
                                  "Not Started",
                                  "In Progress",
                                  "Doing",
                                  "Complete",
                                  "Done",
                                  "Blocked",
                                  "On Hold"),
                       Normalised=c("To Do",
                                     "To Do",
                                     "In Progress",
                                     "In Progress",
                                     "Complete",
                                     "Complete",
                                     "Blocked",
                                     "Blocked"))

kanban_backlog <- "teal"
kanban_planning <- "violet"
kanban_progress <- "blue"
kanban_complete <- "grey"

t.default_colour <- "black"
c.background1 <- "white"
c.background2 <- "violet"
c.background3 <- "#f7fcff"
c.background4 <- "white"
wordcloud_palette <-"Dark2"

font_sizes <- tibble(nbr=c(7,6,5,4,3,2,1),
                     size=c("xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large"))
#####
programme_board <-c("Sample_Programme")

#tasks_states <- "tasks_states.csv"

trello_key <- "files/trello_secret.txt"

RAG_replace <- tibble(colour=c("red","amber","green"),
                      letter=c("R","A","G"))

####

horizon_span1 <- ddays(3)
horizon_span2 <- ddays(7)

c.U_rows <- "red"
c.M_rows <-  "yellow"
c.L_rows <-  "white"
c.X_rows <-  "purple"
####

update_date_limit_1 <- 6
update_date_limit_1 <- 10
task.R.threshold.1 <- 1
task.R.threshold.2  <- 3
task.A.threshold.1 <- 3
task.A.threshold.2 <- 5
issue.R.threshold.1 <- 1
issue.R.threshold.2 <- 3
issue.A.threshold.1 <- 3
issue.A.threshold.2 <- 5
action.R.threshold.1 <- 3
action.R.threshold.2 <- 5
action.A.threshold.1 <- 5 
action.A.threshold.2 <- 7