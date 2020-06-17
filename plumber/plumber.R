# plumber.R
library(tidyverse)

####What happens if file doesnt exist???? it will load ahead of shiny app
rds_location <- "/srv/shiny-server/pm_dashboard/files/"

rendered_rds <- paste(rds_location,"rendered_data.rds",sep="")

rendered_data_current <- readRDS(pins::pin(rendered_rds))

#* Kanban - Backlog - HTML version
#* @html
#* @get /kanban_backlog
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    
    
    shiny::isolate(rendered_data$render.kanban$project_list_backlog)
}

#* Echo back the input
#* @param msg The message to echo
#* @get /retrieved
function(){
    list(msg = shiny::isolate(rendered_data_current$data_retrieved))
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
    list(msg = paste0("The message is: '", msg, "'"))
}
 
#* Plot a histogram
#* @png
#* @get /plot
function(){
    rand <- rnorm(100)
    hist(rand)
}
 
#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
    as.numeric(a) + as.numeric(b)
}

