#!/bin/sh
#  https://github.com/rocker-org/shiny/blob/master/shiny-server.sh
# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
then
    # push the "real" application logs to stdout with xtail in detached mode
    exec xtail /var/log/shiny-server/ &
fi

# start shiny server
exec nginx  &
exec R -e "source('/opt/plumber/api_server.R')" &
exec shiny-server 2>&1