#!/bin/bash
LASTFILE=`ls /var/log/shiny-server | tail -1` 
tail -f -n 100 /var/log/shiny-server/"$LASTFILE"
