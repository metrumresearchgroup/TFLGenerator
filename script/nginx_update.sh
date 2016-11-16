#!/bin/bash

# Run as root
service nginx stop
apt-get install -y python-software-properties
add-apt-repository ppa:nginx/stable
apt-get update
aptitude safe-upgrade -y nginx
service nginx restart
