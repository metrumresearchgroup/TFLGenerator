#!/bin/bash

# Run as root
service nginx stop
apt-get autoremove nginx
echo "deb http://nginx.org/packages/ubuntu/ precise nginx" >> /etc/apt/sources.list
echo "deb-src http://nginx.org/packages/ubuntu/ precise nginx" >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys $key
apt-get update
apt-get install nginx
cp nginx/nginx.conf /etc/nginx
cp nginx/metworx /etc/nginx/sites-available
service nginx restart
