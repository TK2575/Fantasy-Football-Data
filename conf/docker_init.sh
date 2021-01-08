docker run --name mysql-rff5 \
-e MYSQL_ROOT_PASSWORD= \
-p 3306:3306 \
-d mysql:5

# connect using ip from ifconfig - inet addr

docker run -d \
-p 4445:4444 -p 5901:5900 \
--memory 4096mb --shm-size 4g \
selenium/standalone-firefox-debug:latest

docker run -d \
-v ~/repos/rFF_legacy:/rFF_legacy \
-p 8787:8787 \
-e ROOT=TRUE \
-e PASSWORD=somePassword \
rocker/tidyverse