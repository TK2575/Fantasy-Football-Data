docker run --name mysql-rff \
-e MYSQL_ROOT_PASSWORD= \
-p 3306:3306 \
-d mysql:latest

# connect using ip from ifconfig - inet addr