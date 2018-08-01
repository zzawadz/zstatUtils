cd $1
echo $1
setsid sbt pack > r-sbt.log 2>&1
