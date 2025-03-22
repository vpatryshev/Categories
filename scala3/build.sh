#!/bin/zsh

clear && printf '\e[3J'
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk use java 24.ea.17-open
#sdk use java #11.0.21-ms
#sbt evicted
rm -rf target
rm -rf project/target
sbt clean 
sbt compile || { say "oy! bleen... oops... build failed" && exit 1 }

sbt test package | tee build.log
res=$?
echo "Tests returned <<$res>>"
[ -z $res ] && say "Marivanna.... ya gotova" || say "oy! bleen... oops... tests failed"
