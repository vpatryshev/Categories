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
sbt package || { say "oops... package failed" && exit 1 }
sbt test
res=$?
# echo "Tests returned <<$res>>"
[[ $res -eq 0 ]] && say "Marivanna.... ya gotova" || say "oy! bleen... oops... tests failed"
