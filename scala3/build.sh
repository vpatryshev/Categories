#!/bin/zsh
clear && printf '\e[3J'
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk use java 24.ea.17-open
#sdk use java #11.0.21-ms
#sbt evicted
sbt clean test package && say "hey, build is ok" | say "oops, build failed" | tee build.log
