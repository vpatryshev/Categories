#!/bin/zsh

clear && printf '\e[3J'
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk use java 24.ea.17-open
rm -rf target
rm -rf project/target
sbt clean test
res=$?
# echo "Tests returned <<$res>>"
[[ $res -eq 0 ]] && say "Marivanna.... ya gotova" || say "oy! bleen... oops... tests failed"
exit $res
