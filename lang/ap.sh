#!/bin/bash 
for ((i=2;i<=$#;i++)); do
    printf "${!i} "
done
printf "$1"
printf "\n"