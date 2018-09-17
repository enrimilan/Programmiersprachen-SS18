#!/bin/bash 
for i; do 
   printf "$i " > /dev/tty
done
printf "\n" > /dev/tty