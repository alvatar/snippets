#!/bin/sh
X=9
tput clear
tput cup 12 0
banner "  10"
while [ $X -gt 0 ]
do
    sleep 1
    tput cup 12 0
    tput ed
    banner "   $X  "
    X=`expr $X - 1`
done
tput cup 12 0
banner "   BANG!"

