#!/bin/sh
X=10
while [ $X -gt 0 ]
do
    dialog  --backtitle "Wait for it ..." --infobox $X 0 0 
    X=$((X - 1))
    sleep 1
done
dialog --infobox  "   BANG!" 0 0

