#!/bin/bash

DIR=/mnt/usb/TODO/complete
TARGET_NON_STANDARD_DIR=/mnt/usb/TODO
TARGET_RENAMED_DIR=/mnt/usb/music/albums

# The tricky way to get find's output into an array
unset files i
while IFS= read -r -d $'\0' file; do
  files[i++]="$file"
  ALL=$(basename "$file")

  YEAR=$(echo $ALL | grep -o '[1-2][0-9][0-9][0-9]\+')
  LABEL=$(echo $ALL | grep -Po '(?<=\().*(?=\))')
  ARTIST=$(echo $ALL | awk '{split($0,a," - "); print a[1]}')
  ALBUM=$(echo $ALL | awk '{split($0,a," - "); print a[3]}' | sed s/' ([^)]*)'/''/g)

  LABEL_LOW=$(echo $LABEL | awk '{print tolower($0)}')
  ARTIST_LOW=$(echo $ARTIST | awk '{print tolower($0)}')
  ALBUM_LOW=$(echo $ALBUM | awk '{print tolower($0)}')

  if [ -z "$YEAR" ] | [ -z "$LABEL" ] | [ -z "$ARTIST" ] | [ -z "$ALBUM" ]; then
    printf "ATTENTION! Found non-standard naming: $file --> MOVING\n"
    mv "$file" "$TARGET_NON_STANDARD_DIR"
  else
    NEW_NAME="$YEAR + $ARTIST_LOW + $ALBUM_LOW [$LABEL_LOW]"
    mv "$file" "$TARGET_RENAMED_DIR/$NEW_NAME"
  fi
  
done < <(find $DIR -maxdepth 1 -type d ! -path $DIR -type d -print0)
