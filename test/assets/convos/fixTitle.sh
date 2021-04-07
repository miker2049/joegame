#! /bin/bash

for file in *; do 
    num=$(jq length $file)
    # echo $num
    if [ $num -eq 1 ]; then 
        echo "$file" 
        mv $file ./test
    fi 
done
