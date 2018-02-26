#!/bin/bash

git status

git diff --minimal  HEAD -- . ':(exclude)changes.log' > "changes.log"

grep -r --exclude={todo.log,changes.log,ProjectAnalyze.sh} "#TODO" > "todo.log"

find . -name '*.hs' -print0 | 
	while IFS= read -r -d $'\0' file 
	do 
		hasmain=$(grep "main" "$file" | wc -l) 
		if [ $hasmain -lt 1 ]
		then 
			echo "main = undefined" >> $file
		fi
	done
find . -name "*.hs" -print0 -exec ghc -fno-code 2> error.log {} \;
