#!/bin/bash

git status

git diff HEAD --  . ':(exclude)changes.log' > "changes.log"

grep -r --exclude={todo.log,changes.log,ProjectAnalyze.sh} "#TODO" > "todo.log"

find . -name "*.hs" -print0 |
	while IFS=’’ read -r -d $’\0’ file
	do
		ghc -fno-code file.hs 2> error.log 
	done

