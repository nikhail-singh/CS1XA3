This readme details the functions of ProjectAnalyze.sh

REQUIRED functions
1. checks to see if the local repository(on machine) and the remote repository (on GitHub) are in sync
   if not it tells you how many commits behind you are.
2. puts all uncommitted changes into a file called changes.log by redirecting output of git diff to that file
3. Checks every file in the repository excluding log files and ProjectAnalyze.sh for #TODO and
   adds all lines with that tag into todo.log
4. First goes through every haskell file and adds a main = undefined line to it if it does not already have a
   main specified. Then checks every haskell file for errors and puts all errors into a file called error.log

CUSTOM functions
1. Shows the human readable file size of every subdirectory in the repository (total is whats in the CS1XA3 directory
   directly)
2. Using the optional parameter "-ct" will remove any file beginning with tmp. Useful for clearing clutter in the 
   repository
3. Checks all python files for syntax errors and puts those errors into a file called PythonErrors.log
4. Using the optional parameter "-s" will search for a specific line of code and add it to search.log
5. Using the optional parameter "help" will pull up this readme for reference

CREDITS:
Required functions
4. I modified code from https://github.com/Rubinstd specifically to add the main = undefined to haskell files and to
   clean up the output of the command (moving standard output to /dev/null)

Custom functions
3. I used a command that I found here: 
   https://stackoverflow.com/questions/4284313/how-can-i-check-the-syntax-of-python-script-without-executing-it
