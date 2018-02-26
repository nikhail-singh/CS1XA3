This readme details the functions of ProjectAnalyze.sh
1. checks to see if the local repository(on machine) and the remote repository (on GitHub) are in sync
   if not it tells you how many commits behind you are.
2. puts all uncommitted changes into a file called changes.log by redirecting output of git diff to that file
3. Checks every file in the repository excluding todo.log, ProjectAnalyze.sh and changes.log for #TODO and
   adds all lines with that tag into todo.log
4. checks every haskell file for errors and puts all errors into a file called error.log
