#!/bin/bash

git status -u  https://github.com/singhn18/CS1XA3

git diff HEAD -- . ':(exclude)CS1XA3/changes.log'> "changes.log"
