#!/bin/bash

git status

git diff HEAD --  . ':(exclude)changes.log' > "changes.log"
