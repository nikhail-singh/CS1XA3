#!/bin/bash

git status -u

git diff HEAD > "changes.log"
