#!/bin/bash

git status -uno

git diff HEAD > "changes.log"
