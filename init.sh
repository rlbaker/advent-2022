#! /usr/bin/env sh
touch "data/$1.example"
touch "data/$1.input"
touch "src/$1.clj"

echo "(ns $1)" > "src/$1.clj"
