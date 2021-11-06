#!/bin/sh

exec git push --tags ${1:-origin} HEAD:master

