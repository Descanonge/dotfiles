#!/bin/bash

# Dump header of netCDF file in a zenity window

file=$1

ncdump -h $file | zenity --text-info
