#!/bin/bash

wallpaper_dir=~/Pictures/wallpapers/
arr=($(ls $wallpaper_dir | shuf))
while true; do
	for i in "${arr[@]}"
	do
		feh --bg-fill "$wallpaper_dir$i"
		sleep 900
	done
done
