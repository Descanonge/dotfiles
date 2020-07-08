#!/bin/sh

get=$1
if [ "$get" == "" ]; then
    get="cinnamon"
fi


# xfce
if [ "$get" == "xfce" ]; then
    file=$(xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/last-image)

# cinnamon
elif [ "$get" == "cinnamon" ]; then
    fileURI=$(gsettings get org.cinnamon.desktop.background picture-uri)
    file=${fileURI:6:-1}

else
    file="$get"
fi


sudo cp "$file" /usr/share/pixmaps/current-wallpaper.im
