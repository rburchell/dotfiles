#!/bin/sh
rm -rf deploy
mkdir -p deploy/lib
mkdir -p deploy/plugins/platforms/
cp ~/code/qt/qt-blackberry-4.8/stage/lib/*.so.4 deploy/lib
cp -r ~/code/qt/qt-blackberry-4.8/stage/imports deploy
cp -r ~/code/qt/qt-blackberry-4.8/stage/themes deploy/themes
cp ~/code/qt/qt-blackberry-4.8/stage/plugins/platforms/*blackberry* deploy/plugins/platforms
blackberry-nativepackager -devMode -debugToken ~/.rim/debugtoken.bar -package helloworld.bar bar-descriptor.xml
