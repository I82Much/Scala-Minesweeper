#!/bin/bash
# Modified from http://stackoverflow.com/questions/809138/creating-a-jar-file-from-a-scala-file
if [ ! $SCALA_HOME ]
then
    echo ERROR: set a SCALA_HOME environment variable
    exit
fi

if [ ! -f scala-library.jar ]
then
    cp $SCALA_HOME/lib/scala-library.jar .
fi

if [ ! -d bin ]
then
    mkdir bin
fi

# Compile the Minesweeper class in src, putting the .class files in bin
fsc -sourcepath src -d bin src/Minesweeper.scala

# cd bin
# jar -cfm ../Mine.jar ../MANIFEST.MF *
# cd -
# 
# java -jar hellow.jar