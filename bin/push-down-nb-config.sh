#!/bin/bash

for f in `find . -name pom.xml | grep -v '\./pom.xml' | sed 's/pom.xml//'`; do 
  #echo cp nb-configuration.xml $f
 cp nb-configuration.xml $f
done
