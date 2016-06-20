#!/bin/bash
## CONDUIT: checking language version
## for bash no actual checking is done, but the current version is returned
outfile=".languageVersion"
echo "$BASH_VERSION" > $outfile
counter=0
while [ $counter -lt 3 ];
do
    echo "0" >> $outfile
    let counter=counter+1
done

