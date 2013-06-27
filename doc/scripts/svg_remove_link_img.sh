#!/bin/bash

# Strip linked images from UML type SVG files.

FILES=*.svg

for f in $FILES
do
	echo Processing $f
	inkscape --file=$f --export-area-drawing --without-gui --export-plain-svg=$f
	sed -i '/<clipPath/,/<\/clipPath>/d' $f
	sed -i '/<image/,/\/>/d' $f
done
