#!/bin/bash
find . -name "*.el" -not -path "./elpa/*" | while read file; do
  echo "Processing $file"
  # Remove copyright sections
  sed -i '' '/;; Copyright/,/;; URL:/d' "$file"
  # Remove empty comment lines
  sed -i '' '/^;;$/d' "$file"
  # Remove "This file is not part of GNU Emacs" lines
  sed -i '' '/;; This file is not part of GNU Emacs/d' "$file"
done
echo "Cleanup completed: removed copyright sections, empty comments, and 'not part of GNU Emacs' lines"
