##########
# Shell script to remove ancillary LaTeX output files following GreenBook compile
# This is so they won't be pushed to GitHub with every compile
# Christopher Gandrud
# Updated 9 July 2012
##########

# Command to run the script: 
# git_repositories/GreenBook/OtherFiles
# ./LaTeXrm.sh

#!/bin/bash

cd /git_repositories/GreenBook/Paper

rm main_GreenBook_paper-concordance.tex main_GreenBook_paper.bbl main_GreenBook_paper.log main_GreenBook_paper.synctex.gz

cd /git_repositories/GreenBook/

echo "Finished"