##########
# Shell script to remove ancillary LaTeX output files following GreenBook compile
# This is so they won't be pushed to GitHub with every compile
# Christopher Gandrud
# Updated 9 July 2012
##########

# Command to run the script: 
# ./LaTeXrm.sh --verbose

#!/bin/bash

# Local variable of project title

PROJECT="GreenBook"

# Change to the paper's working directory
cd /git_repositories/$PROJECT/Paper

# Remove ancillary files
rm main_$PROJECT-concordance.tex main_$PROJECT.bbl main_$PROJECT.log main_$PROJECT.synctex.gz

# Change back to the root directory 
cd /git_repositories/$PROJECT/

echo "Finished"