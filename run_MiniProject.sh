#!/usr/bin/bash
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "07 Feb 2018"
# Desc: shell script to run miniproject files and compile LaTeX script




################################################################
# Python
################################################################

echo ~~~~~Running Python Cleaning~~~~~

# run python file
python3 adj_matrix.py '../Data/MegaDataBase-v137-201311-201611-FY-Dominance_Lundy_20170512_AST.csv' '../Data/birdsex.1_validcolourcodes_AST.csv' '../Results/adj_data.csv'



################################################################
# R
################################################################

echo ~~~~~Running R Analysis ~~This may take a few...~~~~~~

# Calls R script
Rscript bird_networks01.R

################################################################
# LaTeX
################################################################

echo ~~~~~Creating LaTeX PDF~~~~~~

bash CompileLaTeX.sh

################################################################
# Move file
################################################################

mv miniproject.pdf ../Results/miniproject.pdf

echo ~~~~~Done! Yay!~~~~~~
echo - PDF in Results
