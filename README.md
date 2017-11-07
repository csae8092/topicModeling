[![DOI](https://zenodo.org/badge/42352047.svg)](https://zenodo.org/badge/latestdoi/42352047)

# topicModeling
## About
This repository contains some lines of code for running topic modeling over a bunch of documents and to visualize the results.

## requirements
This topic modeling show case relies on python (ideally with iypthon notebook) and R (ideally RStudio)
TODO - provide a requirments.txt for creating a virtual environment.

## HowTo
1. you have to run the ipython script `getXMLfromThunRegExCleaned.ipynb`. To do so, open a command prompt, change directory to `{root}/python and enter the command `ipython notebook`. This should open your standard browser. Here select `getXMLfromThunRegExCleaned.ipynb` and execute it cell by cell
2. After you downloaded all the data (check `{root}/data/`) open the R script `{root/R/TopicModel_txt.R`and execute it.

## Known Issues
The current R script run on the current dataset stops with the error message `Error in topic.words.m[i, ] : subscript out of bounds`. But  you can continue the script manually and it will produce valid results. 
