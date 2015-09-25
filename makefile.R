### Make file for analysis ----------------------------------------------------
# dependencies:
# create a folder "data" via system command
system("mkdir data")
# create a txt file with the username and password for HMD
system("echo 'jabarthold@health.sdu.dk 1431510742' > /Users/jabarthold/Dropbox/Projects/014_sociality/e0lxequal/data/HMDcred.txt")

# run the source code
source("scr/01-init.R")
source("scr/02-input.R")
source("scr/03-fnct.R")
source("scr/04-transform.R")
source("scr/05-present.R")
