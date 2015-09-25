# Make file for analysis ----------------------------------------------------
# dependencies:
# on first run:
## create a folder "data" via system command
# system("mkdir data")
## create a txt file with the username and password for HMD on first run
# system("echo 'HMDusername HMDpassword' > PATH/HMDcred.txt")

# run the source code
source("scr/01-init.R")
source("scr/02-input.R")
source("scr/03-fnct.R")
source("scr/04-transform.R")
source("scr/05-present.R")
