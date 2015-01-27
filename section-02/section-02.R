
## ----lastTime1, echo=TRUE, eval=FALSE------------------------------------
## setwd("FirstDir/SecondDir/ThirdDir")


## ----lastTime2, echo=TRUE, eval=FALSE------------------------------------
## read.dta("DataFolder/auto.dta")


## ----gitConfig, echo=TRUE, eval=FALSE, highlight=FALSE-------------------
## $ git config --global user.name "Your Name"
## $ git config --global user.name are212ers@example.com


## ----gitConfig2, echo=TRUE, eval=FALSE, highlight=FALSE------------------
## $ git config --global core.editor emacs


## ----gitConfig4, echo=TRUE, eval=FALSE, highlight=FALSE------------------
## $ git config --global core.editor "'C:/Program Files (x86)/Notepad++/notepad++.exe'
##     -multiInst -notabbar -nosession -noPlugin"


## ----gitNav, echo=TRUE, eval=FALSE, highlight=FALSE----------------------
## $ cd Folder1/Folder2/Folder3


## ----gitInit, echo=TRUE, eval=FALSE, highlight=FALSE---------------------
## $ git init


## ----gitClone, echo=TRUE, eval=FALSE, highlight=FALSE--------------------
## $ git clone https://github.com/<yourGithubUsername>/Section2 yourFolderName


## ----ls, echo=TRUE, eval=FALSE, highlight=FALSE--------------------------
## LICENSE  List of names.txt


## ----gitStatus, echo=TRUE, eval=FALSE, highlight=FALSE-------------------
## $ git status
## $ git log


## ----gitStatus2, echo=TRUE, eval=FALSE, highlight=FALSE------------------
## $ git log --pretty=oneline --abbrev-commit


## ----gitAdd, echo=TRUE, eval=FALSE, highlight=FALSE----------------------
## $ git add List\ of\ names.txt


## ----gitCommit, echo=TRUE, eval=FALSE, highlight=FALSE-------------------
## $ git commit -m "Your commit message here"


## ----gitAdd2, echo=TRUE, eval=FALSE, highlight=FALSE---------------------
## $ git add yourFileName.txt
## $ git commit -m "Adding a new file"


## ----gitPush, echo=TRUE, eval=FALSE, highlight=FALSE---------------------
## $ git push origin master


## ----gitPull, echo=TRUE, eval=FALSE, highlight=FALSE---------------------
## $ git pull origin master


## ----gitLogCo, echo=TRUE, eval=FALSE, highlight=FALSE--------------------
## $ git log --oneline


## ----gitCo, echo=TRUE, eval=FALSE, highlight=FALSE-----------------------
## $ git checkout 310e9ce


## ----gitBranch, echo=TRUE, eval=FALSE, highlight=FALSE-------------------
## $ git branch testing


## ----gitMerge, echo=TRUE, eval=FALSE, highlight=FALSE--------------------
## $ git checkout master
## $ git merge master testing


