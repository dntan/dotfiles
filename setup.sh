./set#!/bin/bash
#
##################################################################################################################
# Written to be used on 64 bits computers
# Original Author 	: 	Erik Dubois
# Website 	: 	http://www.erikdubois.be
# Modified by: Danny Tan
##################################################################################################################
##################################################################################################################
#
#   DO NOT JUST RUN THIS. EXAMINE AND JUDGE. RUN AT YOUR OWN RISK.
#
##################################################################################################################

# Problem solving commands

# Read before using it.
# https://www.atlassian.com/git/tutorials/undoing-changes/git-reset
# git reset --hard orgin/master
# ONLY if you are very sure and no coworkers are on your github.

# Command that have helped in the past
# Force git to overwrite local files on pull - no merge
# git fetch all
# git push --set-upstream origin master
# git reset --hard orgin/master

project=$(basename `pwd`)
echo "-----------------------------------------------------------------------------"
echo "this is project https://github.com/dntan/"$project
echo "-----------------------------------------------------------------------------"
git config --global pull.rebase false
git config --global user.name "danny tan"
git config --global user.email "dn-tan@hotmail.com"
sudo git config --system core.editor nano
git config --global push.default simple

git remote set-url origin git@github.com:dntan/$project

echo "Everything set"

echo "################################################################"
echo "###################    T H E   E N D      ######################"
echo "################################################################"
