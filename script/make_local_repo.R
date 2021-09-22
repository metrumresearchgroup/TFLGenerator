
# Prerequisites -- do in Terminal
## 1. You must run `pkgr install` before using this script.
## 2. You must create a directory at `/usr/local/repos/tfl` on your workflow and give global read/write permissions.
### sudo mkdir /usr/local/repos
### sudo mkdir /usr/local/repos/tfl
### sudo chmod 777 /usr/local/repos/tfl
## 3. I think this is optional, but make an "index.html" file in your repo as well.
### echo '<!doctype html><title>empty</title>' > /usr/local/repos/tfl/index.html

# Script

## We need drat to be installed, which is not part of TFL's dependencies, so we're doing it separately.
## You need a fairly recent (as of 07-2021) version of drat for this. I did it with 0.2.1. This MPN Snapshot has 0.2.0, which is probably fine?
install.packages("drat", repos = "https://mpn.metworx.com/snapshots/stable/2021-06-20")

## Compile a list of all tarballs downloaded during the pkgr install process.
toAdd1 <- list.files("tfl-pkgr-cache/MRAN-3f9ff60f0b67/src/", full.names = T)
toAdd2 <- list.files("tfl-pkgr-cache/TFLCRAN-e6449f9a5a44/src/", full.names = T)
toAdd <- c(toAdd1, toAdd2)

# Set the repo path:
repo_path <- "/usr/local/repos/tfl"

## Add the packages from pkgr's cache to the local repository.
## This repository will be accessible by RSConnect.
drat::insertPackages(toAdd, repodir=repo_path)
