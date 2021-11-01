
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
install.packages("drat", repos = "https://mpn.metworx.com/snapshots/stable/2020-03-24")

## Compile a list of all tarballs downloaded during the pkgr install process.
toAdd1 <- list.files("tfl-pkgr-cache/MRAN-3f9ff60f0b67/src/", full.names = T)
toAdd2 <- list.files("tfl-pkgr-cache/S3TFLCRAN-e6449f9a5a44/src/", full.names = T)
toAdd3 <- list.files("tfl-pkgr-cache/TFL-c807e427397b/src/", full.names=T)
toAdd <- c(toAdd1, toAdd2, toAdd3)

# Set the repo path:
repo_path <- "/usr/local/repos/tfl"

## Add the packages from pkgr's cache to the local repository.
## This repository will be accessible by RSConnect.
# drat::insertPackages(toAdd, repodir=repo_path) # Not compatible with drat versions that are compatible with R3.5.3
for(pkg in toAdd) {
  print(paste0("adding package ", pkg, " to local repo."))
  drat::insertPackage(pkg, repodir=repo_path)
}

## UGH
## Add just a few deps that RSConnect forces us to need so that we can get on with our lives.
## 
# toAddRscDeps <- list.files("rsconnect-pkgr-cache/MPN-3bd99e356e31/src", full.names=T)
toAddRscDeps <- c(
  "rsconnect-pkgr-cache/MPN-3bd99e356e31/src/curl_4.3.2.tar.gz",
  "rsconnect-pkgr-cache/MPN-3bd99e356e31/src/jsonlite_1.7.2.tar.gz",
  "rsconnect-pkgr-cache/MPN-3bd99e356e31/src/yaml_2.2.1.tar.gz"
)
for(pkg in toAddRscDeps) {
  print(paste0("adding package ", pkg, " to local repo."))
  drat::insertPackage(pkg, repodir=repo_path)
}
