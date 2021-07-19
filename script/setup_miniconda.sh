#!/bin/sh -x 
#the script must be run as root.  IF called not as root, calling itself as root
if [ "`id -u`" -ne 0 ]; then
 echo "Switching from `id -un` to root"
 exec sudo "$0"
 exit 99
fi

export MINICONDA_VERSION=py37_4.8.3

export PYTHON_VERSION=3.7.7

#install miniconda
curl --insecure -O https://repo.anaconda.com/miniconda/Miniconda3-${MINICONDA_VERSION}-Linux-x86_64.sh && \
    bash Miniconda3-${MINICONDA_VERSION}-Linux-x86_64.sh -bp /opt/python/${PYTHON_VERSION} && \
    /opt/python/${PYTHON_VERSION}/bin/pip  --trusted-host pypi.python.org  install virtualenv && \
    rm -rf Miniconda3-${MINICONDA_VERSION}-Linux-x86_64.sh


#enable python for rsconncet, if not enabled
grep -qF Python /etc/rstudio-connect/rstudio-connect.gcfg
rs="$?"
if [ "$rs" -ne 0 ]
then 
cat <<EOF >> /etc/rstudio-connect/rstudio-connect.gcfg
[Python]
Enabled = true
Executable = /opt/python/${PYTHON_VERSION}/bin/python
EOF
systemctl restart rstudio-connect 
fi 
