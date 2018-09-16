#!/bin/bash

#
# Download data vai Kaggle API instruction
#


usr = dp
readonly usr


# 1. install kaggle package (if necessary)
pip install --user $usr kaggle

# 2. upload kaggle.json to ~/.kaggle/kaggle.json (if necessary)

# 3. download competitions data
kaggle competitions download -c home-credit-default-risk -p /home/$usr/apps/HomeCreditDefaultRisk/data


# References ----
# 1. https://github.com/Kaggle/kaggle-api