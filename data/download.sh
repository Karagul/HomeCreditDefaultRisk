#!/bin/bash


# install (if nessesary)
pip install kaggle

# download
chmod -R ugo+rw /home/<user>/apps/HomeCreditDefaultRisk/data
kaggle competitions download -c home-credit-default-risk -p /home/<user>/apps/HomeCreditDefaultRisk/data