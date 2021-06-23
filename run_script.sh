#! /bin/bash

set -e
set -o pipefail


# temp fixes for v19
Rscript -e "rmarkdown::render('temp_fix_V19.Rmd')"

# QC hgg_dmg
Rscript -e "rmarkdown::render('QC_histology_20210126.Rmd',\
  params=list( latest_histology = '20210126-data/ADAPT-base/pbta-histologies-base.tsv', \
  prev_histology = '20201215-data/pbta-histologies.tsv', \
  add_ids = 'BS_KKDTW11T,BS_X1TRW9RH,BS_46MV2DSY,BS_00FD2KMP,BS_K24D4BGK,BS_VF1R7VC2', \
  output = '20210126-data'), \
  output_file = 'hgg_dmg_release.pdf')"


# QC OpenPBTA v19 
Rscript -e "rmarkdown::render('QC_histology_20210126.Rmd',\
  params=list( latest_histology = '20210126-data/ADAPT-base/pbta-histologies-base.tsv', \
  prev_histology = 'release-v18-20201123/pbta-histologies-base.tsv', \
  output = 'release-v19-20210126',\
  remove_ids = 'BS_JXF8A2A6'),\
  output_file = 'OpnePBTA_v19_release.pdf')"

# review fixes for v19
Rscript -e "rmarkdown::render('review_updates.Rmd')"


