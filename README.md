## v19 release assembly and QC

### Release notes
In release we are using v18 base histology to create a base histology for v19 release. "Base histology" file has the basic clinical information manifest that is required by subtyping modules to add in OpenPBTA subtyping information.

The v18 base histologies was generated in this script: [script](https://github.com/d3b-center/D3b-codes/blob/master/OpenPBTA_v18_release_QC/QC_histology_v18.Rmd).
We will be removing BS_JXF8A2A6 , the reason behind the removal is in  [issue](https://github.com/AlexsLemonade/OpenPBTA-analysis/issues/862)

CNS_region is also updated for 135 samples which were incorectly changed in v18.

20210126-data : hgg-dmg histology file release
release-v19-20210126 : OpenPBTA histology file release
