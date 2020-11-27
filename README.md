# adventure_scientists

_Noah Giebink_

Do Adventure Scientists data improve SDMs?


## [Data aquisition script](https://github.com/nwgiebink/adventure_scientists/blob/master/scripts/adventure_scientists_data.R)
./scripts/adventure_scientists_data.R 

## [Species distribution modeling script](https://github.com/nwgiebink/adventure_scientists/blob/master/scripts/adventure_scientists_sdms.R)
./scripts/adventure_scientists_sdms.R 

We compare performance of models trained on different data sources:

1. iNaturalist data alone
2. iNaturalist + Adventure Scientists data

Key packages and methods:

[BlockCV](https://github.com/rvalavi/blockCV) Spatially explicit k-fold cross validation

[ENMevaluate](https://www.rdocumentation.org/packages/ENMeval/versions/0.3.1/topics/ENMevaluate%20) Hyperparameter tuning

[Maxnet](https://www.rdocumentation.org/packages/maxnet/versions/0.1.2) Open source alternative to "Maxent"


## [Data visualization script](https://github.com/nwgiebink/adventure_scientists/blob/master/scripts/figures.R)
./scripts/figures.R

[hillShade](https://www.rdocumentation.org/packages/raster/versions/3.3-7/topics/hillShade) integrate aspect and slope rasters for topographical shading effect
