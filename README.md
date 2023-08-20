# audioBlastAnalyse
Backend analysis for [audioBlast!](https://audioblast.org).

## System notes
The system works correctly with the RMariaDB R package compiled using libmariadb-dev on recent Ubuntu platforms. Errors may/will occur when using libmysql-dev (these can be resolved by using the legacy workaround: calling analyse() with the parameter `db_legacy=TRUE`).

## Installation
````R
library(devtools)
install_github("audioblast/audioBlastIngest")
````
## Credits
Initial development of audioBlast was supported by the Leverhulme Trust funded [Automated Acoustic Observatories](https://ebaker.me.uk/aao) project at the University of York.

The project is currently hosted by the [Natural History Museum, London](https://www.nhm.ac.uk) and supported in part by the Urban Nature Project.
