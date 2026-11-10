# audioBlastAnalyse
Backend analysis for [audioBlast!](https://audioblast.org).

## System notes
The system works correctly with the RMariaDB R package compiled using libmariadb-dev on recent Ubuntu platforms. Errors may/will occur when using libmysql-dev (these can be resolved by using the legacy workaround: calling analyse() with the parameter `db_legacy=TRUE`).

## Installation
````R
library(devtools)
install_github("audioblast/audioBlastAnalyse")
````
## Download limits
Some sources limit how much one address may download. iNaturalist says that more than 5 GB of
media an hour, or 24 GB a day, may get the downloader blocked for good. In web mode an agent keeps
iNaturalist's downloads to 1 GB an hour and 20 GB a day, and waits before claiming work while
either is reached. What was downloaded is written down per source in
`tools::R_user_dir("audioBlastAnalyse", "data")`, so every agent a user runs on one machine
shares the same allowance, and a restarted agent does not start a fresh one. Limits for other
sources, or changed ones, are set before calling `analyse()`:

````R
options(audioBlastAnalyse.limits = list(iNaturalist = c(hour = 1e9, day = 20e9),
                                        TSA = c(hour = 2e9, day = 40e9)))
````

Where a recording from a source with limits cannot be downloaded after three attempts, the
agent asks the source, with HEAD, for the last recording it did download. If that is refused
too, the source is refusing the agent: the tasks are given back and the agent waits (5 minutes,
doubling, to 2 hours) rather than recording the recording as missing.

## Credits
Initial development of audioBlast was supported by the Leverhulme Trust funded [Automated Acoustic Observatories](https://ebaker.me.uk/aao) project at the University of York.

The project is currently hosted by the [Natural History Museum, London](https://www.nhm.ac.uk) and supported in part by the Urban Nature Project.
