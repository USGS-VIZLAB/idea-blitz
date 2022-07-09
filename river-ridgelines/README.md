# river-ridgelines

To run this repo, change your working directory to `river-ridgelines`:
```r
setwd('river-ridgelines')
```

### Set up ScienceBase credentials 

In order to download the data from ScienceBase, you will need to first set up local credentials by running the following chunk of code with your own ScienceBase username. You will be prompted for a password (this should be your AD password if you are a USGS employee) in a separate dialogue box. This only needs to be run once for as long as your username and password are valid. Re-run when you need to update your password.

```r
library(secret);library(dplyr)
source("src/authentication_helpers.R")
set_up_auth("your_email@usgs.gov")
```

