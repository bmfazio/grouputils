# grouputils

```r
devtools::install_github("bmfazio/grouputils")
```

See `?groupmake` for more info. Example:

```r
library(grouputils)
set.seed(6)
data <- sim_data(8, 13, 0.2)
groupmake(data, 2)
```
