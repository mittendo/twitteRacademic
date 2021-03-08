
# Package *twitteRacademic*

The package *twitteRacademic* is intended for academics who have an academic account on *Twitter*.
This package enables the integration of the Academic Research product track of Twitter, which provides free access to the full-archive search and other v2 endpoints. It provides a feature for circumstantial use of the *full-archive search* call, which is used to access the historical API. For suggestions, questions, ideas, improvements or reporting bugs, please contact the authors of the package. 

More information on *twitteRacademic* is available on the project homepage.        





#  Installation Requirements

To be able to use the package in principle, some requirements are to be fulfilled, which are only mentioned here. For an implementation of these please visit the homepages of the infrastructure used here.  What you need to have basically set up to use the *twitteRacademic* package:   

1. It is necessary that you register as a developer at [*Twitter Developer*](https://developer.twitter.com).
2. You also need to submit your research project to *Twitter* and get approved for use by proving your institutional affiliation, [here](https://developer.twitter.com/en/solutions/academic-research).
3. After successful activation you have to create an app on *Twitter Developer* and assign it to your [academic project](https://developer.twitter.com/en/portal/projects).
4. Make sure that in the app permissions your app has the "Read, write and access Direct Messages" rights at level 3.
5. Finally, it is necessary to generate a ['Bearer-Token'](https://developer.twitter.com/en/portal/projects) which will serve as a key for your access. 

For a detailed description of each step to implement an app in Twitter, please visit the pages of the links provided above.



## Installation (Development releases via the project repository)

The current/last version of the package can be obtained from the corresponding repository. To install the package directly from GitHub, you can use install_github() from the devtools package:

`library(remotes)`

`install_github("mittendo/twitteRacademic")`

## Getting started


```{r eval=FALSE}
# load package
library(twitteRacademic)

# Replace the bearer token below with your bearer key
bearer_token <- "Put Your Bearer-Token in here"

# OR

# Create .Rprofile data with Your Bearer-Token
twitterBearerToken <- "Put Your Bearer Token in here".# in .Rprofile = twitterBearerToken

```

```{r eval=FALSE, echo = TRUE}
# query the term
# Please check out the different options that Twitter provides for "high-performance" 
# research/queries.
query <- '#OCCUPYWALLSTREET' 
```





## Contributing

Some sample code has been taken from twitter development team (https://github.com/twitterdev/Twitter-API-v2-sample-code/blob/master/Full-Archive-Search/full-archive-search.r).

We welcome pull requests and any meaningful additions to this package.

## Bug reports

For suggestions, questions, ideas, improvements or reporting bugs, please contact the authors of the package [mittendo@uni-wuppertal.de; schmale@uni-wuppertal.de].



Please report any bugs to:

https://github.com/mittendo/twitteRacademic/issues


## Citation 

Please cite this package, if you have used it in your works: 

`citation("twitteRacademic")`

## License

GPL-3

