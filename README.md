
# A Random Item Response Model of External Territorial Threat, 1816-2010

Multiple scholars have shown that external territorial threat,
conceptually the level of concern for a state that its territorial
integrity is subject to violent conflict and imposed contraction by
other states, has major implications for the state’s domestic political
environment. However, the strand of scholarship that agrees on the
domestic political effects of external territorial threat disagrees on
how to code this important concept. These works either rely on binary
indicators that do a poor job communicating ‘’increasing’’ or
‘’decreasing’’ territorial threat, or use dyad-year indicators of
conflict propensities as a stand-in for a state-year-level observation.
I use this research note to offer an empirical measurement of state-year
external territorial threat from a Bayesian random item response model
for all states from 1816 to 2010. I assess the face validity and
construct validity of the data these models generate, all of which
suggest the measure does well to capture the concept in question. I
close with a statement of the availability of the data and its potential
applications.

This paper is forthcoming at *Journal of Global Security Studies*
(*JoGSS*).

## Repository Directories

-   `data-raw/`: this contains what I term to be the `1.0-beta` release
    of this random item response model of external territorial threat. I
    had originally termed it an “index”, which would explain the file
    names. An updated version of this data, in light of newer conflict
    data from Gibler and Miller (Forthcoming), will appear in a separate
    repository.
-   `data/`: this contains finished data products for analyses that
    appear in the manuscript and appendix. The files that create them
    are in the `R/` directory.
-   `doc/`: this contains various documents for the project, including
    the manuscript itself along with the response memo and the appendix.
-   `inst/`: this contains assorted items. Here: a .csl for a reference
    style that conforms to the American Political Science Association
    along with a summary of reviewer feedback to guide me through the
    process of revising the manuscript.
-   `R/`: these are R scripts that conduct the analyses reported in the
    manuscript and appendix. The finished data products from them are in
    the `data/` directory.
-   `src/`: these are scripts that are mostly there for making
    documents. The Makefile will suggest more about how these work.

## An Explanation for This Repository’s Name

[I got tired of the peer-review
process](https://twitter.com/stevenvmiller/status/1229788167223398400)
changing the names of my projects. When this happened, my repository
names would no longer make sense. [I started creating nicknames for my
projects](https://twitter.com/stevenvmiller/status/1229788168049676294)
at the project’s onset, using either the Ubuntu Name Generator or the
Wu-Tang Name Generator. This led to the creation of an R
package—[`{codename}`](https://github.com/svmiller/codename)—that would
do this for me. This is what came out for this project.

``` r
library(codename)

codename_message()
#> code name generated by {codename} v.0.4.0

codename::variety_pack("A Random Item Response Model of External Territorial Threat, 1816-2010")
#> [1] "bright blue barbiturate"
#> [1] "skeletal mary"
#> [1] "sapphire stoat"
#> [1] "Respected Killah"
```

`type = "gods"` for the win. “skeletal mary” it is.
