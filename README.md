# inrix-congestion

This repository contains the R scripts I used to visualize road congestion in the TMACOG planning area using INRIX data. One file attempts to show a metric called Distance Weight Delay Hours (DWDH), a concept borrowed from INDOT, while another file, cmp_lottr_viz.r, visualizes a federally-mandated performance measure known as Level of Travel Time Reliability (LOTTR), not to be confused with Lord of the Rings (LOTR). The underlying calculations for the two metrics were done in postgreSQL, but the relevant SQL queries are available in a different [repository](https://github.com/mr-fuller/npmrds).

The test plot pdf provides an example of the type of output that is able to be generated.

