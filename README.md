# Create Structured Data from Wikipedia's Page API

This repository contains both the code to create a normalized dataset based on
the MediaWiki API for grabbing parsed version of Wikipedia pages as well as
several pre-constructed datasets including thematically related subsets of
Wikipedia pages.

## Preconstructed Datasets

There are five preconstructed datasets that are available in this repository
under the data directory. These are:

- thing 1 (English; 1000 pages)
- thing 2 (English; 1000 pages)
- thing 3 (English; 1000 pages)
- thing 4 (English; 1000 pages)
- thing 5 (English; 1000 pages)

These are a good way to start learning to use unsupervised learning and corpus
linguistics techniques.

## Table Schemas

The code in this repository produces N tables, according to the following
schemas. Across the different tables, `pageid` is used as the key to link to
a given page.

The `meta` table contains the following variables:

- **title**
- **pageid**
- **revid**
- **displaytitle**

The `text` table contains the following variables:

- **pageid**
- **pnum**
- **section**
- **subsection**
- **text**

The `langlink` table contains the following variables:

- **pageid**
- **link_order**
- **lang**
- **url**
- **langname**
- **autonym**
- **star**

The `category` table contains the following variables:

- **pageid**
- **record_num**
- **sort_key**
- **hidden**
- **star**

## Usage

In order to run the code on your own data....

```{r}
source("funs.R")
```
