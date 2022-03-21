Dissertation methods and reproducibility
================

# Description

This repository contains all the scripts used to produce the results of
my dissertation. They can be used to repoducibility, however, the
mobility data of users, provided by an industrial partner, can not be
made publicly available. Figures from the final work as well as some
additional ones that are added in the appendix can be found in the
folders **Amenities**, **Mobility** and **Percolation levels**.

# Scripts structure

The scripts are divided into two main parts, those for working with
amenities data: downloading, cleaning, analysing, visualizing and the
other for working with mobility data. while the data on mobility is not
publicly available, the methods can be applied to any set of publicly
available GPS traces.

# Amenities

Working on amenities consisted in several step included in the scripts
in the folder **amenities\_scripts** :

  - Downloading from OSM
  - Cleaning
  - Analysing
      - Creating isochrones
      - Computing index
      - Creating concave areas
  - Visualizing

# Mobility

Mobility data was provided so did not require downloading, the scripts
in the folder **GPS scripts** do the following:

  - Querying data from a local postgreSQL data base
  - Cleaning
  - Analysing
      - transfering the traces to a hexagonal grid
      - building the similarity measure
      - hierarchical clustering
  - Visualizing

# Functions

Local functions are grouped into a single script for simplicity, it is
contained in the **RC scripts** folder `functions.R`.
