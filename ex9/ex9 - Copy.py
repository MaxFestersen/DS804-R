# -*- coding: utf-8 -*-
"""
Created on Fri Apr  9 10:56:53 2021

@author: chris
"""

import math
import csv

#% Ex 9
# (a) Data processing
# a1) download the data set

# http://archive.ics.uci.edu/ml/datasets/seeds

# a2) reformat the data

df = open('seeds_dataset.csv').read().split("\n")

print(df)

# (b) k-means
# b1) Use a k-means implementation in your preferred tool to analyze the seeds dataset


# b2) What is a suitable choice for k?


# b3) For different choices ofkcompare the result to a similar choice of k in the k-means algorithm


# (c) EM-clustering
# c1) Run EM clustering on the seeds dataset


# c2) What is a suitable choice for k?


# c3) For different choices of k compare the result to a similar choice of k in the k-means algorithm


# (d) DBSCAN
# d1) Run DBSCAN on the seeds dataset


# d2) Find suitable parameter values for epsilon and minpts


# (e) SNN clustering
# Given your experience with thisdataset by now – does using these algorithms make sense on this dataset
