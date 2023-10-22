#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 22 12:21:10 2023

@author: bas
"""

import geopandas as gpd
import numpy as np
from sklearn.cluster import SpectralClustering
import matplotlib.pyplot as plt
import cbsodata
import pandas as pd

# Retrieve data with municipal population and a shapefile with municipal boundaries
gemeenten_data = pd.DataFrame.from_dict(cbsodata.get_data('85385NED'))
gemeentegrenzen = gpd.read_file('nl_gemeenten_2023.geojson') # From QGIS Pdok Plugin

# Isolate the important variablesl and merge the datasets
gemeenten_data = gemeenten_data[['Code_1', 'Inwonertal_54']]
gemeenten_data['Code_1'] = gemeenten_data['Code_1'].str.strip()
gemeentegrenzen['identificatie'] = gemeentegrenzen['identificatie'].str.strip()
                                                         

gemeenten = pd.merge(left=gemeentegrenzen,
                     right=gemeenten_data,
                     how='left', 
                     left_on='identificatie',
                     right_on='Code_1')

gemeenten = gemeenten[['identificatie', 'naam', 'ligtInProvincieCode', 'Inwonertal_54', 'geometry']]

# Need a table with provincial max populations to define the similarity matrix
prov_table = gemeenten.groupby('ligtInProvincieCode').agg(max_i=('Inwonertal_54', 'max')).reset_index()

# Calculate pairwise similarity based on the spatial relationship
similarity_matrix = np.zeros((len(gemeenten), len(gemeenten)))

## Municipalities in the same province (sine qua non) and have to touch each other
## Municipalities with low inhabitants relative to the max in the province are more simlar
## 2 Large places in the same province have low similarity
for i in range(len(gemeenten)):
    for j in range(len(gemeenten)):
        if i == j:
            similarity_matrix[i][j] = 1
        if i != j:
            condition = ((gemeenten.iloc[i].geometry.touches(gemeenten.iloc[j].geometry)) 
                         and (gemeenten.iloc[i].ligtInProvincieCode == gemeenten.iloc[j].ligtInProvincieCode))
            # Adjust the similarity matrix entries
            if condition:
                prov = gemeenten.iloc[i].ligtInProvincieCode
                maxi = prov_table[prov_table['ligtInProvincieCode'] == prov].max_i.iloc[0]
                similarity_matrix[i][j] = 1/2 + (1 - gemeenten.iloc[i].Inwonertal_54/maxi - gemeenten.iloc[j].Inwonertal_54/maxi)/2

# Create an affinity matrix
row_sums = similarity_matrix.sum(axis=1)
normalized_matrix = similarity_matrix / row_sums[:, np.newaxis]


# Perform spectral clustering using the customized affinity matrix
n_clusters = 40 # Number of macro polygons
clustering = SpectralClustering(n_clusters=n_clusters, affinity='precomputed', assign_labels='discretize').fit(normalized_matrix)

# Create a new GeoDataFrame to store macro polygons
macro_polygons = gpd.GeoDataFrame(geometry=[])

# Iterate through the clusters
for cluster_id in range(n_clusters):
    # Extract polygons in the current cluster
    cluster_mask = (clustering.labels_ == cluster_id)
    cluster_gdf = gemeenten[cluster_mask]
    
    if len(cluster_gdf) > 0:
        # Merge polygons within the cluster
        merged_polygon = cluster_gdf.unary_union  # Merge all geometries in the cluster

        # Add the merged polygon to the GeoDataFrame
        macro_polygons = macro_polygons.append({'geometry': merged_polygon}, ignore_index=True)

# Define a colormap with distinct colors
macro_polygons.plot()
plt.show()
