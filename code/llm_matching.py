import numpy as np
import pandas as pd
import linktransformer as lt

#%%
data = pd.read_csv('../data/intermediate_exhibition_data/individual_level_exhibition_data.csv',sep=";")
candidates = pd.read_csv('../data/dictionaries/list_of_municipalities_italy.csv', 
                         sep=";", 
                         dtype={"LAU_ID": str})


# %%
out = lt.merge(df1=data[data['place'].notna()],
               df2=candidates, 
               model="h4g3n/multilingual-MiniLM-L12-de-en-es-fr-it-nl-pl-pt",
               left_on='place', 
               right_on='LAU_NAME'
)


# %%
