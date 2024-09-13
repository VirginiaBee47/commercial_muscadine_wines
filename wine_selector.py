import pandas as pd

wine_df = pd.read_csv('commercial_muscadine_wines.csv')

minimum_per_color = 3
minimum_per_sweetness = 3

print(wine_df.head())

sweetnesses = [i for i in range(1, 5)]
colors = [i for i in range(1, 4)]

selected_wines = pd.DataFrame(data=None, index=None, columns=wine_df.columns)
