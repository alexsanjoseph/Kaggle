import numpy as np
import pandas as pd

import plotly.plotly as py
import plotly.graph_objs as go
from plotly import tools
from plotly.offline import iplot, init_notebook_mode
init_notebook_mode()
# state population estimates for July 2015 from US Census Bureau
# www.census.gov/popest/data/state/totals/2015/tables/NST-EST2015-01.csv
state_population = np.asarray([738432, 4858979, 2978204, 6828065, 39144818, 5456574,\
                               3590886, 672228, 945934, 20271272, 10214860, 1431603,\
                               3123899, 1654930, 12859995, 6619680, 2911641, 4425092,\
                               4670724, 6794422, 6006401, 1329328, 9922576, 5489594,\
                               6083672, 2992333, 1032949, 10042802, 756927, 1896190,\
                               1330608, 8958013, 2085109, 2890845, 19795791, 11613423,\
                               3911338, 4028977, 12802503, 1056298, 4896146, 858469,\
                               6600299, 27469114, 2995919, 8382993, 626042, 7170351,\
                               5771337, 1844128, 586107])

# police officer deaths per 100,000 people in state

police_percapita = state_population / 100000

# District of Columbia outlier (1 law enforcement death per 500 people) adjusted
police_percapita[7] = police_percapita[7] / 10

# plotly code for choropleth map
police_scale = [[0, 'rgb(229, 239, 245)'],[1, 'rgb(1, 97, 156)']]

data = [ dict(
        type = 'choropleth',
        colorscale = police_scale,
        autocolorscale = False,
        showscale = False,

        z = police_percapita,
        locationmode = 'USA-states',
        marker = dict(
            line = dict (
                color = 'rgb(255, 255, 255)',
                width = 2
            ) ),
        ) ]

layout = dict(
        title = 'Police Officer Deaths per 100,000 People in United States (1791-2016)',
        geo = dict(
            scope = 'usa',
            projection = dict( type='albers usa' ),
            showlakes = True,
            lakecolor = 'rgb(255, 255, 255)',
            countrycolor = 'rgb(255, 255, 255)')
             )

figure = dict(data=data, layout=layout)

iplot(figure)
