---
output:
  html_document: default
  pdf_document: default
---
## Glyph plots



This section provides a graphical presentation of the precipitation data known as glyph plots.
The idea of glyph maps, its application and general implementation that were
used in this section are taken from @\ref:wickham2012glyph.
Glyph maps use a small icon or *glyph* to show multiple values at each location.
In our case, we show a complete time series at each location instead
of just single values. Different techniques can then be used compare the time series 
between all locations or their individual shape on a local scale.
We will show seasonal, deaseasonalised, and deseasonalised data on a local scale.
Seasonal time series are computed by computing the averages of each month on each location.
Each seasonal time series therefore has only 12 values and can be plotted without smoothing.
The deseasonalised time series are computed by omitting the seasonal effects on each
time series for the *complete* observation period 
and therefore has to be smoothed to be visually inspectable.
The deseasonalised time series then can be used to compare the time series for each location
on a common or local scale. On the common scale all values are displayed on the same axis range,
while on the local scale the axis are changed so that their ranges refer to the range on the respective
location.

So $value_resc = value - min(value) / max(value) - min(value)$

This will help us to see the changes in value at each location *relative* to the range
of the values at the same location.
But this also means that interpreting these plots has to be done carefully because,
in this form of display, large difference might actually refer to only small changes in
absolute values. It can be due to the small range of values at that location in general,
that these changes seem to be large.
To aid the interpretation of these plots we can use color shadings to draw attention to
areas in which ranges are large, meaning larger differences in their relative values also 
point to larger differences in their absolute values (i.e unscaled values, values on the global scale).
Therefore locations with large ranges are shaded in lighter colours and smaller ranges are shaded
in dark colour, to make the lighter shaded areas more easily visible.




![Glyph map of seasonal precipitaton pattern. Each location is presented by a time series. The time series are seperated by boxes. The gray reference lines inside the boxes show the mid-range for easier comparison.](figure/unnamed-chunk-4-1.png)

The above figure is a glyph-map of seasonal precipitation
patterns (averages for each month) in the Central Amazon Basin.
The gray reference lines show the mid-range for easier comparison
of the patterns.
We see differences in the seasonal patterns across the map.
In the upper left for example, the seasonal patterns stay
above mid-range while on the bottom-left they have values
clearly towards the low end of the range. 
Also some areas have multimodal patterns.
The patterns differ in range and month of maximum and minimum precipitation.



![Glyph map of de-seasonalised and smoothed precipitation. Each location is presented by a time series. The time series are seperated by boxes. The gray reference lines inside the boxes show the mid-range for easier comparison. The time series are scaled globally, same positions inside the cells correspond to the same values in all locations.](figure/unnamed-chunk-5-1.png)

This plot shows the smoothed de-seasonalized monthly precipitation, after global scaling. The same position
within each cell corresponds to the same value in all locations.
Some areas have almost a linear course, increasing, decreasing
or constant. Others show a more "wiggly" courses.
As overall pattern we can see that the forms of the patterns
have a spatial connection, patterns are close to similar patterns,
at the same latitude.
Also regarding latitude the closer to the equator the less precipitation.

![Glyph map of de-seasonalised and smoothed precipitation. The time series are scaled locally, ranges are not the same in all cells. The different ranges are given in color shades, where lighter shading indicates a larger range and darker shades smaller ranges.](figure/unnamed-chunk-6-1.png)

Now we inspect the glyph-map with de-seasonalized locally scaled values. This form of scaling emphasizes the individual shapes.
Because of the applied scaling, big patterns may be just be tiny effects. Therefore colors are added according to range.
Areas with lighter color have larger ranges than darker areas.
The areas with steep linear increases and decreases have smaller ranges than or example the areas below -2.5 latitude in the left.

The results of the precipitation glyphs indicate that the CAB might be seperable in 
different regions. If we can find a way to quantify the differences in these regions
and seperate them into clusters, we could then apply our regression models 
to each of these clusters and eventually improve model accuracy on each region
as compared to the complete are on average.
Therefore in section x we will discuss and apply clustering algorithms to
the precipitation data.
But for now lets consider one more explorative analysis of our data.