// Load the SRTM image.
var srtm = ee.Image('USGS/SRTMGL1_003');
var seg = ee.FeatureCollection("users/tim479874/segments_GEE");
var features = seg



// Calculate slope.
var slope = ee.Terrain.slope(srtm);

// Clip the image to the region of interest.
var slope_gee = slope.clip(seg);

var features = features.map(function(feature) {
var slope = slope_gee.reduceRegion({geometry: feature.geometry(),reducer: ee.Reducer.first(),scale: 90}).get('slope');
 return feature.set({'slope': slope,});
          });



Export.table.toDrive({collection: features, 
description: 'slope', 
selectors: ['ID', 'slope'],
fileFormat: 'CSV'});