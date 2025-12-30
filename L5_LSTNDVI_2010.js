//GOAL 1: Calculate LST for 2010 in Chicago. 
//STEP 1: Set up input data and functions
//STEP 2: Run the cloud removal function
//STEP 3: Calculate LST
//STEP 4: Summarize mean and median LST by census 
//STEP 5: Export

//GOAL 2: Compute average and median NDVI per census tract for each city in each year
//STEP 1: Set up input data and functions (same as LST)
//STEP 2: Run the cloud removal function (same as LST)
//STEP 3: Calculate NDVI
//STEP 4: Summarize mean and median NDVI by census 
//STEP 5: Export

//Import census tracts 
var chi = ee.FeatureCollection("projects/ee-mstuhlmacher/assets/Collaborations/Redlining/tractsCHI_v3");

//-----------GOAL 1: LST-----------//

//STEP 1: 
//Select just one year from the megatract files
var filterY = ee.Filter.eq('YEAR','2006-2010');
var cT = chi.filter(filterY);
Map.addLayer(cT,{},'Chicago tracts',false);

// Functions that uses the Landsat 4, 5, 7 Collection 2, Level 2 QA_PIXEL band (CFMask) to mask unwanted pixels.
function maskL457sr(image) {
  // Bit 0 - Fill
  // Bit 1 - Dilated Cloud
  // Bit 2 - Unused
  // Bit 3 - Cloud
  // Bit 4 - Cloud Shadow
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
  //var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image
      //.addBands(opticalBands, null, true)
      .addBands(thermalBand, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}

//STEP 2:
//Create images and image collection with imported images
//16 images for Chicago between 1/1/2009 and 1/1/2011 (June, July, August) with 50% or less cloud cover, Landsat 4-5 TM C2 L2
var chi1QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100820_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi1QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100820_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi1ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100820_20200823_02_T1_ST_B6').rename('ST_B6');
var chi1 = chi1QAP.addBands(chi1QAR).addBands(chi1ST);
//Map.addLayer(chi1,{},'image 1',false);

var chi2QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100827_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi2QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100827_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi2ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100827_20200823_02_T1_ST_B6').rename('ST_B6');
var chi2 = chi2QAP.addBands(chi2QAR).addBands(chi2ST);

var chi3QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090614_20200827_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi3QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090614_20200827_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi3ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090614_20200827_02_T1_ST_B6').rename('ST_B6');
var chi3 = chi3QAP.addBands(chi3QAR).addBands(chi3ST);

var chi4QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090716_20200827_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi4QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090716_20200827_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi4ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20090716_20200827_02_T1_ST_B6').rename('ST_B6');
var chi4 = chi4QAP.addBands(chi4QAR).addBands(chi4ST);

var chi5QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100601_20200824_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi5QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100601_20200824_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi5ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100601_20200824_02_T1_ST_B6').rename('ST_B6');
var chi5 = chi5QAP.addBands(chi5QAR).addBands(chi5ST);

var chi6QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100617_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi6QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100617_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi6ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100617_20200823_02_T1_ST_B6').rename('ST_B6');
var chi6 = chi6QAP.addBands(chi6QAR).addBands(chi6ST);

var chi7QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100703_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi7QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100703_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi7ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100703_20200823_02_T1_ST_B6').rename('ST_B6');
var chi7 = chi7QAP.addBands(chi7QAR).addBands(chi7ST);

var chi8QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100719_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi8QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100719_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi8ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_022031_20100719_20200823_02_T1_ST_B6').rename('ST_B6');
var chi8 = chi8QAP.addBands(chi8QAR).addBands(chi8ST);

var chi9QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090605_20200827_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi9QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090605_20200827_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi9ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090605_20200827_02_T1_ST_B6').rename('ST_B6');
var chi9 = chi9QAP.addBands(chi9QAR).addBands(chi9ST);

var chi10QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090707_20200827_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi10QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090707_20200827_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi10ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090707_20200827_02_T1_ST_B6').rename('ST_B6');
var chi10 = chi10QAP.addBands(chi10QAR).addBands(chi10ST);

var chi11QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090723_20200827_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi11QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090723_20200827_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi11ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090723_20200827_02_T1_ST_B6').rename('ST_B6');
var chi11 = chi11QAP.addBands(chi11QAR).addBands(chi11ST);

var chi12QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090824_20200825_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi12QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090824_20200825_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi12ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20090824_20200825_02_T1_ST_B6').rename('ST_B6');
var chi12 = chi12QAP.addBands(chi12QAR).addBands(chi12ST);

var chi13QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100624_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi13QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100624_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi13ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100624_20200823_02_T1_ST_B6').rename('ST_B6');
var chi13 = chi13QAP.addBands(chi13QAR).addBands(chi13ST);

var chi14QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100710_20200824_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi14QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100710_20200824_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi14ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100710_20200824_02_T1_ST_B6').rename('ST_B6');
var chi14 = chi14QAP.addBands(chi14QAR).addBands(chi14ST);

var chi15QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100726_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi15QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100726_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi15ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100726_20200823_02_T1_ST_B6').rename('ST_B6');
var chi15 = chi15QAP.addBands(chi15QAR).addBands(chi15ST);

var chi16QAP = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100811_20200823_02_T1_QA_PIXEL').rename('QA_PIXEL');
var chi16QAR = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100811_20200823_02_T1_QA_RADSAT').rename('QA_RADSAT');
var chi16ST = ee.Image('projects/ee-mstuhlmacher/assets/Collaborations/Redlining/ChicagoLST2010/LT05_L2SP_023031_20100811_20200823_02_T1_ST_B6').rename('ST_B6');
var chi16 = chi16QAP.addBands(chi16QAR).addBands(chi16ST);

var chiIC = ee.ImageCollection([chi1,chi2,chi3,chi4,chi5,chi6,chi7,chi8,chi9,chi10,chi11,chi12,chi13,chi14,chi15,chi16]);
Map.addLayer(chiIC,{},'CHI ic',false);

//STEP 3: Calculate LST
// Map the function over two years of data.
var collectionCHI = chiIC.map(maskL457sr);
Map.addLayer(collectionCHI,{},'CHI collection',false);

//Calculate LST
var lstCHI = collectionCHI.median().select('ST_B6')//.clip(cT);
Map.addLayer(lstCHI,{},'CHI lst',false);

//STEP 4: Summarize mean and median LST by census tract
// Combine the mean and standard deviation reducers.
var reducers = ee.Reducer.mean().combine({
  reducer2: ee.Reducer.median(),
  sharedInputs: true
});

// Use the combined reducer to get the mean and median of the image.
//Chicago
var chi2010 = lstCHI.select('ST_B6').reduceRegions({
  collection: cT,
  reducer: reducers,
  scale: 30
});

//STEP 5: Export
//Function to remove geometry column for export
var removeGeo = (function(feature){
  feature = feature.setGeometry(null);
  return (feature)});
  
//Map remove geometry function
var chi2010e = chi2010.map(removeGeo);
print(chi2010e.first());

//Export tables
Export.table.toDrive({
  collection: chi2010e, 
  description: 'ChicagoLST2010', 
  fileFormat: 'CSV', 
  selectors: ['cluster_id', 'mean','median']});
  
//-----------GOAL 2: NDVI-----------//
//STEP 1: Set up filter
//Summer month filter (June 1-Aug 31)
var fSummer = ee.Filter.dayOfYear(152,243); 

//STEP 2:
// Map the function over two years of data.
var collection = ee.ImageCollection('LANDSAT/LT05/C02/T1_L2')
                     .filterDate('2009-01-01', '2011-01-01')
                     .filter(fSummer)
                     .map(maskL457sr);

var composite = collection.median();
Map.addLayer(composite, {bands: ['SR_B3', 'SR_B2', 'SR_B1'], min: 0, max: 0.3},'L5 2010',false);

//STEP 3: Calculate NDVI
var ndvi = composite.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');

var ndvi_palette = {min:-0.1, max:1, palette: ['FFFFFF','CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901', '66A000','529400',
                                              '3E8601', '207401', '056201', '004C00', '023B01', '012E01', '011D01', '011301']};
Map.addLayer(ndvi,ndvi_palette,'NDVI',false);

//STEP 4: Summarize mean and median NDVI by census tract
// Combine the mean and standard deviation reducers.
var reducers = ee.Reducer.mean().combine({
  reducer2: ee.Reducer.median(),
  sharedInputs: true
});

// Use the combined reducer to get the mean and median of the image.
//Chicago
var chi2010 = ndvi.select('NDVI').reduceRegions({
  collection: cT,
  reducer: reducers,
  scale: 30
});

//STEP 5: Export
//Map remove geometry function
var chi2010e = chi2010.map(removeGeo);
print(chi2010e.first());

//Export tables
Export.table.toDrive({
  collection: chi2010e, 
  description: 'ChicagoNDVI2010', 
  fileFormat: 'CSV', 
  selectors: ['cluster_id', 'mean','median']});
