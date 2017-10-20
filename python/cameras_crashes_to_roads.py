import arcpy
import glob
import os

os.chdir("C:/Users/soprimeaux/traffic_safety_assessment/data")

arcpy.env.overwriteOutput = True

try:
    os.remove("schema.ini")
except OSError:
    pass

arcpy.env.workspace = "C:/Users/soprimeaux/traffic_safety_assessment"

# create point layers
arcpy.MakeXYEventLayer_management("data/cameras.csv",
                                  "cam_lo",
                                  "cam_la",
                                  'output',
                                  arcpy.SpatialReference(4326))
arcpy.FeatureClassToFeatureClass_conversion('output',
                                            "geoprocessing.gdb",
                                            "cameras")

arcpy.MakeXYEventLayer_management("data/crashes.csv",
                                  "dotd_longitude",
                                  "dotd_latitude",
                                  'output',
                                  arcpy.SpatialReference(4326))
arcpy.FeatureClassToFeatureClass_conversion('output',
                                            "geoprocessing.gdb",
                                            "crashes")

# perform spatial joins
target_features = "geoprocessing.gdb/cameras"
join_features = "geoprocessing.gdb/roads"
arcpy.SpatialJoin_analysis(target_features,
                           join_features,
                           "geoprocessing.gdb/cameras_roads",
                           "JOIN_ONE_TO_MANY",
                           "KEEP_COMMON",
                           "",
                           "WITHIN_A_DISTANCE",
                           "50 feet")

target_features = "geoprocessing.gdb/crashes"
join_features = "geoprocessing.gdb/roads"
arcpy.SpatialJoin_analysis(target_features,
                           join_features,
                           "geoprocessing.gdb/crashes_roads",
                           "JOIN_ONE_TO_MANY",
                           "KEEP_COMMON",
                           "",
                           "WITHIN_A_DISTANCE",
                           "150 feet")

arcpy.CopyFeatures_management("geoprocessing.gdb/roads",
                              "geoprocessing.gdb/roads_copy")

arcpy.FeatureToPoint_management("geoprocessing.gdb/roads_copy",
                                "geoprocessing.gdb/roads_point_0",
                                "INSIDE")

# transform road center points from state plane system to WGS
arcpy.Project_management("geoprocessing.gdb/roads_point_0",
                         "geoprocessing.gdb/roads_point",
                         arcpy.SpatialReference(4326))

arcpy.AddXY_management("geoprocessing.gdb/roads_point")

features_to_write = ["roads_point"] + ["cameras_roads"] + ["crashes_roads"]
for f in features_to_write:
    arcpy.MakeTableView_management("geoprocessing.gdb/" + f,
                                   "geoprocessing.gdb/output_view")
    try:
        [os.remove(x) for x in glob.glob(f + ".*")]
    except:
        pass
    arcpy.TableToTable_conversion("geoprocessing.gdb/output_view",
                                  arcpy.env.workspace,
                                  "data/" + f + ".csv")
