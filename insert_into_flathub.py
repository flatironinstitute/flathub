### Script to ingest CAMELS data into FlatHub.
### Usage:
## 0. git clone https://github.com/illustristng/illustris_python
# where this script will be executed.
# 1. Modify illustris_python/groupcat.py, gcPath() function,
# to return filePath1 = basePath + 'fof_subhalo_tab_%03d.hdf5' % (snapNum)
# This will allow illustris_python to be used with CAMELS files.
## 2. Modify the ELASTICSEARCH variable to point to the correct HTTP endpoint.
## 3. Modify the `basepath` variable within the for loop below,
# to refer to the appropriate path to the CAMELS files.


import os

import illustris_python as il

from elasticsearch import Elasticsearch
from elasticsearch.helpers import streaming_bulk

ELASTICSEARCH = Elasticsearch(
    hosts=["localhost:9200"]
)


def simulation_suite_to_enum(simulation_suite):
    return ["IllustrisTNG", "SIMBA"].index(simulation_suite)

def simulation_set_to_enum(simulation_set):
    return ["LH", "1P", "CV", "EX"].index(simulation_set)

SIMULATION_SUITES = ["IllustrisTNG", "SIMBA"]

SIMULATION_SET_ID_RANGES = {
    "LH": 999,
    "1P": 65,
    "CV": 26,
    "EX": 3,
}

MAX_SNAPSHOT = 33
SNAPSHOTS = range(MAX_SNAPSHOT + 1)

REDSHIFTS = [6.0, 5.0, 4.0, 3.5, 3.0, 2.81329, 2.63529, 2.46560, 2.30383, 2.14961, 2.00259, 1.86243, 1.72882, 1.60144, 1.48001, 1.36424, 1.25388, 1.14868, 1.04838, 0.95276, 0.86161, 0.77471, 0.69187, 0.61290, 0.53761, 0.46584, 0.39741, 0.33218, 0.27, 0.21071, 0.15420, 0.10033, 0.04896, 0.0]


for simulation_suite in SIMULATION_SUITES:
    for simulation_set, simulation_set_id_range in SIMULATION_SET_ID_RANGES.items():
        for simulation_set_id in range(simulation_set_id_range + 1):
            basepath = f"./Simulations/{simulation_suite}/{simulation_set}_{simulation_set_id}/"
            
            for snapshot in SNAPSHOTS:
                redshift = REDSHIFTS[snapshot]

                # Verify if file exists, before trying to read halo data
                if not os.path.exists(f"{basepath}/fof_subhalo_tab_{snapshot:03}.hdf5"):
                    continue

                print("Found file. About to load halos and subhalos.")

                halos = il.groupcat.loadHalos(basepath, snapshot, fields=None)
                subhalos = il.groupcat.loadSubhalos(basepath, snapshot, fields=None)

                ### Generate FoF Halos for indexing
                def generate_FoF_halos():
                    for i in range(halos["count"]):
                        doc = {
                            "redshift": redshift,
                            "simulation_suite": simulation_suite_to_enum(simulation_suite),
                            "simulation_set": simulation_set_to_enum(simulation_set),
                            "simulation_set_id": simulation_set_id,
                            "snapshot": snapshot,
                            "GroupBHMass": halos["GroupBHMass"][i],
                            "GroupBHMdot": halos["GroupBHMdot"][i],
                            "GroupCM_x": halos["GroupCM"][i][0],
                            "GroupCM_y": halos["GroupCM"][i][1],
                            "GroupCM_z": halos["GroupCM"][i][2],
                            "GroupFirstSub": halos["GroupFirstSub"][i],
                            "GroupGasMetalFractions_H": halos["GroupGasMetalFractions"][i][0],
                            "GroupGasMetalFractions_He": halos["GroupGasMetalFractions"][i][1],
                            "GroupGasMetalFractions_C": halos["GroupGasMetalFractions"][i][2],
                            "GroupGasMetalFractions_N": halos["GroupGasMetalFractions"][i][3],
                            "GroupGasMetalFractions_O": halos["GroupGasMetalFractions"][i][4],
                            "GroupGasMetalFractions_Ne": halos["GroupGasMetalFractions"][i][5],
                            "GroupGasMetalFractions_Mg": halos["GroupGasMetalFractions"][i][6],
                            "GroupGasMetalFractions_Si": halos["GroupGasMetalFractions"][i][7],
                            "GroupGasMetalFractions_Fe": halos["GroupGasMetalFractions"][i][8],
                            "GroupGasMetalFractions_total": halos["GroupGasMetalFractions"][i][9],
                            "GroupGasMetallicity": halos["GroupGasMetallicity"][i],
                            "GroupLen": halos["GroupLen"][i],
                            "GroupLenType_gas": halos["GroupLenType"][i][0],
                            "GroupLenType_dm": halos["GroupLenType"][i][1],
                            "GroupLenType_unused": halos["GroupLenType"][i][2],
                            "GroupLenType_tracers": halos["GroupLenType"][i][3],
                            "GroupLenType_stars": halos["GroupLenType"][i][4],
                            "GroupLenType_bh": halos["GroupLenType"][i][5],
                            "GroupMass": halos["GroupMass"][i],
                            "GroupMassType_gas": halos["GroupMassType"][i][0],
                            "GroupMassType_dm": halos["GroupMassType"][i][1],
                            "GroupMassType_unused": halos["GroupMassType"][i][2],
                            "GroupMassType_tracers": halos["GroupMassType"][i][3],
                            "GroupMassType_stars": halos["GroupMassType"][i][4],
                            "GroupMassType_bh": halos["GroupMassType"][i][5],
                            "GroupNsubs": halos["GroupNsubs"][i],
                            "GroupPos_x": halos["GroupPos"][i][0],
                            "GroupPos_y": halos["GroupPos"][i][1],
                            "GroupPos_z": halos["GroupPos"][i][2],
                            "GroupSFR": halos["GroupSFR"][i],
                            "GroupStarMetalFractions_H": halos["GroupStarMetalFractions"][i][0],
                            "GroupStarMetalFractions_He": halos["GroupStarMetalFractions"][i][1],
                            "GroupStarMetalFractions_C": halos["GroupStarMetalFractions"][i][2],
                            "GroupStarMetalFractions_N": halos["GroupStarMetalFractions"][i][3],
                            "GroupStarMetalFractions_O": halos["GroupStarMetalFractions"][i][4],
                            "GroupStarMetalFractions_Ne": halos["GroupStarMetalFractions"][i][5],
                            "GroupStarMetalFractions_Mg": halos["GroupStarMetalFractions"][i][6],
                            "GroupStarMetalFractions_Si": halos["GroupStarMetalFractions"][i][7],
                            "GroupStarMetalFractions_Fe": halos["GroupStarMetalFractions"][i][8],
                            "GroupStarMetalFractions_total": halos["GroupStarMetalFractions"][i][9],
                            "GroupStarMetallicity": halos["GroupStarMetallicity"][i],
                            "GroupVel_x": halos["GroupVel"][i][0],
                            "GroupVel_y": halos["GroupVel"][i][1],
                            "GroupVel_z": halos["GroupVel"][i][2],
                            "GroupWindMass": halos["GroupWindMass"][i],
                            "GroupM_Crit200": halos["Group_M_Crit200"][i],
                            "GroupM_Crit500": halos["Group_M_Crit500"][i],
                            "GroupM_Mean200": halos["Group_M_Mean200"][i],
                            "GroupM_TopHat200": halos["Group_M_TopHat200"][i],
                            "GroupR_Crit200": halos["Group_R_Crit200"][i],
                            "GroupR_Crit500": halos["Group_R_Crit500"][i],
                            "GroupR_Mean200": halos["Group_R_Mean200"][i],
                            "GroupR_TopHat200": halos["Group_R_TopHat200"][i],
                        }
                        yield doc

                ### Generate Subhalos for indexing
                def generate_subhalos():
                    for i in range(subhalos["count"]):
                        doc = {
                            "redshift": redshift,
                            "simulation_suite": simulation_suite_to_enum(simulation_suite),
                            "simulation_set": simulation_set_to_enum(simulation_set),
                            "simulation_set_id": simulation_set_id,
                            "snapshot": snapshot,
                            "SubhaloBHMass": subhalos["SubhaloBHMass"][i],
                            "SubhaloBHMdot": subhalos["SubhaloBHMdot"][i],
                            "SubhaloBfldDisk": subhalos["SubhaloBfldDisk"][i],
                            "SubhaloBfldHalo": subhalos["SubhaloBfldHalo"][i],
                            "SubhaloCM_x": subhalos["SubhaloCM"][i][0],
                            "SubhaloCM_y": subhalos["SubhaloCM"][i][1],
                            "SubhaloCM_z": subhalos["SubhaloCM"][i][2],
                            "SubhaloGasMetalFractions_H": subhalos["SubhaloGasMetalFractions"][i][0],
                            "SubhaloGasMetalFractions_He": subhalos["SubhaloGasMetalFractions"][i][1],
                            "SubhaloGasMetalFractions_C": subhalos["SubhaloGasMetalFractions"][i][2],
                            "SubhaloGasMetalFractions_N": subhalos["SubhaloGasMetalFractions"][i][3],
                            "SubhaloGasMetalFractions_O": subhalos["SubhaloGasMetalFractions"][i][4],
                            "SubhaloGasMetalFractions_Ne": subhalos["SubhaloGasMetalFractions"][i][5],
                            "SubhaloGasMetalFractions_Mg": subhalos["SubhaloGasMetalFractions"][i][6],
                            "SubhaloGasMetalFractions_Si": subhalos["SubhaloGasMetalFractions"][i][7],
                            "SubhaloGasMetalFractions_Fe": subhalos["SubhaloGasMetalFractions"][i][8],
                            "SubhaloGasMetalFractions_total": subhalos["SubhaloGasMetalFractions"][i][9],
                            "SubhaloGasMetalFractionsHalfRad_H": subhalos["SubhaloGasMetalFractionsHalfRad"][i][0],
                            "SubhaloGasMetalFractionsHalfRad_He": subhalos["SubhaloGasMetalFractionsHalfRad"][i][1],
                            "SubhaloGasMetalFractionsHalfRad_C": subhalos["SubhaloGasMetalFractionsHalfRad"][i][2],
                            "SubhaloGasMetalFractionsHalfRad_N": subhalos["SubhaloGasMetalFractionsHalfRad"][i][3],
                            "SubhaloGasMetalFractionsHalfRad_O": subhalos["SubhaloGasMetalFractionsHalfRad"][i][4],
                            "SubhaloGasMetalFractionsHalfRad_Ne": subhalos["SubhaloGasMetalFractionsHalfRad"][i][5],
                            "SubhaloGasMetalFractionsHalfRad_Mg": subhalos["SubhaloGasMetalFractionsHalfRad"][i][6],
                            "SubhaloGasMetalFractionsHalfRad_Si": subhalos["SubhaloGasMetalFractionsHalfRad"][i][7],
                            "SubhaloGasMetalFractionsHalfRad_Fe": subhalos["SubhaloGasMetalFractionsHalfRad"][i][8],
                            "SubhaloGasMetalFractionsHalfRad_total": subhalos["SubhaloGasMetalFractionsHalfRad"][i][9],
                            "SubhaloGasMetalFractionsMaxRad_H": subhalos["SubhaloGasMetalFractionsMaxRad"][i][0],
                            "SubhaloGasMetalFractionsMaxRad_He": subhalos["SubhaloGasMetalFractionsMaxRad"][i][1],
                            "SubhaloGasMetalFractionsMaxRad_C": subhalos["SubhaloGasMetalFractionsMaxRad"][i][2],
                            "SubhaloGasMetalFractionsMaxRad_N": subhalos["SubhaloGasMetalFractionsMaxRad"][i][3],
                            "SubhaloGasMetalFractionsMaxRad_O": subhalos["SubhaloGasMetalFractionsMaxRad"][i][4],
                            "SubhaloGasMetalFractionsMaxRad_Ne": subhalos["SubhaloGasMetalFractionsMaxRad"][i][5],
                            "SubhaloGasMetalFractionsMaxRad_Mg": subhalos["SubhaloGasMetalFractionsMaxRad"][i][6],
                            "SubhaloGasMetalFractionsMaxRad_Si": subhalos["SubhaloGasMetalFractionsMaxRad"][i][7],
                            "SubhaloGasMetalFractionsMaxRad_Fe": subhalos["SubhaloGasMetalFractionsMaxRad"][i][8],
                            "SubhaloGasMetalFractionsMaxRad_total": subhalos["SubhaloGasMetalFractionsMaxRad"][i][9],
                            "SubhaloGasMetalFractionsSfr_H": subhalos["SubhaloGasMetalFractionsSfr"][i][0],
                            "SubhaloGasMetalFractionsSfr_He": subhalos["SubhaloGasMetalFractionsSfr"][i][1],
                            "SubhaloGasMetalFractionsSfr_C": subhalos["SubhaloGasMetalFractionsSfr"][i][2],
                            "SubhaloGasMetalFractionsSfr_N": subhalos["SubhaloGasMetalFractionsSfr"][i][3],
                            "SubhaloGasMetalFractionsSfr_O": subhalos["SubhaloGasMetalFractionsSfr"][i][4],
                            "SubhaloGasMetalFractionsSfr_Ne": subhalos["SubhaloGasMetalFractionsSfr"][i][5],
                            "SubhaloGasMetalFractionsSfr_Mg": subhalos["SubhaloGasMetalFractionsSfr"][i][6],
                            "SubhaloGasMetalFractionsSfr_Si": subhalos["SubhaloGasMetalFractionsSfr"][i][7],
                            "SubhaloGasMetalFractionsSfr_Fe": subhalos["SubhaloGasMetalFractionsSfr"][i][8],
                            "SubhaloGasMetalFractionsSfr_total": subhalos["SubhaloGasMetalFractionsSfr"][i][9],
                            "SubhaloGasMetalFractionsSfrWeighted_H": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][0],
                            "SubhaloGasMetalFractionsSfrWeighted_He": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][1],
                            "SubhaloGasMetalFractionsSfrWeighted_C": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][2],
                            "SubhaloGasMetalFractionsSfrWeighted_N": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][3],
                            "SubhaloGasMetalFractionsSfrWeighted_O": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][4],
                            "SubhaloGasMetalFractionsSfrWeighted_Ne": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][5],
                            "SubhaloGasMetalFractionsSfrWeighted_Mg": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][6],
                            "SubhaloGasMetalFractionsSfrWeighted_Si": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][7],
                            "SubhaloGasMetalFractionsSfrWeighted_Fe": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][8],
                            "SubhaloGasMetalFractionsSfrWeighted_total": subhalos["SubhaloGasMetalFractionsSfrWeighted"][i][9],
                            "SubhaloGasMetallicity": subhalos["SubhaloGasMetallicity"][i],
                            "SubhaloGasMetallicityHalfRad": subhalos["SubhaloGasMetallicityHalfRad"][i],
                            "SubhaloGasMetallicityMaxRad": subhalos["SubhaloGasMetallicityMaxRad"][i],
                            "SubhaloGasMetallicitySfr": subhalos["SubhaloGasMetallicitySfr"][i],
                            "SubhaloGasMetallicitySfrWeighted": subhalos["SubhaloGasMetallicitySfrWeighted"][i],
                            "SubhaloGrNr": subhalos["SubhaloGrNr"][i],
                            "SubhaloHalfmassRad": subhalos["SubhaloHalfmassRad"][i],
                            "SubhaloHalfmassRadType_gas": subhalos["SubhaloHalfmassRadType"][i][0],
                            "SubhaloHalfmassRadType_dm": subhalos["SubhaloHalfmassRadType"][i][1],
                            "SubhaloHalfmassRadType_unused": subhalos["SubhaloHalfmassRadType"][i][2],
                            "SubhaloHalfmassRadType_tracers": subhalos["SubhaloHalfmassRadType"][i][3],
                            "SubhaloHalfmassRadType_stars": subhalos["SubhaloHalfmassRadType"][i][4],
                            "SubhaloHalfmassRadType_bh": subhalos["SubhaloHalfmassRadType"][i][5],
                            "SubhaloIDMostbound": subhalos["SubhaloIDMostbound"][i],
                            "SubhaloLen": subhalos["SubhaloLen"][i],
                            "SubhaloLenType_gas": subhalos["SubhaloLenType"][i][0],
                            "SubhaloLenType_dm": subhalos["SubhaloLenType"][i][1],
                            "SubhaloLenType_unused": subhalos["SubhaloLenType"][i][2],
                            "SubhaloLenType_tracers": subhalos["SubhaloLenType"][i][3],
                            "SubhaloLenType_stars": subhalos["SubhaloLenType"][i][4],
                            "SubhaloLenType_bh": subhalos["SubhaloLenType"][i][5],
                            "SubhaloMass": subhalos["SubhaloMass"][i],
                            "SubhaloMassInHalfRad": subhalos["SubhaloMassInHalfRad"][i],
                            "SubhaloMassInMaxRad": subhalos["SubhaloMassInMaxRad"][i],
                            "SubhaloMassInRad": subhalos["SubhaloMassInRad"][i],
                            "SubhaloMassInHalfRadType_gas": subhalos["SubhaloMassInHalfRadType"][i][0],
                            "SubhaloMassInHalfRadType_dm": subhalos["SubhaloMassInHalfRadType"][i][1],
                            "SubhaloMassInHalfRadType_unused": subhalos["SubhaloMassInHalfRadType"][i][2],
                            "SubhaloMassInHalfRadType_tracers": subhalos["SubhaloMassInHalfRadType"][i][3],
                            "SubhaloMassInHalfRadType_stars": subhalos["SubhaloMassInHalfRadType"][i][4],
                            "SubhaloMassInHalfRadType_bh": subhalos["SubhaloMassInHalfRadType"][i][5],
                            "SubhaloMassInMaxRadType_gas": subhalos["SubhaloMassInMaxRadType"][i][0],
                            "SubhaloMassInMaxRadType_dm": subhalos["SubhaloMassInMaxRadType"][i][1],
                            "SubhaloMassInMaxRadType_unused": subhalos["SubhaloMassInMaxRadType"][i][2],
                            "SubhaloMassInMaxRadType_tracers": subhalos["SubhaloMassInMaxRadType"][i][3],
                            "SubhaloMassInMaxRadType_stars": subhalos["SubhaloMassInMaxRadType"][i][4],
                            "SubhaloMassInMaxRadType_bh": subhalos["SubhaloMassInMaxRadType"][i][5],
                            "SubhaloMassInRadType_gas": subhalos["SubhaloMassInRadType"][i][0],
                            "SubhaloMassInRadType_dm": subhalos["SubhaloMassInRadType"][i][1],
                            "SubhaloMassInRadType_unused": subhalos["SubhaloMassInRadType"][i][2],
                            "SubhaloMassInRadType_tracers": subhalos["SubhaloMassInRadType"][i][3],
                            "SubhaloMassInRadType_stars": subhalos["SubhaloMassInRadType"][i][4],
                            "SubhaloMassInRadType_bh": subhalos["SubhaloMassInRadType"][i][5],
                            "SubhaloMassType_gas": subhalos["SubhaloMassType"][i][0],
                            "SubhaloMassType_dm": subhalos["SubhaloMassType"][i][1],
                            "SubhaloMassType_unused": subhalos["SubhaloMassType"][i][2],
                            "SubhaloMassType_tracers": subhalos["SubhaloMassType"][i][3],
                            "SubhaloMassType_stars": subhalos["SubhaloMassType"][i][4],
                            "SubhaloMassType_bh": subhalos["SubhaloMassType"][i][5],
                            "SubhaloParent": subhalos["SubhaloParent"][i],
                            "SubhaloPos_x": subhalos["SubhaloPos"][i][0],
                            "SubhaloPos_y": subhalos["SubhaloPos"][i][1],
                            "SubhaloPos_z": subhalos["SubhaloPos"][i][2],
                            "SubhaloSFR": subhalos["SubhaloSFR"][i],
                            "SubhaloSFRinHalfRad": subhalos["SubhaloSFRinHalfRad"][i],
                            "SubhaloSFRinMaxRad": subhalos["SubhaloSFRinMaxRad"][i],
                            "SubhaloSFRinRad": subhalos["SubhaloSFRinRad"][i],
                            "SubhaloSpin_x": subhalos["SubhaloSpin"][i][0],
                            "SubhaloSpin_y": subhalos["SubhaloSpin"][i][1],
                            "SubhaloSpin_z": subhalos["SubhaloSpin"][i][2],
                            "SubhaloStarMetallicity": subhalos["SubhaloStarMetallicity"][i],
                            "SubhaloStarMetallicityHalfRad": subhalos["SubhaloStarMetallicityHalfRad"][i],
                            "SubhaloStarMetallicityMaxRad": subhalos["SubhaloStarMetallicityMaxRad"][i],
                            "SubhaloStarMetalFractions_H": subhalos["SubhaloStarMetalFractions"][i][0],
                            "SubhaloStarMetalFractions_He": subhalos["SubhaloStarMetalFractions"][i][1],
                            "SubhaloStarMetalFractions_C": subhalos["SubhaloStarMetalFractions"][i][2],
                            "SubhaloStarMetalFractions_N": subhalos["SubhaloStarMetalFractions"][i][3],
                            "SubhaloStarMetalFractions_O": subhalos["SubhaloStarMetalFractions"][i][4],
                            "SubhaloStarMetalFractions_Ne": subhalos["SubhaloStarMetalFractions"][i][5],
                            "SubhaloStarMetalFractions_Mg": subhalos["SubhaloStarMetalFractions"][i][6],
                            "SubhaloStarMetalFractions_Si": subhalos["SubhaloStarMetalFractions"][i][7],
                            "SubhaloStarMetalFractions_Fe": subhalos["SubhaloStarMetalFractions"][i][8],
                            "SubhaloStarMetalFractions_total": subhalos["SubhaloStarMetalFractions"][i][9],
                            "SubhaloStarMetalFractionsHalfRad_H": subhalos["SubhaloStarMetalFractionsHalfRad"][i][0],
                            "SubhaloStarMetalFractionsHalfRad_He": subhalos["SubhaloStarMetalFractionsHalfRad"][i][1],
                            "SubhaloStarMetalFractionsHalfRad_C": subhalos["SubhaloStarMetalFractionsHalfRad"][i][2],
                            "SubhaloStarMetalFractionsHalfRad_N": subhalos["SubhaloStarMetalFractionsHalfRad"][i][3],
                            "SubhaloStarMetalFractionsHalfRad_O": subhalos["SubhaloStarMetalFractionsHalfRad"][i][4],
                            "SubhaloStarMetalFractionsHalfRad_Ne": subhalos["SubhaloStarMetalFractionsHalfRad"][i][5],
                            "SubhaloStarMetalFractionsHalfRad_Mg": subhalos["SubhaloStarMetalFractionsHalfRad"][i][6],
                            "SubhaloStarMetalFractionsHalfRad_Si": subhalos["SubhaloStarMetalFractionsHalfRad"][i][7],
                            "SubhaloStarMetalFractionsHalfRad_Fe": subhalos["SubhaloStarMetalFractionsHalfRad"][i][8],
                            "SubhaloStarMetalFractionsHalfRad_total": subhalos["SubhaloStarMetalFractionsHalfRad"][i][9],
                            "SubhaloStarMetalFractionsMaxRad_H": subhalos["SubhaloStarMetalFractionsMaxRad"][i][0],
                            "SubhaloStarMetalFractionsMaxRad_He": subhalos["SubhaloStarMetalFractionsMaxRad"][i][1],
                            "SubhaloStarMetalFractionsMaxRad_C": subhalos["SubhaloStarMetalFractionsMaxRad"][i][2],
                            "SubhaloStarMetalFractionsMaxRad_N": subhalos["SubhaloStarMetalFractionsMaxRad"][i][3],
                            "SubhaloStarMetalFractionsMaxRad_O": subhalos["SubhaloStarMetalFractionsMaxRad"][i][4],
                            "SubhaloStarMetalFractionsMaxRad_Ne": subhalos["SubhaloStarMetalFractionsMaxRad"][i][5],
                            "SubhaloStarMetalFractionsMaxRad_Mg": subhalos["SubhaloStarMetalFractionsMaxRad"][i][6],
                            "SubhaloStarMetalFractionsMaxRad_Si": subhalos["SubhaloStarMetalFractionsMaxRad"][i][7],
                            "SubhaloStarMetalFractionsMaxRad_Fe": subhalos["SubhaloStarMetalFractionsMaxRad"][i][8],
                            "SubhaloStarMetalFractionsMaxRad_total": subhalos["SubhaloStarMetalFractionsMaxRad"][i][9],
                            "SubhaloStellarPhotometrics_U": subhalos["SubhaloStellarPhotometrics"][i][0],
                            "SubhaloStellarPhotometrics_B": subhalos["SubhaloStellarPhotometrics"][i][1],
                            "SubhaloStellarPhotometrics_V": subhalos["SubhaloStellarPhotometrics"][i][2],
                            "SubhaloStellarPhotometrics_K": subhalos["SubhaloStellarPhotometrics"][i][3],
                            "SubhaloStellarPhotometrics_g": subhalos["SubhaloStellarPhotometrics"][i][4],
                            "SubhaloStellarPhotometrics_r": subhalos["SubhaloStellarPhotometrics"][i][5],
                            "SubhaloStellarPhotometrics_i": subhalos["SubhaloStellarPhotometrics"][i][6],
                            "SubhaloStellarPhotometrics_z": subhalos["SubhaloStellarPhotometrics"][i][7],

                            "SubhaloStellarPhotometricsMassInRad": subhalos["SubhaloStellarPhotometricsMassInRad"][i],
                            "SubhaloStellarPhotometricsRad": subhalos["SubhaloStellarPhotometricsRad"][i],
                            "SubhaloVel_x": subhalos["SubhaloVel"][i][0],
                            "SubhaloVel_y": subhalos["SubhaloVel"][i][1],
                            "SubhaloVel_z": subhalos["SubhaloVel"][i][2],
                            "SubhaloVelDisp": subhalos["SubhaloVelDisp"][i],
                            "SubhaloVmax": subhalos["SubhaloVmax"][i],
                            "SubhaloVmaxRad": subhalos["SubhaloVmaxRad"][i],
                            "SubhaloWindMass": subhalos["SubhaloWindMass"][i],
                        }
                        yield doc

                print("Loading dataset...")

                print("Indexing FoF Halos...")
                successes = 0
                for ok, action in streaming_bulk(
                    client=ELASTICSEARCH, index="camels", actions=generate_FoF_halos(),
                ):
                    successes += ok
                print("Indexed %d FoF halo documents" % (successes))

                ELASTICSEARCH.indices.refresh(index="camels")

                print("Indexing Subhalos...")
                successes = 0
                for ok, action in streaming_bulk(
                    client=ELASTICSEARCH, index="camels_sub", actions=generate_subhalos(),
                ):
                    successes += ok
                print("Indexed %d Subfind Subhalo documents" % (successes))

                ELASTICSEARCH.indices.refresh(index="camels_sub")

