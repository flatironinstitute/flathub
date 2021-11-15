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

                print("Found file. About to load halos")
                halos = il.groupcat.loadHalos(basepath, snapshot, fields=None)

                def generate_actions():
                    """ For each row of data, yields a single document.
                    This function is passed into the bulk()
                    helper to create many documents in sequence.
                    """
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

                print("Loading dataset...")

                print("Indexing documents...")
                successes = 0
                for ok, action in streaming_bulk(
                    client=ELASTICSEARCH, index="camels", actions=generate_actions(),
                ):
                    successes += ok
                print("Indexed %d documents" % (successes))

                ELASTICSEARCH.indices.refresh(index="camels")

