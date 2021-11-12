# Step 1: read data using illustris_python
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

SNAPSHOTS_TO_REDSHIFTS = 1 # TODO




for simulation_suite in SIMULATION_SUITES:
    for simulation_set, simulation_set_id_range in SIMULATION_SET_ID_RANGES.items():
        for simulation_set_id in range(simulation_set_id_range + 1):
            basepath = f"./Simulations/{simulation_suite}/{simulation_set}_{simulation_set_id}/"
            
            for snapshot in SNAPSHOTS:
                redshift = 1 # TODO determine from snapshot

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
                            "id": i, # TODO decide if necessary. If so, how to make unique across snapshots and all?
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
                            "GroupGasMetalFractions_Fi": halos["GroupGasMetalFractions"][i][8], # TODO Fe, not Fi
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
                            "GroupStarMetalFractions_Fi": halos["GroupStarMetalFractions"][i][8], # TODO Fe, not Fi
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

