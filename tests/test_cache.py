import os
import pickle
import sys

ORG_CHANGE_DIR = os.path.dirname(
    os.path.dirname(os.path.realpath(sys.argv[0]))
)
print(ORG_CHANGE_DIR)
os.chdir(ORG_CHANGE_DIR)
WWW = os.path.dirname(ORG_CHANGE_DIR)
CACHE_PATH = os.path.join(WWW, ".orgchange_cache")

global_cache = {}
if os.path.exists(CACHE_PATH):
    with open(CACHE_PATH, "rb") as f:
        global_cache = pickle.load(f)

sample = list(global_cache.items())[0]
index = [
    (key, value) for key, value in list(global_cache.items()) if ".org" in key
]
breakpoint()
