import xrenner
import glob
import re
import warnings
import numpy as np

warnings.filterwarnings("ignore", category=np.VisibleDeprecationWarning)

files = glob.glob("/Volumes/extICY/depparse/*")
#print(files)

path = "/Volumes/extICY/animacy/"

xrenner = xrenner.Xrenner(override='GUM')

for file in files:
    try:
        out = re.sub('.*\/|\.txt', '', file)

        output = xrenner.analyze(file, "mixed")
        with open(path + out + ".html", "w") as outfile:
            outfile.write(output[0])

        with open(path + out, "w") as outfile:
            outfile.write(output[1])
    except:
        pass

