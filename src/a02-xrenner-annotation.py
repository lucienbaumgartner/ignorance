import xrenner
import glob
import re
import warnings
import numpy as np

warnings.filterwarnings("ignore", category=np.VisibleDeprecationWarning)

files = glob.glob("/Users/lucienbaumgartner/phd/projects/done/tc_methods_paper/output/animacy-annotation/depparse-2/*")
files = files[0:5]
print(files)

path = "/Users/lucienbaumgartner/phd/projects/done/tc_methods_paper/output/animacy-annotation/animacy_multicore/"

xrenner = xrenner.Xrenner(override='GUM')

for file in files:
    try:
        out = re.sub('.*\/|\.txt', '', file)

        output = xrenner.analyze(file, "conll")
        with open(path + out, "w") as outfile:
            outfile.write(output)

        output = xrenner.analyze(file, "html")
        with open(path + out + ".html", "w") as outfile:
            outfile.write(output)
    except:
        pass