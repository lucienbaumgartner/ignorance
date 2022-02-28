import re
import stanza
from stanza.utils.conll import CoNLL
from stanza.models.common.doc import Document
from stanza_batch import batch
from typing import List
import glob
import toma
import time
from joblib import Parallel, delayed

def run_batch(batch_size: int, stanza_nlp: stanza.Pipeline, data: List[str]
              ) -> List[Document]:
    # So that we can see what the batch size changes to.
    print(batch_size)
    return [doc for doc in batch(data, stanza_nlp, batch_size=batch_size)]

def run_multicore(i):
    processed_document = nlp(corpus[i])
    out = re.sub('.*\/|\.txt', '', files[i])
    CoNLL.write_doc2conll(processed_document, path_multi + out + ".conllu")

files = glob.glob("/Users/lucienbaumgartner/phd/lab/Jobs/ignorance/output/animacy-annotation/raw/*")
#path_single = "/Users/lucienbaumgartner/phd/lab/Jobs/ignorance/output/animacy-annotation/depparse/"
path_single = "/Volumes/extICY/depparse/"
path_multi = "/Users/lucienbaumgartner/phd/lab/Jobs/ignorance/output/animacy-annotation/depparse-multi/"
print(files)

corpus = [open(file).read() for file in files]

assert(len(files) == len(corpus))

nlp = stanza.Pipeline()

start = time.time()
for i in range(1, len(corpus)):
    processed_document = nlp(corpus[i])
    out = re.sub('.*\/|\.txt', '', files[i])
    CoNLL.write_doc2conll(processed_document, path_single + out + ".conllu")
end = time.time()
print('{:.4f} s'.format(end-start))

"""
start = time.time()
Parallel(n_jobs=3)(delayed(run_multicore)(i) for i in range(1, len(corpus)))
end = time.time()
print('{:.4f} s'.format(end-start))
"""
