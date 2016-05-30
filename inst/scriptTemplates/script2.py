#!/usr/bin/python2

## CONDUIT: import modules
import os
import pickle
from platform import python_version
from distutils.version import LooseVersion

## CONDUIT: checking language version
version = {'minVersion':LooseVersion('{{minVersion}}'),
           'maxVersion':LooseVersion('{{maxVersion}}'),
           'version':LooseVersion('{{version}}')}
thisVersion = LooseVersion(python_version())
try:
    failMin = thisVersion < version['minVersion']
except AttributeError:
    failMin = False
try:
    failMax = thisVersion > version['maxVersion']
except AttributeError:
    failMax = False
try:
    failExact = thisVersion != version['version']
except AttributeError:
    failExact = False
languageVersion = [str(thisVersion), str(int(failMin)), str(int(failMax)),
                   str(int(failExact))]
languageVersion = '\n'.join(languageVersion)
with open('.languageVersion', 'w') as outFile:
    n = outFile.write(languageVersion + '\n')
