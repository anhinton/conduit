#!/usr/bin/python

## CONDUIT: import modules
import os
import pickle
from platform import python_version
from distutils.version import LooseVersion

## CONDUIT: checking language version
version = {'minVersion':LooseVersion(''),
           'maxVersion':LooseVersion(''),
           'version':LooseVersion('')}
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

## python3
with open('.languageVersion', 'w', encoding='UTF-8') as outFile:
    n = outFile.write(languageVersion + '\n')

## python2
with open('.languageVersion', 'w') as outFile:
    n = outFile.write(languageVersion + '\n')

