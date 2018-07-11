#!/usr/bin/python

import sys
import time
import inspect, os
import random
import math

projectPath = sys.argv[1]
projectName = os.path.basename(sys.argv[1])
numOfElements = 0
numOfNodes = 0
maxTime = 5 #Replace with time steps
#timeStep = 1 #notAvailableYet

i=0
with open(projectName+".dat") as f:
    for line in f:
        if i < 6:# We only want the 7th line
            i=i+1
        else:
            [numOfElements,numOfNodes] = map(int, line.split())
            break

f.close()
print(numOfNodes)

resultFileHeader = """GiD Post Results File 1.1

# encoding utf-8

GaussPoints "tet4_element_gp" ElemType Tetrahedra
  Number Of Gauss Points: 4
Natural Coordinates: Given
    0.58541    0.138197    0.138197
    0.138197    0.58541    0.138197
    0.138197    0.138197    0.58541
    0.138197    0.138197    0.138197
End gausspoints
"""

def generateResFile(file):
    for i in range(1,maxTime):
        file.write("Result \"VELOCITY\" \"alphaSolver\" " + str(i) + " Vector OnNodes\n")
        file.write("ComponentNames \"X-VELOCITY\", \"Y-VELOCITY\", \"Z-VELOCITY\", \"|VELOCITY|\"\n")
        file.write("Values\n")
        for j in range(1,numOfNodes+1):
            file.write("\t" + str(j))
            x=random.uniform(0, 9)# Replace with X value from FEM analysis
            y=random.uniform(0, 9)# Replace with Y value from FEM analysis
            z=random.uniform(0, 9)# Replace with Z value from FEM analysis
            res=math.sqrt(pow(x,2)+pow(y,2)+pow(z,2)) #This one is the absolute value of the vector
            file.write("\t" + str(x) + "\t" + str(y) + "\t" + str(z) + "\t" + str(res))
            file.write("\n")
        file.write("End Values\n")
        file.write("\n")
        
        file.write("Result \"PRESSURE\" \"alphaSolver\" " + str(i) + " Scalar OnNodes\n")
        file.write("ComponentNames \"PRESSURE\"\n")
        file.write("Values\n")
        for j in range(1,numOfNodes+1):
            file.write("\t" + str(j))
            p=random.uniform(0, 9)# Replace with P value from FEM analysis
            file.write("\t" + str(p))
            file.write("\n")
        file.write("End Values\n")
        file.write("\n")


logFile = open("log.txt","w")
resFile = open(projectName+".post.res","w") 
logFile.write(str(sys.argv)+"\n")
logFile.write(projectName+"\n")
logFile.write(projectPath+".dat\n")
logFile.write(resultFileHeader+"\n")
resFile.write(resultFileHeader+"\n")

generateResFile(resFile)

resFile.close()
logFile.close()
