==================================================================
                        General Data File
==================================================================
Units:
length *Units(length) mass *Units(mass)
Number of elements and nodes:
*nelem *npoin

.................................................................

Node		X *Units(length)	Y *Units(length)	Z *Units(length)
Coordinates:
*set elems(all)
*loop nodes
*NodesNum *NodesCoord(1) *NodesCoord(2) *NodesCoord(3)
*end nodes

.................................................................

Element    Node(1)   Node(2)   Node(3)	Node(4)	  Material
Connectivities:
*loop elems
*ElemsNum *ElemsConec *ElemsMat 
*end elems

.................................................................

Materials:
*nmats
Material      Surface density *Units(surface_density)
*loop materials
*MatNum *MatProp(Superficial_density,real)
*end materials

.................................................................

*Set Cond No_slip *elems
No slip:
*CondNumEntities(int)
Elems
*loop elems *OnlyInCond
*ElemsNum 
*end nodes
.................................................................

*Set Cond Input_velocity *nodes
Input velocity:
*CondNumEntities(int)
Node   Velocity *Units(velocity)
*loop nodes *OnlyInCond
*NodesNum     *Cond(Velocity)
*end nodes
.................................................................

*Set Cond Output_velocity *nodes
Output Velocity:
*CondNumEntities(int)
Node   Mass *Units(velocity)
*loop nodes *OnlyInCond
*NodesNum     *Cond(Velocity)
*end nodes
