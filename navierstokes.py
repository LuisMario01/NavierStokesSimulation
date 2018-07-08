import sympy as sy

#Symbols to be used
firstPart = "result.txt"
secondPart = "result2.txt"

x, y, z, t = sy.var('x y z t')

fx, fy, fz = sy.var('fx fy fz')

x1, x2, x3, x4 = sy.var('x1 x2 x3 x4')
y1, y2, y3, y4 = sy.var('y1 y2 y3 y4')
z1, z2, z3, z4 = sy.var('z1 z2 z3 z4')

a, b, c, d, e, f = sy.var('a b c d e f')

vx1, vy1, vz1 = sy.var('vx1 vy1 vz1')
vx2, vy2, vz2 = sy.var('vx2 vy2 vz2')
vx3, vy3, vz3 = sy.var('vx3 vy3 vz3')
vx4, vy4, vz4 = sy.var('vx4 vy4 vz4')

p1, p2, p3, p4 = sy.var('p1 p2 p3 p4')

advection = sy.var('advection')
density = sy.var('density')

#Main function, get constants and send them to every function in charge of one of the equation's terms
def navier_stokes():
    ''' Ni stands for N integrated '''
    Ni = sy.Matrix([[-1/2,0,0,1/2,0,0,1/2,0,0,1/2,0,0],
                     [0,-1/2,0,0,1/2,0,0,1/2,0,0,1/2,0],
                     [0,0,-1/2,0,0,1/2,0,0,1/2,0,0,1/2]])
    
    ''' Nti stands for N integrated and transposed'''
    Nti = Ni.T
    
    force = sy.Matrix([fx, fy, fz])
    
    D = sy.Matrix([[(x2-x1),(x3-x1),(x4-x1)],[(y2-y1),(y3-y1),(y4-y1)],[(z2-z1),(z3-z1),(z4-z1)]]).det()

    '''deltaN is 3x12'''
    deltaN = sy.Matrix([[(x2-x1),(y2-y1),(z2-z1)],[(x3-x1),(y3-y1),(z3-z1)],[(x4-x1),(y4-y1),(z4-z1)]]).inv()*sy.Matrix([[-1,-1,-1,1,1,1,0,0,0,0,0,0],
                        [-1,-1,-1,0,0,0,1,1,1,0,0,0],
                        [-1,-1,-1,0,0,0,0,0,0,1,1,1]])
    V = sy.Matrix([vx1, vy1, vz1, vx2, vy2, vz2, vx3, vy3, vz3, vx4, vy4, vz4])

    P = sy.Matrix([p1/3, p1/3, p1/3, p2/3, p2/3, p2/3, p3/3, p3/3, p3/3, p4/3, p4/3, p4/3])

    '''Calculating each part of the locale equation'''
    res = sy.zeros(24)
    incognitas = sy.Matrix([V,P])
    
    res[0:12, 0:12] = advection_part(D, Nti, advection, deltaN)+double_delta_part(deltaN, sy.Matrix([x1, x2, x3, x4]),sy.Matrix([y1, y2, y3, y4]),sy.Matrix([z1, z2, z3, z4]))+last_part(D, Nti, deltaN)
    res[12:24,12:24] = density_part(density,D,Nti,deltaN)
    
    '''Analysis done with velocity only'''
    partB = V+(t*force_part(Nti,force,D))
    
    output = open(firstPart, 'w')
    output.write(str(res)+'*'+str(incognitas))
    output.close()
    
    output = open(secondPart, 'w')
    output.write(str(partB))
    output.close()
    print("Done")


#Nti = N, tranposed and integrated
def force_part(Nti, force, D):
    print("force")
    res = D*Nti*force
    return res
    
def advection_part(D, Nti, advection, deltaN):
    print("advection")
    res = advection*D*Nti*deltaN
    return res 

def density_part(density, D, Nti, deltaN):
    print("density")
    res = (D/density)*Nti*deltaN
    return res

def double_delta_part(deltaN, X, Y, Z):
    print("doubledelta")
    '''Definition of a,b,c,d,e,f will be done in R'''
    res = (b-a)*(d-c)*(f-e)*deltaN.transpose()*deltaN
    return res

def last_part(D, Nti, deltaN):
    print("last")
    res = D*Nti*deltaN
    return res

navier_stokes()

