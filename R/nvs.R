#library(bigmemory)

library(Matrix)
library(DataCombine)

advection <- 2
force <- c(1, 2, 3)
density <- 1

element_node <- read.table("example2.dat", skip=grep("Number of elements and nodes:", readLines("example2.dat")), nrows=1)

coordinates <- read.table("example2.dat", skip=grep("Coordinates:", readLines("example2.dat")), nrows=element_node[1, 2])

connection_table <- read.table("example2.dat", skip=grep("Connectivities:", readLines("example2.dat")), nrows=element_node[1, 1])

con <- file("parse.txt", open='r')
lines <- readLines(con)

k_local_str <- lines[[1]]
v_local_str <- lines[[2]]
b_local_str <- lines[[3]]

#k_global <- big.matrix(nrow=element_node[[2]]*6, ncol=element_node[[2]]*6, type="short", backingfile="k_global")
#b_global <- big.matrix(nrow=(element_node[[2]]*6)^2, ncol = 1, type="short", backingfile="b_global")

k_global <- Matrix(0, nrow=element_node[[2]]*6, ncol=element_node[[2]]*6, sparse = TRUE)
b_global <- Matrix(0, (nrow=element_node[[2]]*6)^2, ncol=1, sparse = TRUE)


dataGroup <- data.frame(ab = c(k_local_str, b_local_str))

get_k <- (function(advection, x1, x2, x3, x4, y1, y2, y3, y4, z1, z2, z3, z4, fx, fy, fz, density, a, b, c, d, e, f)
    matrix(eval(parse(text=k_local_str)), nrow=24, ncol=24)
)

get_b <- (function(advection, x1, x2, x3, x4, y1, y2, y3, y4, z1, z2, z3, z4, fx, fy, fz, density, a, b, c, d, e, f)
    matrix(eval(parse(text=b_local_str)), nrow=24, ncol=1)
)

start.time <- Sys.time()

for(i in 1:10){
#for(i in 1:element_node[[1]]){
    dot_1 <- coordinates[connection_table[i,2],2:4]
    dot_2 <- coordinates[connection_table[i,3],2:4]
    dot_3 <- coordinates[connection_table[i,4],2:4]
    dot_4 <- coordinates[connection_table[i,5],2:4]

    con_local <- connection_table[i,2:5]
    
    a <- min(dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]])
    b <- max(dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]])
    c <- min(dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]])
    d <- max(dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]])
    e <- max(dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]])
    f <- max(dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]])

    k_local <- get_k(advection, dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]], dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]], dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]], force[[1]], force[[2]], force[[3]], density, a, b, c, d, e, f)
    b_local <- get_b(advection, dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]], dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]], dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]], force[[1]], force[[2]], force[[3]], density, a, b, c, d, e, f)

    k_local_1 <- k_local[1:6,1:6]

    k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] =
        k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] + k_local_1

    k_local_2 <- k_local[1:6,7:12]    

    k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] =
        k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] + k_local_2

    k_local_3 <- k_local[7:12,1:6]

    k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] =
        k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] + k_local_3
    
    k_local_4 <- k_local[7:12,7:12]

    k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] =
        k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] + k_local_4
    
    k_local_5 <- k_local[13:18,13:18]

    k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] =
        k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] + k_local_5
        
    k_local_6 <- k_local[13:18,19:24]

    k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] =
        k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] + k_local_6

    k_local_7 <- k_local[19:24,13:18]

    k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] =
        k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] + k_local_7

    k_local_8 <- k_local[19:24,19:24]

    k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] =
        k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] + k_local_8
        
    b_local_1 <- b_local[1:6,]

    b_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6),] = b_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6),] + b_local_1
    
    b_local_2 <- b_local[7:12,]

    b_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6),] = b_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6),] + b_local_2
    
}

end.time <- Sys.time()
time.taken <- end.time - start.time
print("time total")
print(time.taken)
print(object.size(k_global))
print(object.size(b_global))

no_slip_count <- read.table("example2.dat", skip=grep("No slip 1:", readLines("example2.dat")), nrows=1)
in_velocity_count <- read.table("example2.dat", skip=grep("Input velocity:", readLines("example2.dat")), nrows=1)
output_velocity_count <- read.table("example2.dat", skip=grep("Output Velocity:", readLines("example2.dat")), nrows=1)

no_slip <- read.table("example2.dat", skip=(grep("No slip 1:", readLines("example2.dat"))+2), nrows=no_slip_count[[1]])
output_velocity <- read.table("example2.dat", skip=(grep("Output Velocity:", readLines("example2.dat"))+2), nrows=output_velocity_count[[1]])
input_velocity <- read.table("example2.dat", skip=(grep("Input velocity:", readLines("example2.dat"))+2), nrows=in_velocity_count[[1]])
