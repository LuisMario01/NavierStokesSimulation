#library(bigmemory)

library(Matrix)

force <- c(1, 2, 3)
density <- 1
no_slip_vel <- 4

# TODO: READ TIME INTERVAL
# time interval 5 seconds, step 1 second.
# these have to be read from input file.
time_interval <- 5
time_step <- 0.5


element_node <- read.table("example2.dat", skip=grep("Number of elements and nodes:", readLines("example2.dat")), nrows=1)

coordinates <- read.table("example2.dat", skip=grep("Coordinates:", readLines("example2.dat")), nrows=element_node[1, 2])

connection_table <- read.table("example2.dat", skip=grep("Connectivities:", readLines("example2.dat")), nrows=element_node[1, 1])

con <- file("parse.txt", open='r')
lines <- readLines(con)

k_local_str <- lines[[1]]
v_local_str <- lines[[2]]
b_local_str <- lines[[3]]

## Reading conditions
no_slip_count <- read.table("example2.dat", skip=grep("No slip:", readLines("example2.dat")), nrows=1)
in_velocity_count <- read.table("example2.dat", skip=grep("Input velocity:", readLines("example2.dat")), nrows=1)
output_velocity_count <- read.table("example2.dat", skip=grep("Output Velocity:", readLines("example2.dat")), nrows=1)

## no_slip: list of all the elements that have a no slip condition
no_slip <- read.table("example2.dat", skip=(grep("No slip:", readLines("example2.dat"))+2), nrows=no_slip_count[[1]])

## output_velocity: list of all the nodes that have the output velocity condition
output_velocity <- read.table("example2.dat", skip=(grep("Output Velocity:", readLines("example2.dat"))+2), nrows=output_velocity_count[[1]])

## input_velocity: list of all the nodes that have the input velocity condition
input_velocity <- read.table("example2.dat", skip=(grep("Input velocity:", readLines("example2.dat"))+2), nrows=in_velocity_count[[1]])

## advection takes the input velocity in the first iteration of time
advection <- input_velocity[1,2]

#k_global <- big.matrix(nrow=element_node[[2]]*6, ncol=element_node[[2]]*6, type="short", backingfile="k_global")
#b_global <- big.matrix(nrow=(element_node[[2]]*6)^2, ncol = 1, type="short", backingfile="b_global")

k_global <- Matrix(0, nrow=element_node[[2]]*6, ncol=element_node[[2]]*6, sparse = TRUE)
b_global <- Matrix(0, nrow=element_node[[2]]*6, ncol=1, sparse = TRUE)

#dataGroup stores both k and to a have a unique reference of the information
dataGroup <- data.frame(ab = c(k_local_str, b_local_str))

get_k <- (function(advection, x1, x2, x3, x4, y1, y2, y3, y4, z1, z2, z3, z4, fx, fy, fz, density, a, b, c, d, e, f)
    matrix(eval(parse(text=k_local_str)), nrow=24, ncol=24)
)

get_b <- (function(advection, x1, x2, x3, x4, y1, y2, y3, y4, z1, z2, z3, z4, fx, fy, fz, density, a, b, c, d, e, f)
    matrix(eval(parse(text=b_local_str)), nrow=24, ncol=1)
)

start.time <- Sys.time()

for(i in 1:32){
##for(i in 1:element_node[[1]]){

    dot_1 <- coordinates[connection_table[i,2],2:4]
    dot_2 <- coordinates[connection_table[i,3],2:4]
    dot_3 <- coordinates[connection_table[i,4],2:4]
    dot_4 <- coordinates[connection_table[i,5],2:4]

    con_local <- connection_table[i,2:5]
    
    a <- min(dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]])
    b <- max(dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]])
    c <- min(dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]])
    d <- max(dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]])
    e <- min(dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]])
    f <- max(dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]])

    k_local <- get_k(advection, dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]], dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]], dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]], force[[1]], force[[2]], force[[3]], density, a, b, c, d, e, f)
    b_local <- get_b(advection, dot_1[[1]], dot_2[[1]], dot_3[[1]], dot_4[[1]], dot_1[[2]], dot_2[[2]], dot_3[[2]], dot_4[[2]], dot_1[[3]], dot_2[[3]], dot_3[[3]], dot_4[[3]], force[[1]], force[[2]], force[[3]], density, a, b, c, d, e, f)

    #Replacing NaN, Inf and -Inf ocurrences with 1, 1000 and -1000 respectively 
    k_local[is.nan(k_local)] = 1
    k_local[is.infinite(k_local) & k_local < 0] = -1000
    k_local[is.infinite(k_local) & k_local > 0] = 1000

    #print(k_local)
    #print(b_local)

    k_local_1 <- k_local[1:6,1:6]

    ## Putting first element in global matrix
    k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] =
        k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] + k_local_1

    k_local_2 <- k_local[1:6,7:12]    

    ## Putting second element in global matrix
    k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] =
        k_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] + k_local_2

    k_local_3 <- k_local[7:12,1:6]

    ## Putting third element in global matrix
    k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] =
        k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6)] + k_local_3
    
    k_local_4 <- k_local[7:12,7:12]

    ## Putting fourth element in global matrix
    k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] =
        k_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6), (1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6)] + k_local_4
    
    k_local_5 <- k_local[13:18,13:18]

    ## Putting fifth element in global matrix
    k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] =
        k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] + k_local_5
        
    k_local_6 <- k_local[13:18,19:24]

    ## Putting sixth element in global matrix
    k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] =
        k_global[(1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] + k_local_6

    k_local_7 <- k_local[19:24,13:18]

    ## Putting seventh element in global matrix
    k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] =
        k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[3]]-1)*6):(6+(con_local[[3]]-1)*6)] + k_local_7

    k_local_8 <- k_local[19:24,19:24]

    ## Putting eight element in global matrix
    k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] =
        k_global[(1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6), (1+(con_local[[4]]-1)*6):(6+(con_local[[4]]-1)*6)] + k_local_8
        
    b_local_1 <- b_local[1:6,]

    b_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6),] = b_global[(1+(con_local[[1]]-1)*6):(6+(con_local[[1]]-1)*6),] + b_local_1
    
    b_local_2 <- b_local[7:12,]

    b_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6),] = b_global[(1+(con_local[[2]]-1)*6):(6+(con_local[[2]]-1)*6),] + b_local_2
    
}
#End of for loop


print(object.size(k_global))
print(object.size(b_global))

## applying coditions
## Newmann condition

solution_tmp <- Matrix(0, (nrow=element_node[[2]]*6), ncol=1, sparse = TRUE)
removed_nodes <- NULL
## Utilities

scalarVector <- function(x) {
    x / sqrt(sum(x^2))
}
crossProduct <- function(ab,ac){
  abci = ab[2] * ac[3] - ac[2] * ab[3];
  abcj = ac[1] * ab[3] - ab[1] * ac[3];
  abck = ab[1] * ac[2] - ac[1] * ab[2];
  return (c(abci, abcj, abck))
}
perpen_vector <- function(p1, p2, p3){
    v1 <- p3 - p1
    v2 <- p2 - p1

    ## Unit vector perpendicular to the plane
    r_tmp <- crossProduct(v1, v2)
    r_tmp <- array(as.numeric(unlist(r_tmp)))
    result <- scalarVector(r_tmp)*no_slip_vel
}


for(i in 1:nrow(no_slip)){
    elem <- array( unlist(connection_table[no_slip[i,1],2:5]))
    
    p1 <- coordinates[elem[[1]],2:4]
    p2 <- coordinates[elem[[2]],2:4]
    p3 <- coordinates[elem[[3]],2:4]
    p4 <- coordinates[elem[[3]],2:4]

    v1 <- perpen_vector(p1, p2, p3)
    v2 <- perpen_vector(p1, p2, p4)
    v3 <- perpen_vector(p2, p3, p4)
    v4 <- perpen_vector(p1, p3, p4)

    min_z <- min(v1[3], v2[3], v3[3], v4[4])

    indexes <- c(1, 2, 3)
    result <- v1
    
    if(min_z %in% v2){
        indexes <- c(1, 2, 4)
        result <- v2
    } else if(min_z %in% v3){
        indexes <- c(2, 3, 4)
        result <- v3
    } else if(min_z %in% v4){
        indexes <- c(1, 3, 4)
        result <- v4
    }

    result <- result*no_slip_vel
    
    ## Storing results in a solution vector for later
    ## Adding new values (to be removed) to b vector on the right side
    for(ie in indexes){
        solution_tmp[(elem[ie]-1)*6+1]  <- solution_tmp[(elem[ie]-1)*6+1] + result[1]
        solution_tmp[(elem[ie]-1)*6+2]  <- solution_tmp[(elem[ie]-1)*6+2] + result[2]
        solution_tmp[(elem[ie]-1)*6+3]  <- solution_tmp[(elem[ie]-1)*6+3] + result[3]

        b_global <- b_global + k_global[,(elem[ie]-1)*6+1]*(-no_slip_vel) + k_global[,(elem[ie]-1)*6+2]*(-no_slip_vel) + k_global[,(elem[ie]-1)*6+3]*(-no_slip_vel)
    }

    ## Storing index of the removed nodes 
    removed_nodes <- cbind(removed_nodes, c( (elem[indexes[1]]-1)*6+1, (elem[indexes[1]]-1)*6+2, (elem[indexes[1]]-1)*6+3,
    (elem[indexes[2]]-1)*6+1, (elem[indexes[2]]-1)*6+2, (elem[indexes[2]]-1)*6+3,
    (elem[indexes[3]]-1)*6+1, (elem[indexes[3]]-1)*6+2, (elem[indexes[3]]-1)*6+3))
    
}

## Removing Columns and Rows in the Global matrix

removed_nodes <- as.numeric(unique(array(unlist(removed_nodes))))

k_global <- k_global[-removed_nodes, -removed_nodes]
b_global <- b_global[-removed_nodes,]

for(i in 1:nrow(k_global)){
    for(j in 1:ncol(k_global)){
        if(k_global[i,j]==0){
            k_global[i,j] <- runif(1,0.01,0.02)
        }
    }
}

s_global <- Matrix(0, nrow=length(b_global), ncol=, sparse = TRUE)
identity <- Diagonal(nrow(k_global))

s_global <- data.matrix(s_global)
## Filling solution with seed values. Velocities will be equal to 10X-Y-Z, pressure 0
i <- 1

while(i <= length(s_global)){
    s_global[i,] <- input_velocity[1,2]
    s_global[i+1,] <- input_velocity[1,2]
    s_global[i+2,] <- input_velocity[1,2]
    i=i+6
}

old_advection <- advection
new_advection <- advection

## Time calculations
if(time_step>0 && time_interval%%time_step==0){

    for (i in 1:(time_interval/time_step)){
        acum <- 0

        ## Calculating new value of advection
        j <- 1
        while(j <= length(s_global)){
            acum <- s_global[j,] + s_global[j+1,] + s_global[j+2,] + acum
            j=j+6
        }

        ## New advection  is the mean of all the velocities in vector s_global
        new_advection <- acum/(length(s_global)/3)
        factor <- new_advection/old_advection

        k_global <- factor*k_global
        b_global <- factor*b_global


        left <- solve(identity+(time_step*k_global))
        right <- s_global + (time_step*b_global)
        
        ## Calculating new value of s
        s_global = left%*%right

        print("/////////////////")
        print(i)
        print("::::")
        for(k in 1:length(s_global)){
            print(s_global[k,])
        }

        #TODO s_global here contains what should be written in file
    }
}


end.time <- Sys.time()
time.taken <- end.time - start.time
print("time total")
print(time.taken)