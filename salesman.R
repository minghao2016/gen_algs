City <- setClass(
        #name of the class
        "City",
        
        #defining slots
        #in this case its coordinates
        
        slots = c(
                  x = "numeric",
                  y = "numeric"
                  ),
        #setting default values for the slots (optional)
        prototype = list(
                  x=0,
                  y=0
                  )
        )

#calculating distance from the city to another city
setGeneric(name="getDistance",
           def=function(theObject, city)
           {
             standardGeneric("getDistance")
           }
           )
setMethod(f="getDistance",
          signature = "City",
          definition = function(theObject, city)
          {
            xDis <- abs(theObject@x-city@x)
            yDis <- abs(theObject@y-city@y)
            distance <- sqrt(xDis^2+yDis^2)
            return(distance)
          }
          )

#generating city list
gen_city_list <- function(n_cities){
  city_list <- list()
  for(i in 1:n_cities){
    loc <- runif(2, 1,100)
    city <- City(x=loc[1], y=loc[2])
    city_list[[i]] <- city
  }
  return(city_list)
}


#Class fitness assigns evaluation function results to a route
Fitness <- setClass("Fitness",
                    slots=c(
                      route="list",
                      distance="numeric",
                      fitness="numeric"
                    ),
                    prototype = list(
                      route=list(),
                      distance = 0,
                      fitness = 0
                    )
                    )
#calculating route distance


setGeneric(name="routeDistance",
           def=function(theObject)
           {standardGeneric("routeDistance")}
            )
setMethod(f="routeDistance",
          signature = "Fitness",
          definition = function(theObject)
          {
            if(theObject@distance==0)
            {
              pathDistance <- 0
              for(i in 1:length(theObject@route))
              {
                fromCity <- theObject@route[i][[1]]
                toCity <- NaN
                if(i+1 < length(theObject@route))
                {
                  toCity <- theObject@route[i+1][[1]]
                } else {toCity <- theObject@route[1][[1]]}
                pathDistance <- pathDistance + getDistance(fromCity, toCity)
              }
              theObject@distance <- pathDistance
              theObject@fitness <- 1/theObject@distance
            }
            return(theObject) 
          }
          )
#just call a method on an obj like routeDistance(fitnessObj)

#calc fitness (just an inverse of distance)

setGeneric(name="routeFitness",
           def=function(theObject)
             {standardGeneric("routeFitness")})
setMethod(f="routeFitness",
          signature = "Fitness",
          definition = function(theObject)
            {if(theObject@fitness==0){
            distance <- routeDistance(theObject)@distance  
            theObject@distance <- distance
            theObject@fitness <- 1/distance
            }
          return(theObject)  
            }  
          )
          



#creating initial population
create_route <- function(city_list){
  route <- sample(city_list)
  return(route)
}

gen_init_pop <- function(pop_size, city_list)
              {
              pop <- list()
              for(i in 1:pop_size){
                pop[[i]] <- create_route(city_list)
                  }
              return(pop)
              }


rank_routes <- function(pop){
              ranked_pop <- list()
              ranking <- c()
              for(i in 1:length(pop)){
                route <- routeFitness(Fitness(route=pop[[i]]))
                ranked_pop[[i]] <- route
                ranking[i] <- route@fitness
              }
              ranked_pop <- rev(ranked_pop[order(ranking)])
              return(ranked_pop)
              }

selection <- function(popRanked, eliteSize){
              selectin_res <- list()
              elite <- round((length(popRanked)/2)*eliteSize,0)
              for(i in 1:elite){
                selection_res[[i]] <- popRanked[[i]]
              }
              random <- round(length(popRanked)/2) - elite
              for(i in 1:random){
                pick = sample(popRanked[elite:length(popRanked)],1)
                selectin_res[[elite+i]] <- pick
              }
              if(rem(length(selection_res),2)!=0){
                selectin_res <- selectin_res[-length(selectin_res)]
              }
              return(selectin_res)
              }

breed <- function(p1, p2){
            child <- vector("list", length=20)
            parent1 <- p1@route
            parent2 <- p2@route
            
            geneA <- as.integer(runif(1,0,1)*length(parent1))
            geneB <- as.integer(runif(1,0,1)*length(parent2))
            
            startGene <- max(min(geneA,geneB),1)
            endGene <- min(max(geneA,geneB),length(parent1))
            
            for(i in startGene:endGene){
              child[[i]] <- parent1[[i]]
            }
            
            if(startGene >=2){
              for(i in 1:(startGene-1)){
                gene <- parent2[[i]]
                if(list(gene) %in% child){next}
                else{
                  child[[i]]<-gene
                  }
                }
              }
            if(endGene<length(parent2)){
              for(i in (endGene+1):length(parent2)){
                gene <- parent2[[i]]
                if(list(gene) %in% child){next}
                else{
                  child[[i]]<-gene
                }
              }
            }
            new<- parent1[which(!child %in% parent1)]
            nulls <- c()
            for(i in 1:length(child)){if(is.null(child[[i]])){nulls <- c(rbind(nulls,c(i)))}}
            nulls <- unique(nulls)
            print(nulls)
            child[nulls] <- new
            return(child)
          }


