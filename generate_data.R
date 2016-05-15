gridsize = 200
sets = 2
width = 40
n_hills = 10
stepsize = width / gridsize

func = function(x, y, mux, muy){
  term1 = -0.5*((x-mux)^2+(y-muy)^2)
  term2 = exp(term1)
  z = (1/pi)*term2
  return(z)
}

vertices = matrix(0, nrow = (gridsize+1)*(gridsize+1)*sets, ncol = 3)
counter = 1

for (i in 1:sets){
  
  centersx = list()
  centersy = list()
  for (h in 1:n_hills){
    centersx[[h]] = runif(1,-width/1.7 - width/4, +width/1.7 - width/4)
    centersy[[h]] = runif(1,-width/1.7 - width/4, +width/1.7 - width/4)
  }
  
  tempx = -width/2
  tempy = -width/2 + i*width
  for (y in 1:(gridsize+1)){
    tempy = -width/2 + (i-1)*width
    for (x in 1:(gridsize+1)){
      
      val = 0
      for (h in 1:n_hills){
        val = val+func(tempx, tempy, centersx[[h]], centersy[[h]])
      }
      
      vertices[counter,] = c(tempx, tempy, val)
      tempy = tempy+stepsize
      counter = counter+1
    }
    tempx = tempx+stepsize
  }
}

vertices = data.frame(vertices)
names(vertices) = c('x','y','val')

require(jsonlite)
vertices_ = toJSON(vertices)