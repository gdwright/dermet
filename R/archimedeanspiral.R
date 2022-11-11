archimedean.spiral<-function(x,y, colour="blue", lwd=2){
  
  Rx<-((x[1]^2)+(x[2]^2))^0.5
  Ry<-((y[1]^2)+(y[2]^2))^0.5
  
  #Find out inner point
  if(Rx<=Ry){
    Ra<-Rx
    Rb<-Ry
    a<-x
    b<-y
  } else if(Ry<Rx){
    Ra<-Ry
    Rb<-Rx
    a<-y
    b<-x
  }
  
  
  #calculate theta values.  If either value is origin then set theta values equal and line becomes straight. R=N+M0, M==0
  if(Ra!=0 & Rb!=0){
    thetaA<-acos(a[1]/Ra)
    thetaB<-acos(b[1]/Rb)
  } else if(Rb==0){
    thetaA<-acos(a[1]/Ra)
    thetaB<-thetaA
  } else if(Ra==0){
    thetaB<-acos(b[1]/Rb)
    thetaA<-thetaB    
  }
    
  #Check theta for below x axis
  if(a[2]<0){
    thetaA<-2*pi-thetaA
  }
  if(b[2]<0){
    thetaB<-2*pi-thetaB
  }
  
  #Check direction of spiral +ve->-ve or -ve->+ve
  if(thetaB<thetaA){
    thetaA<--2*pi+thetaA
    thetaB<--2*pi+thetaB
  }
  
  #Check spiral is shortest route
  if(abs(thetaA-thetaB)>pi){
    if(thetaA>=0){
      thetaA<-(-2)*pi+thetaA
      thetaB<-(-2)*pi-(2*pi-thetaB)
    } else {
      thetaB<-(-2)*pi+thetaB
      thetaA<-(-2)*pi-(2*pi-thetaA)
    }
  }
  
  if(thetaA==thetaB){
    lines(c(x[1],y[1]), c(x[2],y[2]), col=col, lwd=lwd)
  } else{
    
    
    #Archimedean Spiral R=N+M0 (0 represents theta, N&M constants)
    M<-(Ra-Rb)/(thetaA-thetaB)
    N<-Ra-M*thetaA
    
    Res<-round(abs(thetaA-thetaB)*(180/pi), digits=0)
    
    if(Res<2){
      Res<-2
    }
  
  
    data<-matrix(,Res,2)
    
    #R Values
    data[,1]<-seq(Ra, Rb, length.out=Res)
    
    #Theta Values
    if(M!=0){
      data[,2]<-(data[,1]-N)/M
    } else if(M==0){
      data[,2]<-seq(min(thetaA, thetaB), max(thetaA, thetaB), length.out=Res)
    }
    
    
    dataXY<-matrix(,Res,2)
    dataXY[,1]<-data[,1]*cos(data[,2])
    dataXY[,2]<-data[,1]*sin(data[,2])
    
  
  

    lines(dataXY[,1],dataXY[,2], col=colour, asp=1, lwd=lwd)
  }
  


}
