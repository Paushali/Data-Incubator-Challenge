
library(data.tree)
m <- matrix(data=1:9,nrow=3,ncol=3)
m<-t(m)
m1<-matrix(data=c(NA,0,NA),nrow=1, ncol=3)
m<-rbind(m,m1)
a=4
b=2
i<- m[a,b]
mod_val <-c()
sum=0

#For 10 Moves
for(Ti in 1:10){
            p<-a
            q<-b
            #Possible moves
            ifelse (a-2>0 && b+1<4,x<-(m[a-2,b+1]),x<-NA)
            ifelse (a-2>0 && b-1>0,y<- (m[a-2,b-1]),y<-NA)
            ifelse(a+2<5 && b+1<4,z<- (m[a+2,b+1]),z<-NA)
            ifelse(a+2<5 && b-1>0,j<-(m[a+2,b-1]),j<-NA)
            ifelse(a+1<5 && b+2<4,k<- (m[a+1,b+2]),k<-NA)
            ifelse(a+1<5 && b-2>0,l<- (m[a+1,b-2]),l<-NA)
            ifelse(a-1>0 && b+2<4,t<- (m[a-1,b+2]),t<-NA)
            ifelse(a-1>0 && b-2>0,u<-(m[a-1,b-2]),u<-NA)
            kn<-c(x,y,z,j,k,l,t,u)
            kn<-kn[!is.na(kn)]
            
            knight<-sample(kn,1)
            sum=sum+knight
            mod<-sum%%10
            print(mod)
            mod_val<-c(mod_val,mod)
            sd_val<-(sd(mod_val))
            
            if (!is.na(x) && knight == x)
            {
                a<-a-2
                b<-b+1
            }
            else if(!is.na(y) && knight==y){
                a<-a-2
                b<-b-1
            }
            else if(!is.na(z) && knight==z){
                a<-a+2
                b<-b+1
            }
            else if(!is.na(j) && knight==j){
                a<-a+2
                b<-b-1
            }
            else if(!is.na(k) && knight==k){
                a<-a+1
                b<-b+2
            }
            else if(!is.na(l) && knight==l){
                a<-a+1
                b<-b-2
            }
            else if(!is.na(t) && knight==t){
                a<-a-1
                b<-b+2
            }
            else if(!is.na(u) && knight==u){
                a<-a-1
                b<-b-2
            }
            
        #}
    #}

}
print(sum) #S calculated

print(sd_val) #Standard deviation calculated

#1024 moves
for(Ti in 1:1024){
    p<-a
    q<-b
    #Possible moves calculated
    ifelse (a-2>0 && b+1<4,x<-(m[a-2,b+1]),x<-NA)
    ifelse (a-2>0 && b-1>0,y<- (m[a-2,b-1]),y<-NA)
    ifelse(a+2<5 && b+1<4,z<- (m[a+2,b+1]),z<-NA)
    ifelse(a+2<5 && b-1>0,j<-(m[a+2,b-1]),j<-NA)
    ifelse(a+1<5 && b+2<4,k<- (m[a+1,b+2]),k<-NA)
    ifelse(a+1<5 && b-2>0,l<- (m[a+1,b-2]),l<-NA)
    ifelse(a-1>0 && b+2<4,t<- (m[a-1,b+2]),t<-NA)
    ifelse(a-1>0 && b-2>0,u<-(m[a-1,b-2]),u<-NA)
    kn<-c(x,y,z,j,k,l,t,u)
    kn<-kn[!is.na(kn)]
    
    knight<-sample(kn,1)
    sum=sum+knight
    mod<-sum%%1024
    print(mod)
    mod_val<-c(mod_val,mod)
    sd_val<-(sd(mod_val))
    
    if (!is.na(x) && knight == x)
    {
        a<-a-2
        b<-b+1
    }
    else if(!is.na(y) && knight==y){
        a<-a-2
        b<-b-1
    }
    else if(!is.na(z) && knight==z){
        a<-a+2
        b<-b+1
    }
    else if(!is.na(j) && knight==j){
        a<-a+2
        b<-b-1
    }
    else if(!is.na(k) && knight==k){
        a<-a+1
        b<-b+2
    }
    else if(!is.na(l) && knight==l){
        a<-a+1
        b<-b-2
    }
    else if(!is.na(t) && knight==t){
        a<-a-1
        b<-b+2
    }
    else if(!is.na(u) && knight==u){
        a<-a-1
        b<-b-2
    }
    

    
}
print(sum) #Prints S

print(sd_val) #Prints Standard deviation

