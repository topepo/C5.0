################################################################
## Date:         8-30-2014
## Programmer:   Mark V. Culp
## Description:  Figure out how to make an rpart tree in pary
##               by brute force. This is the first step to
##               getting C5.0 into party.
################################################################

model.frame.C5.0<-function (formula, ...) {
  if (!is.null(formula$model))
    return(formula$model)
  mf <- formula$call
  mf <- mf[c(1L, match(c("formula", "data", "subset", "na.action",
                         "weights"), names(mf), 0L))]
  if (is.null(mf$na.action))
    mf$na.action <- na.omit
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  env <- if (!is.null(environment(formula$terms)))
    environment(formula$terms)
  else parent.frame()
  mf <- eval(mf, env)
  
  rsp<-strsplit(paste(formula$call[2])," ")[[1]][1]
  tmp<-mf[rsp]
  mf[,1:length(formula$pred)]<-mf[formula$pred]
  mf[,length(formula$pred)+1]<-tmp
  names(mf)<-c(formula$pred,rsp)
  
  return(mf)
}
as.party.C5.0<-function(obj,...){
  out<-strsplit( obj$output, "\n")[[1]]
  indx<-1:which(out=="Decision tree:")
  out<-out[-indx]
  out<-out[out!=""]	
  out<-out[grep("^\t",out,invert=TRUE)]
  l1=length(out)
  out<-out[1:(l1-2)]
  
  is.default<-length(grep("default",paste(obj$call[1])))>0
  if(!is.default){
    mf<-model.frame(obj)
  }else{
    mf<-data.frame(x=eval(parse(text=paste(obj$call)[2])),y=eval(parse(text=paste(obj$call)[3])))
    names(mf)[-c(length(obj$pred)+1)]<-obj$pred
  }
  
  indvars<-sapply(1:length(out),function(i){
    a1<-strsplit(out[i]," ")[[1]]
    
    ind<-a1==""
    if(sum(ind)>0)
      a1<-a1[!ind]
    for(j in 1:length(obj$pred)){
      ind<-which(sapply(1:length(obj$pred),function(i)grepl(obj$pred[i],a1[j])))
      if(length(ind)>0)
        break
    }
    if(length(ind)>1)
      ind=max(ind)
    ind
  })
  
  
  treestr<-sapply(1:length(out),function(i)nchar(strsplit(out[[i]],obj$predictors[indvars[i]])[[1]][1]))
  treestr<-as.numeric(as.factor(treestr))
  
  indclass<-sapply(1:length(out),function(i){
    a=obj$predictor[indvars[i]]
    v1<-gsub(":","",gsub(paste(" *",a,sep=""),"",out[[i]]))
    a1<-strsplit(v1," ")[[1]]
    ind<-a1==""
    if(sum(ind)>0)
      a1<-a1[!ind]
    ind<-a1=="..."
    if(sum(ind)>0)
      a1<-a1[!ind]
    if(length(a1)>2){
      a1<-match(obj$lev,a1[3])
      which(!is.na(a1))
    }else{
      NA
    }
  })
  
  cuts<-sapply(1:length(out),function(i){
    a=obj$predictor[indvars[i]]
    v1<-gsub(":","",gsub(paste(" *",a,sep=""),"",out[[i]]))
    a1<-strsplit(v1," ")[[1]]
    ind<-a1==""
    if(sum(ind)>0)
      a1<-a1[!ind]
    ind<-a1=="..."
    if(sum(ind)>0)
      a1<-a1[!ind]
    
    a1[2]
  })
  
  vars<-sapply(1:length(out),function(i){
    a=obj$predictor[indvars[i]]
    v1<-gsub(":","",gsub(paste(" *",a,sep=""),"",out[[i]]))
    a1<-strsplit(v1," ")[[1]]
    ind<-a1==""
    if(sum(ind)>0)
      a1<-a1[!ind]
    ind<-a1=="..."
    if(sum(ind)>0)
      a1<-a1[!ind]
    strsplit(a1,"=")[[1]][1]
  })
  
  n.cat<-sapply(1:length(obj$pred),function(i)is.factor(mf[,obj$pred[i]]))
  xlevels<-list()
  xlevs<-list()
  if(sum(n.cat)>0){
    r1=1
    for(i in 1:length(n.cat)){
      if(n.cat[i]){
        xlevels[[r1]]<-list(varid=obj$pred[i],lev=levels(mf[,obj$pred[i]]))
        r1=r1+1
      }
    }
  }
  
  c5.split<-function(i,j,r,k=NULL){  ##i=variable, j=cuts, r=TRUE,xlevs
    if(!n.cat[i]){
      split1=partysplit(varid=as.integer(i),breaks=as.numeric(j[1]),right=r,info=k,prob=NULL)
    }else{
      ind1<-match(obj$pred[i],sapply(1:length(xlevels),function(i)xlevels[[i]]$varid))
      xlev<-xlevels[[ind1]]$lev
      lj=length(j)
      a1s=sapply(j,function(i)strsplit(i,","))
      
      a1s<-sapply(j,function(i1){
        a1=strsplit(i1,",")[[1]]
        if(length(a1)>1){
          a1[1]<-strsplit(a1[1],"\\{")[[1]][2]
          a1[length(a1)]<-strsplit(a1[length(a1)],"\\}")[[1]]
          a1
        }else{
          a1
        }
      })
      index=rep(0L,length(xlev))
      for(i1 in 1:lj){
        index[match(a1s[[i1]],xlev)]=as.integer(i1)
      }
      if(any(index<1L))index[index==0L]=NA
      split1=partysplit(varid=as.integer(i),index=index,info=k,prob=NULL)
    }
    split1
  }
  
  c5.node<-function(tvec,vvec,bvec,vvars){
    if(length(tvec)==1 | any(tvec<0)){
      return(partynode(1L))
    }
    l<-list()
    ind<-which(tvec==1)
    ind2<-!vvars[ind[1]]==">"
    split1<-c5.split(vvec[ind[1]],bvec[ind],TRUE)  
    
    lind<-length(ind)
    for(i in 1:lind){
      str=ind[i]
      if(i==lind){
        term=length(tvec)
      }else{
        term=ind[i+1]-1
      }
      val=ind[i]:term
      
      l[[i]]=list(tvec=tvec[val]-1,vec=vvec[val],bvec=bvec[val],
                  vvars=vvars[val])
    }
    if(!ind2){
      tmp=l[[1]]
      l[[1]]=l[[2]]
      l[[2]]=tmp
    }
    partynode(1L,split=split1,
              kids=lapply(1:length(l),function(i){i=l[[i]];c5.node(i$tvec,i$vec,i$bvec,i$vvars)}))
  }
  pn<-as.partynode(c5.node(treestr,indvars,cuts,vars), from = 1L)
  if(is.default){
    p<-dim(mf)[2]
    fn=as.formula(paste("y ~ ",paste(obj$pred,collapse=" + "),sep=""))
    g7<-party(pn,data=mf[0L,],fitted = data.frame(
      "(fitted)" = fitted_node(pn, data = mf[,-p]),
      "(response)" = mf[,p],check.names = FALSE),terms=terms(fn))
  }else{
    C5.0_fitted <- function() {
      ret <- as.data.frame(matrix(nrow = NROW(mf), ncol = 0))
      ret[["(fitted)"]] <- fitted_node(pn, data = mf)
      ret[["(response)"]] <- mf[,dim(mf)[2]]
      ret[["(weights)"]] <- model.weights(mf)
      ret
    }
    fitted <- C5.0_fitted()
    g7<-party(pn,data=mf[0L,],fitted = fitted,terms=terms(mf),
              info=list(method="C5.0"))
  }
  
  class(g7) <- c("constparty", class(g7))
  g7
}

