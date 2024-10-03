
## Ben's Code -- from https://github.com/ben-domingue/rt_meta/tree/master/rtmeta_pkg/R

gen_x_sdf = function(df){
  
  
  df = df %>% 
    mutate(firstButtonRT = firstButtonRT*1000,
           answered_correctly = as.numeric(answered_correctly)) %>% 
    filter(timepoint==0) %>% 
    filter(firstButtonRT>200)
  
  
  df<-df[,c("question_text","pid","answered_correctly","firstButtonRT",'class',
            'cohort','sbac_quartile',"ProblemType",'operation_type','sbac_decile')]
  df$pid -> df$id
  df$question_text -> df$item
  df$answered_correctly -> df$resp
  df$firstButtonRT -> df$rt
  NULL->df$firstButtonRT
  NULL->df$pid
  NULL->df$question_text
  NULL->df$answered_correctly
  
  df$rt<-log(df$rt/1000)
  x<-df[,c("id","item","resp","rt",'class',
           'cohort','sbac_quartile',"ProblemType",'operation_type','sbac_decile')]
  x<-x[rowSums(is.na(x))==0,]
  
  
  return(x)
}


mean_curvature <- function(x, y) {
  dx <- gradient(x)
  dy <- gradient(y)
  ddx <- gradient(dx)
  ddy <- gradient(dy)
  curvature <- (dx * ddy - dy * ddx) / ((dx^2 + dy^2)^(3/2))
  return(curvature)
}


##need to add rapid to all data for those that don't have a rapid rsponse flag
qc<-function(x,repeated.trials=FALSE) {
  ## We only study items with dichotomously coded responses.
  x<-x[!is.na(x$resp),]
  x<-x[x$resp %in% 0:1,]
  ## For those data with such flags, we excluded responses that were coded as being uninformative due to having occurred too rapidly.
  # x<-x[!x$rapid,]
  ## Timed out responses
  ##handled for individual datasets
  ## Removal of items that were never answered correctly or incorrectly
  m<-by(x$resp,x$item,mean,na.rm=TRUE)
  nms<-names(m)[m>0 & m<1]
  x<-x[x$item %in% nms,]
  ## Multiple responses
  if (!repeated.trials) {
    id<-paste(x$id,x$item)
    tab<-table(id)
    tab<-tab[tab==1]
    x<-x[id %in% names(tab),]
  }
  ## impose a max time limit: 5x the 90th percentile
  m0<-quantile(x$rt,.9,na.rm=TRUE)
  max.time<- 5*exp(m0)
  test <- x$rt<log(max.time)
  x<-x[test,]
  print(table(test))
  ## N response per item
  tab<-table(x$item)
  tab<-tab[tab>=20]
  x<-x[x$item %in% names(tab),]
  ## N responses per person
  tab<-table(x$id)
  ni<-length(unique(x$item)) ##
  tab<-tab[tab>=min(10,ni)] ##need to allow for smaller numbers of items when that is all that is available
  x<-x[x$id %in% names(tab),]
  ##
  x
}

irt<-function(x,lmer.flag=FALSE,combo=F) {
  if (lmer.flag) { ##lmer
    library(lme4)
    m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
    #m<-lmer(resp~0+(1|item)+(1|id),x)
    ranef(m)$item->fe
    item<-data.frame(item=rownames(fe),diff=-1*fe[,1])
    re<-ranef(m)$id
    stud<-data.frame(id=rownames(re),th=re[,1])
    x<-merge(x,item)
    x<-merge(x,stud)
  } else { ##mirt
    ##muck with item names
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
      z<-L[[i]]
      index<-match(z$id,id)
      resp<-rep(NA,length(id))
      resp[index]<-z$resp
      out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    ##
    library(mirt)
    index<-grep('id',names(resp))
    m<-mirt(resp[,-index],1,"Rasch")
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],diff=-1*co[,2])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
  }
  ##
  kk<-x$th-x$diff
  kk<-exp(kk)
  x$pv<-kk/(1+kk)
  ##
  x
}

interplay<-function(x,nms,std.time.in.item=FALSE,nspl=4,
                    fe.terms='item+id',nboot=NULL,filter=NULL) {
  ##x needs to have columns:
  ## item [item id]
  ## id [person id]
  ## diff [item difficulty]
  ## th [person theta]
  ## pv [irt-based p-value]
  ## rt [response time in metric you want to analyze]
  ## resp [item response]
  #####################################################################
  # nms<-c("item","id","diff","th","pv","rt",
  #        'carrying','digit_load','cohort','sbac_quartile')
  if (!(all(nms %in% names(x)))) stop("need more columns")
  ##standardizd item times within item
  if (std.time.in.item) {
    L<-split(x,x$item)
    std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
    for (i in 1:length(L)) {
      L[[i]]->y
      y$rt<-std(y$rt)
      L[[i]]<-y
    }
    x<-data.frame(do.call("rbind",L))
  }
  tmp<-x[,nms]
  x<-x[rowSums(is.na(tmp))==0,]
  #############################################################################
  library(splines)
  x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
  library(splines)
  
  if (!is.null(filter)){
    for (filt in filter){
      
      x = filter(x, eval(parse(text=filt)))
    }
  }
  
  bs(x$rt,df=nspl)->spl
  for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
  ##########################################################################
  ##now model accuracy
  modfun<-function(x,xv) {
    library(fixest) ##won't work on ozzy
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    ##fitted accuracy
    fe<-fixest::fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv<-mean(x$pv, na.rm=T)
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    z<-expand.grid(pv=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    pts<-cbind(z$rt,z$resp)
  }
  rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
  xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
  pts<-modfun(x,xv)
  ##bootstrap?
  if (!is.null(nboot)) {
    pb<-list()
    for (i in 1:nboot) {
      boot.index<-sample(1:nrow(x),nrow(x),replace=TRUE)
      pb[[i]]<-modfun(x[boot.index,],xv=xv)[,2]
    }
    pb<-do.call("cbind",pb)
    cil<-apply(pb,1,quantile,.025)
    cih<-apply(pb,1,quantile,.975)
    pts<-cbind(pts,cil,cih)
  }
  ##densities
  xcenter<-mean(rt.lims)
  c(-.2,.2)->rt.lims
  dens<-list()
  for (resp in 0:1) {
    den<-density(x$rt[x$resp==resp])
    scale.factor<-.25
    m<-min(den$y)
    dy<-den$y-m
    M<-max(den$y)
    dy<-dy/M
    dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
    dens[[as.character(resp)]]<-cbind(den$x,dy)
  }
  list(pts=pts,dens=dens)
}


interplay_center<-function(x,nms, std.time.in.item=FALSE,nspl=4,
                           fe.terms='item+id',nboot=NULL,filter=NULL) {
  ##x needs to have columns:
  ## item [item id]
  ## id [person id]
  ## diff [item difficulty]
  ## th [person theta]
  ## pv [irt-based p-value]
  ## rt [response time in metric you want to analyze]
  ## resp [item response]
  #####################################################################
  # nms<-c("item","id","diff","th","pv","rt",
  #        'carrying','digit_load','cohort','sbac_quartile')
  if (!(all(nms %in% names(x)))) stop("need more columns")
  ##standardizd item times within item
  if (std.time.in.item) {
    L<-split(x,x$item)
    std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
    for (i in 1:length(L)) {
      L[[i]]->y
      y$rt<-std(y$rt)
      L[[i]]<-y
    }
    x<-data.frame(do.call("rbind",L))
  }
  tmp<-x[,nms]
  x<-x[rowSums(is.na(tmp))==0,]
  #############################################################################
  library(splines)
  x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
  library(splines)
  
  if (!is.null(filter)){
    for (filt in filter){
      
      x = filter(x, eval(parse(text=filt)))
    }
  }
  
  bs(x$rt,df=nspl)->spl
  for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
  ##########################################################################
  ##now model accuracy
  modfun<-function(x,xv) {
    library(fixest) ##won't work on ozzy
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv.center+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    ##fitted accuracy
    fe<-fixest::fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv<-0
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    z$resp<-z$resp-mean(z$resp)
    pts<-cbind(z$rt,z$resp)
  }
  rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
  xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
  pts<-modfun(x,xv)
  ##bootstrap?
  if (!is.null(nboot)) {
    pb<-list()
    for (i in 1:nboot) {
      boot.index<-sample(1:nrow(x),nrow(x),replace=TRUE)
      pb[[i]]<-modfun(x[boot.index,],xv=xv)[,2]
    }
    pb<-do.call("cbind",pb)
    cil<-apply(pb,1,quantile,.025)
    cih<-apply(pb,1,quantile,.975)
    pts<-cbind(pts,cil,cih)
  }
  ##densities
  xcenter<-mean(rt.lims)
  c(-.2,.2)->rt.lims
  dens<-list()
  for (resp in 0:1) {
    den<-density(x$rt[x$resp==resp])
    scale.factor<-.25
    m<-min(den$y)
    dy<-den$y-m
    M<-max(den$y)
    dy<-dy/M
    dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
    dens[[as.character(resp)]]<-cbind(den$x,dy)
  }
  list(pts=pts,dens=dens)
}


plotSAT<-function(L,nm='',
                  tl=1000,
                  axtext=FALSE, #text on axes
                  legendtext=FALSE,
                  xl=c(-2.5,5.5),
                  line.col='blue',
                  plot.rt.density=TRUE,
                  lwd=1.5,
                  yax=TRUE,
                  ...
)
{
  plot(NULL,xlim=xl,ylim=c(-.18,.18),xlab='',ylab='',yaxt='n',...)
  if (yax) axis(side=2,at=c(-.1,0,.1))
  legend("topleft",bty='n',legend=nm,cex=.75)
  segments(tl,-100,tl,.1,col='gray',lwd=3)
  abline(h=0,col='gray')
  if (axtext) {
    mtext(side=1,'log(t)',line=2,cex=1)
    mtext(side=2,'Offset to Pr(x=1)',line=2,cex=1)
  }
  resp.col<-c("firebrick1","darkorchid")
  if (legendtext) {
    legend("bottomright",bty='n',c("Incorrect","Correct"),title="Density, log(t)",fill=resp.col,cex=.75)
  }
  if (plot.rt.density) {
    for (resp in 0:1) {
      den<-L$dens[[as.character(resp)]]
      col<-col2rgb(resp.col[resp+1])/255
      col<-rgb(col[1],col[2],col[3],alpha=.5)
      dy<-min(den[,2])
      polygon(c(den[,1],rev(den[,1])),c(rep(dy,nrow(den)),rev(den[,2])),col=col,border=NA)
    }
  }
  ##
  tmp<-L$pts
  lines(tmp[,1:2],col=line.col,lwd=lwd)
  if (ncol(tmp)>2) {
    col<-col2rgb("blue")/255
    col<-rgb(col[1],col[2],col[3],alpha=.5)
    polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,3],rev(tmp[,4])),col=col,border=NA)
  }
}

plot_sat_ggplot = function(carry_df, no_carry_dff){
  
  as.data.frame(carry_df$pts) %>% 
    rename('p(corr)'=V2,
           'log(rt)'=V1) %>% 
    mutate(carry='carry') %>% 
    rbind(as.data.frame(no_carry_dff$pts) %>% 
            rename('p(corr)'=V2,
                   'log(rt)'=V1) %>% 
            mutate(carry='nocarry')) %>% 
    ggplot(aes(x=`log(rt)`,y=`p(corr)`,color=carry))+
    geom_line()+
    geom_ribbon(aes(ymin=cil,ymax=cih,fill=carry),
                alpha = 0.2,colour = NA)+
    theme_bw()
  
}

person_analysis<-function(x) {
  id<-unique(x$id)
  if (length(id)>100000) {
    id<-sample(id,100000)
    x<-x[x$id %in% id,]
  }
  ##
  library(lme4)
  mm<-by(x$rt,x$item,mean,na.rm=TRUE)
  tmp<-data.frame(item=names(mm),m=as.numeric(mm))
  x<-merge(x,tmp)
  x$rt.centered<-x$rt-x$m
  m<-lmer(rt.centered~(1|id),x)
  re<-ranef(m)$id
  re<-data.frame(id=rownames(re),tau=-1*re[,1])
  x2<-x[!duplicated(x$id),]
  x2<-merge(x2,re)
  #x2<-x2[!duplicated(x2$id),]
  coors<-cor(x2$th,x2$tau,use='p')
  ##
  L<-split(x,x$item)
  nn<-sapply(L,nrow)
  L<-L[nn>50]
  ff<-function(y) {
    ec<-ecdf(y$rt)
    y$rank<-ec(y$rt)
    y
  }
  L<-lapply(L,ff)
  x<-data.frame(do.call("rbind",L))
  y<-x[,c("id","th","rank")]
  L<-split(y,y$id)
  f<-function(x) {
    z1<-mean(x$th,na.rm=TRUE)
    z2<-sd(x$rank,na.rm=TRUE)
    c(z1,z2)
  }
  tmp<-lapply(L,f)
  tmp<-do.call("rbind",tmp)
  m<-mean(tmp[,2],na.rm=TRUE)
  r<-cor.test(tmp[,1],tmp[,2],use='p')
  c(coors,m,r$est,r$conf.int)
}

pder<-function(x) {
  
  
  x<-x[order(x$rt.num),]
  dy<-diff(x$resp)
  dt<-diff(x$rt)
  pd<-dy/dt
  x$pd<-c(NA,pd)
  x = x %>% 
    mutate(inflection = if_else((lag(pd)>0 & pd < 0)&!is.na(pd), T,F)) 
  
  #   %>% 
  # filter(inflection==T)
}

pd<-function(x,nspl=4,std.time.in.item=FALSE,
             fe.terms='item+id',nboot=NULL,filter=NULL) {
  
  ##x needs to have columns:
  ## item [item id]
  ## id [person id]
  ## diff [item difficulty]
  ## th [person theta]
  ## pv [irt-based p-value]
  ## rt [response time in metric you want to analyze]
  ## resp [item response]
  #####################################################################
  nms<-c("item","id","diff","th","pv","rt",
         'digit_load','carrying','cohort','sbac_quartile')
  if (!(all(nms %in% names(x)))) stop("need more columns")
  ##standardizd item times within item
  if (std.time.in.item) {
    L<-split(x,x$item)
    std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
    for (i in 1:length(L)) {
      L[[i]]->y
      y$rt<-std(y$rt)
      L[[i]]<-y
    }
    x<-data.frame(do.call("rbind",L))
  }
  tmp<-x[,nms]
  x<-x[rowSums(is.na(tmp))==0,]
  #############################################################################
  library(splines)
  x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
  library(splines)
  
  if (!is.null(filter)){
    for (filt in filter){
      
      x = filter(x, eval(parse(text=filt)))
    }
  }
  
  bs(x$rt,df=nspl)->spl
  for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
  ##########################################################################
  ##now model accuracy
  modfun<-function(x,xv) {
    library(fixest) ##won't work on ozzy
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    ##fitted accuracy
    fe<-fixest::fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv=mean(x$pv,na.rm=TRUE)
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    z<-expand.grid(pv=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    # z$resp<-z$resp-mean(z$resp)
    z
  }
  rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
  xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
  z<-pder(modfun(x,xv))
  
  #bootstrap?
  if (!is.null(nboot)) {
    pb<-list()
    for (i in 1:nboot) {
      boot.index<-sample(1:nrow(x),nrow(x),replace=TRUE)
      pb[[i]]<-pder(modfun(x[boot.index,],xv=xv))$rt
    }
    pb<-do.call("cbind",pb)
    cil<-apply(pb,1,quantile,.025,na.rm=T)
    cih<-apply(pb,1,quantile,.975,na.rm=T)
    z$cil_pd<-cil
    z$cih_pd<-cih
  }
  z
}


pd_center<-function(x,nspl=4,std.time.in.item=FALSE,
                    fe.terms='item+id',nboot=NULL,filter=NULL) {
  
  ##x needs to have columns:
  ## item [item id]
  ## id [person id]
  ## diff [item difficulty]
  ## th [person theta]
  ## pv [irt-based p-value]
  ## rt [response time in metric you want to analyze]
  ## resp [item response]
  #####################################################################
  nms<-c("item","id","diff","th","pv","rt",
         'digit_load','carrying','cohort','sbac_quartile')
  if (!(all(nms %in% names(x)))) stop("need more columns")
  ##standardizd item times within item
  if (std.time.in.item) {
    L<-split(x,x$item)
    std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
    for (i in 1:length(L)) {
      L[[i]]->y
      y$rt<-std(y$rt)
      L[[i]]<-y
    }
    x<-data.frame(do.call("rbind",L))
  }
  tmp<-x[,nms]
  x<-x[rowSums(is.na(tmp))==0,]
  #############################################################################
  library(splines)
  x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
  library(splines)
  
  if (!is.null(filter)){
    for (filt in filter){
      
      x = filter(x, eval(parse(text=filt)))
    }
  }
  
  bs(x$rt,df=nspl)->spl
  for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
  ##########################################################################
  ##now model accuracy
  modfun<-function(x,xv) {
    library(fixest) ##won't work on ozzy
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv.center+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    ##fitted accuracy
    fe<-fixest::fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv<-0
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    # z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
    z<-expand.grid(pv=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    z$resp<-z$resp-mean(z$resp)
    z
  }
  rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
  xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
  z<-pder(modfun(x,xv))
  
  #bootstrap?
  if (!is.null(nboot)) {
    pb<-list()
    for (i in 1:nboot) {
      boot.index<-sample(1:nrow(x),nrow(x),replace=TRUE)
      pb[[i]]<-pder(modfun(x[boot.index,],xv=xv))$rt
    }
    pb<-do.call("cbind",pb)
    cil<-apply(pb,1,quantile,.025,na.rm=T)
    cih<-apply(pb,1,quantile,.975,na.rm=T)
    z$cil_pd<-cil
    z$cih_pd<-cih
  }
  
  z
}