library("ggplot2")
library("httk")
my.new.data <- as.data.frame(c("A","B","C"),stringsAsFactors=FALSE)
my.new.data <- cbind(my.new.data,as.data.frame(c("111-11-5","222-22-0","333-33-5"),
                                               stringsAsFactors=FALSE))
my.new.data <- cbind(my.new.data,as.data.frame(c(200,200,200)))
my.new.data <- cbind(my.new.data,as.data.frame(c(2,3,4)))
my.new.data <- cbind(my.new.data,as.data.frame(c(0.01,0.02,0.3)))
my.new.data <- cbind(my.new.data,as.data.frame(c(0,10,100)))
colnames(my.new.data) <- c("Name","CASRN","MW","LogP","Fup","CLint")
chem.physical_and_invitro.data <- add_chemtable(my.new.data,
                                                current.table=chem.physical_and_invitro.data,
                                                data.list=list(
                                                  Compound="Name",
                                                  CAS="CASRN",
                                                  MW="MW",
                                                  logP="LogP",
                                                  Funbound.plasma="Fup",
                                                  Clint="CLint"),
                                                species="Human",
                                                reference="MyPaper 2015")
parameterize_pbtk(chem.name="C")
parameterize_3comp(chem.name="C")
parameterize_steadystate(chem.name="C")

calc_css(chem.name="B")

css <- calc_analytic_css(chem.name='B',output.units='uM',
                         model='3compartmentss',concentration='plasma')

out<-solve_pbtk(chem.name="B", day =50, doses.per.day = 3)
plot.data <- as.data.frame(out)
c.vs.t <- ggplot(plot.data,aes(time,Cplasma))+geom_line()+
  geom_hline(yintercept = css)
print(c.vs.t)

#theophyline
out<-solve_pbtk(chem.cas="58-55-9", day =2, doses.per.day = 1) 
plot.data <- as.data.frame(out)
css <- calc_analytic_css(chem.cas="58-55-9",output.units='uM',
                         model='3compartmentss',concentration='plasma')
c.vs.t <- ggplot(plot.data,aes(time,Cplasma))+geom_line()+
  geom_hline(yintercept = css)

plot(plot.data$Cven, plot.data$Cplasma)

#Acetaminophen
which(chem.dt$chemcas == "103-90-2")
out<-solve_pbtk(chem.cas = "103-90-2", day =50, doses.per.day = 1)
plot.data <- as.data.frame(out)
css <- calc_analytic_css(chem.cas="103-90-2",output.units='uM',
                         model='3compartmentss',concentration='plasma')
c.vs.t <- ggplot(plot.data,aes(time,Cplasma))+geom_line()+
  geom_hline(yintercept = css)

# Midazolam
which(chem.dt$chemcas == "59467-70-8")
out<-solve_pbtk(chem.cas = "59467-70-8", day =50, doses.per.day = 1)
plot.data <- as.data.frame(out)
css <- calc_analytic_css(chem.cas="103-90-2",output.units='uM',
                         model='3compartmentss',concentration='plasma')
c.vs.t <- ggplot(plot.data,aes(time,Cplasma))+geom_line()+
  geom_hline(yintercept = css)
