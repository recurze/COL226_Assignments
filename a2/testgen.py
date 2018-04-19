from random import *
intfunc=["Plus","Minus","Mul","Div","Mod","Pow","Abs"]
boolfunc=["Not","And","Orr","Imply","Equal","Great","Leequ","Grequ","Lesss"]
intvariables=["Var \"x\"","Const"]
boolvariables=["True","False"]
def genint(n):
	num=randint(-100,100)
	if num==0:
		num+=1
	v=intvariables[randint(1,1000)%2]
	if n==1:
		return str(v)+(" ({})".format(num))*(v=="Const")
	shuffle(intfunc)
	s=""
	i=n%7
	f=intfunc[i]
	if f=="Abs":
		s+=intfunc[i]+"({})".format(genint(n-1))
	else:
		s+=intfunc[i]+"({},{})".format(genint(n-1),v+(" ({})".format(num))*(v=="Const"));
	return s

def genbool(n):
	num=randint(-100,100)
	if num==0:
		num+=1
	if n==1:
		v=boolvariables[randint(1,1000)%2]
		return v
	shuffle(boolfunc)
	s=""
	i=n%9
	f=boolfunc[i]
	if f in "Not":
		s+=boolfunc[i]+"({})".format(genbool(n-1))
	else:
		if f=="And" or f=="Orr" or f=="Imply":
			v=boolvariables[randint(1,1000)%2]
			s+=boolfunc[i]+"({},{})".format(genbool(n-1),v);
		else:
			v=intvariables[randint(1,1000)%2]
			s+=boolfunc[i]+"({},{})".format(genint(n-1),v+(" ({})".format(num))*(v=="Const"));
	return s

for i in xrange(5):
	size=randint(1,5)
	if size&1:
		a=genbool(size)
	else:
		a=genint(size)
	print "let a{}={};;".format(i,a)
	# print "Printf.printf \"%B\\n\" ((eval a{}) = (execute ([],rho,compile a{})));;".format(i,i)
	print "if ((eval a{})<>(execute ([],rho,compile a{}))) then print_int {};;".format(i,i,i)
