#!/usr/bin/env python
# coding: utf-8

# In[29]:
# Quiz 1

from sympy import *
from numpy import *
from math import *
from sympy.solvers import solve

pos_infinity=math.inf
neg_infinity=-math.inf

init_printing(use_unicode=False, wrap_line=False)

x = Symbol('x')
x_p = Symbol('x_p')


#integrate(f, (x, a, b))

print('The pdf is normalized since', integrate(e**(-x)*(1+e**(-x))**(-2), (x,neg_infinity,pos_infinity)))
print('The result of integral is', integrate(e**(-x)*(1+e**(-x))**(-2), (x,neg_infinity,x_p)))
solvers.solve(integrate(e**(-x)*(1+e**(-x))**(-2), (x,neg_infinity,x_p))-x_p,x_p)


# In[11]:
# Quiz 1 - Week 1 - Ex 12
## Suppose that a density is of the form $(k + 1)x^k$ for some constant $k > 1$ and $0 \leq x \leq 1$. 
## What is the mean associated with this density?

from sympy import *
from numpy import *
from math import *
from sympy.solvers import solve

x = Symbol('x')

print('The pdf is normalized', integrate((k+1)*x**k,(x,0,1)))
print('The expected value is', integrate(x*(k+1)*x**k, (x,0,1)))


# In[16]:


positive_inf=float('inf')
print("positive_inf:",positive_inf)


# In[32]:
# Quiz 1 - Week 1 - Ex 17
##Quality control experts estimate that the time (in years) until a specific electronic part from an assembly line fails follows (a specific instance of) the Pareto density,
## $f(x) = \frac{3}{x^4}$  for $1 < x < \infty$. Which option is closest to the mean failure time?

from sympy import *
from numpy import *
from math import *

pos_infinity=math.inf
neg_infinity=-math.inf

x = Symbol('x')

print('The pdf is normalized since', integrate(3*x**-4, (x, 1, pos_infinity)))
print('The expected value is', integrate(3*x*x**-4, (x, 1, pos_infinity)))



