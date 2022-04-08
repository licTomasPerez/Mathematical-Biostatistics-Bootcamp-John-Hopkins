#!/usr/bin/env python
# coding: utf-8

# In[29]:


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


from sympy import *
from numpy import *
init_printing(use_unicode=False, wrap_line=False)
x = Symbol('x')
e**(-x)


# In[16]:


positive_inf=float('inf')
print("positive_inf:",positive_inf)


# In[32]:


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

#print('The pdf is normalized since', integrate(e**(-x)*(1+e**(-x))**(-2), (x,neg_infinity,pos_infinity)))
#print('The result of integral is', integrate(e**(-x)*(1+e**(-x))**(-2), (x,neg_infinity,x_p)))
solvers.solve(integrate(, (x,1,x_p))-x_p,x_p)


# In[ ]:




