import numpy as np 
import matplotlib.pyplot as plt

size = 256

phi = np.loadtxt("Mag_pot.dat")
phi = phi.reshape((256,256))

field_hx = np.loadtxt("field_hx.dat")
field_hx = field_hx.reshape((256,256))

field_hy = np.loadtxt("field_hy.dat")
field_hy = field_hy.reshape((256,256))



plt.imshow(phi)
plt.colorbar()
plt.savefig("Magnetic_potential")
#plt.show()

plt.imshow(np.sqrt(field_hx**2+field_hy**2))
plt.colorbar()
plt.savefig("field")
#plt.show()

def f(x):
	return x
