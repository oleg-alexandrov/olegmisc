import matplotlib.pyplot as plt
import numpy as np

def func(x):
  return np.exp(2*x) / (x**2 - 5*x + 6)

x = np.linspace(3, 7, 100)
y = func(x)

dx = (7 - 3) / 10
x_trap = np.arange(3, 7 + dx, dx)
print("xtrap is ", x_trap)
print("ytrap is ", func(x_trap))

y_trap = func(x_trap)

# plt.plot(x, y, 'r')
# plt.axvline(x=0, color='blue', linewidth=2)
# plt.axhline(y=0, color='blue', linewidth=2)

for i in range(len(x_trap) - 1):
  plt.plot([x_trap[i], x_trap[i + 1]], [y_trap[i], y_trap[i + 1]], 'b-', label='Trapezoid')
  plt.plot([x_trap[i], x_trap[i]], [0, y_trap[i]], 'b-', label='Trapezoid')
  plt.plot([x_trap[i + 1], x_trap[i + 1]], [0, y_trap[i + 1]], 'b-', label='Trapezoid')

# Add the missing trapezoid on the left
plt.plot([3, x_trap[0]], [func(3), y_trap[0]], 'b-', label='Trapezoid')
plt.plot([3, 3], [0, func(3)], 'b-', label='Trapezoid')
plt.plot([x_trap[0], x_trap[0]], [0, y_trap[0]], 'b-', label='Trapezoid')

plt.xlabel('x')
plt.ylabel('f(x)')
plt.title('Integral of e^(2x)/(x^2-5x+6) from 3 to 7')
plt.grid(True)
# plt.legend()
plt.show()
