#/usr/bin/env python
# -*- coding: utf-8 -*-
import os, sys
import numpy as np

# Given C1 having 3 elements, and R1 having 9 elements,
# create a function that returns a 4x4 numpy matrix, which starts as the identity matrix. Then, let the upper-left 3x3 being the rotation matrix, and the upper-right 3x1 being the translation matrix.
def gen_mat(C1, R1):
    M1 = np.matrix(np.identity(4))
    C1 = np.matrix(C1.split(" ")).reshape(3,1)
    R1 = np.matrix(R1.split(" ")).reshape(3,3)

    M1[0:3,0:3] = R1
    M1[0:3,3] = C1
    return M1

def pretty_print(name, M):
    print(name + ":")
    for i in range(4):
        for j in range(4):
            print("{:17.17g}".format(M[i,j]), end=" ")
        print("")

def breakup_print(M):
    # Let R be the upper-left 3x3 block of M
    R = M[0:3,0:3]

    # Let C be the upper-right 3x1 block of M
    C = M[0:3,3] 
    
    # Convert R to a row vector
    R = np.array(R).reshape(1,9)
    # pretty-print R as one row, with 17 digits of precision
    np.set_printoptions(precision=17)
    print("R: ", R)

    # Convert C3 to a row vector
    C = np.array(C).reshape(1,3)
    np.set_printoptions(precision=17)
    # pretty-print C as one row, with full precision
    print("C: ", C)

# this is pin1
C = "-2488118.469552923 2287764.1280680583 -274220.28648485913"
R =  "-0.17343567486904843 -0.43803448314906884 0.88206907793836997 0.90625431247298938 -0.42158247428640849 -0.031166303897952607 0.38551678018839541 0.79397355683100568 0.47008808030654703"
P1 = gen_mat(C, R)

pretty_print("Pin1", P1)

# This is pin2
C = "-2488118.7926293206 2287764.4251070749 -274220.32208977453"
R = "-0.25991051854215474 -0.47655152532168876 0.83984829943551098 0.88560026429331384 -0.4643273076209366 0.010598267835103925 0.38491387898432644 0.74652447723504944 0.5427177080713097"

P2 = gen_mat(C, R)
pretty_print("Pin2", P2)

# This is cah1
C = "0.68699080000000001 0.48720580000000002 -1.9719500000000001"
R = "0.68057751456381022 -0.0091146051483276831 0.73261939206535442 0.73265272655025282 0.00048167267266542639 -0.6806024883424443 0.00585055113876013 0.99995834511464876 0.0070056571328884773"
C1 = gen_mat(C, R)
pretty_print("Cah1", C1)

# This is cah2
C = "0.69277719999999998 0.4785546 -1.97197"
R = "0.62362113644769523 -0.0097034710047226684 0.7816665018872786 0.78170530291167484 0.00033753551580770376 -0.62364790236846801 0.0057877089985215683 0.99995286324918131 0.0077957487590938389"
C2 = gen_mat(C, R) # cam2_to_rover
pretty_print("Cah2", C2)

# pin1
C = "-2488118.469552923 2287764.1280680583 -274220.28648485913"
R = "-0.17343567486904843 -0.43803448314906884 0.88206907793836997 0.90625431247298938 -0.42158247428640849 -0.031166303897952607 0.38551678018839541 0.79397355683100568 0.47008808030654703"
P1 = gen_mat(C, R)
pretty_print("P1", P1)

# rover to world
C = "-2488118.469552923 2287764.1280680583 -274220.28648485913"
R = "0.67480819304704631 0.082770826395182351 0.73333682090458208 0.72042735181877138 0.14163062048319161 -0.67891472078084925 -0.16005728145877246 0.98645311983864947 0.035943135820662032"
R1 = gen_mat(C, R)
pretty_print("R1", R1)

# ale cah1
#
C = "0 0 0"
R = "0.6805775030067589 0.732652718621007 0.005850540123205708 0.009114613337673426 -0.00048166549424400915 -0.9999583313035884 0.7326194 -0.6806025 0.007005656"
Cah1 = gen_mat(C, R)
pretty_print("Cah1", Cah1)


pretty_print("inv(R1) * P1", np.matmul(np.linalg.inv(R1), P1))
# pretty_print("R1 * inv(P1)", np.matmul(R1, np.linalg.inv(P1)))

# pretty_print("inv(P1) * R1", np.matmul(np.linalg.inv(P1), R1))
# pretty_print("P1 * inv(R1)", np.matmul(P1, np.linalg.inv(R1)))


print("now here")
sys.exit(0)

 
# cam_to_world = rover_to_world * cam_to_rover
# rover_to_world = cam_to_world * np.linalg.inv(cam_to_rover)
rover1_to_world = np.matmul(P1, np.linalg.inv(C1))
pretty_print("Rover1 to world", rover1_to_world)

cam2_to_world = np.matmul(rover1_to_world, C2)

pretty_print("Cam2 to world", cam2_to_world)

breakup_print(cam2_to_world)

#rover2_to_world = np.matmul(P2, np.linalg.inv(C2))
#pretty_print("Rover2 to world", rover2_to_world)


sys.exit(0)

# This is pin44
#C1 = "-2488118.7926293206 2287764.4251070749 -274220.32208977453"
#R1 = "0.67480819304704631 0.08277082639518224 0.73333682090458208 0.72042735181877138 0.14163062048319153 -0.67891472078084925 -0.16005728145877235 0.98645311983864947 0.035943135820662095"

# this is pin3
C2 = "0.68699080000000001 0.48720580000000002 -1.9719500000000001"
R2 = "0.68057751456381022 -0.0091146051483276831 0.73261939206535442 0.73265272655025282 0.00048167267266542639 -0.6806024883424443 0.00585055113876013 0.99995834511464876 0.0070056571328884773"
C1_to_R = gen_mat(C2, R2)

# This is pin4
C2 = "0.69277719999999998 0.4785546 -1.97197"
R2 = "0.62362113644769523 -0.0097034710047226684 0.7816665018872786 0.78170530291167484 0.00033753551580770376 -0.62364790236846801 0.0057877089985215683 0.99995286324918131 0.0077957487590938389"
C2_to_R = gen_mat(C2, R2)

C2_to_C1 = np.matmul(np.linalg.inv(C1_to_R), C2_to_R)

M3 = C2_to_C1
#M3 = np.matmul(R2W, C2_to_C1)



# M1 = gen_mat(C1, R1)
# print("M1: ", M1)

# M2 = gen_mat(C2, R2)
# print("M2: ", M2)

# # Let M3 be M1 * M2, use the numpy matmul function to do this.
# # So, do camera to rover, then rover to world.
# M3 = np.matmul(M1, M2)
# #np.matmul(a, b)
# print("M3: ", M3)

# # Now, let C3 be the upper-right 3x1 of M3, and R3 be the upper-left 3x3 of M3.
C3= M3[0:3,3]
R3= M3[0:3,0:3]

# Will make the code below into a function
# # Convert C3 to a row vector
C3 = np.array(C3).reshape(1,3)
np.set_printoptions(precision=17)
# pretty-print C3 as one row, with full precision
print("C3: ", C3)

# Convert R3 to a row vector
R3 = np.array(R3).reshape(1,9)
# pretty-print R3 as one row, with 17 digits of precision
np.set_printoptions(precision=17)
print("R3: ", R3)





