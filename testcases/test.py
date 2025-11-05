import filecmp
import os
if os.path.exists("temp"):
    os.system("rm -rf ./temp")
os.makedirs("temp")
for i in range(16):
    inst ="./code < testcases/basic-testcases/test"+str(i)+".in > temp/test"+str(i)+".out"
    print(inst)
    os.system(inst)
    if not filecmp.cmp("testcases/basic-testcases/test"+str(i)+".out","temp/test"+str(i)+".out"):
        f = open("testcases/basic-testcases/test" + str(i) + ".in", mode='r')
        title = f.readline()[1:]
        print("test", i, "wrong:", title)
os.system("rm -rf ./temp")
os.makedirs("temp")
for i in range(20):
    inst = "./code < testcases/bigint-testcases/BigIntegerTest" + str(i) + ".in > temp/test" + str(i) + ".out"
    print(inst)
    os.system(inst)
    if not filecmp.cmp("testcases/bigint-testcases/BigIntegerTest" + str(i) + ".out", "temp/test" + str(i) + ".out"):
        print("big integer test", i, "wrong")
