import os

os.system("type Halls\header.asm Halls\\rbank.asm Halls\lbank.asm Halls\ebank.asm Halls\sbank.asm > out.asm")
os.system("dasm out.asm -f3 -oHalls\\bin\halls.a26 -sHalls\\bin\halls.sym -lHalls\\bin\halls.lst")
os.system("rm out.asm")
os.system("\"C:\Program Files\Stella\stella\" Halls\\bin\halls.a26")