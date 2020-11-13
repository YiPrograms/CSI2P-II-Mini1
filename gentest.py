#! /usr/bin/env python3

import random

allOps = ["ADD", "SUB", "MUL", "DIV", "REM", "PREINC", "PREDEC", "POSTINC", "POSTDEC", *["IDENTIFIER"]*3, *["CONSTANT"] * 3, "LPAR", "PLUS", "MINUS"]
allOps = [*(allOps*100), "ASSIGN"]
idenOps = ("ASSIGN", "LPAR", "LPAR", "LPAR", "IDENTIFIER", "IDENTIFIER", "IDENTIFIER")
idenOps = [*(idenOps*100), "ASSIGN"]


symbols = {"ASSIGN": '=', "ADD":'+', "SUB": '-', "MUL": '*', "DIV": '/', "REM": '%', "PREINC": '++', "PREDEC": '--', "POSTINC": '++', "POSTDEC": '--', "IDENTIFIER": '', "CONSTANT": '', "LPAR": '', "PLUS": '+', "MINUS": '-'}


def randomAST(allow):
    ins = random.choice(allow)
    root = [ins, 0, [], []]
    if ins == "ASSIGN":
        root[2] = randomAST(idenOps)
        root[3] = randomAST(allOps)
    elif ins in ("ADD", "SUB", "MUL", "DIV", "REM"):
        root[2] = randomAST(allOps)
        root[3] = randomAST(allOps)
    elif ins in ("PREINC", "PREDEC"):
        root[3] = randomAST(idenOps)
    elif ins in ("POSTINC", "POSTDEC"):
        root[2] = randomAST(idenOps)
    elif ins in ("PLUS", "MINUS"):
        root[3] = randomAST(allOps)
    elif ins == "LPAR":
        root[2] = randomAST(allow)
    elif ins == "IDENTIFIER":
        root[1] = random.choice(('x', 'y', 'z'))
    elif ins == "CONSTANT":
        root[1] = random.randint(0, 50000)

    return root

def printAST(now):
    if len(now) == 0:
        return
    
    ins, val, lhs, rhs = now
    
    if ins in ("ASSIGN", "ADD", "SUB", "MUL", "DIV", "REM" ,"PREINC", "PREDEC", "POSTINC", "POSTDEC", "PLUS", "MINUS"):
        print('(', end="")
        printAST(lhs)
        print(symbols[ins], end="")
        printAST(rhs)
        print(')', end="")
    elif ins == "LPAR":
        print('(', end="")
        printAST(lhs)
        print(')', end="")
    elif ins in ("IDENTIFIER", "CONSTANT"):
        print(val, end="")

root = randomAST(("ASSIGN", ))
printAST(root)
print()
