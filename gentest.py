#! /usr/bin/env python3

import random
import os

allOps = ["ADD", "SUB", "MUL", "DIV", "REM", "PREINC", "PREDEC", "POSTINC", "POSTDEC", *["IDENTIFIER"]*3, *["CONSTANT"] * 3, "LPAR", "PLUS", "MINUS"]
allOps = [*(allOps*2), "ASSIGN"]
idenOps = ("LPAR", "IDENTIFIER")


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
        return ""
    
    ins, val, lhs, rhs = now
    
    line = ""

    if ins in ("ASSIGN", "ADD", "SUB", "MUL", "DIV", "REM" ,"PREINC", "PREDEC", "POSTINC", "POSTDEC", "PLUS", "MINUS", "LPAR"):
        line += "("
        line += printAST(lhs)
        line += symbols[ins]
        line += printAST(rhs)
        line += ')'
    elif ins in ("IDENTIFIER", "CONSTANT"):
        line += str(val)
    return line



while True:
    code = []

    for _ in range(random.randint(3, 200)):
        root = randomAST((*allOps, *["ASSIGN"]*10))
        line = printAST(root)
        code.append(line)

    open("test_tmp.c", "w").write("""
#include <stdio.h>

int main() {{
    int x = 2, y = 3, z = 5;
    {};
    printf("Ans: %d %d %d\\n", x, y, z);
}}
""".format(";\n\t".join(code)))

    res = os.system("gcc test_tmp.c -Werror=div-by-zero -Werror=sequence-point -fsanitize=undefined -o test.out &> /dev/null")
    if res == 0:
        print("\n".join(code))
        os.system("./test.out 1>&2")
        break