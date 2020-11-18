#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
#define NULLREG -300

typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, LOAD, STORE
} Kind;

typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;

typedef enum {
	EMPTY, REGISTER, VALUE, ADDRESS
} OperandType;

typedef enum {
	FREE, USED, IDEN
} RegisterState;

typedef struct TokenUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;

typedef struct ASTUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;

typedef struct ISAUnit {
	Kind ins; // Reuse Kind enum
	OperandType o1_type;
	unsigned int o1; // Register from 1 to 256
	OperandType o2_type;
	unsigned int o2;
	OperandType o3_type;
	unsigned int o3;
	int redundant;
	int deleted;
	int first_run, last_res, is_const;
} ISA;



/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
int DEBUG = 0;
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);
// Generate ASM code.
int codegen(AST *root);
// Free the whole AST.
void freeAST(AST *now);
// Add ASM code
void addISA(Kind ins, int o1, int o2, int o3);
// Print single instruction
void printISA(ISA *isa);
// Find a not used register
int findNewRegister();
// Free used registers (not identifiers)
void freeRegisters();
// Store modified identifiers to memory
void storeValues();
// Print ASM code starting from s
void printASM(int start);

int isConstant(AST *root);
int getConstant(AST *root);
AST *newConstantAST(int val);
AST *simplifyTree(AST *root);

// Check if ASSIGN exists in AST
int checkAssign(AST *root);
// If there are no assigns, we can just check for INC/DECs
void lazy_codegen(AST *root);

void removeRedundantASM();
void traceASM(int i, unsigned int r);

void loadArgs(ISA *isa, int* regs, int *l, int *r);
void runASM(int* init);
void constantFinding();

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);
// Print register status
void printRegisters();

char input[MAX_LENGTH];

ISA code[MAX_LENGTH * 20];
int code_len = 0;
int last_len = 0;

// Register state, 0: free, 1: used, 2: identifier
// Stores register from 1 to 256, deduct 1 when printing
int registers[260];
// Registers of the three identifiers, 1 ~ 256
int identifiers[3] = {-1, -1, -1};
// Track modified registers
int modified[3] = {0, 0, 0};

int main(int argc, char **argv) {
	if (argc == 2)
		DEBUG = 1;
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);

		if (len <= 0)
			continue;

		// if (DEBUG) token_print(content, len);
		AST *ast_root = parser(content, len);
		// if (DEBUG) AST_print(ast_root);

		semantic_check(ast_root);
		ast_root = simplifyTree(ast_root);
		if (DEBUG) AST_print(ast_root);

		if (checkAssign(ast_root)) {
			codegen(ast_root);
		} else {
			if (DEBUG) printf("Using LAZY codegen\n");
			lazy_codegen(ast_root);
		}
		
		freeRegisters();
		if (DEBUG) {
			printASM(last_len);
			last_len = code_len;
			printRegisters();
		}

		free(content);
		freeAST(ast_root);
	}
	
	removeRedundantASM();
	constantFinding();
	removeRedundantASM();
	storeValues();
	printASM(0);
	return 0;
}

Token *lexer(const char *in) {
	Token *head = NULL;
	Token **now = &head;
	for (int i = 0; in[i]; i++) {
		if (in[i] == ' ' || in[i] == '\n') // ignore space and newline
			continue;
		else if (isdigit(in[i])) {
			(*now) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

Token *new_token(Kind kind, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *now = (*head), *del;
	for (res = 0; now != NULL; res++) now = now->next;
	now = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < (int)res; i++) {
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}

AST *parser(Token *arr, size_t len) {
	for (int i = 1; i < (int)len; i++) {
		// correctly identify "ADD" and "SUB"
		if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
			switch (arr[i - 1].kind) {
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind - PLUS + ADD;
				default: break;
			}
		}
	}
	return parse(arr, 0, len - 1, STMT);
}

AST *parse(Token *arr, int l, int r, GrammarState S) {
	AST *now = NULL;
	if (l > r) {
		err("Expected expression")
	}
	int nxt;
	switch (S) {
		case STMT:
			if (l > r) return now;
			else return parse(arr, l, r, EXPR);
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
				now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt = findNextSection(arr, r, l, condADD)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
				now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
			// TODO: Implement MUL_EXPR.
			// hint: Take ADD_EXPR as reference.
			if((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
				now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, UNARY_EXPR);
		case UNARY_EXPR:
			// TODO: Implement UNARY_EXPR.
			// hint: Take POSTFIX_EXPR as reference.
			if (arr[l].kind == PREINC || arr[l].kind == PREDEC || arr[l].kind == PLUS || arr[l].kind == MINUS) {
				now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
				now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(arr, l, r, condRPAR) == r) {

				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l + 1, r - 1, EXPR);
				return now;
			}
			if (l == r) {
				if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].kind == LPAR) par++;
		if (arr[i].kind == RPAR) par--;
		if (par == 0 && cond(arr[i].kind) == 1) return i;
	}
	return -1;
}

int condASSIGN(Kind kind) {
	return kind == ASSIGN;
}

int condADD(Kind kind) {
	return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
	return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
	return kind == RPAR;
}

void semantic_check(AST *now) {
	if (now == NULL) return;
	
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (now->kind == ASSIGN) {
		AST *tmp = now->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
	}
	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	// TODO: Implement the remaining semantic_check code.
	// hint: Follow the instruction above and ASSIGN-part code to implement.
	// hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
	if (now->kind == PREINC || now->kind == POSTINC ||
		now->kind == PREDEC || now->kind == POSTDEC) {
		AST *tmp = now->mid;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("INC/DEC must follow an identifier.");
	}

	semantic_check(now->lhs);
	semantic_check(now->mid);
	semantic_check(now->rhs);
}

int isConstant(AST *root) {
	if (root->kind == CONSTANT)
		return 1;
	if (root->kind == MINUS && root->mid->kind == CONSTANT)
		return 1;
	return 0;
}

int getConstant(AST *root) {
	if (root->kind == CONSTANT)
		return root->val;
	if (root->kind == MINUS && root->mid->kind == CONSTANT)
		return -(root->mid->val);
	err("Not a constant!");
}

int canCut(AST *root) {
	if (root == NULL)
		return 1;

	if (root->kind == ASSIGN || root->kind == PREINC || root->kind == POSTINC ||
		root->kind == PREDEC || root->kind == POSTDEC)
		return 0;

	return canCut(root->lhs) && canCut(root->mid) && canCut(root->rhs);
}

AST *newConstantAST(int val) {
	if (val >= 0)
		return new_AST(CONSTANT, val);
	if (val == -2147483648) {
		AST *tmp = new_AST(SUB, 0);
		tmp->lhs = new_AST(MINUS, 0);
		tmp->lhs->mid = new_AST(CONSTANT, 2147483647);
		tmp->rhs = new_AST(CONSTANT, 1);
		return tmp;
	}

	AST *tmp = new_AST(MINUS, 0);
	tmp->mid = new_AST(CONSTANT, -val);
	return tmp;
}

AST *simplifyTree(AST *root) {
	if (root == NULL)
		return NULL;

	root->lhs = simplifyTree(root->lhs);
	root->mid = simplifyTree(root->mid);
	root->rhs = simplifyTree(root->rhs);

	// Essential
	if (root->kind == PLUS || root->kind == LPAR) {
		if (root->mid == NULL) {
			err("Expected expression")
		}
		AST *tmp = root;
		root = root->mid;
		free(tmp);
		return root;
	}
	
	if (root->kind == ADD) {
		if (isConstant(root->lhs) && isConstant(root->rhs)) {
			AST *tmp = root;
			root = newConstantAST(getConstant(root->lhs) + getConstant(root->rhs));
			freeAST(tmp);
			return root;
		}
		if (isConstant(root->lhs) && getConstant(root->lhs) == 0) {
			AST *tmp = root;
			root = root->rhs;
			freeAST(tmp->lhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->rhs) && getConstant(root->rhs) == 0) {
			AST *tmp = root;
			root = root->lhs;
			freeAST(tmp->rhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->lhs) && root->rhs->kind == ADD) {
			if (isConstant(root->rhs->lhs)) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(getConstant(root->lhs) + getConstant(root->rhs->lhs));
				freeAST(tmp);
				root->rhs = root->rhs->rhs;
				return root;
			}
			if (isConstant(root->rhs->rhs)) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(getConstant(root->lhs) + getConstant(root->rhs->rhs));
				freeAST(tmp);
				root->rhs = root->rhs->lhs;
				return root;
			}
		}
		if (isConstant(root->rhs) && root->lhs->kind == ADD) {
			if (isConstant(root->lhs->lhs)) {
				AST *tmp = root->rhs;
				root->rhs = newConstantAST(getConstant(root->rhs) + getConstant(root->lhs->lhs));
				freeAST(tmp);
				root->lhs = root->lhs->rhs;
				return root;
			}
			if (isConstant(root->lhs->rhs)) {
				AST *tmp = root->rhs;
				root->rhs = newConstantAST(getConstant(root->rhs) + getConstant(root->lhs->rhs));
				freeAST(tmp);
				root->lhs = root->lhs->lhs;
				return root;
			}
		}
		if (root->lhs->kind == ADD && root->rhs->kind == ADD) {
			int res = -1;
			AST *ls, *rs;
			int ok = 0;
			if (isConstant(root->lhs->lhs) && isConstant(root->rhs->lhs)) {
				res = getConstant(root->lhs->lhs) + getConstant(root->rhs->lhs);
				ls = root->lhs->rhs, rs = root->rhs->rhs;
				ok = 1;
			} else if (isConstant(root->lhs->lhs) && isConstant(root->rhs->rhs)) {
				res = getConstant(root->lhs->lhs) + getConstant(root->rhs->rhs);
				ls = root->lhs->rhs, rs = root->rhs->lhs;
				ok = 1;
			} else if (isConstant(root->lhs->rhs) && isConstant(root->rhs->lhs)) {
				res = getConstant(root->lhs->rhs) + getConstant(root->rhs->lhs);
				ls = root->lhs->lhs, rs = root->rhs->rhs;
				ok = 1;
			} else if (isConstant(root->lhs->rhs) && isConstant(root->rhs->rhs)) {
				res = getConstant(root->lhs->rhs) + getConstant(root->rhs->rhs);
				ls = root->lhs->lhs, rs = root->rhs->lhs;
				ok = 1;
			}
			if (ok) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(res);
				free(tmp);
				root->rhs = new_AST(ADD, 0);
				root->rhs->lhs = ls;
				root->rhs->rhs = rs;
				return root;
			}
		}
	}
	if (root->kind == MUL) {
		if (isConstant(root->lhs) && isConstant(root->rhs)) {
			AST *tmp = root;
			root = newConstantAST(getConstant(root->lhs) * getConstant(root->rhs));
			freeAST(tmp);
			return root;
		}
		if (((isConstant(root->lhs) && getConstant(root->lhs) == 0) ||
			(isConstant(root->rhs) && getConstant(root->rhs) == 0)) && canCut(root)) {
			AST *tmp = root;
			root = newConstantAST(0);
			freeAST(tmp);
			return root;
		}
		if (isConstant(root->lhs) && getConstant(root->lhs) == 1) {
			AST *tmp = root;
			root = root->rhs;
			freeAST(tmp->lhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->rhs) && getConstant(root->rhs) == 1) {
			AST *tmp = root;
			root = root->lhs;
			freeAST(tmp->rhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->lhs) && getConstant(root->lhs) == -1) {
			AST *tmp = root;
			root = new_AST(MINUS, 0);
			root->mid = tmp->rhs;
			freeAST(tmp->lhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->rhs) && getConstant(root->rhs) == -1) {
			AST *tmp = root;
			root = new_AST(MINUS, 0);
			root->mid = tmp->lhs;
			freeAST(tmp->rhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->lhs) && root->rhs->kind == MUL) {
			if (isConstant(root->rhs->lhs)) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(getConstant(root->lhs) * getConstant(root->rhs->lhs));
				freeAST(tmp);
				root->rhs = root->rhs->rhs;
				return root;
			}
			if (isConstant(root->rhs->rhs)) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(getConstant(root->lhs) * getConstant(root->rhs->rhs));
				freeAST(tmp);
				root->rhs = root->rhs->lhs;
				return root;
			}
		}
		if (isConstant(root->rhs) && root->lhs->kind == MUL) {
			if (isConstant(root->lhs->lhs)) {
				AST *tmp = root->rhs;
				root->rhs = newConstantAST(getConstant(root->rhs) * getConstant(root->lhs->lhs));
				freeAST(tmp);
				root->lhs = root->lhs->rhs;
				return root;
			}
			if (isConstant(root->lhs->rhs)) {
				AST *tmp = root->rhs;
				root->rhs = newConstantAST(getConstant(root->rhs) * getConstant(root->lhs->rhs));
				freeAST(tmp);
				root->lhs = root->lhs->lhs;
				return root;
			}
		}
		if (root->lhs->kind == MUL && root->rhs->kind == MUL) {
			int res = -1;
			AST *ls, *rs;
			int ok = 0;
			if (isConstant(root->lhs->lhs) && isConstant(root->rhs->lhs)) {
				res = getConstant(root->lhs->lhs) * getConstant(root->rhs->lhs);
				ls = root->lhs->rhs, rs = root->rhs->rhs;
				ok = 1;
			} else if (isConstant(root->lhs->lhs) && isConstant(root->rhs->rhs)) {
				res = getConstant(root->lhs->lhs) * getConstant(root->rhs->rhs);
				ls = root->lhs->rhs, rs = root->rhs->lhs;
				ok = 1;
			} else if (isConstant(root->lhs->rhs) && isConstant(root->rhs->lhs)) {
				res = getConstant(root->lhs->rhs) * getConstant(root->rhs->lhs);
				ls = root->lhs->lhs, rs = root->rhs->rhs;
				ok = 1;
			} else if (isConstant(root->lhs->rhs) && isConstant(root->rhs->rhs)) {
				res = getConstant(root->lhs->rhs) * getConstant(root->rhs->rhs);
				ls = root->lhs->lhs, rs = root->rhs->lhs;
				ok = 1;
			}
			if (ok) {
				AST *tmp = root->lhs;
				root->lhs = newConstantAST(res);
				free(tmp);
				root->rhs = new_AST(ADD, 0);
				root->rhs->lhs = ls;
				root->rhs->rhs = rs;
				return root;
			}
		}
	}
	if (root->kind == SUB) {
		if (isConstant(root->lhs) && isConstant(root->rhs)) {
			AST *tmp = root;
			root = newConstantAST(getConstant(root->lhs) - getConstant(root->rhs));
			freeAST(tmp);
			return root;
		}
		if (isConstant(root->lhs) && getConstant(root->lhs) == 0) {
			AST *tmp = root;
			root = new_AST(MINUS, 0);
			root->mid = tmp->rhs;
			freeAST(tmp->lhs);
			free(tmp);
			return root;
		}
		if (isConstant(root->rhs) && getConstant(root->rhs) == 0) {
			AST *tmp = root;
			root = root->lhs;
			freeAST(tmp->rhs);
			free(tmp);
			return root;
		}
		if (root->lhs->kind == IDENTIFIER && root->rhs->kind == IDENTIFIER) {
			if (root->lhs->val == root->rhs->val) {
				AST *tmp = root;
				root = newConstantAST(0);
				freeAST(tmp);
				return root;
			}
		}
	}
	if (root->kind == DIV) {
		if (isConstant(root->lhs) && isConstant(root->rhs)) {
			AST *tmp = root;
			root = newConstantAST(getConstant(root->lhs) / getConstant(root->rhs));
			freeAST(tmp);
			return root;
		}
		if (isConstant(root->lhs) && getConstant(root->lhs) == 0 && canCut(root)) {
			AST *tmp = root;
			root = newConstantAST(0);
			freeAST(tmp);
			return root;
		}
		if (isConstant(root->rhs) && getConstant(root->rhs) == 0) {
			err("Division by zero!")
		}
		if (root->lhs->kind == IDENTIFIER && root->rhs->kind == IDENTIFIER) {
			if (root->lhs->val == root->rhs->val) {
				AST *tmp = root;
				root = newConstantAST(1);
				freeAST(tmp);
				return root;
			}
		}
	}
	if (root->kind == REM) {
		if (isConstant(root->lhs) && isConstant(root->rhs)) {
			AST *tmp = root;
			root = newConstantAST(getConstant(root->lhs) % getConstant(root->rhs));
			freeAST(tmp);
			return root;
		}
		if (((isConstant(root->lhs) && getConstant(root->lhs) == 0) ||
			(isConstant(root->rhs) && getConstant(root->rhs) == 1)) && canCut(root)) {
			AST *tmp = root;
			root = newConstantAST(0);
			freeAST(tmp);
			return root;
		}
		if (root->lhs->kind == IDENTIFIER && root->rhs->kind == IDENTIFIER) {
			if (root->lhs->val == root->rhs->val) {
				AST *tmp = root;
				root = newConstantAST(0);
				freeAST(tmp);
				return root;
			}
		}
	}
	if (root->kind == MINUS) {
		if (root->mid->kind == MINUS) {
			AST *tmp = root;
			root = root->mid->mid;
			free(tmp);
			return root;
		}
	}
	
	return root;
}

int DONT_LOAD = 0;

// Returns value if >0, register if <0 (-1 ~ -256) or NULLREG
int codegen(AST *root) {
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.

	if (root == NULL)
		return NULLREG;

	int l, r, dest;
	switch (root->kind) {
		case ASSIGN:
			r = codegen(root->rhs);
			//printf("Rvalue: %d\n", r);
			if (r >= 0 || registers[-r] == IDEN) {
				dest = findNewRegister();
				addISA(ADD, dest, 0, r);
			} else {
				dest = r;
			}

			DONT_LOAD = 1;
			l = codegen(root->lhs);
			DONT_LOAD = 0;
			
			if (l >= 0 || (l != NULLREG && registers[-l] != IDEN)) {
				err("Lvalue should be an identifier!")
			}

			if (l != NULLREG)
				registers[-l] = FREE;

			identifiers[root->lhs->val - 'x'] = -dest;
			modified[root->lhs->val - 'x'] = 1;

			registers[-dest] = IDEN;
			return dest;

		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
			l = codegen(root->lhs);
			r = codegen(root->rhs);
			if (l >= 0 && r >= 0) {
				switch (root->kind) {
					case ADD:
						return l + r;
					case SUB: // May become negative!
						if (l - r >= 0)
							return l - r;
						if (l - r == -2147483648) {
							AST *tmp = new_AST(SUB, 0);
							tmp->lhs = new_AST(MINUS, 0);
							tmp->lhs->mid = new_AST(CONSTANT, 2147483647);
							tmp->rhs = new_AST(CONSTANT, 1);
							return codegen(tmp);
						}
						AST *tmp = new_AST(MINUS, 0);
						tmp->mid = new_AST(CONSTANT, -(l - r));
						return codegen(tmp);
					case MUL:
						return l * r;
					case DIV:
						return l / r;
					case REM:
						return l % r;
					default:
						err("Codegen flow broken!")
				}
			}

			if (l < 0 && registers[-l] != IDEN)
				dest = l;
			else if (r < 0 && registers[-r] != IDEN)
				dest = r;
			else 
				dest = findNewRegister();

			addISA(root->kind, dest, l, r);

			if (l < 0 && registers[-l] != IDEN)
				registers[-l] = FREE;
			if (r < 0 && registers[-r] != IDEN)
				registers[-r] = FREE;
			registers[-dest] = USED;
			return dest;

		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
			l = codegen(root->mid);

			if (l >= 0 || registers[-l] != IDEN) {
				err("INC/DEC should follow an identifier!")
			}

			if (root->kind == POSTINC || root->kind == POSTDEC) {
				dest = findNewRegister();
				registers[-dest] = USED;
				addISA(ADD, dest, 0, l);
			} else {
				dest = l;
			}

			addISA((root->kind == PREINC || root->kind == POSTINC)? ADD: SUB, l, l, 1);
			modified[root->mid->val - 'x'] = 1;
			return dest;

		case IDENTIFIER:
			if (identifiers[root->val - 'x'] != -1)
				return -identifiers[root->val - 'x'];
			if (DONT_LOAD)
				return NULLREG;
			dest = findNewRegister();
			registers[-dest] = IDEN;
			identifiers[root->val - 'x'] = -dest;
			addISA(LOAD, dest, 4 * (root->val - 'x'), 0);
			// printf("Loaded %c into r%d\n", root->val, dest);
			return dest;

		case CONSTANT:
			return root->val;

		case MINUS:
			r = codegen(root->mid);
			if (r >= 0 || registers[-r] == IDEN) {
				dest = findNewRegister();
				registers[-dest] = USED;
			} else {
				dest = r;
			}

			addISA(SUB, dest, 0, r);
			return dest;

		default:
			err("AST kind not found!")
	}
	
}

int checkAssign(AST *root) {
	if (root == NULL)
		return 0;

	if (root->kind == ASSIGN)
		return 1;

	return checkAssign(root->lhs) || checkAssign(root->mid) || checkAssign(root->rhs);
}

void lazy_codegen(AST *root) {
	if (root == NULL)
		return;

	lazy_codegen(root->lhs);
	lazy_codegen(root->mid);
	lazy_codegen(root->rhs);
	
	if (root->kind == PREINC || root->kind == POSTINC) {
		int l = codegen(root->mid);

		if (l >= 0 || registers[-l] != IDEN) {
			err("INC/DEC should follow an identifier!")
		}

		addISA(ADD, l, l, 1);
		modified[root->mid->val - 'x'] = 1;
		return;
	}

	if (root->kind == PREDEC || root->kind == POSTDEC) {
		int l = codegen(root->mid);

		if (l >= 0 || registers[-l] != IDEN) {
			err("INC/DEC should follow an identifier!")
		}

		addISA(SUB, l, l, 1);
		modified[root->mid->val - 'x'] = 1;
		return;
	}
}

// Returns (-1 ~ -256)
int findNewRegister() {
	for (int i = 1; i <= 256; i++)
		if (registers[i] == FREE)
			return -i;
	
	return NULLREG; // No registers left
}

void freeRegisters() {
	for (int i = 1; i <= 256; i++)
		if (registers[i] == USED)
			registers[i] = FREE;
}

void addISA(Kind ins, int o1, int o2, int o3) {
	ISA *new = &code[code_len++];
	new->ins = ins;
	if (ins == LOAD || ins == STORE) {
		if (o1 < 0) {
			new->o1_type = REGISTER;
			new->o1 = -o1;
		} else {
			new->o1_type = ADDRESS;
			new->o1 = o1;
		}
		if (o2 < 0) {
			new->o2_type = REGISTER;
			new->o2 = -o2;
		} else {
			new->o2_type = ADDRESS;
			new->o2 = o2;
		}
		new->o3_type = EMPTY;
		new->o3 = 0;
	} else {
		if (o1 < 0) {
			new->o1_type = REGISTER;
			new->o1 = -o1;
		} else {
			err("Destination must be a register!")
		}
		if (o2 < 0) {
			new->o2_type = REGISTER;
			new->o2 = -o2;
		} else {
			new->o2_type = VALUE;
			new->o2 = o2;
		}
		if (o3 < 0) {
			new->o3_type = REGISTER;
			new->o3 = -o3;
		} else {
			new->o3_type = VALUE;
			new->o3 = o3;
		}
	}

	new->redundant = 0;
	new->deleted = 0;

	new->last_res = -1;
	new->is_const = 1;
	new->first_run = 1;
}

void traceASM(int i, unsigned int r) {
	while (i >= 0 && (code[i].deleted || code[i].o1 != r))
		i--;

	if (i < 0)
		return;
	
	code[i].redundant = 0;

	if (code[i].o2_type == REGISTER)
		traceASM(i - 1, code[i].o2);
	if (code[i].o3_type == REGISTER)
		traceASM(i - 1, code[i].o3);
}

void removeRedundantASM() {
	for (int i = 0; i < code_len; i++) {
		if (!code[i].deleted && !(code[i].ins == STORE))
			code[i].redundant = 1;
	}
	for (int i = 0; i < 3; i++) {
		if (modified[i]) {
			traceASM(code_len - 1, identifiers[i]);
		}
	}
	for (int i = 0; i < code_len; i++) {
		if (!code[i].deleted && code[i].redundant)
			code[i].deleted = 1;
	}
}

void loadArgs(ISA *isa, int* regs, int *l, int *r) {
	if (isa->o2_type == VALUE)
		*l = isa->o2;
	else
		*l = regs[isa->o2];

	if (isa->o3_type == VALUE)
		*r = isa->o3;
	else if (isa->o3_type == EMPTY)
		*r = 0;
	else
		*r = regs[isa->o3];
}

void runASM(int* init) {
	int regs[260];
	memset(regs, 0, sizeof(regs));
	for (int i = 0; i < code_len; i++) {
		if (code[i].deleted)
			continue;

		if (code[i].ins == LOAD) {
			regs[code[i].o1] = init[code[i].o2 / 4];
		} else {
			int l, r;
			long long res;
			loadArgs(&code[i], regs, &l, &r);
			
			if (code[i].ins == ADD)
				res = l *1LL + r;
			else if (code[i].ins == SUB)
				res = l *1LL - r;
			else if (code[i].ins == MUL)
				res = l *1LL * r;
			else if (code[i].ins == DIV) {
				if (r == 0)
					return;
				res = l *1LL / r;
			}
			else if (code[i].ins == REM) {
				if (r == 0)
					return;
				res = l *1LL % r;
			}
			else {
				err("Broken!")
			}
			
			if (res > 2147483647 || res < -2147483648)
				return;
			// printf("%lld\n", res);
			regs[code[i].o1] = res;

			if (!code[i].first_run && code[i].is_const && code[i].last_res != res)
				code[i].is_const = 0;
			code[i].first_run = 0;
			code[i].last_res = res;
		}
	}
}

void constantFinding() {
	for (int x = -100; x <= 100; x++) {
		for (int y = -100; y <= 100; y++) {
			for (int z = -100; z <= 100; z++) {
				// printf("%d %d %d\r", x, y, z);
				int init[] = {x, y, z};
				runASM(init);
				// return;
			}
		}
	}

	for (int i = 0; i < code_len; i++) {
		if (code[i].deleted || code[i].ins == LOAD)
			continue;
		if (code[i].is_const) {
			if (code[i].last_res >= 0) {
				code[i].ins = ADD;
				code[i].o2_type = code[i].o3_type = VALUE;
				code[i].o2 = 0;
				code[i].o3 = code[i].last_res;
				// for (int j = i + 1; j < code_len && !(code[j].o1_type == REGISTER && code[j].o1 == code[i].o1); j++) {
				// 	if (code[j].deleted || code[j].ins == LOAD)
				// 		continue;
				// 	if (code[j].o2_type == REGISTER && code[j].o2 == code[i].o1) {
				// 		code[j].o2_type = VALUE;
				// 		code[j].o2 = code[i].last_res;
				// 	}
				// 	if (code[j].o3_type == REGISTER && code[j].o3 == code[i].o1) {
				// 		code[j].o3_type = VALUE;
				// 		code[j].o3 = code[i].last_res;
				// 	}
				// }
			} else if (code[i].last_res != -2147483648) {
				code[i].ins = SUB;
				code[i].o2_type = code[i].o3_type = VALUE;
				code[i].o2 = 0;
				code[i].o3 = -code[i].last_res;
			}
		}
	}
}

void storeValues() {
	for (int i = 0; i < 3; i++) {
		if (modified[i]) {
			addISA(STORE, 4 * i, -identifiers[i], 0);
		}
	}
}

#pragma GCC push_options
#pragma GCC optimize ("O0")
void printASM(int start) {
	for (int i = start; i < code_len; i++) {
		if (code[i].deleted)
			continue;
		printISA(&code[i]);
	}
}

void printISA(ISA *isa) {
	const static char insName[][10] = {
		"UNKNOWN", "add", "sub", "mul", "div", "rem", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "UNKNOWN", "load", "store"
	};
	const static char format_memory[] = "%s %s %s\n";
	const static char format_arithmetic[] = "%s %s %s %s\n";
	const static char format_register[] = "r%u";
	const static char format_address[] = "[%u]";
	const static char format_value[] = "%u";
	static char op1[20], op2[20], op3[20];
	// if (isa->deleted) {
	// 	printf("D ");
	// }
	if (isa->ins == LOAD) {
		sprintf(op1, format_register, isa->o1 - 1);
		sprintf(op2, format_address, isa->o2);
		printf(format_memory, insName[LOAD], op1, op2);
	} else if (isa->ins == STORE) {
		sprintf(op1, format_address, isa->o1);
		sprintf(op2, format_register, isa->o2 - 1);
		printf(format_memory, insName[STORE], op1, op2);
	} else {
		sprintf(op1, format_register, isa->o1 - 1);
		if (isa->o2_type == REGISTER)
			sprintf(op2, format_register, isa->o2 - 1);
		else
			sprintf(op2, format_value, isa->o2);
		if (isa->o3_type == REGISTER)
			sprintf(op3, format_register, isa->o3 - 1);
		else
			sprintf(op3, format_value, isa->o3);
		
		printf(format_arithmetic, insName[isa->ins], op1, op2, op3);
	}
}
#pragma GCC pop_options


void printRegisters() {
	for (int i = 1; i <= 256; i++)
		if (registers[i] != FREE)
			printf("r%d\t", i - 1);
	printf("\n");
	for (int i = 1; i <= 256; i++)
		if (registers[i] != FREE)
			printf("%d\t", registers[i]);
	printf("\n");
}

void freeAST(AST *now) {
	if (now == NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

void token_print(Token *in, size_t len) {
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus"
	};
	const static char KindSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i = 0; i < (int)len; i++) {
		switch(in[i].kind) {
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				printf(format_str,i , KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
				break;
			case CONSTANT:
				printf(format_int,i , KindName[in[i].kind], "value", in[i].val);
				break;
			case IDENTIFIER:
				printf(format_str,i , KindName[in[i].kind], "name", (char*)(&(in[i].val)));
				break;
			default:
				puts("=== unknown token ===");
		}
	}
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "";
	static int indent = 0;
	char *indent_now = indent_str + indent;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head == NULL) return;
	indent_str[indent - 1] = '-';
	printf("%s", indent_str);
	indent_str[indent - 1] = ' ';
	if (indent_str[indent - 2] == '`')
		indent_str[indent - 2] = ' ';
	switch (head->kind) {
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			printf(format, KindName[head->kind]);
			break;
		case IDENTIFIER:
			printf(format_str, KindName[head->kind], "name", (char*)&(head->val));
			break;
		case CONSTANT:
			printf(format_val, KindName[head->kind], "value", head->val);
			break;
		default:
			puts("=== unknown AST type ===");
	}
	indent += 2;
	strcpy(indent_now, "| ");
	AST_print(head->lhs);
	strcpy(indent_now, "` ");
	AST_print(head->mid);

	AST_print(head->rhs);
	indent -= 2;
	(*indent_now) = '\0';
}