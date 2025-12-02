#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_TOKENS 100      // Maximum number of tokens per line
#define MAX_VARS 100        // Maximum number of variables
#define MAX_VAR_NAME 32     // Maximum variable name length

// Token types
typedef enum { NUM, ID, OP, LPAREN, RPAREN, ASSIGN, END } TokenType;

// Token structure
typedef struct {
    TokenType type;
    char str[MAX_VAR_NAME];
    double value;
} Token;

// Variable structure
typedef struct {
    char name[MAX_VAR_NAME];
    double value;
} Variable;

// Global arrays for tokens and variables
Token tokens[MAX_TOKENS];
int tokenPos = 0;
int numTokens = 0;

Variable vars[MAX_VARS];
int numVars = 0;

// ======================== LEXER ========================
// Converts input string into tokens
void tokenize(const char *input) {
    numTokens = 0;
    tokenPos = 0;
    int i = 0;

    while (input[i]) {
        if (isspace(input[i])) { i++; continue; }

        // Number token
        if (isdigit(input[i])) {
            double val = 0;
            while (isdigit(input[i])) { val = val * 10 + (input[i]-'0'); i++; }
            if (input[i] == '.') {
                i++;
                double frac = 0.1;
                while (isdigit(input[i])) {
                    val += (input[i]-'0') * frac;
                    frac /= 10;
                    i++;
                }
            }
            tokens[numTokens].type = NUM;
            tokens[numTokens].value = val;
            numTokens++;
            continue;
        }

        // Identifier token (variable name)
        if (isalpha(input[i])) {
            int j = 0;
            char buf[MAX_VAR_NAME] = {0};
            while (isalnum(input[i])) buf[j++] = input[i++];
            tokens[numTokens].type = ID;
            strcpy(tokens[numTokens].str, buf);
            numTokens++;
            continue;
        }

        // Operator or special character
        switch(input[i]) {
            case '+': case '-': case '*': case '/': case '%':
                tokens[numTokens].type = OP;
                tokens[numTokens].str[0] = input[i]; tokens[numTokens].str[1] = '\0';
                numTokens++; i++; break;
            case '(': tokens[numTokens++].type = LPAREN; i++; break;
            case ')': tokens[numTokens++].type = RPAREN; i++; break;
            case '=': tokens[numTokens++].type = ASSIGN; i++; break;
            default:
                printf("Error: Unknown character '%c'\n", input[i]);
                exit(1);
        }
    }
    tokens[numTokens].type = END;
}

// ======================== PARSER / INTERPRETER ========================
Token* currentToken() { return &tokens[tokenPos]; }
void nextToken() { if (tokenPos < numTokens) tokenPos++; }

double parseExpr(); // forward declaration

// Parse numbers, variables, or parenthesized expressions
double parsePrimary() {
    Token *t = currentToken();

    if (t->type == NUM) {
        double val = t->value;
        nextToken();
        return val;
    }

    if (t->type == ID) {
        char name[MAX_VAR_NAME];
        strcpy(name, t->str);
        nextToken();
        // Lookup variable value
        for(int i = 0; i < numVars; i++)
            if (strcmp(vars[i].name, name) == 0) return vars[i].value;
        printf("Error: Undefined variable '%s'\n", name);
        exit(1);
    }

    if (t->type == LPAREN) {
        nextToken();
        double val = parseExpr();
        if (currentToken()->type != RPAREN) {
            printf("Error: Missing closing parenthesis\n");
            exit(1);
        }
        nextToken();
        return val;
    }

    printf("Error: Unexpected token\n");
    exit(1);
}

// Handle multiplication, division, and modulo
double parseFactor() {
    double val = parsePrimary();
    while (currentToken()->type == OP && 
          (currentToken()->str[0] == '*' || currentToken()->str[0] == '/' || currentToken()->str[0] == '%')) {
        char op = currentToken()->str[0];
        nextToken();
        double right = parsePrimary();
        if (op == '*') val *= right;
        else if (op == '/') val /= right;
        else val = (int)val % (int)right;
    }
    return val;
}

// Handle addition and subtraction
double parseTerm() {
    double val = parseFactor();
    while (currentToken()->type == OP && 
          (currentToken()->str[0] == '+' || currentToken()->str[0] == '-')) {
        char op = currentToken()->str[0];
        nextToken();
        double right = parseFactor();
        if (op == '+') val += right;
        else val -= right;
    }
    return val;
}

// Entry point for expression parsing
double parseExpr() {
    return parseTerm();
}

// Parse a single statement (assignment or expression)
void parseStatement() {
    Token *t = currentToken();

    if (t->type == ID && tokens[tokenPos+1].type == ASSIGN) {
        // Variable assignment
        char name[MAX_VAR_NAME];
        strcpy(name, t->str);
        nextToken(); // Skip variable name
        nextToken(); // Skip '='
        double val = parseExpr();
        // Store variable or update existing
        int found = 0;
        for(int i = 0; i < numVars; i++)
            if (strcmp(vars[i].name, name) == 0) { vars[i].value = val; found = 1; break; }
        if (!found) { strcpy(vars[numVars].name, name); vars[numVars].value = val; numVars++; }
        printf("%s = %.2lf\n", name, val);
    } else {
        // Just an expression
        double val = parseExpr();
        printf("Result: %.2lf\n", val);
    }
}

// ======================== MAIN ========================
int main() {
    char line[256];

    printf("=== Advanced Expression Interpreter (C) ===\n");
    printf("Supports variables, +, -, *, /, %%, and parentheses.\n");
    printf("Type 'exit' to quit.\n");

    while (1) {
        printf(">>> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        if (strncmp(line, "exit", 4) == 0) break;

        tokenize(line);
        tokenPos = 0;
        parseStatement();
    }

    printf("Goodbye!\n");
    return 0;
}
