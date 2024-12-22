#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#define GRID_WIDTH 80
#define GRID_HEIGHT 20
#define MAX_EQUATION_LENGTH 256
#define MAX_TOKENS 100
#define MAX_ITER 100
#define EPSILON 1e-10
#define NUM_INITIAL_GUESSES 40
#define POINTS_PER_COLUMN 10

#ifndef PI
#define PI 3.14159265358979323846
#endif


typedef struct {
    double zoom;
    double x_offset;
    double y_offset;
} PlotSettings;

typedef enum {
    TOKEN_NUMBER,
    TOKEN_VARIABLE,
    TOKEN_OPERATOR,
    TOKEN_FUNCTION,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_EQUALS
} TokenType;

typedef struct {
    TokenType type;
    char str[32];
    double value;
} Token;

// Function prototypes
double evaluate_expression(Token* tokens, int num_tokens, double x, double y);
int tokenize_expression(const char* expr, Token* tokens);

// Utility functions for token handling
int is_operator(char c) {
    return (c == '+' || c == '-' || c == '*' || c == '/' || c == '^');
}

int is_function(const char* str) {
    return (strcmp(str, "sin") == 0 || strcmp(str, "cos") == 0 || 
            strcmp(str, "tan") == 0 || strcmp(str, "log") == 0 || 
            strcmp(str, "ln") == 0 || strcmp(str, "exp") == 0);
}

int get_precedence(char op) {
    switch (op) {
        case '^': return 3;
        case '*': case '/': return 2;
        case '+': case '-': return 1;
        default: return 0;
    }
}

// Enhanced tokenizer
int tokenize_expression(const char* expr, Token* tokens) {
    int num_tokens = 0;
    int i = 0;
    char buffer[32];
    int buf_pos;

    while (expr[i]) {
        // Skip whitespace
        while (isspace(expr[i])) i++;
        if (!expr[i]) break;

        Token* token = &tokens[num_tokens];
        
        if (isdigit(expr[i]) || expr[i] == '.') {
            // Parse number
            buf_pos = 0;
            while (isdigit(expr[i]) || expr[i] == '.') {
                buffer[buf_pos++] = expr[i++];
            }
            buffer[buf_pos] = '\0';
            token->type = TOKEN_NUMBER;
            token->value = atof(buffer);
            strcpy(token->str, buffer);
            num_tokens++;
        }
        else if (isalpha(expr[i])) {
            // Parse variable or function
            buf_pos = 0;
            while (isalpha(expr[i])) {
                buffer[buf_pos++] = expr[i++];
            }
            buffer[buf_pos] = '\0';

            if (is_function(buffer)) {
                token->type = TOKEN_FUNCTION;
            } else {
                token->type = TOKEN_VARIABLE;
            }
            strcpy(token->str, buffer);
            num_tokens++;
        }
        else if (expr[i] == '=') {
            token->type = TOKEN_EQUALS;
            token->str[0] = '=';
            token->str[1] = '\0';
            i++;
            num_tokens++;
        }
        else if (expr[i] == '(') {
            token->type = TOKEN_LPAREN;
            token->str[0] = '(';
            token->str[1] = '\0';
            i++;
            num_tokens++;
        }
        else if (expr[i] == ')') {
            token->type = TOKEN_RPAREN;
            token->str[0] = ')';
            token->str[1] = '\0';
            i++;
            num_tokens++;
        }
        else if (is_operator(expr[i])) {
            token->type = TOKEN_OPERATOR;
            token->str[0] = expr[i];
            token->str[1] = '\0';
            i++;
            num_tokens++;
        }
        else {
            i++; // Skip unrecognized characters
        }
    }
    return num_tokens;
}

// Recursive expression evaluator
double evaluate_expression(Token* tokens, int num_tokens, double x, double y) {
    // Handle empty expression
    if (num_tokens == 0) return 0;

    // Single token case
    if (num_tokens == 1) {
        Token token = tokens[0];
        if (token.type == TOKEN_NUMBER) return token.value;
        if (token.type == TOKEN_VARIABLE) {
            if (strcmp(token.str, "x") == 0) return x;
            if (strcmp(token.str, "y") == 0) return y;
        }
        return 0;
    }

    // Find the operator with lowest precedence
    int min_prec_pos = -1;
    int min_prec = 999;
    int paren_depth = 0;

    for (int i = num_tokens - 1; i >= 0; i--) {
        Token token = tokens[i];
        
        if (token.type == TOKEN_RPAREN) paren_depth++;
        else if (token.type == TOKEN_LPAREN) paren_depth--;
        else if (paren_depth == 0 && token.type == TOKEN_OPERATOR) {
            int prec = get_precedence(token.str[0]);
            if (prec <= min_prec) {
                min_prec = prec;
                min_prec_pos = i;
            }
        }
    }

    // If no operator found outside parentheses
    if (min_prec_pos == -1) {
        // Check for function call
        if (tokens[0].type == TOKEN_FUNCTION) {
            double arg = evaluate_expression(tokens + 2, num_tokens - 3, x, y);
            if (strcmp(tokens[0].str, "sin") == 0) return sin(arg);
            if (strcmp(tokens[0].str, "cos") == 0) return cos(arg);
            if (strcmp(tokens[0].str, "tan") == 0) return tan(arg);
            if (strcmp(tokens[0].str, "log") == 0) return log10(arg);
            if (strcmp(tokens[0].str, "ln") == 0) return log(arg);
            if (strcmp(tokens[0].str, "exp") == 0) return exp(arg);
        }
        // Remove surrounding parentheses
        if (tokens[0].type == TOKEN_LPAREN && tokens[num_tokens-1].type == TOKEN_RPAREN) {
            return evaluate_expression(tokens + 1, num_tokens - 2, x, y);
        }
        return 0;
    }

    // Split and evaluate
    double left = evaluate_expression(tokens, min_prec_pos, x, y);
    double right = evaluate_expression(tokens + min_prec_pos + 1, 
                                     num_tokens - min_prec_pos - 1, x, y);

    // Perform operation
    switch (tokens[min_prec_pos].str[0]) {
        case '+': return left + right;
        case '-': return left - right;
        case '*': return left * right;
        case '/': return right != 0 ? left / right : INFINITY;
        case '^': return pow(left, right);
        default: return 0;
    }
}

// Improved equation solver
double solve_equation(const char* equation, double x, double initial_y) {
    Token tokens[MAX_TOKENS];
    char left_side[MAX_EQUATION_LENGTH], right_side[MAX_EQUATION_LENGTH];
    
    // Split equation at equals sign
    const char* equals = strchr(equation, '=');
    if (equals) {
        strncpy(left_side, equation, equals - equation);
        left_side[equals - equation] = '\0';
        strcpy(right_side, equals + 1);
    } else {
        strcpy(left_side, equation);
        strcpy(right_side, "0");
    }

    // Tokenize both sides
    Token left_tokens[MAX_TOKENS], right_tokens[MAX_TOKENS];
    int left_num_tokens = tokenize_expression(left_side, left_tokens);
    int right_num_tokens = tokenize_expression(right_side, right_tokens);

    double y = initial_y;
    double prev_y;
    int iter = 0;
    double h = 1e-7;  // Step size for numerical derivative

    do {
        prev_y = y;

        // Evaluate both sides of equation
        double f = evaluate_expression(left_tokens, left_num_tokens, x, y) - 
                  evaluate_expression(right_tokens, right_num_tokens, x, y);

        // Compute numerical derivative
        double f_h = evaluate_expression(left_tokens, left_num_tokens, x, y + h) - 
                    evaluate_expression(right_tokens, right_num_tokens, x, y + h);
        double df = (f_h - f) / h;

        // Prevent division by very small numbers
        if (fabs(df) < EPSILON) {
            df = (df < 0 ? -EPSILON : EPSILON);
        }

        // Newton step with dampening
        double delta = f / df;
        double damping = 0.5;  // Dampening factor
        y -= delta * damping;

        // Handle periodic functions
        if (strstr(equation, "sin") || strstr(equation, "cos") || strstr(equation, "tan")) {
            y = fmod(y + PI, 2 * PI) - PI;
        }

        iter++;
    } while (fabs(y - prev_y) > EPSILON && iter < MAX_ITER);

    return iter < MAX_ITER ? y : NAN;
}

void plot_equation(const char* equation, PlotSettings settings) {
    char grid[GRID_HEIGHT][GRID_WIDTH];
    memset(grid, ' ', sizeof(grid));

    // Calculate axes positions
    int center_x = (int)(GRID_WIDTH / 2 - settings.x_offset * 5.0 * settings.zoom);
    int center_y = (int)(GRID_HEIGHT / 2 + settings.y_offset * 5.0 * settings.zoom);

    // Draw axes
    for (int i = 0; i < GRID_HEIGHT; i++) {
        for (int j = 0; j < GRID_WIDTH; j++) {
            if (i == center_y) grid[i][j] = (j % 2 == 0) ? '+' : '-';
            if (j == center_x) grid[i][j] = (i % 2 == 0) ? '+' : '|';
        }
    }

    // Determine y range based on equation type
    double y_min = -5, y_max = 5;
    if (strstr(equation, "sin") || strstr(equation, "cos")) {
        y_min = -1.5; y_max = 1.5;
    }
    else if (strstr(equation, "ln") || strstr(equation, "log")) {
        y_min = -10; y_max = 10;
    }

    // Store previous valid points for line interpolation
    double prev_y_vals[NUM_INITIAL_GUESSES];
    int prev_plot_y[NUM_INITIAL_GUESSES];
    int has_prev[NUM_INITIAL_GUESSES];
    memset(has_prev, 0, sizeof(has_prev));

    // Plot equation with increased point density
    for (int j = 0; j < GRID_WIDTH; j++) {
        for (int sub_j = 0; sub_j < POINTS_PER_COLUMN; sub_j++) {
            double x_val = (j - GRID_WIDTH / 2 + (double)sub_j/POINTS_PER_COLUMN) / 
                          (5.0 * settings.zoom) + settings.x_offset;
            
            // Use multiple initial guesses
            for (int k = 0; k < NUM_INITIAL_GUESSES; k++) {
                double initial_y = y_min + (y_max - y_min) * k / (NUM_INITIAL_GUESSES - 1);
                double y_val = solve_equation(equation, x_val, initial_y);

                if (!isnan(y_val) && !isinf(y_val)) {
                    int plot_y = (int)(GRID_HEIGHT / 2 - y_val * 5.0 * settings.zoom 
                                     + settings.y_offset * 5.0 * settings.zoom);

                    // If this is a valid point
                    if (plot_y >= 0 && plot_y < GRID_HEIGHT) {
                        // Mark the main point
                        grid[plot_y][j] = '*';

                        // If we have a previous valid point for this guess
                        if (has_prev[k] && j > 0) {
                            // Interpolate between previous and current point
                            int y_start = prev_plot_y[k];
                            int y_end = plot_y;
                            int x_start = j - 1;
                            int x_end = j;
                            
                            // Draw line using Bresenham's algorithm
                            int dx = x_end - x_start;
                            int dy = abs(y_end - y_start);
                            int sy = y_start < y_end ? 1 : -1;
                            int err = dx / 2;
                            int y = y_start;

                            for (int x = x_start; x <= x_end; x++) {
                                if (y >= 0 && y < GRID_HEIGHT && x >= 0 && x < GRID_WIDTH) {
                                    if (grid[y][x] == ' ') grid[y][x] = '*';
                                }
                                err -= dy;
                                if (err < 0) {
                                    y += sy;
                                    err += dx;
                                }
                            }
                        }

                        // Store current point as previous for next iteration
                        prev_y_vals[k] = y_val;
                        prev_plot_y[k] = plot_y;
                        has_prev[k] = 1;
                    }
                }
            }
        }
    }

    // Draw the plot
    printf("\n+");
    for (int j = 0; j < GRID_WIDTH; j++) printf("-");
    printf("+\n");

    for (int i = 0; i < GRID_HEIGHT; i++) {
        printf("|");
        for (int j = 0; j < GRID_WIDTH; j++) {
            printf("%c", grid[i][j]);
        }
        printf("|\n");
    }

    printf("+");
    for (int j = 0; j < GRID_WIDTH; j++) printf("-");
    printf("+\n");

    printf("\nPlot (Zoom: %.2f, Offset: %.2f, %.2f)\n", 
           settings.zoom, settings.x_offset, settings.y_offset);
}

int main() {
    char equation[MAX_EQUATION_LENGTH];
    PlotSettings settings = {1.0, 0.0, 0.0};
    char choice;

    
    printf("\nInstruction:\n");
    printf("   -Supports +, -, *, /, ^, sin, cos, tan, log, ln, exp.\n");
    printf("   -Does not support asin, acos, atan, or advanced functions like abs or floor.\n");
    printf("   -Avoid undefined operations like division by zero.\n");

    printf("\nEnter equation with 'x' and 'y': ");
    fgets(equation, MAX_EQUATION_LENGTH, stdin);
    equation[strcspn(equation, "\n")] = 0;

    while (1) {
        plot_equation(equation, settings);

        printf("\nOptions:\n");
        printf("1. Zoom in (+)\n");
        printf("2. Zoom out (-)\n");
        printf("3. Move left (<)\n");
        printf("4. Move right (>)\n");
        printf("5. Move up (^)\n");
        printf("6. Move down (v)\n");
        printf("7. New equation\n");
        printf("8. Exit\n");
        printf("Choose option: ");

        scanf(" %c", &choice);

        switch (choice) {
            case '1': settings.zoom *= 1.5; break;
            case '2': settings.zoom /= 1.5; break;
            case '3': settings.x_offset -= 1.0 / settings.zoom; break;
            case '4': settings.x_offset += 1.0 / settings.zoom; break;
            case '5': settings.y_offset += 1.0 / settings.zoom; break;
            case '6': settings.y_offset -= 1.0 / settings.zoom; break;
            case '7': 
                printf("Enter new equation: ");
                getchar(); // Clear the newline character from previous input
                fgets(equation, MAX_EQUATION_LENGTH, stdin);
                equation[strcspn(equation, "\n")] = 0;
                settings = (PlotSettings){1.0, 0.0, 0.0}; // Reset plot settings
                break;
            case '8': return 0;
            default: printf("Invalid option!\n");
        }
    }

    return 0;
}