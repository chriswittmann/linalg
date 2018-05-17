#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*
Goals:
Resolve matrix dimensions (currently they are transposed)
Two dimensional inverse
Three dimensional inverse (use submatrix determinants)

Make functions to free memory allocated to matrices or vectors

Make determinant pick optimal (greatest number of zeros) direction (top row or left column).

Make cholesky struct, LU struct, SVD struct.
when looking to solve Ax=b, run A through conditional checks to decide on method
then 


Eigenvalues
Eigenvectors
Principal components
Linear Discriminant Analysis

LU decomposition
LDL' Cholesky factorization
QR decomposition

Gram-schmidt

*/

typedef struct Vector {
    int length;
    double* values;
} Vector;

typedef struct Matrix { 
    int nrow; 
    int ncol;
    double** values;
} Matrix;

double** fill_matrix(int nrow, int ncol, double* entries) {
    // Maybe attempt to do the malloc in one line eventually
    // double (*matrix)[c] = malloc(sizeof(double[r][c]));
    double** matrix = malloc((sizeof *matrix) * nrow * ncol);
    for (int i = 0; i < nrow; i++) {
        matrix[i] = malloc((sizeof *matrix[i]) * nrow);
        for (int j = 0; j < ncol; j++) {
            matrix[i][j] = entries[i + j*nrow];
        }
    }
    return matrix;
}

struct Matrix create_matrix(int nrow, int ncol, double* entries) {
    struct Matrix matrix;
    matrix.nrow = nrow;
    matrix.ncol = ncol;
    matrix.values = fill_matrix(nrow, ncol, entries);
    return matrix;
}

// free entries?
struct Vector create_vector(int length, double* entries) {
    struct Vector vector;
    vector.length = length;
    vector.values = calloc(length, (sizeof *vector.values));
    for (int i = 0; i < length; i++) {
        vector.values[i] += entries[i];
    }
    return vector;
    
}
struct Matrix I(int n) {
    double* entries = calloc(n*n, (sizeof *entries));
    for (int i = 0; i < n*n;    ) {
        entries[i] = 1.0;
        i += (n+1);
    }
    struct Matrix matrix = create_matrix(n, n, entries);
    free(entries);
    return matrix;
}

struct Matrix ZeroMatrix(int nrow, int ncol) {
    double* entries = calloc(nrow*ncol, (sizeof *entries));
    struct Matrix matrix = create_matrix(nrow, ncol, entries);
    free(entries);
    return matrix;
}

struct Vector ZeroVector(int n) {
    double* entries = calloc(n, sizeof *entries);
    struct Vector zero = create_vector(n, entries);
    return zero;
}

struct Vector get_column(int j, struct Matrix matrix) {
    double* columnvals = malloc(sizeof *columnvals * matrix.nrow);
    for (int i = 0; i < matrix.nrow; i++) {
        columnvals[i] = matrix.values[i][j];
    }
    struct Vector column = create_vector(matrix.nrow, columnvals);
    free(columnvals);
    return column;
}

void set_column(int j, struct Matrix matrix, struct Vector vector) {
    if (matrix.nrow != vector.length) {
        printf("New column improper size!");
        return;
    }
    for (int i = 0; i < matrix.nrow; i++) {
        matrix.values[i][j] = vector.values[i];
    }
    return;
}

struct Vector get_row(int i, struct Matrix matrix) {
    double* rowvals = malloc(sizeof *rowvals * matrix.ncol);
    for (int j = 0; j < matrix.ncol; j++) {
        rowvals[j] = matrix.values[i][j];
    }
    struct Vector row = create_vector(matrix.ncol, rowvals); 
    free(rowvals);
    return row;
}

void set_row(int i, struct Matrix matrix, struct Vector vector) {
    if (matrix.ncol != vector.length) {
        printf("New column improper size!");
        return;
    }
    for (int j = 0; j < matrix.ncol; j++) {
        matrix.values[i][j] = vector.values[j];
    }
    return;
}


void print_matrix(struct Matrix matrix) {
    for (int i = 0; i < matrix.nrow; i++) {
        for (int j = 0; j < matrix.ncol; j++) {
            printf("%f ", matrix.values[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void print_vector(struct Vector vector) {
    for (int i = 0; i < vector.length; i++) {
        printf("%f ", vector.values[i]);
    }
    printf("\n \n");
}

struct Matrix transpose(struct Matrix matrix) {
    int nrow = matrix.nrow;
    int ncol = matrix.ncol;
    double* entries = malloc((sizeof *entries) * nrow * ncol);
    int counter = 0;
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            entries[counter] = matrix.values[i][j];
            counter++;
        }
    }
    struct Matrix trans = create_matrix(nrow, ncol, entries);
    return trans;
}

double inner_product(struct Vector x, struct Vector y) {
    double acc = 0.0; 
    if (x.length != y.length) {
        printf("Error: Vectors must have same length!");
        return 0;
    }
    for (int i = 0; i < x.length; i++) {
        acc += x.values[i] * y.values[i];
    }
    return acc;
}

// intended only for use within matrix multiplications (where can check to make sure dimensions agree)
double dot(int n, double* x, double* y) {
    double acc = 0.0;
    for (int i = 0; i < n; i++) {
        acc += x[i] * y[i];
    }
    return acc;
}

// outer product
struct Matrix outer_product(struct Vector x, struct Vector y) {
    // each element of x will be scalar multiplied with the elements of y to make a row
    int nrow = x.length;
    int ncol = y.length;
    double* entries = malloc((sizeof *entries) * nrow * ncol);
    int counter = 0;
    for (int j = 0; j < ncol; j++) {
        for (int i = 0; i < nrow; i++) {
            entries[counter] = x.values[i] * y.values[j];
            counter++;
        }
    }
    struct Matrix outer = create_matrix(nrow, ncol, entries);
    return outer;
}

struct Matrix matrix_sum(struct Matrix A, struct Matrix B) {
    double* entries = calloc(A.nrow * A.ncol, sizeof *entries);
    struct Matrix C = create_matrix(A.nrow, A.ncol, entries);
    free(entries);
    for (int i = 0; i < A.nrow; i++) {
        for (int j = 0; j < A.ncol; j++) {
            C.values[i][j] = A.values[i][j] + B.values[i][j];
        }
    }
    return C;
}

struct Vector vector_sum(struct Vector x, struct Vector y) {
    double* entries = malloc(sizeof *entries * x.length);
    for (int i = 0; i < x.length; i++) {
        entries[i] = x.values[i] + y.values[i];
    }
    struct Vector z = create_vector(x.length, entries);
    free(entries);
    return z;
}

// maybe make this an operator instead of a function if possible
struct Matrix mult(struct Matrix A, struct Matrix B) {
    // Matrix multiplication is just a sequence of dot products.
    // ij entry is equal to the dot product of ith row of A and jth column of B
    int rowcheck = A.ncol;
    int colcheck = B.nrow;
    if (rowcheck != colcheck) {
        printf("Dimensions incompatible!");
        return ZeroMatrix(1, 1);
    }
    int nrow = A.nrow;
    int ncol = B.ncol;
    struct Matrix product = ZeroMatrix(nrow, ncol);
    double d;
    for (int j = 0; j < ncol; j++) {
        struct Vector column = get_column(j, B);
        for (int i = 0; i < nrow; i++) {
            struct Vector row = get_row(i, A);
            d = inner_product(column, row);
            product.values[i][j] = d;
            free(row.values);
        }
        free(column.values);
    }
    return product;
}

// maybe make this an operator instead of a function if possible
struct Matrix scalarMult(double c, struct Matrix matrix) {
    int nrow = matrix.nrow;
    int ncol = matrix.ncol;
    for (int i = 0; i < ncol; i++) {
        for (int j = 0; j < nrow; j++) {
            matrix.values[i][j] *= c;
        }
    }
    return matrix;
}

struct Vector scalar(double c, struct Vector vector) {
    for (int i = 0; i < vector.length; i++) {
        vector.values[i] *= c;
    }
    return vector;
}

double trace(struct Matrix matrix) {
    double t = 0;
    int nrow = matrix.nrow;
    int ncol = matrix.ncol;
    if (nrow != ncol) {
        printf("Trace only defined for square matrices!");
        return 0;
    }
    for (int i = 0; i < nrow; i++) {
        t += matrix.values[i][i];
    }
    return t;
}

int check_square(struct Matrix matrix) {
    if (matrix.nrow == matrix.ncol) {
        return 1;
    }
    return 0;
}
// clunky but works, goes along top row
double determinant(struct Matrix matrix) {
    double det = 0.0;
    if (!check_square(matrix)) {
        printf("Determinant only defined for square matrices!");
        return det;
    }
    if (matrix.nrow == 1) {
        return matrix.values[0][0];
    }
    if (matrix.nrow == 2) {
        return matrix.values[0][0]*matrix.values[1][1] - matrix.values[0][1]*matrix.values[1][0];
    }
    int n = matrix.ncol;
    for (int i = 0; i < n; i++) {
        double weight = matrix.values[i][0];
        if (weight == 0.0) {
            continue;
        }
        double sgn = pow(-1.0, i);
        double* entries = malloc(sizeof *entries * (n-1) * (n-1));
        int counter = 0;
        for (int j = 0; j < n; j++) {
            if (i == j) {
                continue;
            }
            for (int k = 1; k < n; k++) {
                entries[counter] = matrix.values[j][k];
                counter++;
            }
        }
        struct Matrix submatrix = create_matrix(n-1, n-1, entries);
        double subdet = determinant(submatrix);
        det += (sgn * weight * subdet);
        free(submatrix.values);
        free(entries);
    }
    return det;
}

int check_positive_definite(struct Matrix matrix) {
    if (!check_square(matrix)) {
        return 0;
    }
    double det = determinant(matrix);
    if (det == 0.0) {
        return 0;
    }
    for (int i = 1; i < matrix.nrow; i++) {
        double* entries  = malloc((sizeof *entries) * i * i);
        int counter = 0;
        for (int j = 0; j < i; j++) {
            for (int k = 0; k < i; k++) {
                entries[counter] = matrix.values[j][k];
                counter++;
            }
        }
        struct Matrix submatrix = create_matrix(i, i, entries);
        det = determinant(submatrix);
        if (det == 0.0) {
            return 0;
        }
        free(entries);
    }
    return 1;
}

int check_symmetric(struct Matrix matrix) {
    struct Matrix trans = transpose(matrix);
    for (int i = 0; i < matrix.ncol; i++) {
        for (int j = 0; j < matrix.nrow; j++) {
            if (matrix.values[i][j] != trans.values[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

void cholesky(struct Matrix matrix) {
    if (!check_positive_definite(matrix)) {
        printf("Matrix not positive definite!");
        return;
    }
    if (!check_symmetric(matrix)) {
        printf("Matrix not symmetric!");
        return;
    }
    double* entries = calloc(matrix.nrow * matrix.ncol, sizeof *entries);
    struct Matrix L = create_matrix(matrix.nrow, matrix.ncol, entries);
    // returns L' for some reason
    for (int j = 0; j < matrix.ncol; j++) {
        double calc = 0.0;
        for (int k = 0; k < j; k++) {
            calc += L.values[j][k] * L.values[j][k];
        }
        L.values[j][j] = sqrt(matrix.values[j][j] - calc);
        for (int i = j+1; i < matrix.nrow; i++) {
            calc = 0.0;
            for (int k = 0; k < j; k++) {
                calc += L.values[i][k] * L.values[j][k];
            }
            L.values[i][j] = (matrix.values[i][j] - calc) / L.values[j][j];
        }
    }
    printf("L\n");
    print_matrix(L);
    printf("\nL'\n");
    print_matrix(transpose(L));
    printf("\n \n");
    return;
}


void LDL(struct Matrix matrix) {
    if (!check_positive_definite(matrix)) {
        printf("Matrix not positive definite!");
        return;
    }
    if (!check_symmetric(matrix)) {
        printf("Matrix not symmetric!");
        return;
    }
    struct Matrix L = I(matrix.nrow);
    struct Matrix D = I(matrix.nrow);
    double calc = 0.0;
    for (int j = 0; j < matrix.ncol; j++) {
        calc = 0.0;
        for (int k = 0; k < j; k++) {
            calc += L.values[j][k] * L.values[j][k] * D.values[k][k];
        }
        D.values[j][j] = matrix.values[j][j] - calc;
        for (int i = j; i < matrix.nrow; i++) {
            calc = 0.0;
            for (int k = 0; k < j; k++) {
                calc += L.values[i][k] * L.values[j][k] * D.values[k][k];
            }
            L.values[i][j] = (matrix.values[i][j] - calc) / D.values[j][j];
        }
    }
    printf("L \n");
    print_matrix(L);
    printf("D \n");
    print_matrix(D);
    printf("L' \n");
    print_matrix(transpose(L));
    return;
}

struct Vector Usolve(struct Matrix U, struct Vector b) {
    double* entries = malloc(sizeof *entries * U.ncol);
    for (int i = U.ncol-1; i > -1; i--) {
        double calc = 0.0;
        for (int j = U.ncol-1; j > i; j--) {
            calc += U.values[i][j] * entries[j];
        }
        entries[i] = (b.values[i] - calc) / U.values[i][i];
    }
    struct Vector x = create_vector(U.ncol, entries);
    return x;
}

struct Vector Lsolve(struct Matrix L, struct Vector b) {
    double* entries = malloc(sizeof *entries * L.ncol);
    for (int i = 0; i < L.ncol; i++) {
        double calc = 0.0;
        for (int j = 0; j < i; j++) {
            calc += L.values[i][j] * entries[j];
        }
        entries[i] = (b.values[i] - calc) / L.values[i][i];
    }
    struct Vector x = create_vector(L.ncol, entries);
    return x;
}
struct Vector solve(struct Matrix A, struct Vector b) {
    if (!check_positive_definite(A)) {
        printf("Matrix not positive definite!");
        b = ZeroVector(A.ncol);
        return b;
    }
    if (!check_symmetric(A)) {
        printf("Matrix not symmetric!");
        b = ZeroVector(A.ncol);
        return b;
    }
    double* entries = calloc(A.nrow * A.ncol, sizeof *entries);
    struct Matrix L = create_matrix(A.nrow, A.ncol, entries);
    for (int j = 0; j < A.ncol; j++) {
        double calc = 0.0;
        for (int k = 0; k < j; k++) {
            calc += L.values[j][k] * L.values[j][k];
        }
        L.values[j][j] = sqrt(A.values[j][j] - calc);
        for (int i = j+1; i < A.nrow; i++) {
            calc = 0.0;
            for (int k = 0; k < j; k++) {
                calc += L.values[i][k] * L.values[j][k];
            }
            L.values[i][j] = (A.values[i][j] - calc) / L.values[j][j];
        }
    }
    struct Matrix Lt = transpose(L);
    // Ly = b, L'x = y -> LL'x = b. 
    struct Vector y = Lsolve(L, b);
    struct Vector x = Usolve(Lt, y);
    return x;
}

// projecting v onto u
struct Vector proj(struct Vector u, struct Vector v) {
    double c = inner_product(u,v) / inner_product(u,u);
    struct Vector projection = scalar(c, u);
    return projection;
}


struct Vector matvec_mult(struct Matrix matrix, struct Vector vector) {
    if (matrix.ncol != vector.length) {
        printf("Dimensions don't agree!");
        return ZeroVector(vector.length);
    }
    struct Vector new = ZeroVector(vector.length);
    for (int i = 0; i < matrix.nrow; i++) {
        for (int j = 0; j < matrix.ncol; j++) {
            new.values[i] += matrix.values[i][j] * vector.values[j];
        }
    }
    return new;
}

// not well defined
struct Vector power_method(struct Matrix matrix, struct Vector vector) {
    double c;
    for (int i = 0; i < 2; i++) {
        vector = matvec_mult(matrix, vector);
        c = inner_product(vector, vector);
        vector = scalar(sqrt(c), vector);
    }
    return vector;
}

struct Matrix gram_schmidt(struct Matrix matrix) {
    // orthogonalize
    struct Matrix new = ZeroMatrix(matrix.nrow, matrix.ncol);
    for (int j = 0; j < matrix.ncol; j++) {
        struct Vector u = get_column(j, matrix);
        for (int k = 0; k < j; k++) {
            struct Vector v = get_column(k, new);
            struct Vector p = scalar(-1.0, proj(v, u));
            u = vector_sum(u, p);
        }
        // normalize
        double normalization_factor = 1.0 / sqrt(inner_product(u, u));
        u = scalar(normalization_factor, u);
        set_column(j, new, u);
    }
    return new;
}

void QR(struct Matrix A) {
    // decompose A = QR where Q orthogonal, R upper triangular
    // A = QR => Q'A = R
    struct Matrix Q = gram_schmidt(A);
    struct Matrix R = mult(transpose(Q), A);
    printf("Q \n");
    print_matrix(Q);
    printf("R \n");
    print_matrix(R);
    return;
}

struct Matrix givens_rotation(int j, struct Matrix matrix) {
    double r = sqrt(pow(matrix.values[j][j], 2) + pow(matrix.values[j+1][j], 2));
    double c = matrix.values[j][j] / r;
    double s = matrix.values[j+1][j] / r;
    struct Matrix G = I(matrix.nrow);
    G.values[j+1][j] = s;
    G.values[j][j+1] = -1.0 * s;
    G.values[j][j] = c;
    G.values[j+1][j+1] = c;
    return G;
}


// givens rotations only work on entries adjacent to diagonal?
void QR_givens(struct Matrix A) {
    struct Matrix Q = I(A.nrow);
    for (int j = 0; j < A.ncol - 1; j++) {
        if (A.values[j+1][j] == 0.0) {
            continue;
        }
        struct Matrix G = givens_rotation(j, A);
        Q = mult(Q, transpose(G));
        A = mult(G, A);
        free(G.values);
    }
    printf("Q \n");
    print_matrix(Q);
    printf("R \n");
    print_matrix(A);
}

int main(void) {
    


}

