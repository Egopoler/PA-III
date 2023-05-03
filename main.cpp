//Egor Poliakov
#include <vector>
#include <iostream>
#include <iomanip>
#include <string>
#include <math.h>


using namespace std;

class Matrix {
public:
    int m_size;
    int n_size;
    vector<vector<double>> Arr_M;
    bool error_matrix;
public:
    Matrix(int m_size, int n_size, bool error_matrix) {

        this->error_matrix = error_matrix;
        this->m_size = m_size;
        this->n_size = n_size;
        vector<double> v;
        for(int mi = 0; mi < m_size; mi++){
            v.clear();
            for (int ni = 0; ni < n_size; ni++){
                v.push_back(0);
            }
            this->Arr_M.push_back(v);
        }
    }
    Matrix(int m_size, int n_size, vector<vector<double>> Arr_inp, bool error_matrix) {
        this->error_matrix = error_matrix;
        this->Arr_M = Arr_inp;
        this->m_size = m_size;
        this->n_size = n_size;
    }
    Matrix operator=(Matrix other) {
        Arr_M = other.Arr_M;
        m_size = other.m_size;
        n_size = other.n_size;
        error_matrix = other.error_matrix;
        return *this;
    }
    Matrix operator+(Matrix other){
        Matrix temp(m_size, n_size, Arr_M, false);
        if (this->m_size == other.m_size && this->n_size == other.n_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < this->getNsize(); ni++)
                    temp.set(mi, ni,this->get(mi, ni) +
                                    other.get(mi, ni));
            }
            return temp;
        }else {
            return Matrix(0,0, true);
        }
    }
    Matrix operator-(Matrix other){
        Matrix temp(m_size, n_size, Arr_M, false);
        if (this->m_size == other.m_size && this->n_size == other.n_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < this->getNsize(); ni++)
                    temp.set(mi, ni,
                             this->get(mi, ni) -
                             other.get(mi, ni));
            }
            return temp;
        }else{
            return Matrix(0,0, true);
        }
    }

    Matrix operator*(Matrix& other){
        Matrix new_matrix(this->getMsize(), other.getNsize(),
                          false);
        if(this->n_size == other.m_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < other.getNsize(); ni++){
                    new_matrix.Arr_M[mi][ni] = 0;
                    for(int counter = 0; counter < this->getNsize();counter++){
                        new_matrix.Arr_M[mi][ni] += this->Arr_M[mi][counter] *
                                                    other.Arr_M[counter][ni];
                    }
                }
            }
            return new_matrix;
        }else {

            this->error_matrix = true;
            return Matrix(0,0, true);
        }
    }

    void Transition(){
        Matrix new_matrix(this->getNsize(),
                          this->getMsize(),false);
        for (int mi = 0; mi < this->getMsize(); mi++) {
            for (int ni = 0; ni < this->getNsize(); ni++)
                new_matrix.set(ni, mi,
                               this->get(mi, ni));
        }
        this->Arr_M = new_matrix.Arr_M;
        this->m_size = new_matrix.getMsize();
        this->n_size = new_matrix.getNsize();
    }
    int getNsize() {
        return this->n_size;
    }
    int getMsize() {
        return this->m_size;
    }
    void set(int m_pos, int n_pos, int value){
        this->Arr_M[m_pos][n_pos] = value;
    }
    int get(int m_pos, int n_pos){
        return Arr_M[m_pos][n_pos];
    }
    vector<vector<double>> getArr(){return this->Arr_M;}
    bool getErrorMatrix(){
        return this->error_matrix;
    }

    friend istream& operator>>(istream& in, Matrix& mat) {
        vector<vector<double>> now_vector;
        vector<double> v;
        double element;
        for(int mi = 0; mi < mat.m_size; mi++){
            v.clear();
            for (int ni = 0; ni < mat.n_size; ni++){
                in >> element;
                v.push_back(element);
            }
            now_vector.push_back(v);
        }
        mat.Arr_M = now_vector;
        return in;
    }

    friend ostream& operator<<(ostream& out, const Matrix& mat) {
        if (mat.error_matrix){
            out << "Error: the dimensional problem occurred" << endl;
            return out;
        }
        for(int i = 0; i < mat.m_size; i++) {
            for(int j = 0; j < mat.n_size; j++) {
                double value = mat.Arr_M[i][j];
                if (abs(value) < 0.00005) {
                    value = 0;
                }
                out << fixed << setprecision(4) << value << " ";
            }
            out << endl;
        }
        return out;
    }

};
class ColumnVector: public Matrix{
public:
    ColumnVector(int m_size, bool error_matrix) : Matrix(m_size, 1, error_matrix) {}
    ColumnVector(int m_size, vector<vector<double>> v, bool error_matrix) : Matrix(m_size, 1, v,  error_matrix) {}
    Matrix operator*(ColumnVector& other){
        Matrix new_matrix(this->getMsize(), other.getNsize(),
                          false);
        if(this->n_size == other.m_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < other.getNsize(); ni++){
                    new_matrix.Arr_M[mi][ni] = 0;
                    for(int counter = 0; counter < this->getNsize();counter++){
                        new_matrix.Arr_M[mi][ni] += this->Arr_M[mi][counter] *
                                                    other.Arr_M[counter][ni];
                    }
                }
            }
            return new_matrix;
        }else {

            this->error_matrix = true;
            return Matrix(0,0, true);
        }
    }
    ColumnVector operator=(Matrix other) {
        Arr_M = other.Arr_M;
        m_size = other.m_size;
        n_size = other.n_size;
        error_matrix = other.error_matrix;
        return *this;
    }
    Matrix diagonalNormal(Matrix A){
        for(int i = 0; i < this->m_size; i++){
            double value = A.Arr_M[i][i];
            A.Arr_M[i][i] = 1;
            this->Arr_M[i][0] /= value;

        }
        return A;
    }

};

class SquareMatrix: public Matrix{
public:
    SquareMatrix(int m_size, bool error_matrix) : Matrix(m_size, m_size, error_matrix) {
    }
    SquareMatrix(int m_size, vector<vector<double>> Arr_inp, bool error_matrix) : Matrix(m_size, m_size, Arr_inp, error_matrix) {
    }
    Matrix operator*(SquareMatrix& other){
        Matrix new_matrix(this->getMsize(), other.getNsize(),
                          false);
        if(this->n_size == other.m_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < other.getNsize(); ni++){
                    new_matrix.Arr_M[mi][ni] = 0;
                    for(int counter = 0; counter < this->getNsize();counter++){
                        new_matrix.Arr_M[mi][ni] += this->Arr_M[mi][counter] *
                                                    other.Arr_M[counter][ni];
                    }
                }
            }
            return new_matrix;
        }else {

            this->error_matrix = true;
            return Matrix(0,0, true);
        }
    }

    Matrix operator*(ColumnVector& other){
        Matrix new_matrix(this->getMsize(), other.getNsize(),
                          false);
        if(this->n_size == other.m_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < other.getNsize(); ni++){
                    new_matrix.Arr_M[mi][ni] = 0;
                    for(int counter = 0; counter < this->getNsize();counter++){
                        new_matrix.Arr_M[mi][ni] += this->Arr_M[mi][counter] *
                                                    other.Arr_M[counter][ni];
                    }
                }
            }
            return new_matrix;
        }else {

            this->error_matrix = true;
            return Matrix(0,0, true);
        }
    }
    Matrix operator*(Matrix& other){
        Matrix new_matrix(this->getMsize(), other.getNsize(),
                          false);
        if(this->n_size == other.m_size){
            for (int mi = 0; mi < this->getMsize(); mi++){
                for (int ni = 0; ni < other.getNsize(); ni++){
                    new_matrix.Arr_M[mi][ni] = 0;
                    for(int counter = 0; counter < this->getNsize();counter++){
                        new_matrix.Arr_M[mi][ni] += this->Arr_M[mi][counter] *
                                                    other.Arr_M[counter][ni];
                    }
                }
            }
            return new_matrix;
        }else {

            this->error_matrix = true;
            return Matrix(0,0, true);
        }
    }
    SquareMatrix operator=(Matrix other) {
        Arr_M = other.Arr_M;
        m_size = other.m_size;
        n_size = other.n_size;
        error_matrix = other.error_matrix;
        return *this;
    }


};

class IdentityMatrix: public SquareMatrix{
public:

    IdentityMatrix(int m_size,  bool error_matrix) : SquareMatrix(m_size,   error_matrix){
        this->Arr_M.clear();
        vector<double> v;
        for(int mi = 0; mi < m_size; mi++){
            v.clear();
            for (int ni = 0; ni < n_size; ni++){
                if(mi == ni){
                    v.push_back(1);
                }else {
                    v.push_back(0);
                }
            }
            this->Arr_M.push_back(v);
        }
    }
    friend ostream& operator<<(ostream& out, const IdentityMatrix& mat) {
        if (mat.error_matrix){
            out << "Error: the dimensional problem occurred" << endl;
            return out;
        }
        for(int i = 0; i < mat.m_size; i++) {
            for(int j = 0; j < mat.n_size; j++) {
                double value = mat.Arr_M[i][j];
                if (abs(value) < 0.00005) {
                    value = 0;
                }
                out << fixed << setprecision(4) << value << " ";
            }
            out << endl;
        }
        return out;
    }
    IdentityMatrix operator=(Matrix other) {
        Arr_M = other.Arr_M;
        m_size = other.m_size;
        n_size = other.n_size;
        error_matrix = other.error_matrix;
        return *this;
    }

};

class EliminationMatrix: public IdentityMatrix{
public:
    EliminationMatrix(int m_size, bool error_matrix) : IdentityMatrix(m_size,  error_matrix){

    }
    void Elim(int mi, int ni, Matrix A){
        double value = A.Arr_M[mi - 1][ni - 1];
        value = (value * -1) / A.Arr_M[ni - 1][ni - 1];
        if (abs(value) < 0.00005) {
            value = 0;
        }
        this->Arr_M[mi - 1][ni - 1] = value;
    }

};

class PermutationMatrix: public EliminationMatrix{
public:
    PermutationMatrix(int m_size, bool error_matrix) : EliminationMatrix(m_size,   error_matrix){}
    void perm(int m1, int m2){
        vector<double> temp = Arr_M[m1 - 1];
        Arr_M[m1 - 1] = Arr_M[m2 - 1];
        Arr_M[m2 - 1] = temp;
    }
};



class Inverse{
public:
    SquareMatrix getInverse(SquareMatrix A){
        int M_size = A.m_size;
        IdentityMatrix I(M_size, false);
        int counter = 1;
        //cout << "step #0: Augmented Matrix" << endl;
        //gAM(A, I);
        //direct way
        //cout << "Direct way:" << endl;
        for(int i = 0; i < M_size; i++){
            int code = -1;
            double max_abs_value = abs(A.Arr_M[i][i]);
            for(int j = i + 1; j < M_size; j++){
                if ((max_abs_value) < abs(A.Arr_M[j][i])){
                    max_abs_value = abs(A.Arr_M[j][i]);
                    code = j;
                }

            }
            if(code != -1){
                PermutationMatrix P(M_size, false);
                P.perm(code + 1, i + 1);
                A = P * A;
                I = P * I;
                //cout << "step #" << counter << ": permutation" << endl;
                counter++;
                //gAM(A, I);
            }


            for(int j = i + 1; j < M_size; j++){
                if (A.Arr_M[j][i] == 0)
                    continue;
                //elim
                EliminationMatrix E(M_size, false);
                E.Elim(j + 1, i + 1, A);
                A = E * A;
                I = E * I;
                //cout << "step #"<<counter<<": elimination" << endl;
                counter++;
                //gAM(A, I);
            }
        }
        //cout << "Way back:" << endl;
        for(int i = M_size - 1; i > 0; i--){
            for(int j = i - 1; j >= 0; j--){
                if (A.Arr_M[j][i] == 0)
                    continue;
                EliminationMatrix E(M_size, false);
                E.Elim(j + 1, i + 1, A);
                A = E * A;
                I = E * I;
                //cout << "step #"<<counter<<": elimination" << endl;
                counter++;
                //gAM(A, I);
            }
        }
        //cout << "Diagonal normalization:" << endl;
        for(int i = 0; i < M_size; i++){
            double value = A.Arr_M[i][i];
            A.Arr_M[i][i] = 1;
            for(int j = 0; j < M_size; j++){
                I.Arr_M[i][j] /= value;
            }
        }
        return I;
    }
    void gAM(Matrix A, Matrix I){
        for(int i = 0; i < A.m_size; i++) {
            for(int j = 0; j < A.n_size; j++) {
                printf("%.4f ", A.Arr_M[i][j]);
            }
            for(int j = 0; j < A.n_size; j++) {
                printf("%.4f ", I.Arr_M[i][j]);
            }
            printf("\n");
        }
    }
};

class SolveAbxLSA{
public:
    string SolveSystem(Matrix A, ColumnVector b){
        Matrix At = A;
        At.Transition();
        cout << "A:" << endl;
        cout << A;
        Matrix AtA = At * A;
        cout << "A_T*A:" << endl;
        cout << AtA;
        Inverse inverse;
        SquareMatrix AtAinv(1, false);
        AtAinv = AtA;
        cout << "(A_T*A)^-1:" << endl;
        AtAinv = inverse.getInverse(AtAinv);
        cout << AtAinv;
        Matrix AtB = At * b;
        cout << "A_T*b:" << endl;
        cout << AtB;
        cout << "x~:" << endl;
        Matrix xh = AtAinv * AtB;
        cout << xh;
        string a = "";
        string tempst = "";
        for (int i = 0; i < xh.m_size; ++i) {
            tempst = to_string(xh.Arr_M[i][0]) + "*x**" + to_string(i);
            if (i != 0)
                a += " + ";
            a += tempst;
        }
        return a;

    }
};

#define GNUPLOT_PATH "/opt/homebrew/bin/gnuplot -persist"

int main() {
    FILE *pipe = popen(GNUPLOT_PATH, "w");

    double v0, k0, a1, b1, a2, b2, T, N, V, K;
    cin >> V;
    cin >> K;
    cin >> a1;
    cin >> b1;
    cin >> a2;
    cin >> b2;
    cin >> T;
    cin >> N;
    v0 = V - (a2/b2);
    k0 = K - (a1/b1);
    vector<double> times;
    vector<double> victims;
    vector<double> killers;
    double cur_v, cur_k;
    double time_tick = T/N;
    double current_time = 0;
    for (int i = 0; i <= N; ++i) {
        cur_v = v0 * cos(sqrt(a1 * a2) * current_time) - k0 * ((sqrt(a2) *b1 / (b2*sqrt(a1))) * sin(sqrt(a1 * a2) * current_time) ) + (a2/b2);
        victims.push_back(cur_v);
        times.push_back(current_time);
        cur_k = (v0 * sqrt(a1) * b2 * sin(sqrt(a1 * a2) * current_time)) / (b1 * sqrt(a2)) + (k0 * cos(sqrt(a1 * a2) * current_time)) + (a1/b1);
        killers.push_back(cur_k);
        current_time += time_tick;
    }
    cout << "t:" << endl;
    for (int i = 0; i <= N; ++i) {
        cout << fixed << setprecision(2) << times[i];
        if (i != N)
            cout << " ";
    }
    cout << endl;
    cout << "v:" << endl;
    for (int i = 0; i <= N; ++i) {
        cout << fixed << setprecision(2) << victims[i];
        if (i != N)
            cout << " ";
    }
    cout << endl;
    cout << "k:" << endl;
    for (int i = 0; i <= N; ++i) {
        cout << fixed << setprecision(2) << killers[i];
        if (i != N)
            cout << " ";
    }
    cout << endl;

    /* this part for v(k)
    fprintf(pipe, "plot '-' using 1:2 t 'data1' with lines \n");
    for (int i = 0; i < N; ++i) {
        fprintf(pipe, "%f\t%f\n", killers[i], victims[i]);
    }
    fprintf(pipe, "e\n");

    fflush(pipe);
    pclose(pipe);
     */
    /* this part for v(t) and k(t)
    fprintf(pipe, "plot '-' using 1:2 t 'data1' with lines, '' using 1:2 t 'data2' with lines \n");
    for (int i = 0; i < N; ++i) {
        fprintf(pipe, "%f\t%f\n", times[i], victims[i]);
    }
    fprintf(pipe, "e\n");
    for (int i = 0; i < N; ++i) {
        fprintf(pipe, "%f\t%f\n", times[i], killers[i]);
    }
    fprintf(pipe, "e\n");

    fflush(pipe);
    pclose(pipe);
     */



    return 0;
}
