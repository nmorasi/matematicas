#include<iostream>
struct par{
  void* fst;
  void* snd;

};
/* para que esto funcione la matriz u debe tener 0's bajo la diagonal
   la matriz l debe tener 0's sobre la diagonal y 1 en la diagonal 
 */
void swap(double* a , double* b){
  double temp = *a;
  *a = *b;
  *b = temp;
}
void descomposicion_L_U(double m[4][4], double l[4][4] , double u[4][4]){
  for(int k = 0; k < 4 ; k ++){
    u[k][k] = m[k][k];
    for(int i = k+1 ; i < 4 ; i++){
      l[i][k] = m[i][k] / u[k][k];
      u[k][i] = m[k][i];
    }
    for(int i = k+1 ; i < 4; i ++){
      for(int j = k+1 ; j < 4; j++){
	m[i][j] = m[i][j] - (l[i][k]*u[k][j]);
      }
    }
  }
}
double productoPunto(double m1[4][4],int i,double m2[4][4],int j){
  double res = 0;
  for(int k = 0; k < 4; k++){
    res = res + (m1[i][k] * m2[k][j]);
  }
  return res;
}
void mult(double m1[4][4] ,double m2[4][4],double res[4][4] ){
  for(int i = 0 ; i < 4 ; i++){
    for(int j = 0 ; j < 4 ; j++){
      res[i][j] = productoPunto(m1,i,m2,j);
    }
  }
}
void imprimir_matriz(double m[4][4]){
  for(int i = 0 ; i < 4 ; i++){
    for(int j = 0 ; j < 4 ; j++){
      std::cout << m[i][j] << " ";
    }
    std::cout << "\n";
  }
}

int main(){
  double m1[4][4]= {{2,3,1,5},{6,13,5,19},
		   {2,19,10,23},{4,10,11,31}};
  double l1[4][4] = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};
  double u1[4][4] = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
  descomposicion_L_U(m1,l1,u1);
  imprimir_matriz(l1);
  std::cout << "------------\n" ;
  imprimir_matriz(u1);
  std::cout << "prueba para la multiplicacion de matrices\n";
  double res[4][4] = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
  imprimir_matriz(res);
  std::cout <<"----------\n";
  mult(l1,u1,res);
  imprimir_matriz(res);
  std::cout << "prueaba para el intercambio\n";
  double arr1[] = {1,2,3,4,5};
  std::cout << "imprimir arrelglo\n";
  for(int i = 0 ; i < 5 ; i++){
    std::cout << arr1[i]<< " "; 
  }
  swap(&arr1[0],&arr1[1]);
  std::cout << "imprimir arreglo intercambiado\n";
  for(int i = 0 ; i < 5 ; i++){
    std::cout << arr1[i]<< " "; 
  }
}
