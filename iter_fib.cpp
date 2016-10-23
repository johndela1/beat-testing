using namespace std;
#include <iostream>

int fib(int n, int a=0, int b=1) {
  if (n <= 1) return a;
  return fib(n-1, b, a+b);
}

int main (int argc, char *argv[])
{
  cout << fib(atoi(argv[1])) << endl;
  return 0;
}
