#include <pthread.h>
#include <stdio.h>

typedef void (*fptr)(void*);

int i;

void bar(void* arg) {
  fptr x = (fptr) arg;
  i++;//race
  x(arg);
}

fptr foo = &bar;

int main() {
  pthread_t t1;
  pthread_create(&t1, NULL, foo, foo);
  foo(foo);
}


