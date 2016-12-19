#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <pthread.h>


enum val_ty {
  INT,
  UINT,
  FLOAT,
  BOOL
};

typedef struct stream_t {
  int buf[256];
  long size;
  long n;
  pthread_mutex_t m;
  pthread_cond_t c;
} stream_t;

typedef void (*kernel_t)(stream_t*, stream_t*);  

typedef struct arg_t {
  stream_t* in;
  stream_t* out;
  kernel_t c;
} arg_t;

int pop(stream_t* s) {
  pthread_mutex_lock(&(s->m));
  while (s->n == -1) {
    pthread_cond_wait(&(s->c), &(s->m));
  }    

  int val;
  if (s->n == s->size) {
    val = s->buf[s->n--];
    pthread_cond_signal(&(s->c));
  } else {
    val = s->buf[s->n--];
  }

  pthread_mutex_unlock(&(s->m));
  return val;
} 

void push(stream_t* s, int val) {
  pthread_mutex_lock(&(s->m));
  while (s->n == s->size) {
    pthread_cond_wait(&(s->c), &(s->m));
  } 

  if (s->n == -1) {
    s->n++;
    s->buf[s->n++] = val;
    pthread_cond_signal(&(s->c));
  } else {
    s->buf[s->n++] = val;
  }

  pthread_mutex_unlock(&(s->m));
}

void source(stream_t* in, stream_t* out) {
  for (int i=0; i < 10; i++) { 
    printf("[Source] %d\n", i);  
    push(out, i);
  }
}

void sink(stream_t* in, stream_t* out) {
  while(1) {
    int val = pop(in);
    printf("[Sink] %d\n", val);
  }
}

void map(stream_t* in, stream_t* out) {
  for (int i=0; i < 10; i++) {
    int val = pop(in);
    printf("[Map] %d\n", val);
    val += val+1;
    // printf("[Map] %d\n", val);
    push(out, val);
  }
}

/*
void prefix_sum(stream_t* in, stream_t* out) {
  int sum = 0;
  for (int i=0; i < 10; i++) {
    int val = pop(in); 
    sum += val;
  }
  push(out, sum);
}
*/

void* runnable(void* arg) {
  arg_t* args = (arg_t*) arg;
  kernel_t c = (kernel_t) args->c;
  c(args->in, args->out);
  return NULL;
}

pthread_t* spawn_kernel(kernel_t c, stream_t* in, stream_t* out) {
  pthread_t* thr = (pthread_t*)malloc(sizeof(pthread_t));
  arg_t* args = (arg_t*) malloc(sizeof(arg_t));
  args->in = in;
  args->out = out;
  args->c = c; 
  int rc = pthread_create(thr, NULL, runnable, (void *) args);
  if (rc){
    printf("ERROR; return code from pthread_create() is %d\n", rc);
    exit(-1);
  }
  return thr;
}

stream_t* init_stream() {
  stream_t* s = (stream_t*)calloc(1, sizeof(stream_t)); 
  pthread_mutex_init(&(s->m), NULL);
  pthread_cond_init (&(s->c), NULL);
  return s;
}

int main() {

  // source -> map -> sink 
  // Creates the stream graph
  stream_t* sin  = NULL;    
  stream_t* sout = init_stream();    
  pthread_t* t0 = spawn_kernel(source, sin, sout);

  sin  = sout; 
  sout = init_stream(); 
  pthread_t* t1 = spawn_kernel(map, sin, sout);

  sin  = sout; 
  sout = NULL; 
  pthread_t* t2 = spawn_kernel(sink, sin, sout);

  pthread_join(*t0, NULL);
  pthread_join(*t1, NULL);
  pthread_join(*t2, NULL);

}
