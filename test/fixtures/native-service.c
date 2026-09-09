#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
static int exact(int fd, unsigned char *p, size_t n, int writing) {
  while(n) { ssize_t k = writing ? write(fd,p,n) : read(fd,p,n); if(k<=0) return 0; p+=k; n-=(size_t)k; } return 1;
}
int main(void) {
 unsigned char b[10];
 while(exact(0,b,10,0)) {
  if(b[0]!=0 || b[1]!=8) return 3;
  uint64_t bits=0; for(int i=2;i<10;i++) bits=(bits<<8)|b[i];
  double x; memcpy(&x,&bits,8);
  if(x==91.0) return 7;
  if(x==92.0) { sleep(5); }
  if(x==93.0) { b[2]=0x7f; b[3]=0xf0; memset(b+4,0,6); }
  if(x==94.0) b[1]=9;
  if(!exact(1,b,10,1)) return 4;
 }
 return 0;
}
