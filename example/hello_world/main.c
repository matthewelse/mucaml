#include "stdio.h"

extern int mucaml_main(void);

int main() {
  int result = mucaml_main();

  printf("Result: %d\n", result);

  return result;
}
