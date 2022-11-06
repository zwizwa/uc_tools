typedef float val_t;
typedef int idx_t;
val_t add(val_t a, val_t b) { return a + b; }

void fun(
  //state:
  val_t s7[13][17][29],
  //args: #<nil>
  //out:
  val_t v2[13][17][29]
)
{
  for(idx_t i1 = 0; i1 < 13; i1++) {
    for(idx_t i3 = 0; i3 < 17; i3++) {
      for(idx_t i5 = 0; i5 < 29; i5++) {
        val_t r8 = s7[i1][i3][i5];
        val_t r9 = add(r8, 1);
        s7[i1][i3][i5] = r9;
        val_t r10 = add(i1, i3);
        val_t r11 = add(r10, i5);
        val_t r12 = add(r11, r8);
        v2[i1][i3][i5] = r12;
      }
    }
  }
}

void test(void) {
    val_t s7[13][17][29];
    val_t v2[13][17][29];
    fun(s7,v2);
}
