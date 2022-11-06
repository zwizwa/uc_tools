typedef float val_t;
typedef int idx_t;
val_t add(val_t a, val_t b) { return a + b; }

/* types:
i2: idx
r10: val
v5: vec(val, 17)
i4: idx
a1: vec(vec(val, 13), 17)
r8: val
v3: vec(vec(val, 17), 13)
s6: vec(vec(val, 17), 13)
r9: val
r7: val
r11: val
*/
void fun(
  val_t s6[13][17],
  val_t a1[17][13],
  val_t v3[13][17]
)
{
  for(idx_t i2 = 0; i2 < 13; i2++) {
    for(idx_t i4 = 0; i4 < 17; i4++) {
      val_t r7 = s6[i2][i4];
      val_t r8 = add(r7, 1);
      s6[i2][i4] = r8;
      val_t r9 = add(r7, i2);
      val_t r10 = add(r9, i4);
      val_t r11 = add(r10, a1[i4][i2]);
      v3[i2][i4] = r11;
    }
  }
}
