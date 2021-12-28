
// first pass
#if 0
void testmod(state_t *s) {
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = SM_READ(s,s->chan1);
      s->e[2]/*c*/ = SM_READ(s,s->chan1);
      ({
        s->e[3]/*#0*/ = add(s->e[1]/*b*/,s->e[2]/*c*/);
        add(s->a/*free*/,s->e[3]/*#0*/);
      });
    });
    s->e[1]/*d*/ = SM_READ(s,s->chan1);
    s->e[2]/*e*/ = ({
      s->e[3]/*x*/ = ({
        s->e[4]/*#1*/ = ({
          s->e[5]/*#2*/ = 1;
          s->e[6]/*#3*/ = 2;
          add(s->e[5]/*#2*/,s->e[6]/*#3*/);
        });
        add(s->e[0]/*a*/,s->e[4]/*#1*/);
      });
      s->e[4]/*y*/ = ({
        s->e[5]/*#4*/ = 2;
        add(s->e[1]/*d*/,s->e[5]/*#4*/);
      });
      add(s->e[3]/*x*/,s->e[4]/*y*/);
    });
    /*inline:fun3,x=e*/
    s->e[3]/*f*/ = ({
      s->e[4]/*a*/ = SM_READ(s,s->chan2);
      s->e[5]/*b*/ = SM_READ(s,s->chan2);
      ({
        s->e[6]/*#5*/ = add(s->e[4]/*a*/,s->e[5]/*b*/);
        add(s->e[2]/*x*/,s->e[6]/*#5*/);
      });
    });
    s->e[4]/*g*/ = ({
      s->e[5]/*l*/ = 5;
      add(s->e[3]/*f*/,s->e[5]/*l*/);
    });
    send(s->e[1]/*d*/);
    send(s->e[2]/*e*/);
    send(s->e[3]/*f*/);
    for(s->e[5]/*i*/ = 0 ; s->e[5]/*i*/ < 3 ; s->e[5]/*i*/ = s->e[5]/*i*/ + 1) {
      ({
        s->e[6]/*#6*/ = add(s->e[2]/*e*/,s->e[5]/*i*/);
        send(s->e[6]/*#6*/);
      });
    }
    goto fun2;
  });
fun2:
  goto fun1;
fun3:
  ({
    s->e[0]/*a*/ = SM_READ(s,s->chan2);
    s->e[1]/*b*/ = SM_READ(s,s->chan2);
    ({
      s->e[2]/*#7*/ = add(s->e[0]/*a*/,s->e[1]/*b*/);
      add(s->x/*free*/,s->e[2]/*#7*/);
    });
  });
}
// stack_size: 7
#endif

// second pass
void testmod(state_t *s) {
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = SM_READ(s,s->chan1);
      T l3/*c*/ = SM_READ(s,s->chan1);
      ({
        T l4/*#0*/ = add(s->e[1]/*b*/,l3/*c*/);
        add(s->a/*free*/,l4/*#0*/);
      });
    });
    T l5/*d*/ = SM_READ(s,s->chan1);
    s->e[1]/*e*/ = ({
      T l7/*x*/ = ({
        T l8/*#1*/ = ({
          T l9/*#2*/ = 1;
          T l10/*#3*/ = 2;
          add(l9/*#2*/,l10/*#3*/);
        });
        add(s->e[0]/*a*/,l8/*#1*/);
      });
      T l11/*y*/ = ({
        T l12/*#4*/ = 2;
        add(l5/*d*/,l12/*#4*/);
      });
      add(l7/*x*/,l11/*y*/);
    });
    /*inline:fun3,x=e*/
    T l13/*f*/ = ({
      s->e[2]/*a*/ = SM_READ(s,s->chan2);
      T l15/*b*/ = SM_READ(s,s->chan2);
      ({
        T l16/*#5*/ = add(s->e[2]/*a*/,l15/*b*/);
        add(s->e[1]/*x*/,l16/*#5*/);
      });
    });
    T l17/*g*/ = ({
      T l18/*l*/ = 5;
      add(l13/*f*/,l18/*l*/);
    });
    send(l5/*d*/);
    send(s->e[1]/*e*/);
    send(l13/*f*/);
    for(T l19/*i*/ = 0 ; l19/*i*/ < 3 ; l19/*i*/ = l19/*i*/ + 1) {
      ({
        T l20/*#6*/ = add(s->e[1]/*e*/,l19/*i*/);
        send(l20/*#6*/);
      });
    }
    goto fun2;
  });
fun2:
  goto fun1;
}
// stack_size: 3

