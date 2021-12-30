struct state {
  T e[3];
  T a;
};

// second pass
T testmod(struct state *s, void *next) {
  if(s->next) goto *s->next;
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = SM_READ(s,s->chan1,l0);
      T l3/*c*/ = SM_READ(s,s->chan1,l1);
      ({
        T l4/*;2*/ = add(s->e[1]/*b*/,l3/*c*/);
        add(s->a/*free*/,l4/*;2*/);
      });
    });
    T l5/*d*/ = SM_READ(s,s->chan1,l3);
    s->e[1]/*e*/ = ({
      T l7/*x*/ = ({
        T l8/*;4*/ = ({
          T l9/*;5*/ = 1;
          T l10/*;6*/ = 2;
          add(l9/*;5*/,l10/*;6*/);
        });
        add(s->e[0]/*a*/,l8/*;4*/);
      });
      T l11/*y*/ = ({
        T l12/*;7*/ = 2;
        add(l5/*d*/,l12/*;7*/);
      });
      add(l7/*x*/,l11/*y*/);
    });
    /*inline:fun3,x=e*/
    T l13/*f*/ = ({
      s->e[2]/*a*/ = SM_READ(s,s->chan2,l8);
      T l15/*b*/ = SM_READ(s,s->chan2,l9);
      ({
        T l16/*;10*/ = add(s->e[2]/*a*/,l15/*b*/);
        add(s->e[1]/*x*/,l16/*;10*/);
      });
    });
    T l17/*g*/ = ({
      T l18/*l*/ = 5;
      add(l13/*f*/,l18/*l*/);
    });
    T l19/*h*/ = l17/*g*/ ? ({
      add(l13/*f*/,l17/*g*/);
    }) : ({
      add(l13/*f*/,l13/*f*/);
    });
    ({
      T l20/*;11*/ = add(l5/*d*/,l19/*h*/);
      send(l20/*;11*/);
    });
    send(s->e[1]/*e*/);
    send(l13/*f*/);
    for(s->e[2]/*i*/ = 0 ; s->e[2]/*i*/ < 3 ; s->e[2]/*i*/++) {
      ({
        T l22/*;12*/ = ({
          T l23/*;13*/ = SM_READ(s,s->chan1,l14);
          add(l23/*;13*/,s->e[2]/*i*/);
        });
        send(l22/*;12*/);
      });
    }
    goto fun2;
  });
fun2:
  ({
    T l24/*;15*/ = ({
      s->e[0]/*;16*/ = SM_READ(s,s->chan1,l18);
      T l26/*;17*/ = SM_READ(s,s->chan2,l19);
      add(s->e[0]/*;16*/,l26/*;17*/);
    });
    l24/*;15*/ ? ({
      goto fun1;
    }) : ({
      goto fun2;
    });
  });
/* fun3 inline only */
}
// stack_size: 3

