struct state {
    T e[3];
    T a;
};

// second pass
void testmod(struct state *s) {
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = SM_READ(s,s->chan1);
      T l3/*c*/ = SM_READ(s,s->chan1);
      ({
        T l4/*;0*/ = add(s->e[1]/*b*/,l3/*c*/);
        add(s->a/*free*/,l4/*;0*/);
      });
    });
    T l5/*d*/ = SM_READ(s,s->chan1);
    s->e[1]/*e*/ = ({
      T l7/*x*/ = ({
        T l8/*;1*/ = ({
          T l9/*;2*/ = 1;
          T l10/*;3*/ = 2;
          add(l9/*;2*/,l10/*;3*/);
        });
        add(s->e[0]/*a*/,l8/*;1*/);
      });
      T l11/*y*/ = ({
        T l12/*;4*/ = 2;
        add(l5/*d*/,l12/*;4*/);
      });
      add(l7/*x*/,l11/*y*/);
    });
    /*inline:fun3,x=e*/
    T l13/*f*/ = ({
      s->e[2]/*a*/ = SM_READ(s,s->chan2);
      T l15/*b*/ = SM_READ(s,s->chan2);
      ({
        T l16/*;5*/ = add(s->e[2]/*a*/,l15/*b*/);
        add(s->e[1]/*x*/,l16/*;5*/);
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
    send(l5/*d*/);
    send(s->e[1]/*e*/);
    send(l13/*f*/);
    for(s->e[2]/*i*/ = 0 ; s->e[2]/*i*/ < 3 ; s->e[2]/*i*/++) {
      ({
        T l21/*;6*/ = ({
          T l22/*;7*/ = SM_READ(s,s->chan1);
          add(l22/*;7*/,s->e[2]/*i*/);
        });
        send(l21/*;6*/);
      });
    }
    goto fun2;
  });
fun2:
  ({
    T l23/*;8*/ = ({
      s->e[0]/*;9*/ = SM_READ(s,s->chan1);
      T l25/*;10*/ = SM_READ(s,s->chan2);
      add(s->e[0]/*;9*/,l25/*;10*/);
    });
    l23/*;8*/ ? ({
      goto fun1;
    }) : ({
      goto fun2;
    });
  });
/* fun3 inline only */
}
// stack_size: 3

