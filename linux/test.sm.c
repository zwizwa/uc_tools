struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[0]; // nb events used
  void *next;
  T e[4];
  T a;
};

// second pass
void *testmod(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = ({
        CSP_EVT(&(s->task),s,chan1,s->e[2]/*;1*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      T l4/*c*/ = ({
        CSP_EVT(&(s->task),s,chan1,s->e[2]/*;3*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      ({
        T l6/*;4*/ = add(s->e[1]/*b*/,l4/*c*/);
        add(s->a/*free*/,l6/*;4*/);
      });
    });
    T l7/*d*/ = ({
      CSP_EVT(&(s->task),s,chan1,s->e[1]/*;6*/);
      CSP_SEL(&(s->task),s,0,1);
    }
    s->e[1]/*e*/ = ({
      T l10/*x*/ = ({
        T l11/*;7*/ = ({
          T l12/*;8*/ = 1;
          T l13/*;9*/ = 2;
          add(l12/*;8*/,l13/*;9*/);
        });
        add(s->e[0]/*a*/,l11/*;7*/);
      });
      T l14/*y*/ = ({
        T l15/*;10*/ = 2;
        add(l7/*d*/,l15/*;10*/);
      });
      add(l10/*x*/,l14/*y*/);
    });
    /*inline:fun3,x=e*/
    T l16/*f*/ = ({
      s->e[2]/*a*/ = ({
        CSP_EVT(&(s->task),s,chan2,s->e[3]/*;12*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      T l19/*b*/ = ({
        CSP_EVT(&(s->task),s,chan2,s->e[3]/*;14*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      ({
        T l21/*;15*/ = add(s->e[2]/*a*/,l19/*b*/);
        add(s->e[1]/*x*/,l21/*;15*/);
      });
    });
    T l22/*g*/ = ({
      T l23/*l*/ = 5;
      add(l16/*f*/,l23/*l*/);
    });
    T l24/*h*/ = l22/*g*/ ? ({
      add(l16/*f*/,l22/*g*/);
    }) : ({
      add(l16/*f*/,l16/*f*/);
    });
    ({
      T l25/*;16*/ = add(l7/*d*/,l24/*h*/);
      send(l25/*;16*/);
    });
    send(s->e[1]/*e*/);
    send(l16/*f*/);
    for(s->e[2]/*i*/ = 0 ; s->e[2]/*i*/ < 3 ; s->e[2]/*i*/++) {
      ({
        T l27/*;17*/ = ({
          T l28/*;18*/ = ({
            CSP_EVT(&(s->task),s,chan1,s->e[3]/*;20*/);
            CSP_SEL(&(s->task),s,0,1);
          }
          add(l28/*;18*/,s->e[2]/*i*/);
        });
        send(l27/*;17*/);
      });
    }
    goto fun2;
  });
fun2:
  ({
    T l30/*;21*/ = ({
      s->e[0]/*;22*/ = ({
        CSP_EVT(&(s->task),s,chan1,s->e[1]/*;25*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      T l33/*;23*/ = ({
        CSP_EVT(&(s->task),s,chan2,s->e[1]/*;27*/);
        CSP_SEL(&(s->task),s,0,1);
      }
      add(s->e[0]/*;22*/,l33/*;23*/);
    });
    l30/*;21*/ ? ({
      goto fun1;
    }) : ({
      goto fun2;
    });
  });
/* fun3 inline only */
}
// stack_size: 4

