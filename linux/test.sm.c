struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[1]; // nb events used
  void *next;
  T e[3];
  T a;
};

// second pass
T testmod(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    ({
      s->e[0]/*a*/ = ({
        s->e[1]/*b*/ = CSP_RCV_W(&(s->task),s,chan1);
        T l3/*c*/ = CSP_RCV_W(&(s->task),s,chan1);
        ({
          T l4/*;0*/ = add(s->e[1]/*b*/,l3/*c*/);
          add(s->a/*free*/,l4/*;0*/);
        });
      });
      T l5/*d*/ = CSP_RCV_W(&(s->task),s,chan1);
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
        ({
          s->e[2]/*a*/ = CSP_RCV_W(&(s->task),s,chan2);
          T l15/*b*/ = CSP_RCV_W(&(s->task),s,chan2);
          ({
            T l16/*;5*/ = add(s->e[2]/*a*/,l15/*b*/);
            add(s->e[1]/*x*/,l16/*;5*/);
          });
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
        T l20/*;6*/ = add(l5/*d*/,l19/*h*/);
        send(l20/*;6*/);
      });
      send(s->e[1]/*e*/);
      send(l13/*f*/);
      for(s->e[2]/*i*/ = 0 ; s->e[2]/*i*/ < 3 ; s->e[2]/*i*/++) {
        ({
          T l22/*;7*/ = ({
            T l23/*;8*/ = CSP_RCV_W(&(s->task),s,chan1);
            add(l23/*;8*/,s->e[2]/*i*/);
          });
          send(l22/*;7*/);
        });
      }
      goto fun2;
    });
  });
fun2:
  ({
    ({
      T l24/*;9*/ = ({
        s->e[0]/*;10*/ = CSP_RCV_W(&(s->task),s,chan1);
        T l26/*;11*/ = CSP_RCV_W(&(s->task),s,chan2);
        add(s->e[0]/*;10*/,l26/*;11*/);
      });
      l24/*;9*/ ? ({
        goto fun1;
      }) : ({
        goto fun2;
      });
    });
  });
/* fun3 inline only */
}
// stack_size: 3

