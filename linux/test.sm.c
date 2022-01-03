struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[1]; // nb events used
  void *next;
  T e[3];
  T a;
};

// second pass
T module(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    ({
      s->e[0]/*a*/ = ({
        s->e[1]/*b*/ = CSP_RCV_W(&(s->task), s, chan1);
        T r3/*c*/ = CSP_RCV_W(&(s->task), s, chan1);
        ({
          T r4/*;0*/ = add(s->e[1]/*b*/, r3/*c*/);
          add(s->a/*free*/, r4/*;0*/);
        });
      });
      T r5/*d*/ = CSP_RCV_W(&(s->task), s, chan1);
      s->e[1]/*e*/ = ({
        T r7/*x*/ = ({
          T r8/*;1*/ = ({
            T r9/*;3*/ = 2;
            T r10/*;2*/ = 1;
            add(r10/*;2*/, r9/*;3*/);
          });
          add(s->e[0]/*a*/, r8/*;1*/);
        });
        T r11/*y*/ = ({
          T r12/*;4*/ = 2;
          add(r5/*d*/, r12/*;4*/);
        });
        add(r7/*x*/, r11/*y*/);
      });
      /*inline:fun3, x=e*/
      T r13/*f*/ = ({
        ({
          s->e[2]/*a*/ = CSP_RCV_W(&(s->task), s, chan2);
          T r15/*b*/ = CSP_RCV_W(&(s->task), s, chan2);
          ({
            T r16/*;5*/ = add(s->e[2]/*a*/, r15/*b*/);
            add(s->e[1]/*x*/, r16/*;5*/);
          });
        });
      });
      T r17/*g*/ = ({
        T r18/*l*/ = 5;
        add(r13/*f*/, r18/*l*/);
      });
      T r19/*h*/ = r17/*g*/ ? ({
        add(r13/*f*/, r17/*g*/);
      }) : ({
        add(r13/*f*/, r13/*f*/);
      });
      ({
        T r20/*;6*/ = add(r5/*d*/, r19/*h*/);
        send(r20/*;6*/);
      });
      send(s->e[1]/*e*/);
      send(r13/*f*/);
      for(s->e[2]/*i*/ = 0 ; s->e[2]/*i*/ < 3 ; s->e[2]/*i*/++) {
        ({
          T r22/*;7*/ = ({
            T r23/*;8*/ = CSP_RCV_W(&(s->task), s, chan1);
            add(r23/*;8*/, s->e[2]/*i*/);
          });
          send(r22/*;7*/);
        });
      }
      goto fun2;
    });
  });
fun2:
  ({
    ({
      T r24/*;9*/ = ({
        s->e[0]/*;11*/ = CSP_RCV_W(&(s->task), s, chan2);
        T r26/*;10*/ = CSP_RCV_W(&(s->task), s, chan1);
        add(r26/*;10*/, s->e[0]/*;11*/);
      });
      r24/*;9*/ ? ({
        goto fun1;
      }) : ({
        goto fun2;
      });
    });
  });
/* fun3 inline only */
}
// stack_size: 3

