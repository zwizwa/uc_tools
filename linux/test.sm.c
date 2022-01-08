struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[2]; // nb events used
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
      T r24/*abc*/ = 123;
      /*((write 1 abc) 123)*/
      /*((read 0 v1) (add 1 v1))*/
      T r25/*def*/; {
        CSP_EVT_BUF(&(s->task), 0, 1, r24/*abc*/, 0);
        CSP_EVT_BUF(&(s->task), 1, 0, NULL, 0);
        CSP_SEL(&(s->task), s, 1, 1);
        switch((&(s->task))->selected) {
        case 0: {
          r25/*def*/ = 123;
          } break;
        case 1: {
          T r26/*v1*/ = s->evt[1].msg.w;
          r25/*def*/ = ({
            T r27/*;9*/ = 1;
            add(r27/*;9*/, r26/*v1*/);
          });
          } break;
        }
      };
      ({
        T r28/*;10*/ = ({
          T r29/*;11*/ = 0;
          add(r25/*def*/, r29/*;11*/);
        });
        r28/*;10*/ ? ({
          goto fun1;
        }) : ({
          goto fun2;
        });
      });
    });
  });
/* fun3 inline only */
}
// stack_size: 3

