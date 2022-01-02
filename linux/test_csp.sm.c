
// first pass
#if 0
T pass1_testmod(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    ({
      s->e[0]/*;0*/ = 123;
      CSP_SND_W(&(s->task), s, 1, s->e[0]/*;0*/);
    });
    ({
      s->e[0]/*a*/ = CSP_RCV_W(&(s->task), s, 0);
      CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
      ({
        s->e[1]/*;1*/ = ({
          s->e[2]/*;2*/ = CSP_RCV_W(&(s->task), s, 0);
          add(s->e[0]/*a*/, s->e[2]/*;2*/);
        });
        s->e[1]/*;1*/ ? ({
          ({
            CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
            goto fun1;
          });
        }) : ({
          goto fun2;
        });
      });
    });
  });
fun2:
  ({
    ({
      s->e[0]/*abc*/ = 123;
      /*((write 1 abc) 123)*/
      /*((read 0 v1) (add 1 v1))*/
      {
        CSP_EVT_BUF(&(s->task), 0, 1, s->e[0]/*abc*/, 0);
        CSP_EVT_BUF(&(s->task), 1, 0, NULL, 0);
        CSP_SEL(&(s->task), s, 1, 1);
        switch((&(s->task))->selected) {
        case 0: {
          s->e[1]/*def*/ = 123;
          } break;
        case 1: {
          s->e[2]/*v1*/ = s->evt[1].msg.w;
          s->e[1]/*def*/ = ({
            s->e[3]/*;3*/ = 1;
            add(s->e[3]/*;3*/, s->e[2]/*v1*/);
          });
          } break;
        }
      };
      ({
        s->e[2]/*;4*/ = ({
          s->e[3]/*;5*/ = 0;
          add(s->e[1]/*def*/, s->e[3]/*;5*/);
        });
        s->e[2]/*;4*/ ? ({
          goto fun1;
        }) : ({
          goto fun2;
        });
      });
    });
  });
}
// stack_size: 4
#endif
struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[1]; // nb events used
  void *next;
  T e[1];
};

// second pass
T testmod(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    ({
      T r1/*;0*/ = 123;
      CSP_SND_W(&(s->task), s, 1, r1/*;0*/);
    });
    ({
      s->e[0]/*a*/ = CSP_RCV_W(&(s->task), s, 0);
      CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
      ({
        T r3/*;1*/ = ({
          T r4/*;2*/ = CSP_RCV_W(&(s->task), s, 0);
          add(s->e[0]/*a*/, r4/*;2*/);
        });
        r3/*;1*/ ? ({
          ({
            CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
            goto fun1;
          });
        }) : ({
          goto fun2;
        });
      });
    });
  });
fun2:
  ({
    ({
      T r5/*abc*/ = 123;
      /*((write 1 abc) 123)*/
      /*((read 0 v1) (add 1 v1))*/
      T r6/*def*/; {
        CSP_EVT_BUF(&(s->task), 0, 1, r5/*abc*/, 0);
        CSP_EVT_BUF(&(s->task), 1, 0, NULL, 0);
        CSP_SEL(&(s->task), s, 1, 1);
        switch((&(s->task))->selected) {
        case 0: {
          r6/*def*/ = 123;
          } break;
        case 1: {
          T r7/*v1*/ = s->evt[1].msg.w;
          r6/*def*/ = ({
            T r8/*;3*/ = 1;
            add(r8/*;3*/, r7/*v1*/);
          });
          } break;
        }
      };
      ({
        T r9/*;4*/ = ({
          T r10/*;5*/ = 0;
          add(r6/*def*/, r10/*;5*/);
        });
        r9/*;4*/ ? ({
          goto fun1;
        }) : ({
          goto fun2;
        });
      });
    });
  });
}
// stack_size: 1

