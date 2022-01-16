struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[2]; // nb events used
  void *next;
  T e[1];
};

// second pass
T testmod(struct state *s) {
  T _0;
  T _1;
  if(s->next) goto *s->next;
fun1:/*(x y)*/
  ({
    T r1/*x*/ = _0;
    T r2/*y*/ = _1;
    ({
      ({
        T r3/*;0*/ = 123;
        CSP_SND_W(&(s->task), s, 1, r3/*;0*/);
      });
      ({
        s->e[0]/*a*/ = CSP_RCV_W(&(s->task), s, 0);
        CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
        ({
          T r5/*;1*/ = ({
            T r6/*;2*/ = CSP_RCV_W(&(s->task), s, 0);
            add(s->e[0]/*a*/, r6/*;2*/);
          });
          r5/*;1*/ ? ({
            ({
              CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
              _0 = s->e[0]/*a*/;
              _1 = s->e[0]/*a*/;
              goto fun1;
            });
            }) : ({
            goto fun2;
            });
        });
      });
    });
  });
fun2:/*()*/
  ({
    ({
      s->e[0]/*abc*/ = 123;
      /*((write 1 abc) 123)*/
      /*((read 0 v1) (add 1 v1))*/
      T r8/*def*/; {
        CSP_EVT_BUF(&(s->task), 0, 1, s->e[0]/*abc*/, 0);
        CSP_EVT_BUF(&(s->task), 1, 0, NULL, 0);
        CSP_SEL(&(s->task), s, 1, 1);
        switch((&(s->task))->selected) {
        case 0: {
          r8/*def*/ = 123;
          } break;
        case 1: {
          T r9/*v1*/ = s->evt[1].msg.w;
          r8/*def*/ = ({
            T r10/*;3*/ = 1;
            add(r10/*;3*/, r9/*v1*/);
          });
          } break;
        }
      };
      ({
        T r11/*;4*/ = ({
          T r12/*;5*/ = 0;
          add(r8/*def*/, r12/*;5*/);
        });
        r11/*;4*/ ? ({
          _0 = s->e[0]/*abc*/;
          _1 = s->e[0]/*abc*/;
          goto fun1;
          }) : ({
          goto fun2;
          });
      });
    });
  });
/* start inline only */
}
// stack_size: 1

