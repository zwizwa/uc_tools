struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[2]; // nb events used
  void *next;
  T e[1];
};

// second pass
T testmod(struct state *s) {
  T _a0;
  if(s->next) goto *s->next;
fun1:/*(x)*/
  ({
    T r1/*x*/ = _a0;
    ({
      ({
        T r2/*;0*/ = 123;
        CSP_SND_W(&(s->task), s, 1, r2/*;0*/);
      });
      ({
        s->e[0]/*a*/ = CSP_RCV_W(&(s->task), s, 0);
        CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
        ({
          T r4/*;1*/ = ({
            T r5/*;2*/ = CSP_RCV_W(&(s->task), s, 0);
            add(s->e[0]/*a*/, r5/*;2*/);
          });
          r4/*;1*/ ? ({
            ({
              CSP_SND_W(&(s->task), s, 1, s->e[0]/*a*/);
              _a0 = s->e[0]/*a*/;
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
      T r7/*def*/; {
        CSP_EVT_BUF(&(s->task), 0, 1, s->e[0]/*abc*/, 0);
        CSP_EVT_BUF(&(s->task), 1, 0, NULL, 0);
        CSP_SEL(&(s->task), s, 1, 1);
        switch((&(s->task))->selected) {
        case 0: {
          r7/*def*/ = 123;
          } break;
        case 1: {
          T r8/*v1*/ = s->evt[1].msg.w;
          r7/*def*/ = ({
            T r9/*;3*/ = 1;
            add(r9/*;3*/, r8/*v1*/);
          });
          } break;
        }
      };
      ({
        T r10/*;4*/ = ({
          T r11/*;5*/ = 0;
          add(r7/*def*/, r11/*;5*/);
        });
        r10/*;4*/ ? ({
          _a0 = s->e[0]/*abc*/;
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

// spawn: fun1 1
// spawn: fun1 2
