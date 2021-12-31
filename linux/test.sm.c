struct state {
  struct csp_task task; // ends in evt[]
  struct csp_evt evt[0]; // nb events used
  void *next;
  T e[3];
  T a;
};

// second pass
T testmod(struct state *s) {
  if(s->next) goto *s->next;
fun1:
  ({
    s->e[0]/*a*/ = ({
      s->e[1]/*b*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan1,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
      T l3/*c*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan1,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
      ({
        T l4/*;2*/ = add(s->e[1]/*b*/,l3/*c*/);
        add(s->a/*free*/,l4/*;2*/);
      });
    });
    T l5/*d*/ = ({
      CSP_EVT_BUF(&(s->task),0,chan1,NULL,0);
      CSP_SEL(&(s->task),s,0,1);
      s->evt[0].msg.w;
    });
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
      s->e[2]/*a*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan2,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
      T l15/*b*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan2,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
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
          T l23/*;13*/ = ({
            CSP_EVT_BUF(&(s->task),0,chan1,NULL,0);
            CSP_SEL(&(s->task),s,0,1);
            s->evt[0].msg.w;
          });
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
      s->e[0]/*;16*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan1,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
      T l26/*;17*/ = ({
        CSP_EVT_BUF(&(s->task),0,chan2,NULL,0);
        CSP_SEL(&(s->task),s,0,1);
        s->evt[0].msg.w;
      });
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

