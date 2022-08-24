#ifndef PHANTOM_H
#define PHANTOM_H

/* Approximating phantom types in C.  The idea is to represent
   information about constraints on a data type by casting to a struct
   that is never defined.

   E.g.

   // API: structs are opaque
   struct vector;
   struct unit_vector;
   struct unit_vector *normalize(struct vector *);
   struct vector *n2v(struct unit_vector *);


   // Implementation can cast to storage layout.
   struct vector_impl {
      float x,y;
   };
   struct unit_vector *normalize(struct vector *v_phantom) {
      struct vector_impl *v = (void*)v_phantom;
      float scale = 1.0f / sqrt(v->x * v->x + v->y * v->y);
      v->x *= scale;
      v->y *= scale;
      return (void*)v;
   }
   struct vector *n2v(struct unit_vector *v_phantom) {
      return (void*)v;
   }

   // TODO: Is this more of a manual design pattern or is generic
   // code possible?

*/

#endif
