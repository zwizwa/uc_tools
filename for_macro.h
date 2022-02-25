#ifndef FOR_MACRO
#define FOR_MACRO

/* The "for macro" pattern is very useful, and in these circles
   (embedded systems, static structures, not _too_ much reliance on
   external code generation) it is considered responsible macro use.

   So standardize some of the patterns that have come up over the
   years.  In the examples below, The 'FOR_NAMES' macro could be
   something like:

   #define FOR_NAMES(m) m(first) m(second) m(third)

   It represents a list of names, which can then be used for code
   generation of data structures, data instances, and code, by using
   the C preprocessor symbol concatenation and stringification
   features.

   The core ideas being:

   - To abstract over names in C you have to use macros

   - Lists of things are hard to impossible to represent in C macros,
     so use an iteration pattern instead.

   A generalization of this is to use it to abstract tables.

   #define FOR_INAMES(m) \
      m(1,first)  \
      m(2,second) \
      m(3,third)  \

   This is enough to construct a lot of static structure without
   reliance on external code generation.

*/

/* E.g. to declare struct members.
   struct {
      FOR_NAMES(DECLARE_UINT8_T)
   };
*/
#define DECLARE_UINT8_T(name) uint8_t name;

#endif
