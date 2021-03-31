#ifndef OS_TCP_H
#define OS_TCP_H

/* Instead of abstracting at the socket API, abstract at a higher
   level and provide a simplified view of a TCP server.

   Note that close/shutdown is abstracted behind a higher level "done"
   operation.  In the practical cases I've encountered, the difference
   between RST and FIN-ACK/FIN-ACK doesn't seem to be relevant to the
   application as long as all data has been exchanged, which typically
   can be guaranteed in the higher level protocol.
*/

/* About error handling.

   I want the following:

   - Errors should be type-checked, so no more casting one integer
     error representation to another without some form of type
     checking.  This means errors should be pointers.

   - No memory management at the user side: a pointer to an error
     should be managed by the abstraction.  Note that it is perfectly
     possible to cast the pointers to integer error codes inside the
     abstraction.  Just the user should not know about this.

   - No error is represented by boolean false.

   - Errors that cannot be handled just need an error logger.

   - Errors that are meaningful to handle just need a selector /
     accessor to extract some information about the error.  These can
     be represented by individual structs and methods.

   - Provide a wrapper API for the "negative error code", by
     extracting a payload.
*/


/* Sockets expose read/write via blocking_io struct. */
#include "blocking_io.h"
#include "macros.h"


#if defined(LWIP_SOCKET) && LWIP_SOCKET
#include "os_tcp_lwip_socket.h"

#elif defined(LWIP_NETCONN) && LWIP_NETCONN
#include "os_tcp_lwip_netconn.h"


/* Berkeley sockets */
#else
#include "os_tcp_berkeley.h"
#endif

#endif
