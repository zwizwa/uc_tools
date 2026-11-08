/* lwIP options for lwip_plugin.c: NO_SYS, raw API, main loop only. */
#ifndef LWIPOPTS_H
#define LWIPOPTS_H

#define NO_SYS                  1
#define SYS_LIGHTWEIGHT_PROT    0
#define LWIP_SOCKET             0
#define LWIP_NETCONN            0

#define MEM_ALIGNMENT           4
#define MEM_SIZE                (256 * 1024)
#define MEMP_NUM_PBUF           32
#define MEMP_NUM_TCP_PCB        8
#define MEMP_NUM_TCP_SEG        64
#define PBUF_POOL_SIZE          64

#define LWIP_ARP                1
#define LWIP_ETHERNET           1
#define LWIP_IPV4               1
#define LWIP_IPV6               0
#define LWIP_ICMP               1  // udp_mon answers ping, see KERNEL_EVENT_NET
#define LWIP_RAW                0
#define LWIP_UDP                1
#define LWIP_TCP                1
#define LWIP_DHCP               0
#define LWIP_DNS                0
#define LWIP_IGMP               0

#define TCP_MSS                 1460
#define TCP_WND                 (8 * TCP_MSS)
#define TCP_SND_BUF             (8 * TCP_MSS)
#define TCP_SND_QUEUELEN        (4 * TCP_SND_BUF / TCP_MSS)

#define LWIP_STATS              0
#define LWIP_NETIF_STATUS_CALLBACK 0
#define LWIP_NETIF_LINK_CALLBACK   0

#endif
