#ifndef ICSLOG_H
#define ICSLOG_H 1
/* ===[ICSLOG.H]===================================================

   Description: ICSLOG definitions and prototypes
   
   v03 26-may-2000 oxk System type request & response added
   V02 25-nov-1999 uxn initial release for EuroGOLS
   V01 12-aug-1996  mf initial version

   --------------------------------------------------------------------
   This item is the property of GTECH Corporation, West Greewich, Rhode
   Island, and contains confidential and trade secret information. It
   may not be transferred from the custody or control of GTECH except
   as authorized in writing by an officer of GTECH. Neither this item
   nor the information it contains may be used, transferred,
   reproduced, published, or disclosed, in whole or in part, and
   directly or indirectly, except as expressly authorized by an
   officer of GTECH, pursuant to written agreement.

   Copyright 1996 GTECH Corporation. All rights reserved.
   --------------------------------------------------------------------
   ==================================================================== */



#define VERSION 100

#include "inclib:prosys.h"
#include "inclib:icscom.h"

#define EXIT_ERROR     42

#pragma member_alignment save
#pragma nomember_alignment

#define MAX_INPLEN  600		/* max len of client's message */
#define MAX_ERRLEN  200		/* max len of error message */
#define MAX_IDLE    5000	/* max msec without an I/O  */

	/* server-host application level protocol   */
#define NETHDR 0x4d3c2b1a  /* msg header marker in network order */

	/* message types */
#define REQMJF	1	    /* request mjf data transfer    */
#define MJFXFER	2	    /* mjf data follows	*/ 
#define REQXOFF	3	    /* request stop data transfer   */
#define REQDISC	4	    /* request disconnect   */
#define ERREPLY	5	    /* error message reply  */
#define REQSYST	6	    /* request system status	*/
#define SYSREPL	7	    /* system status reply	*/

	/* result code values	*/
#define MJFDATA	0	    /* mjf data follows	    */
#define MJFNAVL 1	    /* mjf not available    */
#define BUCKAVL	2	    /* bucuket not full yet  */
#define BUCKINV 3	    /* invalid log bucket # */
#define INVPARM 4	    /* invalid parameter(s) */
#define MJFLAST 5	    /* last block of mjf for this CDC */

	/* error codes for error message */
#define INVTYPE	1	    /* invalid message type */
#define INVCHKS	2	    /* checksum error	*/
#define INVHDR	3	    /* invalid msg header */

	/* header of messages passed between host and ICS system  */
struct MSGHDR 
{
   int   msghdr;   /* fixed message header                */
   int   chksum;   /* CRC32 checksum of the whole message */
   short msgtype;  /* message type                        */
   short msglen;   /* number of bytes to follow           */
};

	/* error message descriptor */
struct ERR_MSG
{
    int errcode;
    char errmsg[MAX_ERRLEN];
};

	/* get mjf block(s) request */
struct REQ_MJF 
{
    short cdc;
    int startbucknum;
    int endbucknum;
};

	    /* get mjf block(s) reply */
struct RPL_MJF 
{
    char environment;	/* environment type   */
    char systype;       /* system type	*/
    short cdc;		/* cdc date */
    char resultcode;	/* error code */
    char fillerr[3];
    int bucknum;
	/* mjf bucket if result code 0 */
};

	    /* system status data */
struct SYS_STAT
{
    char systype;       /* system type	*/
    short cdc;		/* cdc date */
};

	    /* get system status reply */
struct RPL_SSTAT
{
    struct MSGHDR msghdr;
    struct SYS_STAT stat;
};

	/* error message */
struct ICS_ERR
{
    struct MSGHDR msghdr;
    struct ERR_MSG em;
};

	/* the whole xfer message descriptor */
struct MJFBUCK {
 char tmfbuf[8192];
};

/* Structure to keep blocks in memory */

#define MAX_CACHE  20

struct CACHE
{
    unsigned int num;
    struct MJFBUCK buf;    
};

struct ICSBUF
{
    struct MSGHDR header;
    struct RPL_MJF rplhdr;
    struct MJFBUCK mjfbuck;
};

        /* client's status values   */
#define NEVERCONNECTED  0       /* never connected  */
#define DISCONNECTED    1       /* diconnected (own)*/
#define KILLED		2	/* disconn. by host */
#define CONNECTED       3       /* connected        */
#define RECEIVING       4       /* receiving data   */
#define XOFFBREAK	5       /* requested XOFF   */
#define BATCHBREAK      6       /* inter-batch break*/
#define RETRYBREAK      7       /* retry break      */

	/* mjf state values */
#define NOMJF		0	/* no associated mjf*/
#define REQNAME		1	/* requested mjf's name */
#define GOTNAME		2	/* got name	    */
#define MJFOPEN		3	/* mjf is open	    */ 

	/* product state values */
#define PROD_NOT_INIT   0       /* not initialized */
#define PROD_RUNNING    1       /* product running */
#define PROD_STOPPED    2       /* product stopped */

    /* reporting levels */
#define RL_MAJOR    0	    /* major fault  */
#define RL_EVENT    1	    /* major event  */
#define RL_ERROR    2	    /* any error    */
#define RL_DEBUG    3	    /* everything   */

/* GOLS system values */

#define LIVSYS      1
#define BAKSYS      2
#define SPRSYS      3
    
/* function prototyping here !!!! */

void serv_exit(void);
void setup(struct ICSLOG **ccp);
int  g_ioctl();
void do_client_request(void);
void init_tcp(struct ICSLOG *ccp);
void do_connect(void);
void read_client(void);
void disconnect(int cln, int why);
void write_client(void);
void send_out(void *buf, int buflen, int cln);
void send_icserr(int cn, int rc, char *msg);
void send_error_disconn(int cn, int rc, char *msg);
void request_mjf_name(int cdc, int cln);
void go_ready(void);
void senderror(void);
void check_client(void);
void ops_msg(char*);
void g_crash();
void COPYRITE();
void SNIF_AND_WRKSET();
int  GOLS_SYSTYPE();
int  GOLS_ENVIRONMENT();
int  GOLS_CDC();
void GOLS_CLOSE_TMF( void* );
void GOLS_READ_TMF( void*, void*, void*, void*);
void GOLS_OPEN_TMF( void*, void*, void*);
int  decode_reqmjf(int,int);
int  decode_reqxoff(int,int);
int  decode_reqdisc(int,int);
int  decode_erreply(int,int);
int  decode_reqsyst(int,int);
unsigned long crc(unsigned char*, int);
void g_waitms(int);
int g_oprint(char*,int,int,int,int *);
int g_oprstr(char*,char*,int,int,char*);

#pragma member_alignment restore

#endif /* ICSLOG_H */
