/* ics_client.c	

   V03 26-May-2000  oxk System type request & response added
   V02 25-NOV-1999  UXN Initial release for EuroGOLS.
   V01 12-aug-1996  mf  initial version

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

#include  <errno.h>
#include  <types.h>
#include  <stdio.h>
#include  <time.h>
#include  <stdlib.h>
#include  <string.h>
#include  <iodef.h>
#include  <unixio.h>
#include  <socket.h>
#include  <in.h>
#include  <netdb.h>		/* change hostent to comply with BSD 4.3*/
#include  <inet.h>
#include  <ucx$inetdef.h>	/* INET symbol definitions */
#include  <ssdef.h>
#include  <starlet.h> 
#include "inclib:icslog.h"

int g_ioctl();
void client_exit(void);
unsigned long crc(unsigned char *buf, int len);

int mysock = -1;               	/* socket */
struct  sockaddr msaddr;
struct  hostent hostentstruct;  /* Storage for hostent data.  */
struct  hostent *hostentptr;    /* Pointer to hostent data.   */
char    hostname[128];	    /* Name of local host.        */
int	one = 1, zero = 0;		
char server[128], message[240];
int portnum;
u_short stmp;
int chksum = 1, validate = 1, rcv = 5, snd = 1, nacks = 5, mystate = 0;
int max_input = 8216, timper = 500, do_recv = 1, ioerr = 0, debug = 0;
int timarg[2];

#pragma member_alignment save
#pragma nomember_alignment
struct ICSHDR
{
    int stamp;
    int chksum;
    unsigned short msgtyp;
    unsigned short msglen;
};

struct EMSG
{
    struct ICSHDR hdr;
    int er;
    char msg[240];
}; 

struct XFRQST
{
    unsigned short cdc;
    int first;
    int last;
};

struct DREQ
{
    struct ICSHDR hdr;
    struct XFRQST rqs;
} dreq;

struct SREQ
{
    struct ICSHDR hdr;
} sreq;

struct XFRHDR
{
    char env;
    char syst;
    unsigned short cdc;
    char rslt;
    char filler[3];
    int bucknum;
};
#pragma member_alignment restore

#define HDRSIZ sizeof(struct ICSHDR)
#define XFRSIZ sizeof(struct XFRHDR)

char *fbuf;	    /* staging buffer */
char *fnxt;
char *buffer;	    /* tcp input/output buffer */
int bufflen;

/* ------------ hex_dump of a buffer -------------- */

void hex_dump(char *title, void *buf, int len)
{
    int k;
    unsigned char *bp;

    bp = (unsigned char *)buf;
    printf("%s",title);
    for (k = 0; k < len; k++)
        printf(" %2.2x",(unsigned char) (*(bp+k)));
    printf("\n");
}

/* ----------------- locate header ------------------ */

struct ICSHDR *find_hdr(char *buf, char *ebuf)
{
    struct ICSHDR *hp;

    if (debug)
	printf("looking for header %x  %x\n", buf, ebuf);

    while(buf <= ebuf)
    {
	hp = (struct ICSHDR *)buf;
	if (hp->stamp == 0x4d3c2b1a)
	    return hp;
	buf += 4;
    }
    return 0;
}

/* ---------------- process input stream --------------- */

void process_inp(void *buf, int len)
{
    int ml, mtp, n, left, need, tail, inchk, mychk;
    char *bp;
    struct ICSHDR *mh;
    struct EMSG *em;
    struct XFRHDR *dm;
    struct SYS_STAT *st;

    memcpy(fnxt, buf, len);		/* append next chunk */
    fnxt += len;
    
    while ((fnxt - fbuf) >= 4) 
    {
	mh = find_hdr(fbuf, fnxt - 4);
	if (!mh)				/* no header - ignore all */
	{
	    fnxt = fbuf;
	    need = 0;
	    return;
	}
	n = fnxt - fbuf;
	if (n < HDRSIZ)	/* partial header */
	    return;

	left = n - HDRSIZ;
	ml = ntohs(mh->msglen);		/* # bytes to follow */
	inchk = ntohl(mh->chksum);
	
	if ((ml < 0) || (ml > max_input))
	{
	    printf("invalid inp_msg len %d\n",ml);
	    hex_dump("bad header",mh, HDRSIZ);
	    memmove(fbuf, fbuf+ 4, 4);	/* shift buffer 4 bytes */
	    fnxt -= 4;
	    continue;
	}

	mtp = ntohs(mh->msgtyp);

	if (left < ml)			/* partial message */
	{
	    if (debug)
		printf("partial msg_typ %d - has=%d need=%d\n", 
			    mtp, n, ml-left);
	    return; 
	}

	if (chksum)
	{
	    mh->chksum = 0;
	    mychk = crc((unsigned char *)fbuf, ml + HDRSIZ);
	    if (mychk != inchk)
		printf("checksum mismatch in=%x  my=%x len=%d\n",
			 inchk,mychk,ml + HDRSIZ);
	}
				/* got at least a whole message */
	switch (mtp)
	{
	    case 2:
		dm = (struct XFRHDR *)(fbuf + HDRSIZ);
		if (ml < XFRSIZ)
		{
		    printf("bad len=%d for MJF data\n",ml);
		    hex_dump("header",fbuf, HDRSIZ + XFRSIZ);
		    ml = XFRSIZ;
		}
		else
		{
		    printf(
		    "*** MJF buck# %d ec=%d env=%d sys=%d cdc=%d len = %d\n",
			ntohl(dm->bucknum), dm->rslt, dm->env,
			dm->syst, ntohs(dm->cdc), ml-XFRSIZ );
		}
		break;
	    case 3:
		if (ml != 0)
		    printf("bad len=%d for X_OFF\n",ml);
		else
		    printf("X_OFF message\n");
		ml = 0;
		break;
	    case 4:
		if (ml != 0)
		    printf("bad len=%d for DISCONNECT\n",ml);
		else
		    printf("DISCONNECT message\n");
		ml = 0;
		break;
	    case 5:
		if ((ml < 5) || (ml > 240))
		{
		    printf("bad len=%d of error message\n",ml);
		    ml = 4;
		}
		else
		{
		    em = (struct EMSG *)fbuf;
		    printf("E_M ec=%d >%s<\n",em->er, em->msg);
		}
		break;
	    case 7:
		st = (struct SYS_STAT *)(fbuf + HDRSIZ);
		if (ml != 3)
		    printf("bad len=%d for SYSSTATUS\n",ml);
		else
		    printf("SYSSTATUS message id=%d cdc=%d \n", st->systype,
			    ntohs(st->cdc));
		ml = sizeof (struct SYS_STAT);
		break;
	    case 1:
	    default:
		printf("invalid message type=%d len=%d\n", mtp,ml);
		ml = 4 - HDRSIZ;
		break;
	}
	bp = fbuf + HDRSIZ + ml;  /* start of next header - if any */
	tail = n - (HDRSIZ + ml);
	if (tail > 0)
	    memmove(fbuf, bp, tail);	/* shift buffer */
	else if (tail < 0)
	{
	    printf("? tail=%d ?\n",tail);
	    tail = 0;	    /* just in case */
	}

	fnxt = fbuf + tail;
    } /* end while(1) */
}

/* ------------------- exit handler ------------------ */

void client_exit(void)
{
    printf("exit handler - sock %d\n",mysock);
    if (mysock > 0)
	close(mysock);
}

/* ---------- initialize message header ------------- */

void sethdr(struct ICSHDR *hdr, int mt, int len)
{
    unsigned short mtyp, mlen;

    mtyp = mt,
    mlen = len;

    hdr->stamp  = 0x4d3c2b1a;
    hdr->chksum = 0;
    hdr->msgtyp = htons(mtyp);
    hdr->msglen = htons(mlen);

#if 0
    printf("sethdr out stamp=%x chk=%d mtyp=%4.4x mlen=%4.4x\n",
	hdr->stamp, hdr->chksum, (unsigned short)hdr->msgtyp, 
	(unsigned short)hdr->msglen);
#endif
}

/* ------------- tcp write ------------- */

void tcp_write(void *buf, int buflen)
{
    int sts, wmask, flag = 0, hchk;
    struct timeval ws;
    unsigned char *bp;
    struct ICSHDR *mh;

    bp = (unsigned char *)buf;
    mh = (struct ICSHDR *)buf;

    if (chksum)
    {
	mh->chksum = 0;
	hchk = crc(bp, buflen);
	mh->chksum = htonl(hchk);
    }

    if (debug)
    {
	printf("sending ");
	for (sts=0; sts < buflen ;sts++)
	    printf("%2.2x ",(unsigned char)(*(bp+sts)));
	printf("\n");
    }

    sts = send(mysock, (char *)buf, buflen, flag);
    if (sts < 0)
    {
	sprintf(message,"send err %d", errno);
	perror (message);
    }
    else
    {
        ws.tv_sec = snd;
        ws.tv_usec = 0;
        wmask = 1 << mysock;
        sts = select(32, (struct fd_set *)&zero, (struct fd_set *)&wmask, (struct fd_set *)&zero, &ws);
        if (sts < 0)
        {
            perror("select failed");
        }
        else if (sts == 0)
        {
	    perror("select timed out");
        }
        else
	{
	    if (debug)
		printf("message was sent\n");
	}
    }
}

/* ----------- drop connection ----------- */

void do_disconnect()
{
    if (close(mysock))
    {
	sprintf(message,"socket close err %d",errno);
	perror(message);
    }
    else
	printf("disconnected from server\n");

    mysock = -1;
    mystate = 0;
}

/* -------------- connect ------------- */

void do_connect()
{
    int k, sts;
	    
    if (g_oprint("enter port number",4567, 4000, 30000, &portnum))
        return;

    if (g_oprstr("enter server name","", 1, 100, server))
	return;

    if ((mysock = socket (AF_INET, SOCK_STREAM, 0)) == -1) 
    {
	sprintf(message,"socket err %d",errno);
	perror(message);
	return;
    }

    sts = setsockopt(mysock, SOL_SOCKET, SO_REUSEADDR,
                                                (char *)&one, sizeof(one));
    if (sts)
    {
        perror("setsockopt reuse");
        return;
    }

    sts = setsockopt(mysock, SOL_SOCKET, SO_KEEPALIVE,
			(char *)&one, sizeof(one));
    if (sts)
    {
	perror("setsockopt keep_alive");
	return;
    }

    sts = setsockopt(mysock, SOL_SOCKET, TCPIP$C_TCP_NODELAY,
			(char *)&one, sizeof(one));
    if (sts)
    {
	perror("setsockopt nodelay");
	return;
    }


    if ((hostentptr = gethostbyname (server)) == NULL) 
    {
	perror( "gethostbyname");
	return;
    }
	    
    hostentstruct = *hostentptr;

    printf("server name >%s<\n",hostentstruct.h_name);
    printf("server addr %8.8X\n", hostentstruct.h_addr);

    stmp = htons(portnum);
    msaddr.sa_family = AF_INET;	    /*hostentstruct.h_addrtype; */
    memcpy(msaddr.sa_data, &stmp, 2);
    memcpy(msaddr.sa_data + 2, hostentstruct.h_addr, 4);
    for (k = 0; k < 4; k++)
    {
	sts = *(msaddr.sa_data + 2 + k);
	printf("%2.2X", sts);
    }
    printf("\n");
/*
    printf("enter server address: ");
    scanf("%X",&sts);
    memcpy(msaddr.sa_data + 2, &sts, 4);
*/
    msaddr.sa_family = AF_INET;	    /*hostentstruct.h_addrtype; */
    memcpy(msaddr.sa_data, &stmp, 2);
    memcpy(msaddr.sa_data + 2, hostentstruct.h_addr, 4);
    sts = connect(mysock, &msaddr, sizeof (msaddr));
    if (sts)
    {
	if (errno == ECONNREFUSED)
	{
	    printf("connection refused\n");
	    return;
	}
	else if (errno == ETIMEDOUT)
	{
	    printf("connection attempt timed out\n");
	    return;
	}
	else
	{
	    sprintf(message,"connect err %d",errno);
	    perror(message);
	    return;
	}
    }
	    /* set socket to non-blocking i/o   */
    sts = g_ioctl(mysock, FIONBIO, (char *)&one);
    if (sts)
    {
	sprintf(message,"ioctl err %d",errno);
	perror(message);
	printf("socket set for blocking i/o\n");
    }
    printf("connected OK\n");
    mystate = 1;
}

/* ------------ send error message ------------ */

void do_errmsg(void)
{
    int ec;
    char msg[240];
    struct EMSG emsg;

    if (g_oprstr("Message","",0,sizeof(msg)-1,emsg.msg))
	return;
    if (g_oprint("Err_code",0,0,0x7fffffff,&emsg.er))
	return;
    sethdr(&emsg.hdr,5,strlen(emsg.msg)+5);
    tcp_write(&emsg,sizeof(emsg.hdr)+4+strlen(emsg.msg)+1);
}

/* ------------- send data request --------------- */

void do_request()
{
    int tmp, beg, fin;
    unsigned short stmp;

    if (g_oprint("CDC day",3,0,30000,&tmp))
	return;
    if (g_oprint("first block",0,0,1000000,&beg))
        return;
    if (g_oprint(" last block",0,0,1000000,&fin))
        return;
    sethdr(&dreq.hdr, 1, sizeof(dreq.rqs));
    stmp = (unsigned short) tmp;
    dreq.rqs.cdc = htons(stmp);
    dreq.rqs.first = htonl(beg);
    dreq.rqs.last = htonl(fin);
    tcp_write(&dreq, sizeof(dreq));
}

/* ------------- send system status request --------------- */

void do_reqsyst()
{
    sethdr(&sreq.hdr, 6, 0);
    tcp_write(&sreq, sizeof(sreq));
}

/* ---------- receive from server -------------- */

void tcp_read(int astid)
{
    int sts, n = 0;

    if ((mysock == -1) || (!do_recv) )
	goto ARM_TIMER;
    sts = recv(mysock, buffer, bufflen, 0);
    if (sts == -1)
    {
	if (errno != EWOULDBLOCK)
	{
	    sprintf(message,"recv err %d",errno);
	    perror(message);
	    ioerr++;
	    if (ioerr > 3)
	    {
		timper = 1000;
		printf("recv error treshold exceeded\n");
		do_recv = 0;
	    }
	}
	else
	    timper = 500;
    }

    else if (sts > 0)
    {
	ioerr = 0;
	if (validate)
	    process_inp(buffer, sts);
	else
	    printf("rec msg of len %d\n", sts);	
	timper = 100;
    }

ARM_TIMER:
    timarg[0] = -10000 * timper;
    timarg[1] = 0xFFFFFFFF;

    if (SYS$SETIMR(0,timarg,tcp_read,0,0) != SS$_NORMAL) 
    {
	printf("tcp_read can't set timer\n");
	g_crash();
    }
}

/* --------------- send junk data request ---------------- */

void do_junk()
{
    int tmp;

    if (g_oprint("heading",0xa1b2c3d4,0,-1,&tmp))
	return;
    dreq.hdr.stamp = htonl(tmp);
    if (g_oprint("checksum",0,0,-1,&tmp))
	return;
    dreq.hdr.chksum = htonl(tmp);
    if (g_oprint("message type",0,0,-1,&tmp))
	return;
    dreq.hdr.msgtyp = (short)htonl(tmp);
    if (g_oprint("message len",10,0,-1,&tmp))
        return;
    dreq.hdr.msglen = (short)htonl(tmp);

    if (g_oprint("CDC day",3,0,30000,&tmp))
	return;
    dreq.rqs.cdc = (short)htonl(tmp);
    if (g_oprint("first block",0,0,1000000,&tmp))
        return;
    dreq.rqs.first = htonl(tmp);
    if (g_oprint(" last block",0,0,1000000,&tmp))
        return;
    dreq.rqs.last = htonl(tmp);
    tcp_write(&dreq, sizeof(dreq.rqs));
}

/* ------------ send fixed error message ------------ */

void do_fixed(void)
{
    struct EMSG fmsg;

    fmsg.er = 0;
    strcpy(fmsg.msg,"hello from your client");

    sethdr(&fmsg.hdr,5,strlen(fmsg.msg)+5);
    tcp_write(&fmsg,sizeof(fmsg.hdr)+4+strlen(fmsg.msg)+1);
}

/* ------------ start AST timer ------------- */

static void do_startimer(void)
{
    if (timper < 100)
	timper = 500;

    timarg[0] = -10000 * timper;
    timarg[1] = 0xFFFFFFFF;

    if (SYS$SETIMR(0,timarg,tcp_read,0,0) != SS$_NORMAL) 
    {
	printf("do_startimer: can't set timer\n");
	g_crash();
    }
}

/*--------------------------------------------------------------------*/

main()
{
    int sts, opt, k;
    struct ICSHDR hdr;

    if (g_oprint("size of MJF bucket", 8192, 4096, 16384, &max_input))
	exit(0);

    bufflen = max_input + HDRSIZ + XFRSIZ + 128;
    max_input += XFRSIZ;

    fbuf = malloc( 2 * bufflen);
    buffer = malloc(bufflen);
    if (!fbuf || !buffer)
    {
	printf("could not allocate buffers\n");
	exit(0);
    }

    fnxt = fbuf;

    atexit(client_exit);
    do_startimer();
 
    while (1)
    {
	printf("Options (%d):\n", mystate);
	printf(" 1 - connect		    2 - drop connection\n");
	printf(" 3 - send error message     4 - send data request\n");
	printf(" 5 - start receiving 	    6 - stop receiving\n");
	printf(" 7 - send junk              8 - send fixed message\n");
	printf(" 9 - enable checksum       10 - disable checksum\n");
        printf("11 - enable checking       12 - disable checking\n");
	printf("13 - request disconnect	   14 - set send timout\n");
	printf("15 - send XOFF		   16 - set recv timer\n");
	printf("17 - show parameters	   18 - request system status\n");
	printf("20 - exit                   0 - continue\n");

	if (g_oprint("Select",0,0,20,&opt))
	    continue;
	switch(opt)
	{
	    case 0:
		break;
	    case 1:
		do_connect();
		break;
            case 2:
                do_disconnect();
                break;
	    case 3:
		do_errmsg();
		break;
	    case 4:
		do_request();
		break;
	    case 5:
		do_recv = 1;
		break;
	    case 6:
		do_recv = 0;
		break;
	    case 7:
		do_junk();
		break;
            case 8:
		do_fixed();
                break;
            case 9:
		chksum = 1;
                break;
            case 10:
		chksum = 0;
                break;
            case 11:
		validate = 1;
                break;
            case 12:
                validate = 0;
                break;
            case 13:		    /* request disconnect */
		sethdr(&hdr, 4, 0);
		tcp_write(&hdr,sizeof(hdr));
                break;
            case 14:
		g_oprint("send timeout (sec)",snd,1,10,&snd);                
                break;
            case 15:
		sethdr(&hdr,3,0);
		tcp_write(&hdr,sizeof(hdr));
                break;
            case 16:
                g_oprint("recv period (msec)",timper,200,2000,&timper);
                break;
            case 17:
                printf("validate=%d chksum=%d snd_to=%d timper=%d do_recv=%d\n",
			validate,chksum,snd,timper, do_recv);
                break;
	    case 18:
		do_reqsyst();
		break;
	    case 20:
		printf("bye bye\n");
		exit(0);
            default:
		printf("  n/a\n");
                break;
	}
    } /* end while... */
}
