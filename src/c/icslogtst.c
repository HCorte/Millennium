/* 

   V01 08-DEC-1999  UXN Initial release (from ICS_CLIENT.C).

   --------------------------------------------------------------------
   This item is the property of GTECH Corporation, West Greewich, Rhode
   Island, and contains confidential and trade secret information. It
   may not be transferred from the custody or control of GTECH except
   as authorized in writing by an officer of GTECH. Neither this item
   nor the information it contains may be used, transferred,
   reproduced, published, or disclosed, in whole or in part, and
   directly or indirectly, except as expressly authorized by an
   officer of GTECH, pursuant to written agreement.

   Copyright 1999 GTECH Corporation. All rights reserved.
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
#include  "inclib:rmsio.h" 
#include  "inclib:prosys.h"

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
struct G_ERROR gerr;
int chksum = 1, rcv = 5, snd = 1, nacks = 5, mystate = 0;
int max_input = 8216, timper = 500, do_recv = 1, ioerr = 0, debug = 0;
int timarg[2];

#define BLKSIZ 8192

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

struct XFRHDR
{
    char env;
    char syst;
    unsigned short cdc;
    char rslt;
    char filler[3];
    int bucknum;
};

#define HDRSIZ sizeof(struct ICSHDR)
#define XFRSIZ sizeof(struct XFRHDR)

char *fbuf;	    /* staging buffer */
char *fnxt;
char *buffer;	    /* tcp input/output buffer */
int bufflen;

char *remote;
struct FAB fab;
struct RAB rab;

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
		    char *ptr = (char*) (fbuf+HDRSIZ+XFRSIZ);
		    int   blk = ntohl(dm->bucknum);	
		    printf(
		    "*** MJF buck# %d ec=%d env=%d sys=%d cdc=%d len = %d\n",
			ntohl(dm->bucknum), dm->rslt, dm->env,
			dm->syst, ntohs(dm->cdc), ml-XFRSIZ );
		    if(dm->rslt == 0)
                    {
		       if( rms_write( &rab, blk, ptr, BLKSIZ) != RMS$_NORMAL )
		       {
			  printf("Error writing block %d to %s\n",
			          blk, remote);
		       }
		    }
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

    hdr->stamp = 0x4d3c2b1a;
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

void do_connect(char *server, int portnum)
{
    int k, sts;
	    
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

/* ------------- send data request --------------- */

void do_request(int cdc, int begblk, int endblk)
{

    sethdr(&dreq.hdr, 1, sizeof(dreq.rqs));
    dreq.rqs.cdc = htons((unsigned short) cdc);
    dreq.rqs.first = htonl(begblk);
    dreq.rqs.last = htonl(endblk);
    tcp_write(&dreq, sizeof(dreq));
}

/*--------------------------------------------------------------------*/
int main(int argc, char *argv[])
{
    int sts, opt, k;
    struct ICSHDR hdr;
    char *p_server;
    int  port;
    int rmask;

    if(argc != 5)
    {
	printf("Usage: icsclient <server> <port> <file> <bucksiz>");
	exit(0);
    }

    p_server  = argv[1];         /* server name */
    port      = atoi( argv[2] ); /* port number */
    remote    = argv[3];         /* remote file name */
    max_input = atoi( argv[4] ); /* MJF bucket size */

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

    if(!rms_open(remote, &fab, &rab))
    {
	printf("Cannot open %s\n", remote);
	exit(0);
    }

/*    do_startimer(); */
 
    do_connect(p_server, port);

    do_request(0, 1, 0x7fffffff );

    while (1)
    {
        rmask = 1 << mysock;
        sts = select(32, (struct fd_set *)&rmask, (struct fd_set *)&zero, 
                         (struct fd_set *)&zero, NULL);
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
			printf("recv error treshold exceeded\n");
			break;
		    }
		}
	    }
	    else if (sts > 0)
	    {
		ioerr = 0;
		process_inp(buffer, sts);
	    }
	}
    } /* end while... */
    rms_close( &fab );
    exit(0);
}
