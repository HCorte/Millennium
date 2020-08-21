/* ===[ICSLOG_TCP.C]===================================================
 *
 *  Description: ICSLOG main module and selected functions
 *
 *  V09 28-JUL-2000 oxk limit number of retries when select() times out
 *                      display message when last block of the day sent
 *  V08 26-May-2000 oxk System type request & response added
 *  V07 18-MaY-2000 uxn retry when select times out
 *  V06 21-MAR-2000 uxn Alpha changes (msock_name defined as static)
 *  V05 13-JAN-2000 uxn error code 6 added.
 *  V04 30-dec-1999 uxn system_status_ok() added.
 *  V03 25-nov-1999 uxn initial release for EuroGOLS
 *  V02 12-aug-1996 mf  initial version
 *  V01 11-mar-1997 sc  check for blocking in tcp_write,
 *		        send the last bucket to ICS,
 *		        do not re-open file if already open
 *	 	        set the finish bucket when read EOF bucket,
 *
 *  --------------------------------------------------------------------
 *  This item is the property of GTECH Corporation, West Greewich, Rhode
 *  Island, and contains confidential and trade secret information. It
 *  may not be transferred from the custody or control of GTECH except
 *  as authorized in writing by an officer of GTECH. Neither this item
 *  nor the information it contains may be used, transferred,
 *  reproduced, published, or disclosed, in whole or in part, and
 *  directly or indirectly, except as expressly authorized by an
 *  officer of GTECH, pursuant to written agreement.
 *
 *  Copyright 1996 GTECH Corporation. All rights reserved.
 *  --------------------------------------------------------------------
 *  ==================================================================== 
 */

#include <types.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <socket.h>
#include <in.h>
#include <netdb.h>
#include <inet.h>
#include <starlet.h>
#include <unixio.h>
#include <ucx$inetdef.h>     /* INET symbol definitions */

#include "inclib:icslog.h"

extern struct ICSLOG *ccp;
extern char line[256];
extern struct ICS_ERR icserr;

static int table_set;
struct ICSBUF icsbuf;

static struct CACHE cache[MAX_CACHE];

int system_status_ok()
{
  return (GOLS_SYSTYPE() == LIVSYS && ccp->run_on_primary) ||
         (GOLS_SYSTYPE() == BAKSYS && ccp->run_on_backup)  ||
         (GOLS_SYSTYPE() == SPRSYS && ccp->run_on_spare);
}
/* --------------------- exit handler --------------------- */

void serv_exit(void)
{
    int k, sts;
    char erm[80];

               /* close all clients */
    for (k = 0; k < ccp->max_clients; k++)
    {
        if (ccp->client[k].status >= CONNECTED)
        {
            sts = close(ccp->client[k].socknum);
            if (sts && ((errno != ECONNRESET) || (errno != EWOULDBLOCK)))
            {
                sprintf(erm, "socket %d close err %d - %s",
                           ccp->client[k].socknum, errno, strerror(errno));
                ops_msg( erm );
            }
        }
    }

    sts = close (ccp->main_sock);
    if (sts)
    {
        sprintf(erm, "main socket close err %d - %s\n", errno, strerror(errno));
        ops_msg( erm );
    }

    ops_msg("STOP0001  ****  SUCCESS");
    ccp->prodstate = PROD_STOPPED;
}
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

/* ------------ find slot for new connection ------------   
                try to use the old one for reconnect        
   Return statuses
        -1 : no free slots
	 0 : ip-address already connected   
	 1 : creating new connection
	 2 : reconnecting old connection
*/

static int get_slot(const char *ipaddr, int *slot)
{
    int k, same_addr, slot_used;

    *slot = -1;

    for (k = 0; k < ccp->max_clients; k++)
    {
	same_addr = !strcmp(ipaddr, ccp->client[k].ipaddr);
	slot_used = ccp->client[k].status >= CONNECTED;

        if (same_addr)
	{
	    if (slot_used)
	    {
		return 0;
	    }
	    else
	    {
		*slot = k;
		return 2;
	    }
	}
	else
        {
	    if (!slot_used && *slot == -1)
	    {
		*slot = k;
	    }
	}
    }

    return (*slot == -1 ? -1 : 1);
}
/* -------------------- tcp fatal error ----------------- */

static void tcpabort(void)
{
	ops_msg( line );        
        g_crash();
}

/* --------------------- initialize tcp ------------------- */

void init_tcp(struct ICSLOG *ccp)
{
    int sts, one = 1;
    struct hostent hostentstruct;   /* Storage for hostent data.  */
    struct hostent *hostentptr;     /* Pointer to hostent data.   */
    static struct sockaddr_in msock_name;  /* Address struct for main_sock*/

                    /* Open main socket */
    if ((ccp->main_sock = socket (AF_INET, SOCK_STREAM, 0)) == -1)
    {
	sprintf(line,"can't open socket - %s",strerror(errno));
	tcpabort();
    }
                    /* Get the host local name */
    sts = gethostname(ccp->hostname,sizeof(ccp->hostname));
    if (sts)
    {
        sprintf(line,"can't get host name - %s",strerror(errno));
	tcpabort();
    }
                    /* fill network data structure for main socket */
    if ((hostentptr = gethostbyname (ccp->hostname)) == NULL)
    {
        sprintf(line,"gethostbyname failed - %s",strerror(errno));
	tcpabort();
    }
                    /* Copy hostent data to safe storage */
    hostentstruct = *hostentptr;

                    /* Fill in the name & address structure for main socket */
    msock_name.sin_family = hostentstruct.h_addrtype;
    msock_name.sin_port = htons(ccp->tcp_port);
    msock_name.sin_addr = * ((struct in_addr *) hostentstruct.h_addr);

                    /* allow reuse of local addresses */
    sts = setsockopt(ccp->main_sock, SOL_SOCKET, SO_REUSEADDR,
                                                (char *)&one, sizeof(one));
    if (sts)
    {
        sprintf(line,"setsockopt (reuse) failed - %s",strerror(errno));
	tcpabort();
    }
                    /* Bind name to main socket  */

    sts = bind (ccp->main_sock, (struct sockaddr*) &msock_name, 
                                 sizeof(msock_name));
    if (sts)
    {
        sprintf(line,"bind failed - %s",strerror(errno));
	tcpabort();
    }

	sts = setsockopt(ccp->main_sock, SOL_SOCKET, TCPIP$C_TCP_NODELAY,
				(char *)&one, sizeof(one));
	if (sts)
	{
	    sprintf(line,"setsockopt no_delay failed - %s",strerror(errno));
	    tcpabort();
	}
                  /* set socket to non-blocking i/o   */
    sts = g_ioctl(ccp->main_sock, FIONBIO, (char *)&one);
    if (sts)
    {
        sprintf(line,"ioctl (main) failed - %s",strerror(errno));
	tcpabort();
    }

                    /* Set max # of concurrent connections  */
    sts = listen (ccp->main_sock, ccp->max_clients);
    if (sts)
    {
        sprintf(line,"listen failed - %s",strerror(errno));
	tcpabort();
    }

    memset(&cache, 0, sizeof(cache));
}

/* --------------------- tcp connect handler ------------------ */

void do_connect()
{
    int i, k, sts, next, addrlen, one = 1, zero = 0;
    char ipaddr[20], ipname[128];
    struct sockaddr_in taddr;
    struct sockaddr nsk;
    struct hostent *clnt;
    struct linger ling = {0, 5};
    int found;

                /*  Accept connection from main socket on socket next */
    addrlen = sizeof(nsk);
    next = accept (ccp->main_sock, &nsk, (unsigned int *)&addrlen);
    if (next == -1)
    {
        if (errno != EWOULDBLOCK)
        {
            sprintf(line,"TCP accept error %s",strerror(errno));
	    tcpabort();
        }
	return;
    }

/* initialize local data for new connection */

    memcpy(&taddr, &nsk, addrlen);
    strcpy(ipaddr,inet_ntoa(taddr.sin_addr));

    clnt = gethostbyaddr((char *)&taddr.sin_addr, 4,AF_INET);
    if (clnt != NULL)
	strcpy(ipname,clnt->h_name);
    else
	strcpy(ipname,"unknown");

    sts = get_slot(ipaddr, &k);
    switch (sts)
    {
	case -1:    
	{
	    send_error_disconn(next,8,"No room for client");
	    return;
	}
	case  0:
	    send_error_disconn(next,7,"IP-addr already connected");
	    return;
	case  1:
	{
	    sprintf(line,"new client %s at %s \n", ipname,ipaddr);
	    break;
	}
	case  2:
	{
	    sprintf(line,"reconnecting %s at %s\n",ipname,ipaddr);
	    break;
	}
	default:    /* notreached */
	    return;
    }

    if (ccp->keep_alive)               /* turn on keep alive */
    {
	sts = setsockopt(next, SOL_SOCKET, SO_KEEPALIVE,
			    (char *)&one, sizeof(one));
	if (sts)
	{
	    sprintf(line,"setsockopt keep_alive failed - %s",
			strerror(errno));
	    tcpabort();
	}
    }

    sts = setsockopt(next, SOL_SOCKET, TCPIP$C_TCP_NODELAY,
			    (char *)&one, sizeof(one));
    if (sts)
    {
	sprintf(line,"setsockopt no_delay failed - %s",
			strerror(errno));
	tcpabort();
    }

    sts = setsockopt(next, SOL_SOCKET, SO_LINGER,
			(char *)&ling, sizeof(ling));
    if (sts)
    {
	sprintf(line,"setsockopt linger failed - %s",
		    strerror(errno));
	tcpabort();
    }

    sts = g_ioctl(next, FIONBIO, (char *)&one);
    if (sts)
    {
	sprintf(line,"ioctl failed - %s",strerror(errno));
	tcpabort();
    }

    ops_msg( line );

    ccp->act_clients++;
    memset(&ccp->client[k], 0, sizeof(struct CLIENT));
 
    ccp->client[k].socknum = next;
    ccp->client[k].mjfsts = NOMJF;
    strcpy(ccp->client[k].ipaddr, ipaddr);
    strcpy(ccp->client[k].name, ipname);
    SYS$GETTIM(&ccp->client[k].conntime);
    ccp->client[k].status = CONNECTED;

    if (ccp->verbosity >= RL_EVENT)
    {
	sprintf(line, "connected client %d %s",k,ccp->client[k].name);
	ops_msg( line );
    }

    found = 0;
    for(i=0; i<MAX_CLIENTS; i++) 
    {
	if( !strcmp(ccp->ac[i].host, ipaddr) )
	    found = 1;
    }
    if( !found )
    {
	sprintf(line,"Unauthorized access from %s at %s", ipname, ipaddr);
	disconnect(k, 6);
    }
}

/* ------------ disconnect client --------------- */

void disconnect(int cln, int why)
{
    if (ccp->client[cln].status < CONNECTED)
    {
	sprintf(line,"trying to disconnect already disconnected client %d",cln);
	ops_msg( line );
	return;
    }

    if (ccp->verbosity >= RL_EVENT) 
    {
	sprintf(line,"disconnecting client %s",ccp->client[cln].name);
	ops_msg( line );
    }

    sprintf(line, "%s ", ccp->client[cln].name);

    switch( why )
    {
        case 1:	/* broken connection */
		strcat(line,"lost connection");
	        ccp->lost_conn++;
		break;
	case 2:	/* too many i/o errors */
		strcat(line, "exceeded error limit");
		ccp->disconn++;
		break;
	case 3: /* requested disconnect */
		strcat(line, "requested disconnect");
		break;
	case 4: /* GTMS ordered disconnect */
		strcat(line, "GTMS ordered disconnect");
		break;
	case 5: /* too many bad messages */
		strcat(line,"exceeded bad_msg limit");
		ccp->disconn++;
		break;
	case 6: /* unauthorized access */   
		strcat(line, "unauthorized access");
		break;
	default:
		sprintf(line,"%s: reason code %d",line, why);
		break;
    }

    ops_msg( line );

    SYS$GETTIM(&ccp->client[cln].disctime);
    ccp->client[cln].status = DISCONNECTED;
    if (ccp->client[cln].mjfsts == MJFOPEN)
    {
	GOLS_CLOSE_TMF( &ccp->client[cln].fdb );
    }
    ccp->client[cln].mjfsts = NOMJF;
    if (ccp->act_clients > 0)
	ccp->act_clients--;

    if (close(ccp->client[cln].socknum) == -1)
    {
	if ((errno != EWOULDBLOCK) && (errno != ECONNRESET))
        {
	    sprintf(line, "disconnect close error %d - %s", 
                    errno,strerror(errno));
            ops_msg( line );
        }
    }
}

/* ------- increment error count and disconnect if too many ------- */

static int count_errors(int cln, int kind)
{
    if (kind == 1)
    {
	ccp->errcnt++;
	ccp->client[cln].toterr++;
	if (ccp->client[cln].toterr++ > ccp->max_errors)
	{
	    disconnect(cln, 2);
	    return -1;
	}
    }
    else
    {
	ccp->badmsg++;
        ccp->client[cln].badmsg++;
        if (ccp->client[cln].badmsg++ > ccp->max_badmsg)
        {
            disconnect(cln, 5);
            return -1;
        }
    }

    return -2;
}

/* ---------------- perform tcp read ---------------- */
/*
** returns  0	- no data
**	    -1	- disconnected
**	    -2	- read error
**	    n	- # bytes read
*/
 
static int tcp_read(int cln, char *buf, int buflen)
{
    int len, rc, sock, flag = 0;

    sock = ccp->client[cln].socknum;
    len = recv( sock, buf ,buflen, flag);
    if (len == -1)
    {
	if (errno != EWOULDBLOCK)
	{
	    if ((errno == EPIPE) || (errno == ECONNRESET))
	    {
		disconnect(cln, 1);
		return -1;
	    }
	    else
	    {
		sprintf(line,"recv failed - (%d)", errno);
		ops_msg( line );
		rc = count_errors(cln, 1);
		return rc;
	    }
	}
	return 0;
    }

    if (len)
	ccp->client[cln].iotime = ccp->cycles;

    if( ccp->verbosity > RL_DEBUG )
       hex_dump("tcp_read():",buf, len);

    return len;
}

/* ---------------- get and verify message header --------------- */
/*
** returns:  0 - no data
**	    -1 - bad header
**	    -2 - disconnected
**	     n - msg type
*/

static int tcp_readhdr(int cln, int sock, struct MSGHDR *hdr, int *datlen)
{
    int len, rc, hdrlen = sizeof(struct MSGHDR);
    int mhdr,  mchk, mtyp, mlen;

    *datlen = 0;
    len = tcp_read(cln, (char *)hdr, hdrlen);
    if (len <= 0)
	return 0;

    if( ccp->verbosity > RL_DEBUG )
       hex_dump("HEADER:",hdr, len);

    mhdr = hdr->msghdr;
    mchk = ntohl(hdr->chksum);
    mtyp = (unsigned int) ntohs(hdr->msgtype);
    mlen = (unsigned int) ntohs(hdr->msglen);
    
    if (ccp->verbosity >= RL_DEBUG)
    {
	sprintf(line, "%s (%d/%d) chk=%d req=%d len=%d \n",
                ccp->client[cln].name,cln,len,mchk,mtyp,mlen);
	ops_msg( line );
    }
	
    if (len < hdrlen) 
	sprintf(line,"bad message header size %d, should be %d from %s\n", 
			len, hdrlen, ccp->client[cln].name);
    else if (mhdr != NETHDR)
	sprintf(line,"bad message header  (%x) from %s\n",
                        mhdr, ccp->client[cln].name);
    else if ((mtyp != REQMJF)  && (mtyp != REQXOFF) && (mtyp != REQDISC) &&
	     (mtyp != ERREPLY) && (mtyp != REQSYST))
        sprintf(line,"bad message type (%d) from %s\n",
                        mtyp, ccp->client[cln].name);
    else
	line[0] = '\0';

    ccp->tot_inmsg++;

    if( line[0] != '\0' )
    {
	ops_msg( line );
	ccp->badmsg++;
	ccp->client[cln].badmsg++;
	rc = count_errors(cln,2);
	return rc;
    }

    if ((mlen < 0) || (mlen >= MAX_INPLEN))
    {
        ops_msg( line );

        ccp->badmsg++;
        ccp->client[cln].badmsg++;
	rc = count_errors(cln,2);
        return rc;
    }

    *datlen = mlen;
    if (ccp->verbosity >= RL_DEBUG)
    {
	sprintf(line, "got msg type %x\n",mtyp);
	ops_msg( line );
    }
    return (int) mtyp;
}

static int disconnect_error(int err)
{
    switch (err) {

        case EPIPE:
        case ENETDOWN:
        case ENETUNREACH:
        case ENETRESET:
        case ECONNABORTED:
        case ECONNRESET:
        case ETIMEDOUT:
        case EHOSTDOWN:
        case EHOSTUNREACH:
            return 1;
	default:
	    break;
    }
    return 0;
}
 

/* -------------- send out a buffer --------------- */
/*		returns: <= 0 - failure    > 0 - OK	    */
 
static int tcp_write(int cln, char *buff, int buflen)
{
    int cnt, flag = 0;

    cnt = send(ccp->client[cln].socknum, buff, buflen, flag); 
    if (cnt == -1)
    {
	if (errno == EWOULDBLOCK) {
	    return 0;
	}

	if ( disconnect_error(errno) ) {
	    disconnect(cln, 1);
	    return -1;
	}
	else {
				    /* retriable error ? */
	    if ( count_errors(cln, 1) == -1) {
		return -1;
	    }
	    else {
		return 0;
	    }
	}
    }
    else if (cnt == 0)
    {
	sprintf(line, "written 0 bytes errno=%d %s\n",errno, strerror(errno));
	ops_msg(line);
	return 0;
    }
    else		/* done some i/o */
    {
	ccp->client[cln].iotime = ccp->cycles;
	return cnt;
    }
}

/* --------------- calculate checksum --------------- */

static int do_crc(void *buf, int buflen)
{
    int hchk, nchk;

    if (!table_set)
    {
	table_set = 1;
    }
 
    hchk = crc((unsigned char *)buf, buflen);
    nchk = htonl(hchk);
/*
    if (ccp->verbosity >= RL_DEBUG)
    {
	sprintf(line, "checksum h=%x n=%x\n", hchk, nchk);
	ops_msg( line );
    }
*/
    return nchk;
}

/* -------------- send error message -------------- */

void send_icserr(int cn, int rc, char *msg)
{
    unsigned short len;
    
    len = strlen(msg) + 5;
    icserr.msghdr.msglen = htons(len);
    icserr.msghdr.msgtype = htons((unsigned short) ERREPLY);
    icserr.em.errcode = htonl(rc);
    strcpy(icserr.em.errmsg, msg);
    send_out(&icserr, len + sizeof(struct MSGHDR), cn);
}

/* -------------- send error & disconnect immediately -------------- */

void send_error_disconn(int sock, int rc, char *msg)
{
    unsigned short len;
    
    len = strlen(msg) + 5;
    icserr.msghdr.msglen = htons(len);
    icserr.msghdr.msgtype = htons((unsigned short) ERREPLY);
    icserr.em.errcode = htonl(rc);
    strcpy(icserr.em.errmsg, msg);

    len += sizeof(struct MSGHDR);

    icserr.msghdr.msghdr = NETHDR;
    icserr.msghdr.chksum = 0;
    if (ccp->do_checksum)
	icserr.msghdr.chksum = do_crc((void *)&icserr, len);

    send(sock,(char *)&icserr, len, 0);

    close(sock);
}


/* check if clients are alive and system status is ok */

void check_client(void)
{
    int k;

    for (k = 0; k < ccp->max_clients; k++)
    {
	if ((ccp->client[k].status == CONNECTED) ||
	    (ccp->client[k].status == XOFFBREAK))
	{
	    if( !system_status_ok() )
            {
		send_icserr(k, 6, "Incorrect system status");
                disconnect(k, 6); 
            }
            else 
            {
	        if ((ccp->cycles - ccp->client[k].iotime) > ccp->io_cycles)
		    send_icserr(k, 0, "hello");
            }
	}
    }
}

/* ---------------- process requests from clients ---------------- */

void read_client()
{
    int k, mlen, mfun, rlen, flag = 0;
    int cdc, first, last, st;
    struct ERR_MSG emsg;
    struct MSGHDR msghdr;

    for (k = 0; k < ccp->max_clients; k++)
    {
	while (ccp->client[k].status >= CONNECTED)
	{
	    mfun = tcp_readhdr(k, ccp->client[k].socknum, &msghdr, &mlen);
	    
	    if (mfun <= 0)
		break;
	
	    switch( mfun )
            {
	        case REQMJF:  /* TMF block requests */
			st = decode_reqmjf(k, mlen);
			break;
	        case REQXOFF: /* XFER Off messages */
			st = decode_reqxoff(k, mlen);
			break;
		case REQDISC: /* disconnect requests */
			st = decode_reqdisc(k, mlen);
			break;
		case ERREPLY: /* error reply */
			st = decode_erreply(k, mlen);
			break;
		case REQSYST: /* system status request */
			st = decode_reqsyst(k, mlen);
			break;
		default:  /* bad message header already handled */
		        st = 1;
			break;
            }
	    if(st) break;
	}
    }
}

/* --------------- complete the header and send a message --------- */

void send_out(void *buf, int buflen, int cln)
{
    int tosend, chunk, cnt, nosend = 0;
    char *pos;
    struct MSGHDR *mh;

    if (ccp->verbosity >= RL_DEBUG)
    {
	sprintf(line, "send_out cln=%d buflen=%d\n",cln,buflen);
	ops_msg( line );
    }

    mh = (struct MSGHDR *)buf;
    mh->msghdr = NETHDR;
    mh->chksum = 0;
    if (ccp->do_checksum)
	mh->chksum = do_crc(buf, buflen);

    tosend = buflen;
    pos = (char *)buf;
    chunk = 0;

    ccp->client[cln].retries = 0;

    while( tosend > 0)
    {
	if (tosend <= ccp->max_out)
	    chunk = tosend;
	else
	    chunk = ccp->max_out;
	

	if (ccp->verbosity >= RL_DEBUG)
        {
	    sprintf(line, "tosend=%d chunk=%d\n",tosend,chunk);
	    ops_msg( line );
        }

	cnt = tcp_write(cln, pos, chunk);
	if (cnt == chunk)
	{
            tosend -= chunk;
            pos += chunk;               /* set pointer to next chunk of data */
	    nosend = 0;
	    ccp->client[cln].retries = 0;
	}
	else if (cnt > 0)
	{
	    tosend -= cnt;
	    pos += cnt;
	    nosend = 0;
	    ccp->client[cln].retries = 0;
	}
	else if (cnt == 0)
	{
	    int sts, wmask, zero = 0;
	    struct timeval ws;
					/* Wait for the socket to become */
					/* available for write		 */
					/* wait at most a 5 secs	 */
					/* ccp->max_retries times	 */
	    ws.tv_sec = 5;
	    ws.tv_usec = 0;
	    wmask = 1 << ccp->client[cln].socknum;
	    sts = select(32, (struct fd_set *)&zero, 
                             (struct fd_set *)&wmask, 
                             (struct fd_set *)&zero, &ws);
	    if (sts < 0)
	    {
		sprintf(line,"select failed - %s",strerror(errno));
		ops_msg( line );
		disconnect(cln,1);
		tcpabort();
	    }
	    else if (sts == 0)
	    {
		sprintf(line,"select timed out for  %s", ccp->client[cln].name);
		ops_msg( line );
		ccp->client[cln].retries++;
		if (ccp->client[cln].retries > ccp->max_retries)
		{
		    disconnect(cln, 6);
		    return;	/* Too many retries */
		}
	    }
	}
	else {
	    return; /* Error, disconnected */
	}
    }
}
/*****************************************************************************/
/* read_tmf(int k)                                                           */
/*****************************************************************************/
int read_tmf(int k)
{
    static int cache_ind = 0;
    int block = ccp->client[k].last;
    int st, found, i;

    st = 0;
    found = 0;

    for(i=0;i<MAX_CACHE;i++)
    {
	if(cache[i].num == ccp->client[k].last) 
        {
           found = 1;
	   break;
	}
    }
    if(found) 
    {
	memcpy (&icsbuf.mjfbuck, &cache[i].buf, sizeof(icsbuf.mjfbuck));
	ccp->client[k].cachecnt++;
	ccp->tot_cache++;
    }
    else
    {
	GOLS_READ_TMF( &ccp->client[k].fdb, &ccp->client[k].last, 
                       &icsbuf.mjfbuck, &st ); 
        if(st == 0)
        {
	    memcpy (&cache[cache_ind].buf, &icsbuf.mjfbuck, 
                    sizeof(icsbuf.mjfbuck));
            cache[cache_ind].num = ccp->client[k].last;	    
            cache_ind = (cache_ind + 1) % MAX_CACHE;
        }   
    }

    switch(st)
    {
        case 0: /* MJFDATA follows */
	     return MJFDATA;
	case 1: /* block not currently available */
	     return BUCKAVL;
	case 2: /* last block for this CDC */
	     return MJFLAST;
	default: /* invalid block number */
	     return BUCKINV;
    }
}

/* --------------- process mjf xfer ----------------- */

void write_client(void)
{
    int buflen, bucksts, k, result, do_break, keep_on;
    unsigned short mlen, mtyp, st;

    for (k = 0; k < ccp->max_clients; k++)
    {
        if (ccp->client[k].status != RECEIVING)
            continue;
	
	do_break = 1;
	keep_on = 1;

	for (ccp->client[k].batchblk = 0;
	     ccp->client[k].batchblk < ccp->batch_size;
	     ccp->client[k].batchblk++)
        {
	    if(ccp->client[k].mjfsts != MJFOPEN) 
            {
	       do_break = 0;
	       keep_on  = 0;
	       break;
            }

	    result = read_tmf(k);

	    mtyp = (unsigned short)MJFXFER;
	    icsbuf.header.msgtype = htons(mtyp);
	    icsbuf.rplhdr.resultcode = (char) result;
	    icsbuf.rplhdr.environment =  GOLS_ENVIRONMENT();
	    icsbuf.rplhdr.systype = GOLS_SYSTYPE();
	    icsbuf.rplhdr.cdc = htons( (unsigned short) GOLS_CDC());
	    icsbuf.rplhdr.bucknum = htonl(ccp->client[k].last);
	    buflen = sizeof(icsbuf.header) + sizeof(icsbuf.rplhdr);
	    if (result == MJFDATA || result == MJFLAST)
	    {
                buflen += sizeof(icsbuf.mjfbuck);
                mlen = sizeof(icsbuf.rplhdr) + sizeof(icsbuf.mjfbuck);
                ccp->client[k].last++;
		ccp->client[k].totblks++;
		ccp->client[k].blkcnt++;
		ccp->totbuck++;
		if (ccp->verbosity >= RL_DEBUG)
                {
		    sprintf(line, "sending to %s bucket %d\n",
			   ccp->client[k].name, ccp->client[k].last - 1);
		    ops_msg( line );
		}
		if  (ccp->client[k].last > ccp->client[k].finish)
		{
		    if (ccp->verbosity >= RL_DEBUG)
                    {
			sprintf(line, "xfer for %s completed bucket %d\n",
			        ccp->client[k].name, ccp->client[k].last - 1);
			ops_msg( line );
                    }
		    ccp->client[k].status = CONNECTED;
		    do_break = 0;
		    keep_on = 0;
		}
	    }
	    else
	    {
		mlen = sizeof(icsbuf.rplhdr);
		icsbuf.rplhdr.bucknum = htonl(ccp->client[k].last);
		ccp->client[k].nackcnt++;
		ccp->totnacks++;
		ccp->client[k].status = RETRYBREAK;
		ccp->client[k].rtrytick = ccp->retry_delay;
		do_break = 0;
		keep_on = 0;
	    }

	    icsbuf.header.msglen = htons(mlen);
	    send_out(&icsbuf, buflen, k);
	    if (result == MJFLAST)
	    {
		sprintf(line, "last block sent to %s\n",
			ccp->client[k].name);
		ops_msg( line );
	    }
	    if (!keep_on)
		break;
	} /* end of batch loop */
	if (do_break)
	{
	    if (ccp->verbosity >= RL_DEBUG)
            {
		sprintf(line, "%s - batch break\n", ccp->client[k].name);
		ops_msg( line );
            }
	    ccp->client[k].status = BATCHBREAK;
	    ccp->client[k].batchtick = ccp->batch_break;
	}
    } /* end of client loop */
}
/*****************************************************************************/
/* decode_reqmjf(int cln, int mlen)                                          */
/*****************************************************************************/
int decode_reqmjf(int cln, int mlen)
{
    struct REQ_MJF reqmjf;
    int rlen, cdc, first, last, st;

    if(mlen != sizeof(struct REQ_MJF)) 
    {
	if( ccp->verbosity > RL_DEBUG )
        {
	    printf("Invalid message len %d, should be %d\n", mlen, 
                    sizeof(struct REQ_MJF));
        }
        return 1;
    }

    rlen = tcp_read(cln, (char *)&reqmjf, sizeof(reqmjf));
    if (rlen != sizeof(struct REQ_MJF))
    {
	sprintf(line,"bad data request message of len %d from %s",rlen,
                     ccp->client[cln].name);
	ops_msg( line );

	send_icserr(cln, 4, line);
	count_errors(cln,2);
	ccp->badmsg++;
	ccp->client[cln].badmsg++;
	return 1;
    }

    cdc = (unsigned int) ntohs(reqmjf.cdc);
    first = ntohl(reqmjf.startbucknum);
    last = ntohl(reqmjf.endbucknum);
    if (ccp->verbosity >= RL_DEBUG)
    {
	sprintf(line, "%s xfr_req cdc=%d first=%d last=%d\n",
		ccp->client[cln].name, cdc, first, last);
	ops_msg( line );
    }
    if ((first > last) || (first<=0))
    {
        sprintf(line,"bad mjf xfer request cdc=%d first=%d last=%d from %s",
		     cdc, first, last, ccp->client[cln].name);
	ops_msg( line );

	send_icserr(cln, 3, line);
	count_errors(cln,2);
	ccp->badmsg++;
	ccp->client[cln].badmsg++;
	return 1;
    }
    ccp->client[cln].start = first;
    ccp->client[cln].finish = last;
    ccp->client[cln].last = first;	/* next to be sent */

    ccp->client[cln].xfrcdc = cdc;
    if (ccp->client[cln].mjfsts != MJFOPEN) {
				    /* Back to connected */
	ccp->client[cln].status = CONNECTED;
	GOLS_OPEN_TMF( &ccp->client[cln].fdb, 
		       &ccp->client[cln].mjfname, &st);
	if(st) {
	    ccp->client[cln].mjfsts = NOMJF;			
	    sprintf(line, "Error opening TMF file");
	    ops_msg( line );
	}
	else {
	     ccp->client[cln].mjfsts = MJFOPEN;
	     ccp->client[cln].status = RECEIVING;
	}

    }
    else {
				    /* Back to receiving */
	ccp->client[cln].status = RECEIVING;
    }

    return 0;
}
/*****************************************************************************/
/* decode_erreply(int cln, int mlen)                                         */
/*****************************************************************************/
int decode_erreply(int cln, int mlen)
{
    struct ERR_MSG emsg;
    int rlen;

    if( mlen < 4 ) return 1;

    rlen = tcp_read(cln, (char *)&emsg, MAX_ERRLEN);
    if (rlen > 5)	    /* non-empty message */
    {
	emsg.errmsg[MAX_ERRLEN -1] = 0;
	sprintf(line,"Message from %s: %s", ccp->client[cln].name, emsg.errmsg);
	ops_msg(line);
    }
    else if (rlen > 4)
	return 0;		    		    
    else
    {
	sprintf(line,"bad error message len %d from %s",rlen, 
                     ccp->client[cln].name);
	ops_msg( line );

	send_icserr(cln, 4, line);
	count_errors(cln,2);
	ccp->badmsg++;
	ccp->client[cln].badmsg++;
    }
    return 0;
}
/*****************************************************************************/
/* decode_reqxoff(int cln, int mlen)                                         */
/*****************************************************************************/
int decode_reqxoff(int cln, int mlen)
{   
    if(mlen != 0) return 1;

    sprintf(line, "%s requested XOFF", ccp->client[cln].name);
    ops_msg( line );

    ccp->client[cln].status = XOFFBREAK;
    ccp->client[cln].totxoff++;
    
    return 0;
}
/*****************************************************************************/
/* decode_reqdisc(int cln, int mlen)                                         */
/*****************************************************************************/
int decode_reqdisc(int cln, int mlen)
{

    if(mlen != 0 ) return 1;
    disconnect(cln, 3);

    return 0;
}
/*****************************************************************************/
/* decode_reqsyst(int cln, int mlen)                                         */
/*****************************************************************************/
int decode_reqsyst(int cln, int mlen)
{
    static struct RPL_SSTAT r;
    unsigned short len;

    if(mlen != 0 ) return 1;
    
    len = sizeof(struct SYS_STAT);
    r.msghdr.msglen = htons(len);
    r.msghdr.msgtype = htons((unsigned short) SYSREPL);
    r.stat.systype = GOLS_SYSTYPE();
    r.stat.cdc = htons( (unsigned short) GOLS_CDC());

    send_out(&r, sizeof(struct RPL_SSTAT), cln);

    return 1;
}

