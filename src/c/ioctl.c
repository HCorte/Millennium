/* ioctl.c 
   VMS version of ioctl function

   based on UCX examples

   V02 25-nov-1999 uxn initial release for EuroGOLS.
   V01 28-jul-1996 mf  initial release.

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
#include <stdio.h>
#include <errno.h>
#include <iodef.h>
#include <socket.h>
#include  <ucx$inetdef.h> 

#ifndef _IO
#define IOCPARM_MASK  0x7f		    /* Parameters are < 128 bytes   */
#define IOC_VOID      (int)0x20000000	    /* No parameters                */
#define IOC_OUT       (int)0x40000000	    /* Copy out parameters          */
#define IOC_IN        (int)0x80000000	    /* Copy in parameters           */
#define IOC_INOUT     (int)(IOC_IN|IOC_OUT)
#define _IO(x,y)      (int)(IOC_VOID|('x'<<8)|y)
#define _IOR(x,y,t)   (int)(IOC_OUT|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOW(x,y,t)   (int)(IOC_IN|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOWR(x,y,t)  (int)(IOC_INOUT|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)`
#endif /* _IO */

#define VMSOK(s) (s & 01)
	/* prototypes */
int SYS$QIOW();

/* ---------- function body ------------- */

int g_ioctl(int d, int request, char *argp)
{
    int ef = 0;			/* Event flag number */
    int sdc;			/* Socket device channel */
    unsigned short fun; 	/* QIOW function code  */
    unsigned short iosb[4];	/* IO status block */
    struct comm
    {
	int command;
	char *addr;
    } ioctl_comm;		/* QIOW ioctl commands. */

    struct it2 
    {
	unsigned short len;
	unsigned short opt;
	struct comm *addr;
    } ioctl_desc;		/* QIOW ioctl commands descriptor */

    struct it2 *p5, *p6;	/* Args p5 & p6 of qiow */
    int status;
    
			/* Get the socket device channel number */
    sdc = vaxc$get_sdc(d);
    if (sdc == 0)	/* Not an open socket descriptor */
    {
	errno = EBADF;
	return -1;
    }
    			/* Fill in ioctl descriptor */
    ioctl_desc.opt = TCPIP$C_IOCTL;
    ioctl_desc.len = sizeof(struct comm);
    ioctl_desc.addr = &ioctl_comm;
    
			/* Decide qio function code and in/out parameter */
    if (request & IOC_OUT)
    {
	fun = IO$_SENSEMODE;
	p5 = 0;
	p6 = &ioctl_desc;
    }
    else
    {
	fun = IO$_SETMODE;
	p5 = &ioctl_desc;
	p6 = 0;
    }
			/* Fill in ioctl command */
    ioctl_comm.command = request;
    ioctl_comm.addr = argp;
    
			/* Do ioctl */
    status = SYS$QIOW(ef, sdc, fun, iosb, 0, 0,
		      0, 0, 0, 0,		/* p1 - p4 are not used*/
		      (char *)p5, (char *)p6);
    if (!VMSOK(status))
    {
	errno = status;
	return -1;
    }
    
    if (!VMSOK(iosb[0]))
    {
	printf("ioctl failed: status = %x, %x, %x%x\n", iosb[0], iosb[1],
	       iosb[3], iosb[2]);
	errno = iosb[0];
	return -1;
    }
    
    return 0;
}
