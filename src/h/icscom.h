#ifndef ICSCOM_H
#define ICSCOM_H
/* ===[ICSCOM.H]===================================================

   Description: ICSCOM definitions 

   V02 22-nov-1999 uxn Initial release for EuroGOLS.
   V01 12-aug-1996 mf  initial version

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
   --------------------------------------------------------------------*/
/*
   Note: 
      This structure and parameters must match exactly with
      definitions in FORTRAN definition file ICSCOM.DEF
      If you make any changes to this file, the same changes must
      be done to ICSCOM.DEF as well.
*/

#define MAX_CLIENTS 5           /* max # of clients */
#define HOSTLEN     80          /* max len of IP system name    */
#define IPADDRLEN   20          /* max len of ip address    */
#define FILNAMLEN   20          /* max len of file name */

#pragma member_alignment save
#pragma nomember_alignment
        /* client descriptor    */

struct CLIENT
{
    int status;                 /* client status        */
    int socknum;                /* socket number        */
    unsigned int conntime[2];   /* time of last connect */
    unsigned int disctime[2];   /* time of last disconnect*/
    int iotime;                 /* time of last good i/o (cycles)*/
    int xfrcdc;                 /* cdc # of current mjf */
    int start;                  /* first block # to send*/
    int finish;                 /* last block # to send */
    int last;                   /* last sent block #    */
    int batchblk;               /* # blocks in current batch*/
    int batchtick;              /* batch break tick count*/
    int totblks;                /* # of all sent blocks */
    int toterr;                 /* total I/O error count*/
    int retries;                /* # retries            */
    int rtrytick;               /* retry break tick count*/
    int fdb[7];                 /* TMF file description block */
    int mjfsts;                 /* mjf state            */
    int totxoff;                /* total X_OFF messages */
    int thruput;                /* blks/sec in prev minute*/
    int blkcnt;                 /* # blocks in current minute */
    int nackcnt;                /* # "no data" responses*/
    int badmsg;                 /* total bad messages   */
    int cachecnt;               /* cache hit count      */
    char ipaddr[IPADDRLEN];     /* client's IP          */
    char mjfname[FILNAMLEN];    /* mjf full name        */
    char name[HOSTLEN];         /* client's name        */
};

struct accept_connection {
    char host[HOSTLEN];
};

    /* main control structure descriptor */
struct ICSLOG
{
    int version;            /* version #    */
    int prodstate;          /* our state    */
    int act_clients;        /* current active clients */
            /* parameters from PCF */
    int max_clients;        /* max # of clients */
    int tcp_port;           /* tcp port number  */
    int max_out;            /* max bytes for send(... */
    int keep_alive;         /* keep alive flag  */
    int sleep_time;         /* lengt of sleep interval msec */
    int batch_size;         /* # of mjf blocks sent in 1 cycle */
    int batch_break;        /* inter batch wait in sleep units */
    int retry_delay;        /* inter retry delay in sleep units */
    int max_retries;        /* # retries before disconnect */
    int max_errors;         /* max # of errors in session before disconnect */
    int max_badmsg;         /* max # of bad msg in session before disconnect */
    int do_checksum;        /* do/don't checksum flag */
    int verbosity;          /* level of reporting */
            /* -- end of PCF params -- */
    int cycles;             /* wakeup counter */
    int io_cycles;          /* # cycles to check if client alive */
    int lost_conn;          /* total # of disconnects */
    int disconn;            /* total # server disconnected */
    int errcnt;             /* total i/o error count */
    int badmsg;             /* total # of bad messages */
    int tick;               /* minute flag */
    int totbuck;            /* total log blocks xfered */
    int totnacks;           /* total # of all NACKs */
    int tot_inmsg;          /* total messages from clients */
    int tot_cache;          /* total # of cache hits */
    int main_sock;          /* main socket */
    char hostname[HOSTLEN]; /* Name of our host */
    int run_on_primary;     /* Enables/disables connections on primary system */
    int run_on_backup;      /* Enables/disables connections on backup system */
    int run_on_spare;       /* Enables/disables connections on spare system */
    struct accept_connection ac[MAX_CLIENTS]; /* list of the systems that are */
                            /* allowed to connect to server */      
    struct CLIENT client[MAX_CLIENTS];  /* client descriptors */

};

#pragma member_alignment restore
#endif /* ICSCOM_H */
