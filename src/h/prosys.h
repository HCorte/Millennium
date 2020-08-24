#ifndef PROSYS_H
#define PROSYS_H

/*
   V01 25-NOV-1999  UXN Initial release for EuroGOLS. Code taken from
                        PROSYS system.

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

#define ICSLOG_PRODNUM 247
#define ICSLOG_PRODNAME "ICSLOG"

                                /* ERROR STRUCTURE */
struct G_ERROR {
    int errflg;                 /* error flag (0 - no error) */
    int errfile;                /* # of error file */
    int errnum;                 /* error # within error file */
    int errlvl;                 /* severity level ( see defines above) */
    int errsys;                 /* operating system (VMS) error # */
    char errloc[16];            /* location (function name) where error set */
    int errtxtlen;              /* length of errtxt in bytes (includes term 0)*/
    char errtxt[128];           /* null delimited strings of parameters */
};

#define GEL_INFO     10
#define GEL_WARNING  11
#define GEL_ERROR    15
#define GEL_FATAL    19

enum MJFBUCKSTS {
    GOOD_BUCKET = 0,            /* Good data bucket                         */
    EOF_BUCKET,                 /* Logical end of file bucket               */
    OLD_BUCKET,                 /* Bucket is for previous day - reused file */
    BAD_BUCKET,                 /* Bad bucket - Invalid data                */
    EOPF_BUCKET                 /* End of partial file                      */
};

#endif /* PROSYS_H */
