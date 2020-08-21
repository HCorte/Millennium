/* ===[DES.H]===================================================

   Description: 

   Revisions:

      REV     DATE      BY      DESCRIPTION
      ----  --------   ----     ---------------------------------------
      1.00  01-dec-96  sbc      Added GTECH banner/multiple include wrapper
                                to file, and cleaned up
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

#ifndef DES_H
#define DES_H

/* Copyright (C) 1992 Eric Young - see COPYING for more details */
typedef unsigned char des_cblock[8];
typedef struct des_ks_struct
        {
        des_cblock _;
        } des_key_schedule[16];

#define DES_KEY_SZ      (sizeof(des_cblock))
#define DES_ENCRYPT     1
#define DES_DECRYPT     0

#ifdef DES_PROTO
int des_set_key(des_cblock *key, des_key_schedule *schedule);
int des_expand_key(des_cblock *key, des_key_schedule *schedule);
int des_ecb_encrypt(des_cblock *input, des_cblock *output,
                    des_key_schedule *ks, int encrypt);
int des_encrypt_data(   unsigned char *input,
                        int len,
                        des_key_schedule *ks,
                        int encrypt,            /* 1=encrypt, 0=decrypt */
                        unsigned char *output);
#else
int des_set_key();
int des_expand_key();
int des_ecb_encrypt();
int des_encrypt_data();
#endif

#endif
