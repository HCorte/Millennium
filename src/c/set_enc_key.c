/* ===[SET_ENC_KEY.C]===================================================

   Description: Set encryption key from password

   Revisions:

      REV     DATE      BY      DESCRIPTION
      ----  --------   ----     ---------------------------------------
      1.00  10-may-95  sbc      initial release - removed from ENC_SETKEY
   --------------------------------------------------------------------
   This item is the property of GTECH Corporation, West Greewich, Rhode
   Island, and contains confidential and trade secret information. It
   may not be transferred from the custody or control of GTECH except
   as authorized in writing by an officer of GTECH. Neither this item
   nor the information it contains may be used, transferred,
   reproduced, published, or disclosed, in whole or in part, and
   directly or indirectly, except as expressly authorized by an
   officer of GTECH, pursuant to written agreement.

   Copyright 1994 GTECH Corporation. All rights reserved.
   --------------------------------------------------------------------
   ==================================================================== */

#include <stddef.h>
#include "enc_parity.h"

/*
****
** convert a binary number into a 4 byte ascii array
*/

static void binary_to_ascii4(unsigned int bin, char *ascii)
{
    ascii[3] = bin % 10 + '0';

    bin /= 10;
    ascii[2] = bin % 10 + '0';

    bin /= 10;
    ascii[1] = bin % 10 + '0';

    bin /= 10;
    ascii[0] = bin % 10 + '0';
}

/*
****
** return the result of flipping 4 bytes
*/

static unsigned int flip_4_bytes(char *value)
{
  char flipped[4];

    flipped[0] = value[3];
    flipped[1] = value[2];
    flipped[2] = value[1];
    flipped[3] = value[0];

    return *((unsigned int *)flipped);
}


/*
****
** set the parity of an encryption key
*/

void enc_set_odd_parity(unsigned char *key)
{
  int offset;
  int high;

    for (offset = 0; offset < 8; offset++) {
        high = key[offset] & 0xfe;
        if ( enc_even_parity[ high ] ) {
            high |= 0x01;
        }
        key[offset] = high;
    }
}

/*
*****
** set an encryption key
*/

void set_enc_key(   int pass,
                    unsigned char *keyp)
{
  static unsigned int base = 0x69696969;
  unsigned int pass_mod;
  char ascii_pass[4];
  unsigned int key;
  unsigned int enckey[2];
  unsigned int *long_keyp = (unsigned int *)keyp;

                                    /* modulo 10000 */
    pass_mod = pass % 10000;
                                    /* convert to ascii */
    binary_to_ascii4(pass_mod, ascii_pass);

                                    /* flip bytes */
    key = flip_4_bytes(ascii_pass);
                                    /* set key */

                                    /* shift key and base-key to left */
    enckey[0] = key << 1;
    enckey[1] = (base - key) << 1;
                                    /* flip bytes */
    enckey[0] = flip_4_bytes((char *)&enckey[0]);
    enckey[1] = flip_4_bytes((char *)&enckey[1]);

                                    /* set parity bits */
    enc_set_odd_parity((unsigned char *)enckey);

                                    /* set encryption key table */
    long_keyp[0] = enckey[0];
    long_keyp[1] = enckey[1];
    return;
}
