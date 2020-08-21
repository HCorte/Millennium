/* ===[ENC_KEYDEF.H]===================================================

   Description: Encryption key definitions

   Revisions:

      REV     DATE      BY      DESCRIPTION
      ----  --------   ----     ---------------------------------------
      1.00  01-may-94  sbc      initial release.
      1.01  27-oct-94  sbc      input and output keys to allow for
                                terminal password changes
      1.02  04-mar-95  sbc      added teller/terminal functionality
      1.03  06-mar-95  sbc      output key moved to context area
      1.04  10-may-95  sbc      Changed keys to unsigned char,
                                Soft key set to 128 for Des
      1.05  30-oct-95  sbc      Removed nonimplemented fixed key
      4.00  28-feb-96  sbc      Added tlr_encryption_tlridn_key
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

#ifndef ENC_KEYDEF_H
#define ENC_KEYDEF_H

#define ENC_KEYCOM_VERSION 4
                                        /* key for soft and hard encryption */
struct ENC_KEYTAB {
    unsigned char key[8];
};
                                        /* expanded keys for soft encryption */
                                        /* soft_keytab follows keytab */
struct ENC_SOFT_KEYTAB {
    unsigned char key[128];
};

struct ENC_KEYCOM {
    int version;
    int nbr_keys;
    int soft_encryption;
    int tlr_encryption;
    int tlr_encryption_tlridn_key;
    /* ENC_KEYTAB entries follow here */
};
#endif /* ENC_KEYDEF_H */
