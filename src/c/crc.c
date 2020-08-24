/* ===[RMSIO.C]===================================================


   V01 13-dec-1999 uxn initial release for EuroGOLS

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

static unsigned long crc_table[256];
static int crc_table_computed = 0;

static void make_crc_table( void )
{
    unsigned long c;
    int n,k;

    for(n=0; n < 256; n++)
    {
        c = (unsigned long) n;
	for(k=0; k < 8; k++)
        {
           if( c & 1 )
           {
               c = 0xedb88320L ^ (c >> 1);
           }
           else
           {
               c = c >> 1;
           }
        }
        crc_table[n] = c;
    }
    crc_table_computed = 1;
}

unsigned long update_crc(unsigned long crc, unsigned char *buf, int len)
{
    unsigned long c = crc ^ 0xffffffffL;
    int n;
    
    if(!crc_table_computed)
	make_crc_table();

    for(n = 0; n < len; n++)
    {
	c = crc_table[ (c^buf[n]) & 0xff ] ^ (c >> 8);
    }
    return c ^ 0xffffffffL;
}

/* Return the CRC of the bytes buf[0..len-1]. */

unsigned long crc(unsigned char *buf, int len)
{
    return update_crc(0L, buf, len);
}
