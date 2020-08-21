/*
 * V01 23-MAY-2000 UXN Initial release.
 *
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * This item is the property of GTECH Corporation, Providence, Rhode
 * Island, and contains confidential and trade secret information. It
 * may not be transferred from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH. Neither this item
 * nor the information it contains may be used, transferred,
 * reproduced, published, or disclosed, in whole or in part, and
 * directly or indirectly, except as expressly authorized by an
 * officer of GTECH, pursuant to written agreement.
 *
 * Copyright 2000 GTECH Corporation. All rights reserved.
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
#include <stdio.h>
#include <stdlib.h>

#include "inclib:date.h"

char output_fmt[] = "%04d%02d%02d";

int main(int argc, char *argv[])
{
    FILE *fp;
    struct DATE date;

    if(argc < 2) 
    {
       printf("Usage: %s cdc [output]\n", argv[0]);
       exit(0);
    }
    
    date.cdc = atoi( argv[1] );
    
    LCDATE( &date );

    if(argc == 2) {
	printf(output_fmt, date.year4, date.month, date.day);
	exit(0);
    }  

    if( !(fp = fopen(argv[2], "w")) )
    {
        perror("fopen()");
	exit(1);
    }
    
    fprintf(fp,output_fmt, date.year4, date.month, date.day);
    fclose(fp);
    exit(0); 
}
