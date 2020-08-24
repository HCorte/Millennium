/*
 * V01 19-MAR-2001 UXN Initial release.
 * 
 * Function to provide memory barrier instruction for FORTRAN. 
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

#include builtins

void MB(void)
{
    __MB();
}
