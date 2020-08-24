C GET_MAX_TERMS
C
C V02 06-JAN-2011 FJG MILLENNIUM MXSRV 
C
C=============================================================================
C
C  This item is the property of GTECH Corporation, West Greewich, Rhode
C  Island, and contains confidential and trade secret information. It may
C  not be transferred from the custody or control of GTECH except as
C  authorized in writing by an officer of GTECH. Neither this item not the
C  information it contains may be used, transferred, reproduced, published
C  or disclosed, in whole or in part, and directly or indirectly, except
C  as expressly authorized by an officer of GTECH, pursuant to written
C  agreement.
C
C  Any and all modifications to this item must have the prior written         
C  authorization of GTECH's Enterprise Series Platform Team.  GTECH shall     
C  not be liable in any way for any direct or indirect damages,  whatsoever,  
C  as a result of any unauthorized modifications.  The Enterprise Series      
C  Platform Team reserves the right to refuse support as a result of          
C  unauthorized modification.
C
C  Copyright 2003 GTECH Corporation. All rights reserved.
C
C=====[GET_MAX_TERMS.FOR]=====================================================
C
C  FUNCTION NAME AND CALLING PARAMETERS GO HERE
C
C  Purpose:
C
C  Input Arguments:
C
C  Output Arguments:
C
C  Return Value:
C
C  Assumptions:
C
C=====[GET_MAX_TERMS.FOR]=====================================================
C
        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_MAX_TERMS (MAX_TERMINAL_NO)
        IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       INCLUDE FILES.
C
        INCLUDE 'INCLIB:X2XPRM.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       LOCAL DECLARATIONS.
C
        INTEGER*4       MAX_TERMINAL_NO
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        MAX_TERMINAL_NO = X2X_TERMS
C
        RETURN
        END
