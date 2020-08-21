C  GXSRC:BGBET.FOR
C  
C  $Log:   GXAFXT:[GOLS]BGBET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   16 Dec 1994 18:12:20   HXK
C  Added Seed
C  
C     Rev 1.3   23 Nov 1994 16:09:26   HXK
C  Added Lucky stuff
C  
C     Rev 1.2   18 Nov 1994 10:45:22   HXK
C  rearrange rows, columns for Bingo Full House
C  
C     Rev 1.1   07 Nov 1994 16:38:42   PXB
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    07 Nov 1994 16:37:14   PXB
C  Initial revision.
C  
C
C
C Format BINGO bet data.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW

        SUBROUTINE BGBET (TRABUF,BIMAGE)

        IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

C---- Argument.

        CHARACTER*56 BIMAGE(12)

C---- Local Variables used.

        INTEGER*4  K 
        INTEGER*4  I 
        INTEGER*4  Q
        INTEGER*4  Q1
        INTEGER*4  ST 
        INTEGER*4  BRD
        INTEGER*4  COL
        INTEGER*4  ROW
        INTEGER*4  OFFSET

        INTEGER*4  BOARDFH(3,BGOROW,BGOCOL)


C------------------------- Start of Program --------------------------------

C---- Get Fullhouse Board.

        CALL GETB_BTMBRD(TRABUF,BOARDFH)
        
C---- Put boards into display format.

        Q = 1
        DO ROW = 1,BGOROW
           WRITE (BIMAGE(Q),9000) (BOARDFH(1,ROW,COL),COL=1,BGOCOL),
     *                            (BOARDFH(2,ROW,COL),COL=1,BGOCOL),
     *                            (BOARDFH(3,ROW,COL),COL=1,BGOCOL)
           Q = Q +1
        END DO

C---- Write out blank line.

        WRITE (BIMAGE(Q),9001) 

        Q = Q +1

C---- Write out seed and lucky number

        WRITE (BIMAGE(Q),9002) TRABUF(TWBSED)
        Q = Q +1
        WRITE (BIMAGE(Q),9003) TRABUF(TWBLUK)/1000,
     *                         MOD(TRABUF(TWBLUK),1000)
        Q = Q +1
        WRITE (BIMAGE(Q),9004) TRABUF(TWBBAS)

        RETURN

C------------------------- Format Statements -------------------------------

9000    FORMAT (3(3X,5(1X,I2)))

9001    FORMAT (56X)

9002    FORMAT (4X,'Seed ',I12.12)

9003    FORMAT (4X,'Lucky # ',I4.4,'-',I3.3)

9004    FORMAT (4X,'Lucky base ',I3.3)

        END

C------------------------- End of Program   --------------------------------
