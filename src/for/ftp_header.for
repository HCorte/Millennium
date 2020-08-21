C FTP_HEADER.FOR
C
C V04 13-JUN-2000 UXN IDATE replaced with GDATE
C V03 06-apr-1999 UXN CDCRND replaced with FIGWEK.
C V02 01-FEB-1999 UXN PeliSuomi changes. Sub game field added.
C V01 12-MAY-1994 HXK Initial revision.
C                                                                               
C SUBROUTINE TO GENERATE HEADER RECORD FOR FTP FILE               
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
      SUBROUTINE FTP_HEADER(PROG)
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:FTP.DEF'

      INTEGER*2 DATBUF(LDATE_LEN)

      CHARACTER*8 TIME_BUF
      BYTE        TIMARY(8)            !HOUR,MINUTE,SECOND
      EQUIVALENCE (TIMARY(1),TIME_BUF)

      INTEGER*4 HOUR,MINUTE,SECOND
      INTEGER*4 YEAR,MONTH,DAY,IND

      COMMON /SREC/ SCFREC                                                      

      INTEGER*4 REV/1/

      INTEGER*4 ST,RECTYPE,TEMP,WEEK,PROG,SUBG

C BUILD HEADER FOR TAPE AND FILE ! The Header record is defined in HSPTRA.DEF
C ------------------------------

      CALL TIME(TIME_BUF)
      CALL ASCBIN (TIMARY,1,2,HOUR,ST)
      CALL ASCBIN (TIMARY,4,2,MINUTE,ST)
      CALL ASCBIN (TIMARY,7,2,SECOND,ST)

      CALL GDATE(MONTH,DAY,YEAR)     
 
C WRITE TRANSFER HEADER RECORD  
C ----------------------------  

      DATBUF(VCDC)=DAYCDC                                                       
      CALL LCDATE(DATBUF)
      RECTYPE=HEADREC                                                           
      IND = 1
      SUBG = 1
      CALL FTPBUF(RECTYPE,2,IND,IBMBUF)     ! RECORD TYPE    1
      TEMP=DATBUF(VYEAR2)                                                   
      CALL FTPBUF(TEMP,2,IND,IBMBUF)        ! DATE           2
      TEMP=DATBUF(VMON)                                                       
      CALL FTPBUF(TEMP,2,IND,IBMBUF)        !                3                 
      TEMP=DATBUF(VDAY)           
      CALL FTPBUF(TEMP,2,IND,IBMBUF)        !                4
      CALL FIGWEK(DAYCDC+1,WEEK,YEAR)                                       
      CALL FTPBUF(YEAR,2,IND,IBMBUF)        ! ROUND          5
      CALL FTPBUF(WEEK,2,IND,IBMBUF)        !                6                  
      CALL FTPBUF(SUBG,2,IND,IBMBUF)        !                7                  
      CALL FTPBUF(DAYCDC,2,IND,IBMBUF)      ! CDC DAY        8                  
      CALL FTPBUF(PROG,2,IND,IBMBUF)        ! PROGRAM NUMBER            9
      CALL FTPBUF(REV,2,IND,IBMBUF)         ! PROGRAM VERSION           10
      CALL FTP_WRTBUF                       ! write buffer to file
      END                                                                       
