C
C SUBROUTINE MESGEN
C
C V15 17-JUN-2014 SCML Added support for Placard IGS 
C V14 11-MAY-2011 FJG  Terminal Overflow in message
C
C $Log:   GXAFIP:[GOLS]MESGEN.FOV  $
C  
C V10 01-JAN-2010 FJG ePASSIVE
C
C     Rev 1.3   01 Feb 1997 17:37:58   RXK
C  Changes for CDU.
C  
C     Rev 1.2   28 Jan 1997 19:24:52   RXK
C  IPS messages added 
C  
C     Rev 1.1   17 May 1996 11:43:10   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.5   18 Feb 1996 20:07:52   HXK
C  Changed FORMAT statement for message 16
C  
C     Rev 1.4   10 Sep 1993 19:36:06   HXK
C  Added Winner reserve fund messages
C  
C     Rev 1.3   12 Jul 1993 19:54:30   GXA
C  Added Ticket Message Revision change message.
C  
C     Rev 1.2   12 Jul 1993 16:51:02   HXK
C  removed CHAR(6) from source code
C  
C     Rev 1.1   12 Jul 1993 16:48:50   HXK
C  added TICKET TEXT (marketing message) commands
C  
C     Rev 1.0   21 Jan 1993 16:59:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mesgen.for **
C
C MESGEN.FOR
C
C V03 26-DEC-91 DAS ADDED MESSAGE FOR X2X FEALARMS
C V02 15-MAR-91 MTK INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO FORMAT GENERAL MESSAGES FOR ERRLOG
C
C MESSAGE #               MESSAGE
C     1                   <TYPE><NAME> TO FILE <X> PENDING SER><XX>
C     2                   PREVIOUS CHECKPOINT NOT COMPLETED
C     3                   <FILNAME> OPEN ERROR> XXXXXXXXXXX
C     4                   <FILNAME> READ ERROR> XXXXXXXXXXX RECORD> XXXX
C     5                   <FILNAME> WRITE ERROR> XXXXXXXXXXX RECORD> XXXX
C     6                   <FILNAME> CHECKPOINT COMPLETE
C     7                   I/O ERROR> XXXXXXXXXXX LUN> XX SER/CDC XXX/XXX
C     8                   EXCHANGE ERROR TKT> <XXXXXX> CDC> <XXXX>
C     9                   VALIDATION REPROCESS ERROR> XXX SER> XXXXX
C    10                   SYNTAX ERROR > XXXX TER> XXXXX GTYP/IND SER>XXXXXXXX
C    11                   TERMINAL XXXX SENT TERMINAL NUMBER XXXX
C    12                   <GAME> WILL CLOSE IN <XX> MINS <XX> SECS
C    13                   <GAME> <ROW> WILL CLOSE IN <XX> MINS <XX>
C    14                   PASS NUMBER VIOLATION TER> XXXX
C    15                   SECURITY VIOLATION TER> XXXX ID#> ZZZZZZZZ
C    16                   <FILENAME> I/O ERROR> XXXXXXXX SERIAL> XXXXXXXX
C    17                   BAD LOAD REQUEST XXXX SEGEMT XXXX
C    18                   UNMESS SENT TO TERMINAL ## (OR ALL)
C    19                   ERROR LOCKING COMMON FOR X2X ADDRESS UPDATE
C    20                   BUFFER ERROR RESETING STATION XXXXXXX
C    21                   LIABILITY LIMIT GAME X NUMBER XXXX POOL XX
C    22                   APPROCHING LIMIT GAME X NUMBER XXXX POOL XX
C    23                   <GAME> <TER> LIABILITY WARNING <ROWS>
C    24                   TEST STRING (X2X)
C    25                   TICKET MESSAGE FOR GAME XX LINE XX SET
C    26                   TICKET MESSAGE FOR GAME XX ENABLED
C    27                   TICKET MESSAGE FOR GAME XX CLEARED
C    28                   TICKET MESSAGE REV FOR GAME XX CHANGHED TO XXXX
C    29                   GAME <XX> OFFLINE PURGE AMOUNT XXXXXXXX
C    30                   GAME <XX> DIV <XX> ADVANCE SHARE MOVED XXXX
C    31                   GAME <XX> MINIMUM POOL SET TO XXXXXXXXX
C    32                   TIMED OUT TRANSACTION RECEIVED FROM IPS XXXXXXXX XXXXX
C    33                   IPS TRANSACTION TIMED OUT  XXXXXX XXX
C    34                   TRANSACTION TIMED OUT BEFORE BEING SENT TO INSTANT
C                         SYSTEM
C    35                   TRANSACTION COULDNT BE SENT TO IPS -> NO BUFS XX
C    36                   BAD LENGTH RECEIVED FROM IPS, DISCONNECTING LEN=XX
C    37                   TCP TRANS REJECTED BECAUSE OF WRITE ERR XRF X PBUF X
C    38                   TERMINAL XXXXX EXCEEDED INSTANT VALIDATION LIMIT
C    39                   BAD ISS ACK FROM IPS
C    40                   BAD ISU UNSLCTD AGTTYP MSG FROM IPS
C    41                   BAD ISU UNSLCTD PASSNUM MSG FROM IPS
C    42                   BAD ISU UNSLCTD GVTID MSG FROM IPS
C    43                   CDU TEXT XX LINE XX SET
C    44                   CDU TEXT XX ENABLED
C    45                   CDU TEXT XX CLEARED
C    46                   CDU TEXT REV CHANGHED TO XXXX
C    47                   SYNTAX ERROR > XXXX TER> XXXXX GTYP/IND MSGID>XXXXXXXXXXXXX
C    99                   DEBUG MESSAGE
C
C
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MESGEN(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'

C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS: Added entry #47
C----+------------------------------------------------------------------
        INTEGER*8 I8TMP
        INTEGER*4 I4TMP(2)
        EQUIVALENCE (I8TMP,I4TMP)
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS: Added entry #47
C----+------------------------------------------------------------------
C
	INTEGER*4 DBUF(*), K, MNUM
	CHARACTER*140	MBUF
	LOGICAL ALARM
C
	GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     *	      21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
     *        38,39,40,41,42,43,44,45,46,47) MNUM
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS: Added entry #47
C----+------------------------------------------------------------------
	GOTO 99
C
C
1	CONTINUE
	WRITE (MBUF,901) (DBUF(K),K=1,6)
901	FORMAT(2A4,1X,2A4,' TO FILE ',I1,' PENDING, SER>',I9)
	RETURN
C
2	CONTINUE
	WRITE (MBUF,902)
902	FORMAT('PREVIOUS CHECKPOINT NOT COMPLETED')
	RETURN
C
3	CONTINUE
	WRITE (MBUF,903) (DBUF(K),K=1,6)
903	FORMAT(5A4,' OPEN ERROR> ',I11)
	RETURN
C
4	CONTINUE
	WRITE (MBUF,904) (DBUF(K),K=1,7)
904	FORMAT(5A4,' READ ERROR> ',I6,' RECORD> ',I10)
	RETURN
C
5	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,905) (DBUF(K),K=1,7)
905	FORMAT(5A4,' WRITE ERROR> ',I6,' RECORD> ',I10)
	RETURN
C
6	CONTINUE
	WRITE (MBUF,906) (DBUF(K),K=1,5)
906	FORMAT(5A4,' CHECKPOINT COMPLETE')
	RETURN
C
7	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,907) (DBUF(K),K=1,4)
907	FORMAT('I/O ERROR> ',I11,' LUN> ',I4,' SER/CDC ',I9,'/',I5)
	RETURN
C
8	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,908) (DBUF(K),K=1,2)
908	FORMAT('EXCHANGE ERROR TKT> ',I9,' CDC> ',I5)
	RETURN
C
9	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,909) (DBUF(K),K=1,2)
909	FORMAT('VALIDATION REPROCESS ERROR> ',I4,' SER> ',I9)
	RETURN
C
10	CONTINUE
	WRITE (MBUF,910) (DBUF(K),K=1,5)
910	FORMAT('SYNTAX ERR>',I4,' TER> ',I5,' GTYP>',I4,' GIND>',I4,
     *         ' SER>',I9)
	RETURN
C
11	CONTINUE
	WRITE (MBUF,911) (DBUF(K),K=1,2)
911	FORMAT('TERMINAL> ',I6,' SENT TERMINAL NUMBER> ',I6)
	RETURN
C
12	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,912) (DBUF(K),K=1,5)
912	FORMAT(2A4,I1,' GAME CLOSE IN ',I2,' MINS ',I2,' SECS')
	RETURN
C
13	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,913) (DBUF(K),K=1,6)
913	FORMAT(2A4,I1,' ROW ',I2,' CLOSE IN ',I2,' MINS ',I2,' SECS')
	RETURN
C
14	CONTINUE
	WRITE (MBUF,914) DBUF(1)
914	FORMAT(' PASS NUMBER VIOLATION TER> ',I5)
	RETURN
C
15	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,915) (DBUF(K),K=1,2)
915	FORMAT(' SECURITY VIOLATION TER> ',I5,' ID#> ',Z8)
	RETURN
C
16	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,916) (DBUF(K),K=1,7)
916	FORMAT(5A4,' I/O ERROR> ',I8,' SER> ',I9)
	RETURN
C
17	CONTINUE
	WRITE (MBUF,917) (DBUF(K),K=1,3)
917	FORMAT(' BAD LOAD REQUEST >',I4,' SEGMENT > ',I8,' TERMINAL >',I4)
	RETURN
C
18	CONTINUE
	IF(DBUF(1).LT.1) THEN
	   WRITE (MBUF,818)
	ELSE
	   WRITE (MBUF,918) (DBUF(K),K=1,2)
	ENDIF
818	FORMAT(' UNSOLICITED NEWS MESSAGE SENT TO ALL TERMINALS ')
918	FORMAT(' UNSOLICITED NEWS MESSAGE SENT TO TERMINAL >',I4,
     *	       ' LINE > ',I4)
	RETURN
C
19	CONTINUE
	WRITE (MBUF,919)
919	FORMAT('ERROR LOCKING COMMON FOR X2X ADDRESS UPDATE')
	RETURN
C
20	CONTINUE
	WRITE (MBUF,920) DBUF(1)
920	FORMAT('BUFFER ERROR RESETING STATION ',I7)
	RETURN
C
21	CONTINUE
	WRITE (MBUF,921) (DBUF(K),K=1,3)
921	FORMAT('NUMBERS ',I1,' NUM ',I4,' POOL ',I2,' LIMIT REACHED')
	RETURN
C
22      CONTINUE
        WRITE (MBUF,922) (DBUF(K),K=1,3)
922     FORMAT('NUMBERS ',I1,' NUM ',I4,' POOL ',I2,
     *         ' APPROACHING LIMIT')
        RETURN
C
23	CONTINUE
	WRITE(MBUF,923) (DBUF(K),K=1,9)
923	FORMAT(2A4,' TER ',I5,' LIABILITY WARNING ' 6(A4,1X))
	RETURN
C
24      CONTINUE
        ALARM=.TRUE.
        WRITE(MBUF,924) (DBUF(K),K=1,EDLEN)
924     FORMAT(40A4)
        RETURN
C
25      CONTINUE
        WRITE(MBUF,925) DBUF(5),DBUF(6),DBUF(7)
925     FORMAT('TICKET MESSAGE FOR GAME ',I2,' LINE ',I2,' SET ',A4)
        RETURN
C
26      CONTINUE
        WRITE(MBUF,926) DBUF(5)
926     FORMAT('TICKET MESSAGE FOR GAME ',I2,' ENABLED')
        RETURN
C
27      CONTINUE
        WRITE(MBUF,927) DBUF(5)
927     FORMAT('TICKET MESSAGE FOR GAME ',I2,' CLEARED')
        RETURN
C
28	CONTINUE
	WRITE(MBUF,928) DBUF(5),DBUF(6)
928	FORMAT(1X,'TICKET MESSAGE REVISION FOR GAME ',I2,' CHANGED TO ',Z8)
	RETURN
C
29      CONTINUE
        WRITE(MBUF,929) DBUF(5),CMONY(DBUF(6),10,BETUNIT)
929     FORMAT('GAME ',I2,' OFFLINE PURGE AMOUNT ',A10)
        RETURN
C
30      CONTINUE
        WRITE(MBUF,930) DBUF(5),DBUF(6),CMONY(DBUF(7),10,BETUNIT)
930     FORMAT('GAME ',I2,' DIV ',I2,' ADVANCE SHARE MOVED ',A10)
        RETURN
C
31      CONTINUE
        WRITE(MBUF,931) DBUF(5),CMONY(DBUF(6),10,BETUNIT)
931     FORMAT('GAME ',I2,' MINIMUM POOL SET TO ',A10)
        RETURN
C
32      CONTINUE
        WRITE (MBUF,932) (DBUF(K),K=1,2)
932     FORMAT('Timed Out Transaction Received From The IPS '
     *         ,I9,I5)
        RETURN
C
33      CONTINUE
        WRITE (MBUF,933) (DBUF(K),K=1,2)
933     FORMAT('IPS Transation Timed Out ',I9,I5)
        RETURN
C
34      CONTINUE
        WRITE (MBUF,934) (DBUF(K),K=1,2)
934     FORMAT('Trans Timed Out before being sent to IPS '
     *         ,I9,I5)
        RETURN
C
35      CONTINUE
        WRITE (MBUF,935) (DBUF(K),K=1,2)
935     FORMAT('Trans couldnt be sent to the IPS no bufs '
     *         ,I9,I5)
        RETURN
C
36      CONTINUE
        WRITE (MBUF,936) (DBUF(K),K=1,1)
936     FORMAT('BAD length received from the IPS'
     *         ' disconnecting len=',I10)
        RETURN
C
37      CONTINUE
        WRITE (MBUF,937) (DBUF(K),K=1,2)
937     FORMAT('TCP Trans rejected because of Write Err  XRFNUM ',I8,
     *         '  PBUF ',I4)
        RETURN
C
38      CONTINUE
        ALARM=.TRUE.
        WRITE (MBUF,938) DBUF(1)
938     FORMAT(' TERMINAL ',I5,' EXCEEDED INSTANT VALIDATION LIMIT ')
        RETURN
C
C BAD INTRASYSTEM ACK
C                    
39      CONTINUE     
        ALARM=.TRUE.                                              
        WRITE (MBUF,939) DBUF(1)                                
939     FORMAT('BAD RETAILER UPDATE ACK FROM IPS, SER>',I9)     
        RETURN                                                  
C                                                               
C BAD INTRASYSTEM AGTTYP CHANGE ATTEMPT FROM IPS                
C                                                               
40      CONTINUE                                                
        ALARM=.TRUE.                                            
        WRITE (MBUF,940) DBUF(1)                                
940     FORMAT('BAD AGTTYP CHANGE MSG FROM IPS, SER>',I9)   
        RETURN                                              
C                                                           
C BAD INTRASYSTEM PASNUM CHANGE ATTEMPT FROM IPS            
C                                                           
41      CONTINUE                                            
        ALARM=.TRUE.                                        
        WRITE (MBUF,941) DBUF(1)                            
941     FORMAT('BAD PASNUM CHANGE MSG FROM IPS, SER>',I9)   
        RETURN                                              
C                                                           
C BAD INTRASYSTEM GVTID CHANGE ATTEMPT FROM IPS             
C
42      CONTINUE
        ALARM=.TRUE.
        WRITE (MBUF,942) DBUF(1)
942     FORMAT('BAD GVTID CHANGE MSG FROM IPS, SER>',I9)
        RETURN
C
43      CONTINUE
        WRITE(MBUF,943) DBUF(5),DBUF(6),DBUF(7)
943     FORMAT('CDU TEXT ',I2,' LINE ',I2,' SET ',A4)
        RETURN
C
44      CONTINUE
        WRITE(MBUF,944) DBUF(5)
944     FORMAT('CDU TEXT ',I2,' ENABLED')
        RETURN
C
45      CONTINUE
        WRITE(MBUF,945) DBUF(5)
945     FORMAT('CDU TEXT ',I2,' CLEARED')
        RETURN
C
46      CONTINUE
        WRITE(MBUF,946) DBUF(6)
946     FORMAT('CDU TEXT REV CHANGED TO ',Z8)
        RETURN
C
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS: Added entry #47
C----+------------------------------------------------------------------
C
47      CONTINUE
        I4TMP(2) = DBUF(5)
        I4TMP(1) = DBUF(6)
        WRITE (MBUF,947) (DBUF(K),K=1,4),I8TMP
947     FORMAT('SYNTAX ERR>',I0,' TER> ',I0,' GTYP>',I0,' GIND>',I0,
     *         ' MESID>',I0)
        RETURN
C
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS: Added entry #47
C----+------------------------------------------------------------------
C INVALID MESSAGE NUMBER
C
99	CONTINUE
        IF(MNUM.NE.99) THEN
	  ALARM=.TRUE.
	  WRITE (MBUF,999) MNUM
999	  FORMAT('INVALID GENERAL MESSAGE NUMBER> ',I4,'    ')
        ELSE
          WRITE(MBUF,998) DBUF(1),DBUF(2),DBUF(2)
998	  FORMAT('DEBUG: ',I4,' - ',I10,'[',Z8,']')
        ENDIF
	RETURN
	END
