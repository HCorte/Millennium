C
C LISTCKFIL.FOR
C
C V03 11-NOV-2010 FJG Rename from LSTTCKFIL
C     14-JAN-2011 FJG PASNUMSER correction
C V02 01-JAN-2010 FJG ePassive
C V01 06-MAR-2001 ANG INITIAL RELEASE FOR PORTUGAL
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Completely rewrite
C        
        OPTIONS /CHECK=NOOVERFLOW
        PROGRAM LISTCKFIL
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        include 'inclib:pasiosubs.def' 	        
!       
        integer*4          gind
        integer*4          gnum
        integer*4          erro       
        integer*4          week
        integer*4          year 
        integer*4          emis
        integer*4          inum
        integer*4          fnum
        integer*4          mnum
        integer*4          iser
        integer*4          fser
        integer*4          mser
        integer*4          ifra
        integer*4          ffra
        integer*4          mfra
        integer*4          sbil
        integer*4          temp
        integer*4          rlun
        integer*4          page
        integer*4          line
        integer*4          lsub
        character*11       ckey
        character*1        ctmp
        character*44       alin(3)
        character*132      tlin
        equivalence        (tlin,alin)
!       
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec  
!       
        integer*4    stab(pbilmin:pbilmax+2)
        character*12 ctab(pbilmin:pbilmax+2)/'RetAftWinner',
     *                                       'RetAfterDraw',
     *                                       'RetWinner   ',
     *                                       'SoldOffWin  ',
     *                                       'ReturnedOff ',
     *                                       'SoldOffline ',
     *                                       'AvailableOff',
     *                                       'NOT DEFINED ',
     *                                       'AvailableOnl',     
     *                                       'SoldOnline  ',
     *                                       'CancelledOnl',     
     *                                       'SoldOnlWin  ',
     *                                       'CanOnlineWin',        
     *                                       'Totals      ',
     *                                       'Errors      '/          
!===============================================================================
!       Start process 
!===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<<<<<<<< LSTTCKFIL - xTicket audit report >>>>>>>>>>>>'
!       
10      continue
        type*,iam()     
        call inpnum('Enter game index or (E)xit program: ',gind,1,numpas,erro)      
        if(erro.lt.0) call gstop(GEXIT_OPABORT)  
        gnum = gtntab(TPAS,gind)
!       
        call inpnum('Enter week or (E)xit: ',week,1,53,erro)
        if(erro.lt.0) goto 10
!       
        call inpnum('Enter year or (E)xit: ',year,2009,2099,erro)
        if(erro.lt.0) goto 10
        year = mod(year,10)
!                
        emis = pasextdrw(week,year,gind)
        if(emis.le.0) then
          type*,iam(),'There is no draw in memory for that week and year. Please retry entry'
          goto 10
        endif
!        
        call inpnum('Enter initial number to search, (A)ll or (E)xit: ',inum,0,pasnumtck(emis,gind)-1,erro)
        if(erro.eq.0) then
          call inpnum('Enter final number to search or (E)xit: ',fnum,inum,pasnumtck(emis,gind)-1,erro)
          if(erro.lt.0) goto 10
        elseif (erro.eq.-4) then
          inum = 0
          fnum = pasnumtck(emis,gind)-1
        else
          goto 10
        endif
!        
        call inpnum('Enter initial serie to search, (A)ll or (E)xit: ',iser,1,pasnumser(emis,gind),erro)
        if(erro.eq.0) then
          call inpnum('Enter final serie to search or (E)xit: ',fser,iser,pasnumser(emis,gind),erro)
          if(erro.lt.0) goto 10
        elseif (erro.eq.-4) then
          iser = 1
          fser = pasnumser(emis,gind)
        else
          goto 10
        endif   
        ifra = 1
        ffra = pasnoffra(emis,gind)            
!       
        type*,iam()
        type*,iam(),'------------ STATUS -------------'
        do temp = pbilmin,pbilmax
          type*,iam(),ctab(temp),' ........',temp
        enddo
        type*,iam(),'Todos ...............           A'
        type*,iam()
        call inpnum('Enter ticket status or (A)ll',sbil,pbilmin,pbilmax,erro)
        if (erro.eq.-4) then
          sbil = 99
        else
          if(erro.lt.0) goto 10        
        endif
!===============================================================================
!       OPENING REPORT
!==============================================================================
        call find_available_lun(rlun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error getting logical unit'
          goto 10
        endif
        call ropen('listckfil.rep',rlun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error opening report file'
          goto 10
        endif
        page = 0
        line = 0
        lsub = 1
        call title('PASSIVE TICKETS REPORT '// cpastpffil(emis,gind),'LSTCKFIL',1,rlun,page,daycdc)
        write(rlun,900)        
!===============================================================================
!      READING TPF FILE
!===============================================================================
        call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                  pasnoffra(emis,gind),cpastpffil(emis,gind))     
        call pasio_openro(pasfdb)  
        if(pasfdb.err.ne.ioe_noerr) then
          type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam  
          call pasio_dump(pasfdb)
          goto 10           
        endif
!       
        type*,iam()
        type*,iam(),'Scanning file: ',pasfdb.filnam   
        do mnum = inum,fnum
          do mser = iser,fser
            do mfra = ifra,ffra
              call pasio_read(pasfdb,mnum,mser,mfra,pasrec)
              if(pasfdb.err.ne.ioe_noerr) then
                stab(pbilmax+2) = stab(pbilmax+2) + 1
                write(ckey,990) mnum,mser,mfra
                type*,iam(),'Error: ',pasfdb.err,' reading record: ',ckey
                pasioerrs(ioreano,emis,gind) = pasioerrs(ioreano,emis,gind) + 1
              else
                pasioerrs(ioreaok,emis,gind) = pasioerrs(ioreaok,emis,gind) + 1
                if(sbil.eq.pasrec.stat.or.sbil.gt.pbilmax) then
                  if(line.gt.52) then
                    call title('PASSIVE TICKETS REPORT '// cpastpffil(emis,gind),'LSTCKFIL',1,rlun,page,daycdc)   
                    write(rlun,900)                 
                    line = 0
                  endif
                  if(lsub.eq.3) then
                    ctmp = ' '
                  else
                    ctmp = '|'                    
                  endif
                  write(alin(lsub),910) mnum,mser,mfra,99999-pasrec.control,
     *                                  pasrec.stat,pasrec.agt,pasrec.cdc,pasrec.serial,ctmp
                  stab(pasrec.stat) = stab(pasrec.stat) + 1
                  stab(pbilmax+1) = stab(pbilmax+1) + 1
                  lsub = lsub + 1
                  if(lsub.eq.4) then
                    write(rlun,920) tlin
                    write(tlin,940)
                    line = line + 1
                    lsub = 1
                  endif
                endif
              endif
            enddo
          enddo
        enddo
!
        type*,iam()
        write(rlun,920) tlin
        call title('PASSIVE TICKETS REPORT '// cpastpffil(emis,gind),'LSTCKFIL',1,rlun,page,daycdc) 
        do temp = pbilmin,pbilmax+2      
          type*,iam(),ctab(temp),stab(temp)
          write(rlun,930) ctab(temp),stab(temp)
        enddo
        type*,iam()
!                
        call pasio_close(pasfdb)   
        close(rlun)
        call gstop(gexit_success)
!===============================================================================
!       Formats
!===============================================================================
900     format(/,1X,3('  NUMER SR FR CONTR ST AGTER SCDC SERIALN   '),
     *         /,1X,3('  ===== == == ===== == ===== ==== =======   '))
910     format(2X,I5.5,1X,I2.2,1X,I2.2,1X,I5.5,1X,I2,1X,I5,1X,I4,I8,2X,A1)
920     format(1X,A132)
930     format(45X,A12,1X,I10)
940     format(132X)
990     format(I5.5,'s',I2.2,'f',I2.2)
        end
