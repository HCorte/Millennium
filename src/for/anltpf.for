C ANLTPF.FOR
C
C V01 01-JAN-2010 FJG ePassive
C
C Program to analyze the TPF files
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /check=nooverflow
	program anltpf
	implicit none
!
        include 'inclib:sysextrn.def'
        include 'inclib:sysdefine.def'
!
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def' 	
!
        integer*4    runi
        parameter    (runi = 8)
!        
        integer*4    page      
        integer*4    erro
        integer*4    gind
        integer*4    gtyp
        integer*4    game
        integer*4    emis
        integer*4    mnum
        integer*4    mser
        integer*4    mfra
        integer*4    vtmp
        integer*4    stab(numpas,pagemi,pbilmin:pbilmax+2)
        integer*4    terr
        character*11 ckey        
        character*8  cgam
        character*10 repn/'anltpf.rep'/
!
        integer*4    nrec
        integer*4    nbuk
        integer*4    obuk        
!
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec        
        record /stpasrec/ pasbuk(pasbukrec) !32 of stpasrec            
C===============================================================================
C       Start process 
C===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<<<<<<<< ANLTPF - Passive Tickets Analysis >>>>>>>>>>>>'
        type*,iam()     
!
        if (daysts.eq.dsopen) then
          type*,iam()
          type*,iam(),'************************* WARNING *************************'
          type*,iam(),'*                                                         *'          
          type*,iam(),'* THIS PROGRAM SHOULD NOT BE RUN DURING ONLINE PROCESSING *'
          type*,iam(),'*     IF YOU CONTINUE YOU WILL DO IT BY YOUR OWN RISK     *'          
          type*,iam(),'*                                                         *'                    
          type*,iam(),'***********************************************************'          
          type*,iam()
          call gpause        
        endif
C===============================================================================
C       Init report 
C===============================================================================
 	call ropen(repn,runi,erro)
	if(erro.ne.0) then
	  type*,iam(),' cannot open ',repn,' status ',erro
	  call gstop(gexit_fatal)
	endif
!
        call fastset(0,stab,(numpas*pagemi*(pbilmax-pbilmin+3)))
	page = 0
        gtyp = tpas
C===============================================================================
C       Main Loop 
C===============================================================================        
        do gind=1,numpas
          game = gtntab(tpas,gind)
          do emis=1,pagemi
            if(passubsts(emis,gind).ne.pdrwnot.and.
     *         passubsts(emis,gind).ne.pdrwclo.and.
     *         passubsts(emis,gind).ne.pdrwerr) then
              call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                        pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_openro(pasfdb)  
              if(pasfdb.err.ne.ioe_noerr) then
                type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam
                call pasio_dump(pasfdb)
                cycle              
              else
                type*,iam(),'Scanning file: ',pasfdb.filnam                 
                terr = 0
                obuk = 0
                do mnum = 0,pasnumtck(emis,gind)-1
                  do mser = 1,pasnumser(emis,gind)
                    do mfra = 1,pasnoffra(emis,gind)
!===============================================================================
!                     This is copied from PASIOSUBS to avoid the sucesive reads
!                     Please keep in mind that should be the same               
!===============================================================================       
                      nrec = (mnum+1-1)*pasfdb.maxser*pasfdb.maxfra !step 1: pasnum-1+1 because starts at zero
                      nrec = nrec + ((mser-1)*pasfdb.maxfra)        !step 2: the previous sers * # of fras
                      nrec = nrec + mfra                            !step 3: add the searched fraction
                      nbuk = ((nrec - 1)/pasbukrec)+1               !step 4: calculate bucket record
                      nrec = nrec-((nbuk-1)*pasbukrec)              !step 5: offset within the bucket                      
!===============================================================================                      
                      if(nbuk.ne.obuk) then
                        call readw(pasfdb.fdb,nbuk,pasbuk,pasfdb.err)
                        obuk = nbuk
                      else
                        pasfdb.err = ioe_noerr 
                      endif
                      pasrec = pasbuk(nrec)
                      if(pasfdb.err.eq.ioe_noerr.and.pasrec.key.ne.pasio_key(mnum,mser,mfra)) pasfdb.err = ioe_keyerr
!                      
                      if(pasfdb.err.ne.ioe_noerr) then
                        if(stab(gind,emis,pbilmax+2).lt.100) stab(gind,emis,pbilmax+2) = stab(gind,emis,pbilmax+2) + 1
                        write(ckey,990) mnum,mser,mfra                        
                        type*,iam(),'Error: ',pasfdb.err,' reading record: ',ckey
                        terr = terr + 1
                        if(terr.ge.20) then
                          terr = 0
                          call gpause
                          type*,iam(),'Skipping file: ',pasfdb.filnam
                          goto 10
                        endif
                      else
                        stab(gind,emis,pasrec.stat) = stab(gind,emis,pasrec.stat) + 1
                        stab(gind,emis,pbilmax+1) = stab(gind,emis,pbilmax+1) + 1
                      endif
                    enddo
                  enddo
                enddo
10              continue                
                call pasio_close(pasfdb)   
              endif
            endif
          enddo
        enddo
!===============================================================================
!       PRINT TOTALS        
!===============================================================================
        do gind=1,numpas
          game = gtntab(tpas,gind)
          write(cgam,'(A3,1X,A4)') 'TPF',gsnames(game)	
          call title('ANALYSIS OF TPFs',cgam,1,runi,page,daycdc)
          write(runi,900) 
          write(runi,901)           
          do emis=1,pagemi
            if(passubsts(emis,gind).ne.pdrwnot.and.
     *         passubsts(emis,gind).ne.pdrwclo.and.
     *         passubsts(emis,gind).ne.pdrwerr) then
              write(runi,910) gsnames(game),pasemis(emis,gind),pasdraw(emis,gind),(stab(gind,emis,vtmp),vtmp=pbilmin,pbilmax+2)
            endif
          enddo
          write(runi,902)
        enddo
        close(runi)
        call gstop(gexit_success)
!===============================================================================
!       Formats
!===============================================================================
900     format(/,1X,'TIPO DRAW EMISIO RETAWIN RETANOW RETDWIN SOFFWIN RETDNOW SOFFNOW AVAILOF NOT+DEF ',
     *            'AVAILON SONLNOW CANCNOW SONLWIN CANCWIN =TOTAL= ER')
901     format(1X,'==== ==== ======',<pbilmax-pbilmin+2>(' ======='),1X,'==')
902     format(/)
910     format(1X,A4,X,I4,X,I6.6,<pbilmax-pbilmin+2>(1X,I7),1X,I2)     
990     format(I5.5,'s',I2.2,'f',I2.2)
        end
