$!
$! Check for backup pending
$!
$  @dss$gtech:dss_back_chk
$!
$ if "''p2'" .eqs. ""
$ then
$   write sys$output "No file specified"
$   exit
$ endif
$ if "''p3'" .eqs. ""
$ then
$  q = ""
$  f = p2
$ else
$  q = p2
$  f = p3
$ endif
$ open/read/share=read/error=phase_error phase_fil gxcom:setphase.nam
$ read/error=phase_error phase_fil phase
$ close/error=phase_error phase_fil
$ proj_phase=f$integer(phase)
$ if proj_phase .le. 6
$ then
$  bnds = "CHECK=BOUNDS"
$ else
$  bnds = "CHECK=NOBOUNDS"
$ endif
$ if ftnversion .ge. 60
$ then
$  wrng="/WARNINGS=(NOUNUSED,NOUNCALLED,NOUSAGE)"
$!  wrng="/WARNINGS=(ALL, NOUNREACH)"
$ else
$  wrng=""
$ endif
$ algn=""
$ vax=f$extract(0,3,platform).eqs."VAX"
$! if .not.vax then wrng="/WARN=(ALL,NOUNREACH)"
$ if .not.vax then wrng="/WARN=(NOALIGN)"
$!
$ dv=f$parse("''f'",,,"device")
$ dr=f$parse("''f'",,,"directory")
$ nm=f$parse("''f'",,,"name")
$ tp=f$parse("''f'",,,"type")
$ vr=f$parse("''f'",,,"version")
$ if tp .eqs. "." then tp = ".FOR"
$ if f$search("''dv'''dr'''nm'''tp'''vr'") .eqs. ""
$ then
$   d="devlib:"
$   fil=d+nm+tp+vr
$ else
$   fil=f
$ endif
$ if .not.vax
$ then
$   algn="/ALIGN=(COMMON=(PACK,MULTILANGUAGE),RECORD=PACK)"
$ endif
$ plat=algn+wrng
$ if "''p4'" .EQS. "DEBUG"
$ then
$       FORTRAN/'bnds'/DEBUG/NOOPT/NODIAG/EXT/LIST'plat''q' 'fil'
$ else
$       FORTRAN/'bnds'/NOOPT/NODIAG/EXT/LIST'plat''q' 'fil'
$ endif
