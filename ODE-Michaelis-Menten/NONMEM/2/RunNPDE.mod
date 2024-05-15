$PROBLEM    Dual Numbers benchmark - NONMEM
$ABBREVIATED DERIV2=NO
$INPUT      ID TIME AMT DV WT
$DATA      ..\..\data\sim_0_1_0_0.csv IGNORE=@

;; data file name must match out file name in the template file for simulations in data folder

;;from data\template.txt

;; $TABLE ID TIME AMT IOBS EVID WT FILE=c:\git\dual_numbers\data\OUT_0_1_0_0.DAT NOPRINT NOHEADER NOAPPEND

;; and file read/write in make_data.r

;; file_name <- paste0(this_comp,"_",this_eta,"_",this_vwt,"_",this_gamma)
$SUBROUTINE ADVAN6 TOL=7
$MODEL      COMP=(DEPOT,DEFDOSE) COMP=(CENTRAL,NODOSE,DEFOBS)
$PK      
  CWT = WT/70 

  TVVMAX= THETA(1)   
  VMAX=TVVMAX*EXP(ETA(1)) 
  TVKM = THETA(2)
  KM = TVKM  
  TVV2=THETA(3) 
  V2=TVV2 *EXP(ETA(2))    
  TVKA=THETA(4) 
  KA=TVKA   

  SC = V2
$ERROR 
  REP = IREP 
  IPRED = F    

  Y=F*EXP(EPS(2)) + EPS(1)
$DES
  CONC = A(2)/SC
  DADT(1) = -KA*A(1)
  DADT(2) =  KA*A(1)-VMAX*CONC/(KM+CONC) 


$THETA  (100,1340,3000) ; THETA(1) VMAX UNITS =  mass/time
 (100,1770,3000) ; THETA(2) KM UNITS = mass/volume
 (1,13.3,40) ; THETA(3) V  UNITS = volume
 (0.01,1.22,3) ; THETA(4) KA UNITS = 1/time
;; Start OMEGA
$OMEGA  0.116  ; ETA(1) ETA ON VMAX
 0.134  ; ETA(2) ETA ON V2
$SIGMA  5930  ;             ; EPS(1) ADDITIVE
 0.244  ;          _  ; EPS(2) PROPORTIONAL
;;$ESTIMATION METHOD=COND INTER MAXEVALS=9999 NOHABORT NOOMEGABOUNDTEST
            ;; NOSIGMABOUNDTEST NOTHETABOUNDTEST
;;$COVARIANCE UNCOND PRECOND=2
;;; Model Identifier =  0,1,0,0

;; Phenotype: ([('COMP', 0), ('ETAs', 1), ('V~WT', 0), ('GAMMA', 0)])

;; Genotype: [0, 1, 0, 0]

;; Num non-influential tokens: 0

$SIM ONLYSIM (1233) SUBPROB = 200 
 $TABLE ID TIME DV MDV NOPRINT NOHEADER NOAPPEND FILE=OBS_DAT
 $TABLE ID REP TIME DV IPRED PRED MDV NOPRINT NOHEADER NOAPPEND FILE=SIM_DAT