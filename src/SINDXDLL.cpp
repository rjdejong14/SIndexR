#include <Rcpp.h>
#include "../src/sindex.h"
using namespace Rcpp;

/*
* sindxdll.c
*
* Interface Module to the Sindex Library
*
* 1998 feb 28 - Initial Implementation (Shawn Brant)
*      apr 6  - Reworking. (Ken Polsson)
*          7  - Added lots more functions.
*      nov 12 - Added Nigh & Courtin's Dr.
*      dec 2  - Bug fix in Sindex_SpecRemap regarding fiz_type.
* 1999 feb 15 - Bug fix: changed si_gi_default array from char to short int.
*          22 - Changed all functions that return doubles to return
*               short ints, returning the double as a parameter.
*      mar 11 - Added Sindex_SpecUse().
*      apr 8  - v1.01
*             - Added missing text for many curves.
*             - Changed default Pli curve from Goudie to Thrower/Nigh.
*          14 - v1.02
*             - Removed duplicate curve text accidentally added in v1.01.
*             - Added Chen's Bl, Pl, Dr, At.
*          27 - v1.03
*             - Changed default Dr from Harrington to Nigh.
*          29 - v1.04
*             - Changed short name of a Pli curve from
*               "Thrower (1994) + Nigh (1998)" to "Nigh (1999)".
*             - Changed short name of a Pli curve from "Nigh & Love (1998)"
*               to "Nigh & Love (1999)".
*             - Changed "&" to "and" in text strings.
*      may 31 - v1.05
*             - Species remapping was sending all AC codes to ACT.
*               It now splits them into ACT and ACB.
*      jun  9 - Temporarily added Nigh's new Cwi.
*          10 - Eliminated need for SI_CURVE_START/END.
*      aug  4 - Added "age > 0.5" constraint to Nigh's Dr.
*          20 - v1.06
*             - Changed computing age from height and site index to always
*               iterate to breast height age, then convert to total age
*               if needed.
*          24 - v1.07
*             - Added additional error checks within the iterate loop of
*               computing age from site index and height.  Also added check
*               for the case where only error values are available (such
*               as high site and low age on some curves) to prevent infinite
*               looping.
*             - Refined the areas of operability (eliminating high site,
                                                  *               low age) for the following species/curves:
                                                  *               Hw Wiley, Cw Kurucz, Ba Kurucz, Dr Harrington.
                                                  *      sep 22 - v1.08
                                                  *             - Altered Pli Nigh 1999 to NOT correct age by 0.5 years.
                                                  *          23 - v1.09
                                                  *             - Added Sindex_AgeToAge().
                                                  *          24 - If iterating to get age, and an error results, don't
                                                  *               try converting age type.
                                                  *          24 - v1.10
                                                  *             - Added Curtis' Bp Noble Fir.
                                                  *          30 - When computing height from site and age,
                                                  *               if age type is total, and age is 0, return error for
                                                  *               certain curves.
                                                  *      oct  1 - Certain curves "go nuts" at low age and high site.
                                                  *               In these cases it used to return an error code.
                                                  *               Now an interpolated value between two good points
                                                  *               is returned (computing height from site and age).
                                                  *          18 - v1.11
                                                  *             - Added Nigh's Hwc GI, SS GI, Sw GI, Lw GI.
                                                  *             - Added Sb <-> Pli conversion.
                                                  *             - Added Nigh/Love's Sw total age curve.
                                                  * 2000 jan 24 - Added new function Sindex_DefCurveEst() to return
                                                  *               default curve for species and establishment type.
                                                  *             - Added citation and notes for the Oct 18 additions.
                                                  *          27 - v1.12
                                                  *             - Added Nigh's Cw GI.
                                                  *             - Changed default GI for Hwc, Ss, Sw.
                                                  *             - Removed Nigh's old Sw, Hwc, Ss from NextCurve() list.
                                                  *      mar 15 - v1.13
                                                  *             - Bug fix in Nigh's 1999 Pli. There was a 0.5 year problem
                                                  *               under some circumstances.
                                                  *      apr 25 - v1.14
                                                  *             - Another bug fix to help Pli Nigh 1999. This time a
                                                  *               call to age_to_age() had to be added when iterating to
                                                  *               solve for age or site, when breast height age is specified.
                                                  *      jul 25 - v1.15
                                                  *             - Split Cw into Cwi and Cwc.
                                                  *             - Added spliced Goudie/Nigh Sw for testing.
                                                  *      aug 1  - Temporarily put Cwi curves back into Cw and
                                                  *             - disabled Goudie/Nigh Sw.
                                                  *      oct 10 - Implemented Cwc/Cwi.
                                                  *             - Sw default curve is Goudie/Nigh spliced curve.
                                                  *      dec 12 - Changed Bac-Ker to Bb-Ker, and made it inactive.
                                                  *             - Updated notes for Bb-Ker, Sb-Ker, Sw-Ker, Fdc-King, Hwc-Farr,
                                                  *               Bl-Chen, Pl-Chen, Sw-Nigh/Goudie.
                                                  *             - Removed Dr-Chen.
                                                  * 2001 jan 4  - v1.16
                                                  *             - If "CW" was passed to the remap function, it always
                                                  *               returned index for "ACT", due to a missing "break;".
                                                  *             - If "C" was passed, it returned "CWC", due to missing
                                                  *               fix check.
                                                  *          17 - v1.17
                                                  *             - Added Se Engelmann spruce, with Chen & Klinka curve.
                                                  *      mar 14 - v1.18
                                                  *             - If "S" was passed to remap, it would always return "SB"
                                                  *               due to a missing "break;".
                                                  *      apr 9  - v1.19
                                                  *             - Added Fdc Nigh total age curve, and spliced with Bruce.
                                                  *          11 - Removed note about y2bh function from Bac Kurucz.
                                                  *      may 3  - Added Lw curve by Brisco, Klinka, Nigh.
                                                  *      jun 12 - Made Chen/Klinka the default for Bl.
                                                  *      aug 27 - v1.20 Changed default Sw plantation to GOUDNIGH.
                                                  *             - Removed some text from Ba/Bl curve notes.
                                                  * 2002 jan 29 - v1.21 Added Sindex_CurveToSpecies().
                                                  *             - Added Sindex_VDYP_SpecRemap().
                                                  *      feb 5  - Tiny change to notes for LW_NIGH.
                                                  *          12 - Added Nigh Sb, making it default.
                                                  *      mar 27 - Removed Sindex_VDYP_SpecRemap().
                                                  *      jun 27 - v1.30 Y2BH now is forced to follow sequence
                                                  *               0.5, 1.5, 2.5, 3.5...
                                                  *      oct 8  - Change to many coeffs of site index conversions.
                                                  *             - Added notes for Fdc Bruce/Nigh.
                                                  *          9  - Added At Nigh as default.
                                                  *          31 - Changed si_curve_intend[] for SI_BL_KURUCZ82 to say it is
                                                  *               intended for Bl, not Ba.
                                                  *      nov 29 - Changed BAC to BA, PP to PY.
                                                  *             - Added many more species.
                                                  *             - Added Sindex_SpecMap().
                                                  * 2003 jan 8  - Changed Fdc default back to Bruce.
                                                  *      jun 13 - v1.31 Copied several curves and "corrected" the origin from
                                                  *               bhage=0 ht=1.3 to bhage=0.5 ht=1.3.
                                                  *               Added "AC" to the end of the define.
                                                  *               They are now the default for their species:
                                                  *               ACT_THROWERAC, BA_KURUCZ82AC, BL_CHENAC, BP_CURTISAC,
                                                  *               HM_MEANSAC, FDI_THROWERAC, ACB_HUANGAC, PW_CURTISAC,
                                                  *               HWC_WILEYAC, FDC_BRUCEAC, CWC_KURUCZAC, PY_HANNAC,
                                                  *               SE_CHENAC.
                                                  *      jul 28 - Updated citation of Nigh's 1999 Pl and 2003 Fdc.
                                                  *      aug 7  - v1.32 Added 40 more species.
                                                  *      sep 11 - v1.33 Added Fd, Pl, Hw, Cw.
                                                  *      dec 15 - v1.34 Added more coast/interior designations for dozens of codes.
                                                  *          16 - v1.35 Changed Pw location designation to none.
                                                  * 2004 feb 10 - v1.36 Changed Cw,Fd,Hw from 0 to 5 in spec_use.
                                                  *             - Changed Pl from 0 to 6 in spec_use.
                                                  *      mar 26 - v1.37 Added SW_GOUDIE_NATAC.
                                                  *      apr 28 - v1.38 Added Nigh's 2002 Py.
                                                  *      may 4  - Updated citation and notes for Nigh's Py.
                                                  *          5  - Changed default Py to Nigh's.
                                                  *             - Changed default Sw natural curve to be SW_GOUDIE_NATAC.
                                                  *      jun 15 - v1.39 Added Nigh's 2004 Pl/Sw/Se total age curves, but not available
                                                  *               on their own.
                                                  *             - Substituted Nigh's total age curves for the 0-1.3m area of
                                                  *               Pl Thrower, Sw Goudie Nat & Pla AC, and Se Chen AC.
                                                  *             - Changed default Sw curve from SW_GOUDNIGH to SW_GOUDIE_PLAAC.
                                                  *             - Changed default Pli curve from PLI_THROWNIGH to PLI_THROWER.
                                                  *      jul 12 - v1.40 Added Pli Thrower to Age-to-Age function.
                                                  *             - Added check for age-to-age conversion going negative.
                                                  *      sep 14 - v1.41 Several si2ht.c changes of checking BHage.
                                                  * 2005 feb 18 - v1.42 Bug fix in Fdc Bruce and Bruce AC, to use total age
                                                  *               directly instead of converting to breast-height age.
                                                  *      oct 20 - Added jack pine (PJ).
                                                  * 2008 feb 28 - Added 2004 Sw Nigh GI.
                                                  *      jul 4  - v1.43 Release.
                                                  * 2009 may 6  - v1.44 Force pure y2bh for Fdc-Bruce.
                                                  * 2009 aug 28 - Added Nigh's 2009 Ep.
                                                  * 2010 mar 4  - v1.45 Added Nigh's 2009 Ba GI.
                                                  *             - Added Nigh's 2009 Ba, as default.
                                                  *      apr 14 - Added 2010 Sw Hu and Garcia.
                                                  * 2014 apr 25 - v1.47 Added Sindex_AgeSIToHtSmooth().
                                                  *      sep 2  - Added 2014 Se Nigh GI.
                                                  * 2015 apr 9  - v1.48 Removed SI_SPEC_BV.
                                                  *          23 - Updated three "in review"/"in press" citations for Py, Pl, Sw, At.
                                                  *             - Updated text notes of SI_PY_NIGH.
                                                  *      may 13 - v1.49 Added 2015 Se Nigh.
                                                  * 2016 mar 9  - v1.50 Added Sindex_Y2BH05() which os now the rounded value,
                                                  *               changing Sindex_Y2BH() to be the unrounded value.
                                                  * 2017 feb 2  - v1.51 Added Nigh's 2016 Cwc equation as default.
                                                  * 2018 jan 11 - Added Nigh's 2017 Pli equation.
                                                  *          18 - Added species codes Ey, Js, Ld, Ls, Oh, Oi, Oj, Ok, Qw.
                                                  */



#define SI_ESTAB_NAT  0
#define SI_ESTAB_PLA  1

#define SI_SPEC_START SI_SPEC_A
#define SI_SPEC_END   SI_SPEC_ZH

#define SI_A_START    SI_ERR_NO_ANS
#define SI_A_END      SI_ERR_NO_ANS
#define SI_ABAL_START SI_ERR_NO_ANS
#define SI_ABAL_END   SI_ERR_NO_ANS
#define SI_ABCO_START SI_ERR_NO_ANS
#define SI_ABCO_END   SI_ERR_NO_ANS
#define SI_AC_START   SI_ERR_NO_ANS
#define SI_AC_END     SI_ERR_NO_ANS
#define SI_ACB_START  SI_ACB_HUANGAC
#define SI_ACB_END    SI_ACB_HUANG
#define SI_ACT_START  SI_ACT_THROWERAC
#define SI_ACT_END    SI_ACT_THROWER
#define SI_AD_START   SI_ERR_NO_ANS
#define SI_AD_END     SI_ERR_NO_ANS
#define SI_AH_START   SI_ERR_NO_ANS
#define SI_AH_END     SI_ERR_NO_ANS
#define SI_AT_START   SI_AT_NIGH
#define SI_AT_END     SI_AT_GOUDIE
#define SI_AX_START   SI_ERR_NO_ANS
#define SI_AX_END     SI_ERR_NO_ANS
#define SI_B_START    SI_ERR_NO_ANS
#define SI_B_END      SI_ERR_NO_ANS
#define SI_BA_START   SI_BA_NIGHGI
#define SI_BA_END     SI_BA_KURUCZ82
#define SI_BB_START   SI_ERR_NO_ANS
#define SI_BB_END     SI_ERR_NO_ANS
#define SI_BC_START   SI_ERR_NO_ANS
#define SI_BC_END     SI_ERR_NO_ANS
#define SI_BG_START   SI_ERR_NO_ANS
#define SI_BG_END     SI_ERR_NO_ANS
#define SI_BI_START   SI_ERR_NO_ANS
#define SI_BI_END     SI_ERR_NO_ANS
#define SI_BL_START   SI_BL_CHENAC
#define SI_BL_END     SI_BL_KURUCZ82
#define SI_BM_START   SI_ERR_NO_ANS
#define SI_BM_END     SI_ERR_NO_ANS
#define SI_BP_START   SI_BP_CURTISAC
#define SI_BP_END     SI_BP_CURTIS
//#define SI_BV_START   SI_ERR_NO_ANS
//#define SI_BV_END     SI_ERR_NO_ANS
#define SI_C_START    SI_ERR_NO_ANS
#define SI_C_END      SI_ERR_NO_ANS
#define SI_CI_START   SI_ERR_NO_ANS
#define SI_CI_END     SI_ERR_NO_ANS
#define SI_CP_START   SI_ERR_NO_ANS
#define SI_CP_END     SI_ERR_NO_ANS
#define SI_CW_START   SI_ERR_NO_ANS
#define SI_CW_END     SI_ERR_NO_ANS
#define SI_CWC_START  SI_CWC_NIGH
#define SI_CWC_END    SI_CWC_BARKER
#define SI_CWI_START  SI_CWI_NIGH
#define SI_CWI_END    SI_CWI_NIGHGI
#define SI_CY_START   SI_ERR_NO_ANS
#define SI_CY_END     SI_ERR_NO_ANS
#define SI_D_START    SI_ERR_NO_ANS
#define SI_D_END      SI_ERR_NO_ANS
#define SI_DG_START   SI_ERR_NO_ANS
#define SI_DG_END     SI_ERR_NO_ANS
#define SI_DM_START   SI_ERR_NO_ANS
#define SI_DM_END     SI_ERR_NO_ANS
#define SI_DR_START   SI_DR_NIGH
#define SI_DR_END     SI_DR_HARRING
#define SI_E_START    SI_ERR_NO_ANS
#define SI_E_END      SI_ERR_NO_ANS
#define SI_EA_START   SI_ERR_NO_ANS
#define SI_EA_END     SI_ERR_NO_ANS
#define SI_EB_START   SI_ERR_NO_ANS
#define SI_EB_END     SI_ERR_NO_ANS
#define SI_EE_START   SI_ERR_NO_ANS
#define SI_EE_END     SI_ERR_NO_ANS
#define SI_EP_START   SI_EP_NIGH
#define SI_EP_END     SI_EP_NIGH
#define SI_ES_START   SI_ERR_NO_ANS
#define SI_ES_END     SI_ERR_NO_ANS
#define SI_EW_START   SI_ERR_NO_ANS
#define SI_EW_END     SI_ERR_NO_ANS
#define SI_EXP_START  SI_ERR_NO_ANS
#define SI_EXP_END    SI_ERR_NO_ANS
#define SI_EY_START   SI_ERR_NO_ANS
#define SI_EY_END     SI_ERR_NO_ANS
#define SI_FD_START   SI_ERR_NO_ANS
#define SI_FD_END     SI_ERR_NO_ANS
#define SI_FDC_START  SI_FDC_BRUCEAC
#define SI_FDC_END    SI_FDC_KING
#define SI_FDI_START  SI_FDI_THROWERAC
#define SI_FDI_END    SI_FDI_MONS_SAF
#define SI_G_START    SI_ERR_NO_ANS
#define SI_G_END      SI_ERR_NO_ANS
#define SI_GP_START   SI_ERR_NO_ANS
#define SI_GP_END     SI_ERR_NO_ANS
#define SI_GR_START   SI_ERR_NO_ANS
#define SI_GR_END     SI_ERR_NO_ANS
#define SI_H_START    SI_ERR_NO_ANS
#define SI_H_END      SI_ERR_NO_ANS
#define SI_HM_START   SI_HM_MEANSAC
#define SI_HM_END     SI_HM_MEANS
#define SI_HW_START   SI_ERR_NO_ANS
#define SI_HW_END     SI_ERR_NO_ANS
#define SI_HWC_START  SI_HWC_WILEYAC
#define SI_HWC_END    SI_HWC_WILEY_MB
#define SI_HWI_START  SI_HWI_NIGH
#define SI_HWI_END    SI_HWI_NIGHGI
#define SI_HXM_START  SI_ERR_NO_ANS
#define SI_HXM_END    SI_ERR_NO_ANS
#define SI_IG_START   SI_ERR_NO_ANS
#define SI_IG_END     SI_ERR_NO_ANS
#define SI_IS_START   SI_ERR_NO_ANS
#define SI_IS_END     SI_ERR_NO_ANS
#define SI_J_START    SI_ERR_NO_ANS
#define SI_J_END      SI_ERR_NO_ANS
#define SI_JR_START   SI_ERR_NO_ANS
#define SI_JR_END     SI_ERR_NO_ANS
#define SI_JS_START   SI_ERR_NO_ANS
#define SI_JS_END     SI_ERR_NO_ANS
#define SI_K_START    SI_ERR_NO_ANS
#define SI_K_END      SI_ERR_NO_ANS
#define SI_KC_START   SI_ERR_NO_ANS
#define SI_KC_END     SI_ERR_NO_ANS
#define SI_L_START    SI_ERR_NO_ANS
#define SI_L_END      SI_ERR_NO_ANS
#define SI_LA_START   SI_ERR_NO_ANS
#define SI_LA_END     SI_ERR_NO_ANS
#define SI_LD_START   SI_ERR_NO_ANS
#define SI_LD_END     SI_ERR_NO_ANS
#define SI_LE_START   SI_ERR_NO_ANS
#define SI_LE_END     SI_ERR_NO_ANS
#define SI_LS_START   SI_ERR_NO_ANS
#define SI_LS_END     SI_ERR_NO_ANS
#define SI_LT_START   SI_ERR_NO_ANS
#define SI_LT_END     SI_ERR_NO_ANS
#define SI_LW_START   SI_LW_NIGH
#define SI_LW_END     SI_LW_MILNER
#define SI_M_START    SI_ERR_NO_ANS
#define SI_M_END      SI_ERR_NO_ANS
#define SI_MB_START   SI_ERR_NO_ANS
#define SI_MB_END     SI_ERR_NO_ANS
#define SI_ME_START   SI_ERR_NO_ANS
#define SI_ME_END     SI_ERR_NO_ANS
#define SI_MN_START   SI_ERR_NO_ANS
#define SI_MN_END     SI_ERR_NO_ANS
#define SI_MR_START   SI_ERR_NO_ANS
#define SI_MR_END     SI_ERR_NO_ANS
#define SI_MS_START   SI_ERR_NO_ANS
#define SI_MS_END     SI_ERR_NO_ANS
#define SI_MV_START   SI_ERR_NO_ANS
#define SI_MV_END     SI_ERR_NO_ANS
#define SI_OA_START   SI_ERR_NO_ANS
#define SI_OA_END     SI_ERR_NO_ANS
#define SI_OB_START   SI_ERR_NO_ANS
#define SI_OB_END     SI_ERR_NO_ANS
#define SI_OC_START   SI_ERR_NO_ANS
#define SI_OC_END     SI_ERR_NO_ANS
#define SI_OD_START   SI_ERR_NO_ANS
#define SI_OD_END     SI_ERR_NO_ANS
#define SI_OE_START   SI_ERR_NO_ANS
#define SI_OE_END     SI_ERR_NO_ANS
#define SI_OF_START   SI_ERR_NO_ANS
#define SI_OF_END     SI_ERR_NO_ANS
#define SI_OG_START   SI_ERR_NO_ANS
#define SI_OG_END     SI_ERR_NO_ANS
#define SI_OH_START   SI_ERR_NO_ANS
#define SI_OH_END     SI_ERR_NO_ANS
#define SI_OI_START   SI_ERR_NO_ANS
#define SI_OI_END     SI_ERR_NO_ANS
#define SI_OJ_START   SI_ERR_NO_ANS
#define SI_OJ_END     SI_ERR_NO_ANS
#define SI_OK_START   SI_ERR_NO_ANS
#define SI_OK_END     SI_ERR_NO_ANS
#define SI_P_START    SI_ERR_NO_ANS
#define SI_P_END      SI_ERR_NO_ANS
#define SI_PA_START   SI_ERR_NO_ANS
#define SI_PA_END     SI_ERR_NO_ANS
#define SI_PF_START   SI_ERR_NO_ANS
#define SI_PF_END     SI_ERR_NO_ANS
#define SI_PJ_START   SI_PJ_HUANG
#define SI_PJ_END     SI_PJ_HUANGAC
#define SI_PL_START   SI_ERR_NO_ANS
#define SI_PL_END     SI_ERR_NO_ANS
#define SI_PLC_START  SI_ERR_NO_ANS
#define SI_PLC_END    SI_ERR_NO_ANS
#define SI_PLI_START  SI_PL_CHEN
#define SI_PLI_END    SI_PLI_DEMPSTER
#define SI_PM_START   SI_ERR_NO_ANS
#define SI_PM_END     SI_ERR_NO_ANS
#define SI_PR_START   SI_ERR_NO_ANS
#define SI_PR_END     SI_ERR_NO_ANS
#define SI_PS_START   SI_ERR_NO_ANS
#define SI_PS_END     SI_ERR_NO_ANS
#define SI_PW_START   SI_PW_CURTISAC
#define SI_PW_END     SI_PW_CURTIS
#define SI_PXJ_START  SI_ERR_NO_ANS
#define SI_PXJ_END    SI_ERR_NO_ANS
#define SI_PY_START   SI_PY_NIGH
#define SI_PY_END     SI_PY_HANN
#define SI_Q_START    SI_ERR_NO_ANS
#define SI_Q_END      SI_ERR_NO_ANS
#define SI_QE_START   SI_ERR_NO_ANS
#define SI_QE_END     SI_ERR_NO_ANS
#define SI_QG_START   SI_ERR_NO_ANS
#define SI_QG_END     SI_ERR_NO_ANS
#define SI_QW_START   SI_ERR_NO_ANS
#define SI_QW_END     SI_ERR_NO_ANS
#define SI_R_START    SI_ERR_NO_ANS
#define SI_R_END      SI_ERR_NO_ANS
#define SI_RA_START   SI_ERR_NO_ANS
#define SI_RA_END     SI_ERR_NO_ANS
#define SI_S_START    SI_ERR_NO_ANS
#define SI_S_END      SI_ERR_NO_ANS
#define SI_SA_START   SI_ERR_NO_ANS
#define SI_SA_END     SI_ERR_NO_ANS
#define SI_SB_START   SI_SB_NIGH
#define SI_SB_END     SI_SB_DEMPSTER
#define SI_SE_START   SI_SE_CHENAC
#define SI_SE_END     SI_SE_NIGH
#define SI_SI_START   SI_ERR_NO_ANS
#define SI_SI_END     SI_ERR_NO_ANS
#define SI_SN_START   SI_ERR_NO_ANS
#define SI_SN_END     SI_ERR_NO_ANS
#define SI_SS_START   SI_SS_NIGHGI99
#define SI_SS_END     SI_SS_GOUDIE
#define SI_SW_START   SI_SW_GOUDNIGH
#define SI_SW_END     SI_SW_GOUDIE_NAT
#define SI_SX_START   SI_ERR_NO_ANS
#define SI_SX_END     SI_ERR_NO_ANS
#define SI_SXB_START  SI_ERR_NO_ANS
#define SI_SXB_END    SI_ERR_NO_ANS
#define SI_SXE_START  SI_ERR_NO_ANS
#define SI_SXE_END    SI_ERR_NO_ANS
#define SI_SXL_START  SI_ERR_NO_ANS
#define SI_SXL_END    SI_ERR_NO_ANS
#define SI_SXS_START  SI_ERR_NO_ANS
#define SI_SXS_END    SI_ERR_NO_ANS
#define SI_SXW_START  SI_ERR_NO_ANS
#define SI_SXW_END    SI_ERR_NO_ANS
#define SI_SXX_START  SI_ERR_NO_ANS
#define SI_SXX_END    SI_ERR_NO_ANS
#define SI_T_START    SI_ERR_NO_ANS
#define SI_T_END      SI_ERR_NO_ANS
#define SI_TW_START   SI_ERR_NO_ANS
#define SI_TW_END     SI_ERR_NO_ANS
#define SI_U_START    SI_ERR_NO_ANS
#define SI_U_END      SI_ERR_NO_ANS
#define SI_UA_START   SI_ERR_NO_ANS
#define SI_UA_END     SI_ERR_NO_ANS
#define SI_UP_START   SI_ERR_NO_ANS
#define SI_UP_END     SI_ERR_NO_ANS
#define SI_V_START    SI_ERR_NO_ANS
#define SI_V_END      SI_ERR_NO_ANS
#define SI_VB_START   SI_ERR_NO_ANS
#define SI_VB_END     SI_ERR_NO_ANS
#define SI_VP_START   SI_ERR_NO_ANS
#define SI_VP_END     SI_ERR_NO_ANS
#define SI_VS_START   SI_ERR_NO_ANS
#define SI_VS_END     SI_ERR_NO_ANS
#define SI_VV_START   SI_ERR_NO_ANS
#define SI_VV_END     SI_ERR_NO_ANS
#define SI_W_START    SI_ERR_NO_ANS
#define SI_W_END      SI_ERR_NO_ANS
#define SI_WA_START   SI_ERR_NO_ANS
#define SI_WA_END     SI_ERR_NO_ANS
#define SI_WB_START   SI_ERR_NO_ANS
#define SI_WB_END     SI_ERR_NO_ANS
#define SI_WD_START   SI_ERR_NO_ANS
#define SI_WD_END     SI_ERR_NO_ANS
#define SI_WI_START   SI_ERR_NO_ANS
#define SI_WI_END     SI_ERR_NO_ANS
#define SI_WP_START   SI_ERR_NO_ANS
#define SI_WP_END     SI_ERR_NO_ANS
#define SI_WS_START   SI_ERR_NO_ANS
#define SI_WS_END     SI_ERR_NO_ANS
#define SI_WT_START   SI_ERR_NO_ANS
#define SI_WT_END     SI_ERR_NO_ANS
#define SI_X_START    SI_ERR_NO_ANS
#define SI_X_END      SI_ERR_NO_ANS
#define SI_XC_START   SI_ERR_NO_ANS
#define SI_XC_END     SI_ERR_NO_ANS
#define SI_XH_START   SI_ERR_NO_ANS
#define SI_XH_END     SI_ERR_NO_ANS
#define SI_Y_START    SI_ERR_NO_ANS
#define SI_Y_END      SI_ERR_NO_ANS
#define SI_YC_START   SI_ERR_NO_ANS
#define SI_YC_END     SI_ERR_NO_ANS
#define SI_YP_START   SI_ERR_NO_ANS
#define SI_YP_END     SI_ERR_NO_ANS
#define SI_Z_START    SI_ERR_NO_ANS
#define SI_Z_END      SI_ERR_NO_ANS
#define SI_ZC_START   SI_ERR_NO_ANS
#define SI_ZC_END     SI_ERR_NO_ANS
#define SI_ZH_START   SI_ERR_NO_ANS
#define SI_ZH_END     SI_ERR_NO_ANS


static char *si_curve_notes[SI_MAX_CURVES][2] =
  {
  /* SI_ACB_HUANG */
  "Huang Shongming, Stephen J. Titus and Tom W. Lakusta. 1994. \
  Ecologically based site index curves and tables for major    \
  Alberta tree species.                                        \
  Ab. Envir. Prot., Land For. Serv., For. Man. Division,       \
  Tech. Rep. 307-308, Edmonton, Ab.",
  "The height-age (site index) curves were developed from stem         \
  analysis of 148 balsam poplar (Populus balsamifera spp. balsamifera) \
  trees from different geographic regions of Alberta.                  \
  Site index ranged from about 10 to 28 m at 50 years                  \
  breast-height age and included trees up to 130 years old.",

  /* SI_ACT_THROWER */
  "J. S. Thrower and Associates Ltd. 1992. Height-age/site-index curves for Black \
  Cottonwood in British Columbia. Ministry of Forests, Inventory Branch. Project  \
  92-07-IB, 21p.",
  "The height-age (site index) curves were developed from 25 stem analysis plots of \
  black cottonwood (Populus balsamifera spp. trichocarpa) located in three          \
  geographic regions of coastal British Columbia. Site index ranged from about 15   \
  to 35 m at 50 years breast-height age and included trees up to 150 years old.",

  /* SI_AT_HUANG */
  "", /* see ACB_HUANG */
  "The height-age (site index) curves were developed from stem analysis of 757     \
  trembling aspen (Populus tremuloides) trees from different geographic regions of \
  Alberta. Site index ranged from about 10 to 26 m at 50 years breast-height age   \
  and included trees up to 138 years old.",

  /* SI_AT_CIESZEWSKI */
  "Cieszewski, Chris J. and Imre E. Bella. 1991. Polymorphic height and site index \
  curves for the major tree species in Alberta. For. Can. NW Reg. North. For.      \
  Cent, For. Manage. Note 51, Edmonton, Alberta.",
  "The height-age (site index) curves were developed from stem analysis of 276       \
  dominant and co-dominant trembling aspen trees located throughout Alberta. Site    \
  index ranged from about 8 to 25 m at 50 years breast-height age and included trees \
  up to 140 years old.",

  /* SI_AT_GOUDIE */
  "Alberta Forest Service. 1985. Alberta phase 3 forest inventory: yield tables for \
  unmanaged stands. ENR Rep. No. Dep. 60a.",
  "The height-age (site index) curves were developed from stem analysis of 207       \
  dominant and co-dominant trembling aspen trees located throughout Alberta. Site    \
  index ranged from about 9 to 24 m at 50 years breast-height age and included trees \
  up to 90 years old.",

  /* SI_BA_DILUCCA */
  "Di Lucca, Carlos M. 1992. Height-age/site-index curves for coastal Amabilis fir \
  (Abies amabilis) in British Columbia. B.C. Ministry of Forests, Research Branch, \
  Unpublish Tech. Report.",
  "The height-age (site index) polymorphic curves were developed from stem analysis \
  of 199 undamaged, dominant Abies amabilis trees from 50 plots located             \
  throughout the coastal region of British Columbia. Plot ages ranged from 50 to    \
  160 years at breast height and site index ranged from 11 to 34 m.",

  /* SI_BB_KER */
  "Ker, M. F. and C. Bowling. 1991. Polymorphic site index equations for four \
  New Brunswick softwood species. Can. J. For. Res. 21:728-732.",
  "The data for this curve consist of 456 trees taken from 12 m radius plots (3 \
  or 4 trees per plot) established in mature and overmature stands in New       \
  Brunswick. The trees ranged in age from 50 to 125 years at breast height and  \
  ranged in site index from 3.6 m to 20.4 m at 50 years breast height age.      \
  Most trees suffered some minor slowing of growth due to an outbreak of spruce \
  budworm.",

  /* SI_BA_KURUCZ86 */
  "Kurucz, John F. 1986. Report on Project 930-4. Site Index curve extension for \
  Abies amabilis, MacMillan Bloedel Ltd., Resource Economics Section,            \
  Woodlands Services, Nanaimo, BC. 27 p.",
  "MacMillan Bloedel has developed site index curves for Amabilis fir (Abies        \
  amabilis) in 1982 using stem analyzed sample tree data obtained from immature     \
  and young-mature stands. These curves have been fitted to give best results       \
  during 0 to 150 years range of growth projections. Occasionally, prediction is    \
  required for a longer time period (0 to 400+ years). Attaching site index to old- \
  mature stands in an inventory is a good example. From the various options         \
  considered, the best solution - to extend the curves to 400+ years - was found in \
  recompiling the 1982 basic data with a new height-growth function.",

  /* SI_BA_KURUCZ82 */
  "Kurucz, John F. 1982. Report on Project 933-3. Polymorphic site-index curves    \
  for balsam -Abies amabilis- in coastal British Columbia, MacMillan Bloedel Ltd., \
  Resource Economics Section, Woodlands Services, Rep. on Project 933-3. 24 p.     \
  + app. Nanaimo, BC.",
  "The height-age (site index) curves were developed from stem analysis of 199   \
  undamaged, dominant Amabilis fir (Abies amabilis) trees from 50 plots located  \
  throughout the coastal region of British Columbia. Plot ages ranged from 50 to \
  160 years at breast height and site index ranged from 11 to 34 m. The          \
  discontinuity in the height-age curve at age 50 is caused by the adjustment    \
  equation to reduce bias at ages below 50 and is exaggerated by extending the   \
  equation beyond the range of the site index from which it was developed.",

  /* SI_BL_THROWERGI */
  "Thrower, James S. 1997. Development of a Growth Intercept Model for Interior Balsam.",
  "Based on balsam trees from 18 plots in the ESSF zone, and 37 plots outside     \
  of the ESSF. Top height ranged from 4.0 to 29.7m, breast-height age ranged from \
  50 to 193 years, and site index ranged from 3.4 to 23.4m.",

  /* SI_BL_KURUCZ82 */
  "", /* see BA_KURUCZ82 */
  "The height-age (site index) curves were developed from stem analysis of 199      \
  undamaged, dominant Abies amabilis trees from 50 plots located throughout the     \
  coastal region of British Columbia. Plot ages ranged from 50 to 160 years at      \
  breast height and site index ranged from 11 to 34 m.                              \
  The discontinuity in the                                                          \
  height-age curve at age 50 is caused by the adjustment equation to reduce bias at \
  ages below 50 and is exaggerated by extending the equation beyond the range of    \
  the site index from which it was developed. The years to breast height function   \
  was developed by the Research Branch from interior balsam data.",

  /* SI_CWC_KURUCZ */
  "This 1985 formulation is an updated version of the curves given in 1978 by   \
  Kurucz 1978. Kurucz, John F. 1978. Preliminary, polymorphic site index curves \
  for western redcedar (Thuja plicata Donn) in coastal British Columbia.        \
  MacMillan Bloedel For. Res. Note No. 3. 14 p. + appendix.",
  "The height-age (site index) curves were developed from stem analysis of         \
  undamaged, dominant and co-dominant trees located in approximately 50 stands     \
  throughout Vancouver Island and the mid-coast region of the mainland. The        \
  sample trees ranged in breast-height age from 33 to 285 years and in site index  \
  from 8 to 37 m. Kurucz suggested using this formulation with caution for breast- \
  height ages less than 10 years and for site indexes greater than 37 m.",

  /* SI_CWC_BARKER */
  "Barker, John E. 1983. Site index relationships for sitka spruce, western hemlock, \
  western redcedar and red alder, Moresby tree SI_farm license #24, Queen Charlotte  \
  Islands. Unpub. Final Rep. on Section 88 project #HR07034 submitted to Inv. Br.,   \
  Min. For. 14 p.",
  "",

  /* SI_DR_NIGH */
  "Nigh, G.D. and P.J. Courtin. 1998 Height models for red alder (Alnus rubra \
  Bong.) in British Columbia. New For. 16:59-70.",
  "The height-age equation was developed from stem analysis of 30 - 0.04 ha    \
  plots from natural red alder stands in tthe CWH biogeoclimatic zone in       \
  British Columbia. Breast height ages ranged up to 54 years and site index    \
  ranged from about 15 to 28 m (at 25 years breast height age). Conversions    \
  from a breast height age 25 site index to a breast height are 50 site index  \
  are derived from the height-age model. Site index can be calculated directly \
  by inverting the height-age model. A years to breast height model was also   \
  developed from the same data.",

  /* SI_DR_HARRING */
  "Harrington, Constance A. and Robert O. Curtis. 1986. Height growth and site  \
  index curves for red alder. U.S. Dep. Agric. For. Serv. Res. Pap. PNW-358. 14 \
  p.",
  "The height-age equation was developed from stem analysis of 156 undamaged,        \
  dominant and co-dominant trees from natural red alder stands in western            \
  Washington and northwestern Oregon. Ages ranged up to 80 years (total age) and     \
  site index ranged from about 8 to 23 m (at 20 years total age). The height-age     \
  equation performs poorly for estimating site index below about site index 20.      \
  Harrington and Curtis developed an equation for directly estimating site index at  \
  20 years total age, but our conversion to site index at 50 years breast-height age \
  was not suitable for field application.                                            \
  The height equation assumes a constant of 2 years to reach breast height. This     \
  may be 1 or 2 years more on poor sites and less on good sites.",

  /* SI_FDC_NIGHGI */
  "Nigh, Gordon D. 1997. Coastal Douglas-fir growth intercept model. B.C. Min. \
  For., Res. Br., Victoria B.C. Res. Rep. 10.",
  "The growth intercept models were developed from 47 stem analysis plots located    \
  in the Coastal Western Hemlock and Coastal Douglas-fir biogeoclimatic zones.       \
  Plots ranged in site index from about 15 to 46 m, and the growth intercepts ranged \
  from about 22 to 108 cm. The models can be used throughout coastal British         \
  Columbia.",

  /* SI_FDC_BRUCE */
  "Bruce, David. 1981. Consistent height-growth and growth-rate estimates for \
  remeasured plots. For. Sci. 27:711-725.",
  "The site index (height-age) curves were developed from remeasured Douglas-fir     \
  (Pseudotsuga menziesii) permanent sample plots in Washington, Oregon, and          \
  British Columbia. The plots covered a wide range of sites up to about 80 years     \
  breast-height age for both natural and planted stands. Tests have shown that these \
  curves reasonably portray the height growth of dominant, undamaged second- and     \
  old-growth trees on coastal British Columbia. Bruce's curves are very similar to   \
  those given by J. E. King (1966. Site index curves for Douglas-fir in the Pacific  \
  Northwest. Weyerhaeuser Co., For. Res. Cent. For. Pap. 8. 49p.).",

  /* SI_FDC_COCHRAN */
  "Cochran, P. H. 1979. Site index and height growth curves for managed, even-     \
  aged stands of white or grand fir east of the cascades in Oregon and Washington. \
  USDA For. Serv. Res. Pap. PNW-252, Portland, Or.",
  "Height growth and site index curves and equations for managed, even-aged stands \
  of Douglas-fir ( +Pseudotsuga menziesii+ [Mirb] Franco ) east of the Cascade     \
  Range in Oregon and Washington are presented. Data were collected in stands      \
  where height growth apparently has not been suppressed by high density or top    \
  damage.",

  /* SI_FDC_KING */
  "King, James E. 1966. Site index curves for Douglas-fir in the Pacific Northwest. \
  Weyerhaeuser For. Pap. No 8, Weyerhaeuser Forestry Paper No. 8, Centralia,        \
  WA.",
  "The data for this curve came from 85 plots located in pure Douglas-fir      \
  stands in western Washington state. Plot sizes were chosen to include 50     \
  trees, of which the 10 largest dbh trees were chosen as site (sample) trees. \
  Instead of conventional stem analysis, heights were measured at 5 year       \
  intervals on standing trees. The breast height ages of the plots ranged from \
  28 to 135 years.",

  /* SI_FDI_NIGHGI */
  "Nigh, G.D. (1997). Interior Douglas-fir growth intercept models. Res. Br., \
  B.C. Min. Forests, Victoria, B.C. Ext. Note. 12.",
  "The growth intercept models were developed from 72 stem analysis plots located \
  throughout the interior of British Columbia. Plots ranged in site index from    \
  about 10 to 29 m, and the growth intercepts ranged from about 10 to 64 cm. The  \
  models can be used throughout the interior of British Columbia",

  /* SI_FDI_HUANG_PLA */
  "", /* see ACB_HUANG */
  "", /* see FDI_HUANG_PLA */

  /* SI_FDI_HUANG_NAT */
  "", /* see ACB_HUANG */
  "The height-age (site index) curves were developed from stem analysis of 66    \
  interior Douglas-fir (Pseudotsuga menziesii) trees from different geographic   \
  regions of Alberta. Site index ranged from about 6 to 18 m at 50 years breast- \
  height age and included trees up to 138 years old.",

  /* SI_FDI_MILNER */
  "Milner, Kelsey S. 1992. Site index and height growth curves for Ponderosa pine, \
  Western larch, Lodgepole pine, and Douglas-fir in Western Montana. West. J.      \
  Appl. For. 7(1):9-14.",
  "The site index (height-age) curves were developed from stem analysis of 129  \
  dominant trees in 46 plots located in even-aged Douglas-fir stands throughout \
  western Montana. The curves were developed from plots ranging in site index   \
  from 8 to 28 m and up to 80 years breast-height age.",

  /* SI_FDI_THROWER */
  "Thrower, James S. and James W. Goudie. 1992. Estimating dominant height and      \
  site index for even-aged interior Douglas-fir in British Columbia. West. J. Appl. \
  For. 7(1):20-25.",
  "The site index curves were developed from stem analysis of 262 dominant trees in   \
  68 plots located in even-aged Douglas-fir stands throughout the interior of British \
  Columbia. The curves were developed from plots ranging in site index from 8 to      \
  30 m and up to 100 years breast-height age. On high sites, 30 m and greater, the    \
  curves may over-estimate height growth at older ages.",

  /* SI_FDI_VDP_MONT */
  "Vander Ploeg, James L. and James A. Moore. 1989. Comparison and          \
  Development of Height Growth and Site Index Curves for Douglas-Fir in the \
  Inland Northwest. West. J. Appl. For. 4(3):85-88.",
  "The site index (height-age) curves were developed from stem analysis of 578  \
  dominant trees in 89 plots located in even-aged Douglas-fir stands throughout \
  Inland northwest. These curves were developed for central Washington and      \
  Montana from plots ranging in site index from 13 to 31 m and up to 100 years  \
  breast-height age.",

  /* SI_FDI_VDP_WASH */
  "", /* see FDI_VDP_MONT */
  "", /* see FDI_VDP_MONT */

  /* SI_FDI_MONS_DF */
  "Monserud, Robert A. 1984. Height growth and site index curves for inland  \
  Douglas-fir based on stem analysis data and forest habitat type. For. Sci. \
  30:943-965.",
  "The site index (height-age) curves were developed from stem analysis in 135 plots \
  located in both even- and uneven-aged Douglas-fir habitat series throughout the    \
  northern Rocky Mountains. The curves were developed from plots ranging in          \
  site index from 8 to 30 m and up to 200 years breast-height age.",

  /* SI_FDI_MONS_GF */
  "", /* see FDI_MONS_DF */
  "", /* see FDI_MONS_DF */

  /* SI_FDI_MONS_WRC */
  "", /* see FDI_MONS_DF */
  "", /* see FDI_MONS_DF */

  /* SI_FDI_MONS_WH */
  "", /* see FDI_MONS_DF */
  "", /* see FDI_MONS_DF */

  /* SI_FDI_MONS_SAF */
  "", /* see FDI_MONS_DF */
  "", /* see FDI_MONS_DF */

  /* SI_HWC_NIGHGI */
  "Nigh, Gordon D. 1996. Growth intercept models for species without distinct \
  annual branch whorls: western hemlock. Can. J. For. Res. 26: 1407-1415 (1996).",
  "The growth intercept models were developed from 46 stem analysis plots located \
  in the Western Hemlock biogeoclimatic zone. Plots ranged in site index          \
  from about 7 to 40 m, and the growth intercepts ranged from about 10 to 100 cm. \
  The models can be used throughout coastal British Columbia.",

  /* SI_HWC_FARR */
  "Farr, W.A. 1984. Site index and height growth curves for unmanaged       \
  even-aged stands of western hemlock and Sitka spruce in southeast Alaska. \
  U.S.D.A. For. Serv. Res. Pap. PNW-326.",
  "The data for these western hemlock curves come from 57 sample plots located \
  in natural, well-stocked, even-aged stands of western hemlock and Sitka      \
  spruce throughout southeast Alaska. Seventeen plots were 1/3 - 1/2 acre in   \
  size and three trees of quadratic mean diameter among the dominants and      \
  co-dominants were stem analyzed. The remaining forty plots were 1/5 acre in  \
  size and trees representative of the 40 largest dbh per acre were sectioned. \
  Plots ranged in breast height age from approximately 45 to 180 years of age. \
  Site index ranged from approximately 41 to 120 feet.",

  /* SI_HWC_BARKER */
  "", /* see CWC_BARKER */
  "",

  /* SI_HWC_WILEY */
  "Wiley, Kenneth N. 1978. Site index tables for western hemlock in the \
  Pacific Northwest. Weyerhaeuser Co., For. Res. Cent. For. Pap. 17. 28 p.",
  "The site index (height-age) curves were developed from stem analysis data        \
  collected from 90 plots in Washington and Oregon. The plots ranged from site      \
  index 18 to 40 m and from about 60 to 130 years breast-height age. The height-    \
  age equation should not be used for ages less than 10 years. In British Columbia, \
  MacMillan Bloedel Ltd. calibrated these curves to better represent the local      \
  growing conditions.",

  /* SI_HWC_WILEY_BC */
  "", /* see HWC_WILEY */
  "The site index (height-age) curves were developed from stem analysis data        \
  collected from 90 plots in Washington and Oregon. The plots ranged from site      \
  index 18 to 40 m and from about 60 to 130 years breast-height age. The height-    \
  age equation should not be used for ages less than 10 years. In British Columbia, \
  MacMillan Bloedel Ltd. calibrated these curves to better represent the local      \
  growing conditions.",

  /* SI_HWC_WILEY_MB */
  "", /* see HWC_WILEY */
  "The site index (height-age) curves were developed from stem analysis data        \
  collected from 90 plots in Washington and Oregon. The plots ranged from site      \
  index 18 to 40 m and from about 60 to 130 years breast-height age. The height-    \
  age equation should not be used for ages less than 10 years. In British Columbia, \
  MacMillan Bloedel Ltd. calibrated these curves to better represent the local      \
  growing conditions.",

  /* SI_HWI_NIGH */
  "Nigh, G. D. 1998. A system for estimating height and site index \
  of western hemlock in the interior of British Columbia.          \
  For. Chron. 74(4): 588-596.",
  "The height-age (site index) curves were developed from 44 stem          \
  analysis plots located throughout the ICH biogeoclimatic zone in British \
  Columbia. Three dominant or codominant, undamaged, healthy top height    \
  trees were sampled in each plot. Plot breast height ages ranged from 50  \
  to 241 years, site index ranged from 5.7m (at bha 50) to 25.2m and top   \
  height ranged up to 36.7m. The years-to-breast-height function should be \
  used with caution in stands with a site index below 10m.",

  /* SI_HWI_NIGHGI */
  "Nigh, G. D. 1998. A system for estimating height and site index \
  of western hemlock in the interior of British Columbia.          \
  For. Chron. 74(4): 588-596.",
  "The growth intercept models were developed from 44 stem analysis plots     \
  plots located throughout the ICH biogeoclimatic zone in British Columbia.   \
  Plot site index ranged from 5.7m (at bha 50) to 25.2m and growth intercepts \
  ranged from about 10 to 50 cm. the models can be used throughout the        \
  interior of British Columbia.",

  /* SI_LW_MILNER */
  "", /* see FDI_MILNER */
  "The height-age (site index) curves were developed from stem analysis of western  \
  larch trees in 37 plots located throughout Western Montana. Site index ranged     \
  from 15 to 30 m. The abnormal shape of the height-age curves at young ages and    \
  low sites is the result of extending the curves beyond the range of the data from \
  which they were developed. Accordingly, the site curves should not be used        \
  below a site index of 10 m and 30 years of age. The years-to-breast-height        \
  function was developed by the Research Branch from interior western larch data.",

  /* SI_PLI_THROWNIGH */
  "Nigh, G.D. 1999. Smoothing top height estimates from two lodgepole pine \
  height models. B.C. Min. For., Res. Br., Victoria, B.C. Ext. Note 30.",
  "The Thrower (1994) and Nigh and Love (1999) Pl curves are spliced together \
  by using the Nigh/Love curve below breast height age 0, the Thrower curve   \
  above breast height 2, and linearly interpolating heights between breast    \
  height age 0 and 2.",

  /* SI_PLI_NIGHTA98 */
  "Nigh, G.D. and B.A. Love. 1999. A model for estimating juvenile height \
  of lodgepole pine. For. Ecol. Manage. 123: 157-166.",
  "The juvenile height-age model was developed from 46 stem analysis plots     \
  ranging from 12 to 24 years (total age) and 19 to 23 m in site index. The    \
  plots were established in the Bulkley valley. Four trees in each plot were   \
  stem analyzed by splitting the bole and measuring height growth from the     \
  terminal bud scars. This model is specifically designed to estimate juvenile \
  height growth from germination up to total age 15, years to breast height,   \
  and green-up ages.",

  /* SI_PLI_NIGHGI97 */
  "Nigh, G.D. (1997). Revised growth intercept models for lodgepole pine: \
  comparing northern and southern models. Res. Br., B.C. Min. Forests,    \
  Victoria, B.C. Ext. Note. Rep. 11.",
  "The growth intercept models were developed from 90 stem analysis plots located \
  throughout British Columbia. Plots ranged in site index from about 12 to 26m,   \
  and the growth intercepts ranged from about 20 to 85 cm. The models can be      \
  used throughout the interior of British Columbia.",

  /* SI_PLI_HUANG_PLA */
  "", /* see ACB_HUANG */
  "The height-age (site index) curves were developed from stem analysis of 1417       \
  lodgepole pine (Pinus contorta) trees from different geographic regions of Alberta. \
  Site index ranged from about 6 to 22 m at 50 years breast-height age and included   \
  trees up to 168 years old.",

  /* SI_PLI_HUANG_NAT */
  "", /* see ACB_HUANG */
  "", /* see PLI_HUANG_PLA */

  /* SI_PLI_THROWER */
  "J.S. Thrower and Associates Ltd. 1994. Revised height-age curves for lodgepole \
  pine and interior spruce in British Columbia. Report to the Res. Br., B.C.      \
  Min. For., Victoria, B.C. 27 p.",
  "The height-age models were developed from 106 plots established throughout   \
  the interior of British Columbia. Ages ranged from 50 to 130 years at breast  \
  height. The site indices of the plots ranged from 6 to 27 m at breast height  \
  age 50. A years to breast height model was also developed. These curves       \
  replace the ones by Goudie (1984). There is little difference between the two \
  curves; however, the new models are developed from data collected in British  \
  Columbia.",

  /* SI_PLI_MILNER */
  "", /* see FDI_MILNER */
  "The height-age (site index) curves were developed from stem analysis of trees in \
  39 lodgepole pine (Pinus contorta) plots located throughout Western Montana.      \
  Site index ranged from 9 to 26 m.",

  /* SI_PLI_CIESZEWSKI */
  "", /* see AT_CIESZEWSKI */
  "The height-age (site index) curves were developed from stem analysis of 188 dominant and    \
  co-dominant lodgepole pine (Pinus contorta) trees located throughout Alberta and Eastern     \
  British Columbia. Plots ranged in site index from about 8 to 35 m at 50 years breast height, \
  and in age up to 260 years.",

  /* SI_PLI_GOUDIE_DRY */
  "Goudie, James W. 1984. Height growth and site index curves for lodgepole pine \
  and white spruce and interim managed stand yield tables for lodgepole pine in  \
  British Columbia. B.C. Min. For., Res. Br. Unpubl. Rep. 75 p.",
  "The height-age (site index) curves were developed from stem analysis of 188  \
  dominant and co-dominant trees located throughout Alberta and Eastern British \
  Columbia. Plots ranged in site index from about 6 to 22 m at 50 years breast  \
  height, and in age from 10 to 150 years.",

  /* SI_PLI_GOUDIE_WET */
  "", /* see PLI_GOUDIE_DRY */
  "", /* see PLI_GOUDIE_DRY */

  /* SI_PLI_DEMPSTER */
  "", /* see AT_GOUDIE */
  "The height-age (site index) curves were developed from stem analysis of 1433    \
  dominant and co-dominant lodgepole pine (Pinus contorta) trees located           \
  throughout Alberta and Eastern British Columbia. Plots ranged in site index from \
  about 5 to 21 m at 50 years breast height, and in age up to 175 years.",

  /* SI_PW_CURTIS */
  "Curtis, Robert O., N. M. Diaz, and G. W. Clendenen. 1990. Height growth and \
  site index curves for western white pine in the Cascade Range of Western     \
  Washington and Oregon. U.S. Dep. Agric. For. Serv. Res. Pap. RNW-PR-423.     \
  14 p.",
  "The height-age (site index) curves were developed from stem analysis of 38  \
  dominant and co-dominant western white pine trees located throughout the     \
  Cascade Range of Washington and Oregon. Site index ranged from about 9 to 31 \
  m at 50 years breast height and included trees up to 200 years old.",

  /* SI_PY_MILNER */
  "", /* see FDI_MILNER */
  "The height-age (site index) curves were developed from stem analysis of trees in \
  31 plots located throughout Western Montana. Site index ranged from 12 to 26 m.",

  /* SI_PY_HANN */
  "Hann, D. W. and J. A. Scrivani. 1987. Dominant height growth and site index  \
  equations for Douglas-fir and ponderosa pine in southwest Oregon. Oreg. State \
  Univ. For. Res. Lab., Corvallis Oreg., Res. Bull. 59. 13 p.",
  "The height-age (site index curves) were developed from stem analysis of 41 trees \
  located throughout southwest Oregon. Selected trees came from natural, even-      \
  and uneven-aged, second-growth stands. Site index ranged from 19 to 34 m and      \
  from about 50 to 148 years breast-height age. Most stem analysis trees were       \
  under 120 years.",

  /* SI_SB_HUANG */
  "", /* see ACB_HUANG */
  "",

  /* SI_SB_CIESZEWSKI */
  "", /* see AT_CIESZEWSKI */
  "The height-age (site index) curves were developed from stem analysis of 282      \
  dominant and co-dominant black spruce (Picea mariana) trees located throughout    \
  Alberta regions. Site index ranged from about 9 to 16 m at 50 years breast height \
  and included trees up to 190 years old.",

  /* SI_SB_KER */
  "Ker, M. F. and C. Bowling. 1991. Polymorphic site index equations for four \
  New Brunswick softwood species. Can. J. For. Res. 21:728-732.",
  "The data for this curve consist of 354 trees taken from 12 m radius plots (3 \
  or 4 trees per plot) established in mature and overmature stands in New       \
  Brunswick. The trees ranged in age from 50 to 203 years at breast height and  \
  ranged in site index from 3.5 m to 17.3 m at 50 years breast height age.      \
  Most trees suffered some minor slowing of growth due to an outbreak of spruce \
  budworm.",

  /* SI_SB_DEMPSTER */
  "", /* see AT_GOUDIE */
  "The height-age (site index) curves were developed from stem analysis of 143  \
  dominant and co-dominant black spruce (Picea mariana) trees located in        \
  temporary and sample plots throughout Alberta regions. Site index ranged from \
  about 8 to 18 m at 50 years breast height and included trees up to 175 years old.",

  /* SI_SS_NIGHGI */
  "Nigh, Gordon D. 1996. A variable growth intercept model for Sitka spruce. \
  B.C. Min. For., Res. Br., Victoria, B.C. Ext. Note 03",
  "The growth intercept models were developed from 38 stem analysis plots located \
  in the Coastal Western Hemlock biogeoclimatic zone. Plots ranged in site index  \
  from about 16 to 40 m, and the growth intercepts ranged from about 20 to 90 cm. \
  The models can be used throughout coastal British Columbia.",

  /* SI_SS_NIGH */
  "Nigh, Gordon D. 1997. A Sitka spruce height-age model with improved extrapolation properties. \
  For. Chron. 73(3): 363-369.",
  "The height-age (site index) curves were developed from 40 stem analysis plots \
  established in ecologically uniform areas of Sitka spruce stands in the Queen  \
  Charlotte Islands. All plots were in the submontane wet hypermaritime Coast    \
  Western Hemlock (CWHwh1) biogeoclimatic variant. Plot ages ranged from 50      \
  to 121 years at breast-height and site index from 13.6 to 40.3 m.",

  /* SI_SS_GOUDIE */
  "Barker, J. E. and J. W. Goudie. 1987. Site index curves for Sitka spruce. B.C. \
  Min. For., Res. Branch, Victoria, B.C.",
  "The height-age (site index) curves were developed from stem analysis of trees in \
  48 plots located throughout the Queen Charlotte Islands. The trees ranged in      \
  breast-height age up to 150 years and in site index from 17 to 38 m.",

  /* SI_SS_FARR */
  "", /* see HWC_FARR */
  "",

  /* SI_SS_BARKER */
  "", /* see CWC_BARKER */
  "",

  /* SI_SW_NIGHGI */
  "Nigh, Gordon D. 1996. Variable growth intercept models for spruce in the Sub- \
  Boreal Spruce and Engelmann Spruce - Subalpine Fir biogeoclimatic zones of     \
  British Columbia. Research Report 05, B.C. Ministry of Forests, Research       \
  Branch. 20 p.",
  "The growth intercept models were developed from 45 stem analysis plots located    \
  in the Sub-Boreal Spruce and the Engelmann Spruce - Subalpine Fir                  \
  biogeoclimatic zones. Plots ranged in site index from about 10 to 26 m, and the    \
  growth intercepts ranged from about 15 to 60 cm. Until further data are available, \
  the models can be used throughout British Columbia.",

  /* SI_SW_HUANG_PLA */
  "", /* see ACB_HUANG */
  "",

  /* SI_SW_HUANG_NAT */
  "", /* see ACB_HUANG */
  "", /* see SW_HUANG_PLA */

  /* SI_SW_THROWER */
  "", /* see PLI_THROWER */
  "",

  /* SI_SW_CIESZEWSKI */
  "", /* see AT_CIESZEWSKI */
  "The height-age (site index) curves were developed from stem analysis of 698       \
  dominant and co-dominant white spruce trees located throughout Alberta. Site       \
  index ranged from about 7 to 41 m at 50 years breast-height age and included trees \
  up to 250 years old.",

  /* SI_SW_KER_PLA */
  "", /* see SB_KER */
  "The data for this curve consist of 234 trees taken from 12 m radius plots (3 \
  or 4 trees per plot) established in mature and overmature stands in New       \
  Brunswick. The trees ranged in age from 50 to 182 years at breast height and  \
  ranged in site index from 3.1 m to 21.2 m at 50 years breast height age.      \
  Most trees suffered some minor slowing of growth due to an outbreak of spruce \
  budworm.",

  /* SI_SW_KER_NAT */
  "", /* see SB_KER */
  "", /* see SW_KER_PLA */

  /* SI_SW_GOUDIE_PLA */
  "", /* see PLI_GOUDIE_DRY */
  "The height-age (site index) curves were developed from stem analysis of 157  \
  dominant and co-dominant trees located throughout Alberta and eastern British \
  Columbia. Plots ranged in site index from about 3 to 24 m at 50 years breast  \
  height, and in age from 10 to 130 years.",

  /* SI_SW_GOUDIE_NAT */
  "", /* see PLI_GOUDIE_DRY */
  "", /* see SW_GOUDIE_PLA */

  /* SI_SW_DEMPSTER */
  "", /* see AT_GOUDIE */
  "Notes: The height-age (site index) curves were developed from stem analysis of \
  207 dominant and co-dominant trembling aspen trees located throughout Alberta.  \
  Site index ranged from about 9 to 24 m at 50 years breast-height age and        \
  included trees up to 90 years old.",

  /* SI_BL_CHEN */
  "Chen, H.Y.H, and K. Klinka. 2000. Height growth models for high-elevation \
  subalpine fir, Engelmann, spruce, and lodgepole pine in British Columbia.  \
  West. J. Appl. For. 15: 62-69.",
  "The data for these curves come from 165 plots located in the ESSF zone of   \
  British Columbia. The plots were 20 x 20 m (0.04 ha) and the three largest   \
  dbh trees of the target species were felled and stem analyzed. The plots     \
  ranged in age from 51 to 217 years at breast height and ranged in site index \
  from 2.7 to 21.8 m.",

  /* SI_AT_CHEN */
  "Chen, H.Y.H., K. Klinka, and R.D. Kabzems. 1998. Height growth and site  \
  index models for trembling aspen (Populus tremuloides Michx.) in northern \
  British Columbia. Forest Ecology and Management 102:157-165.",
  "33 naturally established, undamaged, closed-canopy stands were sampled over \
  a wide range of sites in the Boreal White and Black Spruce zone of British   \
  Columbia. The site index curve is recommended to be used across the eastern  \
  portion of the Boreal White and Black Spruce zone for estimating site index  \
  of aspen stands aged 15 - 70 years at breast-height.",

  /* SI_DR_CHEN */
  "Chen, Han Y. H. 1999.",
  "",

  /* SI_PL_CHEN */
  "Chen, H.Y.H, and K. Klinka. 2000. Height growth models for high-elevation \
  subalpine fir, Engelmann, spruce, and lodgepole pine in British Columbia.  \
  West. J. Appl. For. 15: 62-69.",
  "The data for these curves come from 67 plots located in the ESSF zone of    \
  British Columbia. The plots were 20 x 20 m (0.04 ha) and the three largest   \
  dbh trees of the target species were felled and stem analyzed. The plots     \
  ranged in age from 50 to 114 years at breast height and ranged in site index \
  from 7.8 to 20.4 m.",

  /* SI_CWI_NIGH */
  "Nigh, G.D. 2000. Western redcedar site index models for the interior of \
  British Columbia. B.C. Min. For., Res. Br., Victoria, B.C. Res. Rep. 18. 24 p.",
  "The site index (height-age) and growth intercept models for western redcedar \
  in the interior of British Columbia were developed from 46 stem analysis      \
  plots established in ecologically uniform areas in the northern and southern  \
  portions of the ICH biogeoclimatic zone and the IDF zone. Plot ages ranged    \
  from 67 to 146 years at breast height and site index ranged from 10.50 to     \
  23.89 m. A years-to-breast-height function was also developed with these data.",

  /* SI_BP_CURTIS */
  "Curtis, R.O. 1990. Site index curves from stem analyses - methodology      \
  effects and a new technique applied to noble fir. USDA For. Serv., PNW Res. \
  Stn. Unpubl. Rep.",
  "The height-age (site index) curves were developed from stem analysis of 54  \
  trees taken from mixed species stands from Oregon and Washington. The sample \
  trees ranged in breast height age up to 240 years and in site index from     \
  approximately 8 m to 40 m.",

  /* SI_HWC_NIGHGI99 */
  "Nigh, G.D. 1999. Revised growth intercept models for coastal western \
  hemlock, Sitka spruce, and interior spruce. B.C. Min. For., Res. Br., \
  Victoria, B.C. Exten. Note 37. 8 p.",
  "The western hemlock growth intercept models were developed from 46 stem    \
  analysis plots established in ecologically uniform areas throughout the CWH \
  biogeoclimatic zone. Plot ages ranged from 50 to 173 years at breast-height \
  and site index from 7.7 to 38.1 m. These models were updated from the       \
  original (1996) models to reflect changes in the growth intercept modelling \
  technique.",

  /* SI_SS_NIGHGI99 */
  "Nigh, G.D. 1999. Revised growth intercept models for coastal western \
  hemlock, Sitka spruce, and interior spruce. B.C. Min. For., Res. Br., \
  Victoria, B.C. Exten. Note 37. 8 p.",
  "The Sitka spruce growth intercept models were developed from 38 stem analysis \
  plots established in ecologically uniform areas of Sitka spruce stands in the  \
  Queen Charlotte Islands. All plots were in the submontane wet hypermaritime    \
  Coast Western Hemlock (CWHwh1) biogeoclimatic variant. Plot ages ranged from   \
  50 to 121 years at breast-height and site index from 13.6 to 40.3 m. These     \
  models were updated from the original (1996) models to reflect changes in the  \
  growth intercept modelling technique.",

  /* SI_SW_NIGHGI99 */
  "Nigh, G.D. 1999. Revised growth intercept models for coastal western \
  hemlock, Sitka spruce, and interior spruce. B.C. Min. For., Res. Br., \
  Victoria, B.C. Exten. Note 37. 8 p.",
  "The interior spruce growth intercept models were developed from 87 stem      \
  analysis plots established throughout British Colulmbia. The plots were       \
  established under three different projects. Plot ages ranged from 50 to 209   \
  years at breast-height and site index from 5.98 to 25.52 m. These models were \
  updated from the original (1996) models to reflect changes in the growth      \
  intercept modelling technique.",

  /* SI_LW_NIGHGI */
  "Nigh, G.D., D. Brisco, and D. New. 1999. Growth intercept models for   \
  western larch. B.C. Min. For., Res. Br., Victoria, B.C. Exten. Note 38. \
  4 p.",
  "The western larch growth intercept models were developed from 99 stem       \
  analysis plots established by the University of British Columbia for a larch \
  productivity study. The plots were established to cover the geographic range \
  of western larch in British Columbia. Plot site indexes ranged from 9.7 to   \
  27.01 m.",

  /* SI_SW_NIGHTA */
  "Nigh, G.D. and B.A. Love. 2000. Juvenile height development in interior \
  spruce stands of British Columbia. West. J. Appl. For. 15: 117-121.",
  "The juvenile height model for interior spruce was developed from 39 stem    \
  analysis plots established in ecologically uniform areas in the SBSmc2,      \
  ICHmc1, ICHmc2, and ESSFmc biogeoclimatic subzones. Plot ages (total) ranged \
  from 17 to 33 years and site index ranged from 19.62 to 25.47 m. Functions   \
  for years to breast height and green-up age were derived from this model.",

  /* SI_CWI_NIGHGI */
  "", /* see CWI_NIGH */
  "", /* see CWI_NIGH */

  /* SI_SW_GOUDNIGH */
  "Nigh, G.D. and B.A. Love. 2000. Juvenile height development in interior      \
  spruce stands of British Columbia. West. J. Appl. For. 15: 117-121.           \
  Goudie, J.W. 1984. Height growth and site index curves for lodgepole pine and \
  white spruce and interim managed stand yield tables for lodgepole pine in     \
  British Columbia. B.C. Min. For., Res. Br. Unpubl. Rep. 75 p.",
  "These curves result from the splicing together of the juvenile height curves \
  by Nigh and Love (2000) and the height-age curves by Goudie (1984).",

  /* SI_HM_MEANS */
  "Means, J.E., M.H. Campbell, and G.P. Johnson. 1988. Preliminary    \
  height-growth and site-index curves for moutain hemlock. FIR Report \
  10(1): 8-9.",
  "The height-age curves for mountain hemlock were developed from 95 trees     \
  sampled in the Cascade mountains in Washington and Oregon. The stands from   \
  which the trees were sampled were unmanaged, and the trees were dominant or  \
  co-dominant with no signs of stem breakage or suppression. Most of the       \
  sample trees were between 150 and 350 years of age and the site index ranged \
  from 3 to 15 m (mean 8 m). The years to breast height function for coastal   \
  western hemlock is being used for mountain hemlock.",

  /* SI_SE_CHEN */
  "Chen, H.Y.H, and K. Klinka. 2000. Height growth models for high-elevation \
  subalpine fir, Engelmann, spruce, and lodgepole pine in British Columbia.  \
  West. J. Appl. For. 15: 62-69.",
  "The data for these curves come from 87 plots located in the ESSF zone of    \
  British Columbia. The plots were 20 x 20 m (0.04 ha) and the three largest   \
  dbh trees of the target species were felled and stem analyzed. The plots     \
  ranged in age from 50 to 164 years at breast height and ranged in site index \
  from 5.2 to 25.0 m.",

  /* SI_FDC_NIGHTA */
  "Nigh, G.D. and M.G. Mitchell. 2003. Development of height-age models for \
  estimating juvenile height of coastal Douglas-fir in British Columbia.    \
  West. J. Appl. For. 18: 207-212.",
  "The juvenile height models for coastal Douglas-fir were developed from 100    \
  trees located throughout the range of Douglas-fir on the coast of British      \
  Columbia. The data come from 100 - 0.01 ha plots; one site tree was sampled    \
  from each plot. Each site tree was split and its height growth was measured    \
  from the pith nodes. The ages of the trees ranged from 15 to 42 years in total \
  age, and the site index ranged from 16 to 44.5 m at breast height age 50.      \
  The curves are restricted for use from total age 0 to total age 25. There are  \
  no restrictions in the range of site index, but should be used cautiously      \
  outside the range of sampled site indices.                                     \
  There are accompanying years to breast height and green-up age (years          \
  to 3 m height) models.",

  /* SI_FDC_BRUCENIGH */
  "Nigh, G.D. and M.G. Mitchell. 2003. Development of height-age models for  \
  estimating juvenile height of coastal Douglas-fir in British Columbia.     \
  West. J. Appl. For. 18: 207-212.                                           \
  Bruce, David. 1981. Consistent height-growth and growth-rate estimates for \
  remeasured plots. For. Sci. 27:711-725.                                    \
  Nigh, G.D. and K.R. Polsson. 2002. Splicing height curves. B.C. Min. For., \
  Res. Br., Victoria, B.C. Exten. Note 60.",
  "The Bruce curves were developed from re-measured PSPs in Washington, Oregon, \
  and B.C. The plots covered a wide range of sites up to about 80 yrs bha for   \
  both natural and planted stands. The Nigh / Mitchell curves were developed    \
  from 104 plots located in juvenile managed stands in southwestern B.C.        \
  These two models were spliced together. Note that the final spliced           \
  models differ slightly from the Nigh / Polsson publication.",

  /* SI_LW_NIGH */
  "Brisco, D., K. Klinka, and G. Nigh. 2002. Height growth models for western \
  larch in British Columbia. West. J. Appl. For. 17: 66-74.",
  "The western larch height-age curves were developed from 105 - 0.04 ha plots \
  established throughout the range of western larch in British Columbia. Three \
  trees were sampled in each plot. The stem analysis data were collected as    \
  part of a larch productivity study conducted by researchers at the           \
  University of British Columbia. The ages of the plots ranged from 45 to 134  \
  years at breast height and the site indices ranged from 9.7 m to 27.1 m.     \
  These curves are based on the Chapman-Richards function.",

  /* SI_SB_NIGH */
  "Nigh, G.D., P.V. Krestov, and K. Klinka. 2002. Height growth of black spruce \
  in British Columbia. For. Chron. 78: 306-313.",
  "The data for the black spruce height-age curves consist of 91 stem analysis  \
  plots established as part of a black spruce productivity study by researchers \
  at UBC. These plots are located in the BWBS and SBS biogeoclimatics zone of   \
  British Columbia. The breast height ages of the plots range up to 174 years   \
  and their site index range is from 4.98 m up to 17.09 m. These curves can be  \
  used for black spruce throughout British Columbia.",

  /* SI_AT_NIGH */
  "Nigh, G.D., P.V. Krestov, and K. Klinka. 2002. Trembling aspen height-age \
  models for British Columbia. Northwest Sci. Vol. 36, No. 3.",
  "The 135 plots for the trembling aspen height-age curves come from a trembling \
  aspen productivity study done by researchers at UBC. The plots were            \
  established in the BWBS, SBS, SBPS, IDF, MS, and ICH biogeoclimatic zones.     \
  They range in age from 50 to 177 years at breast height, and from site         \
  indexes 5.60 m to 29.56 m. These curves are recommended for use throughout     \
  British Columbia.",

  /* SI_BL_CHENAC */
  "Chen, H.Y.H, and K. Klinka. 2000. Height growth models for high-elevation \
  subalpine fir, Engelmann, spruce, and lodgepole pine in British Columbia.  \
  West. J. Appl. For. 15: 62-69.",
  "The data for these curves come from 165 plots located in the ESSF zone of    \
  British Columbia. The plots were 20 x 20 m (0.04 ha) and the three largest    \
  dbh trees of the target species were felled and stem analyzed. The plots      \
  ranged in age from 51 to 217 years at breast height and ranged in site index  \
  from 2.7 to 21.8 m.                                                           \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_BP_CURTISAC */
  "Curtis, R.O. 1990. Site index curves from stem analyses - methodology      \
  effects and a new technique applied to noble fir. USDA For. Serv., PNW Res. \
  Stn. Unpubl. Rep.",
  "The height-age (site index) curves were developed from stem analysis of 54   \
  trees taken from mixed species stands from Oregon and Washington. The sample  \
  trees ranged in breast height age up to 240 years and in site index from      \
  approximately 8 m to 40 m.                                                    \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.37 to 0.5,1.37.",

  /* SI_HM_MEANSAC */
  "Means, J.E., M.H. Campbell, and G.P. Johnson. 1988. Preliminary    \
  height-growth and site-index curves for moutain hemlock. FIR Report \
  10(1): 8-9.",
  "The height-age curves for mountain hemlock were developed from 95 trees      \
  sampled in the Cascade mountains in Washington and Oregon. The stands from    \
  which the trees were sampled were unmanaged, and the trees were dominant or   \
  co-dominant with no signs of stem breakage or suppression. Most of the        \
  sample trees were between 150 and 350 years of age and the site index ranged  \
  from 3 to 15 m (mean 8 m). The years to breast height function for coastal    \
  western hemlock is being used for mountain hemlock.                           \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.37 to 0.5,1.37.",

  /* SI_FDI_THROWERAC */
  "Thrower, James S. and James W. Goudie. 1992. Estimating dominant height and      \
  site index for even-aged interior Douglas-fir in British Columbia. West. J. Appl. \
  For. 7(1):20-25.",
  "The site index curves were developed from stem analysis of 262 dominant trees in   \
  68 plots located in even-aged Douglas-fir stands throughout the interior of British \
  Columbia. The curves were developed from plots ranging in site index from 8 to      \
  30 m and up to 100 years breast-height age. On high sites, 30 m and greater, the    \
  curves may over-estimate height growth at older ages.                               \
  Note: the formulation was modified in 2003 to move the age,height origin from       \
  0,1.3 to 0.5,1.3.",

  /* SI_ACB_HUANGAC */
  "Huang Shongming, Stephen J. Titus and Tom W. Lakusta. 1994. \
  Ecologically based site index curves and tables for major    \
  Alberta tree species.                                        \
  Ab. Envir. Prot., Land For. Serv., For. Man. Division,       \
  Tech. Rep. 307-308, Edmonton, Ab.",
  "The height-age (site index) curves were developed from stem                  \
  analysis of 148 balsam poplar (Populus balsamifera spp. balsamifera)          \
  trees from different geographic regions of Alberta.                           \
  Site index ranged from about 10 to 28 m at 50 years                           \
  breast-height age and included trees up to 130 years old.                     \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_PW_CURTISAC */
  "Curtis, Robert O., N. M. Diaz, and G. W. Clendenen. 1990. Height growth and \
  site index curves for western white pine in the Cascade Range of Western     \
  Washington and Oregon. U.S. Dep. Agric. For. Serv. Res. Pap. RNW-PR-423.     \
  14 p.",
  "The height-age (site index) curves were developed from stem analysis of 38   \
  dominant and co-dominant western white pine trees located throughout the      \
  Cascade Range of Washington and Oregon. Site index ranged from about 9 to 31  \
  m at 50 years breast height and included trees up to 200 years old.           \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.37 to 0.5,1.37.",

  /* SI_HWC_WILEYAC */
  "Wiley, Kenneth N. 1978. Site index tables for western hemlock in the \
  Pacific Northwest. Weyerhaeuser Co., For. Res. Cent. For. Pap. 17. 28 p.",
  "The site index (height-age) curves were developed from stem analysis data        \
  collected from 90 plots in Washington and Oregon. The plots ranged from site      \
  index 18 to 40 m and from about 60 to 130 years breast-height age. The height-    \
  age equation should not be used for ages less than 10 years. In British Columbia, \
  MacMillan Bloedel Ltd. calibrated these curves to better represent the local      \
  growing conditions.                                                               \
  Note: the formulation was modified in 2003 to move the age,height origin from     \
  0,1.37 to 0.5,1.37.",

  /* SI_FDC_BRUCEAC */
  "Bruce, David. 1981. Consistent height-growth and growth-rate estimates for \
  remeasured plots. For. Sci. 27:711-725.",
  "The site index (height-age) curves were developed from remeasured Douglas-fir     \
  (Pseudotsuga menziesii) permanent sample plots in Washington, Oregon, and          \
  British Columbia. The plots covered a wide range of sites up to about 80 years     \
  breast-height age for both natural and planted stands. Tests have shown that these \
  curves reasonably portray the height growth of dominant, undamaged second- and     \
  old-growth trees on coastal British Columbia. Bruce's curves are very similar to   \
  those given by J. E. King (1966. Site index curves for Douglas-fir in the Pacific  \
  Northwest. Weyerhaeuser Co., For. Res. Cent. For. Pap. 8. 49p.).                   \
  Note: the formulation was modified in 2003 to move the age,height origin from      \
  0,1.37 to 0.5,1.37.",

  /* SI_CWC_KURUCZAC */
  "This 1985 formulation is an updated version of the curves given in 1978 by   \
  Kurucz 1978. Kurucz, John F. 1978. Preliminary, polymorphic site index curves \
  for western redcedar (Thuja plicata Donn) in coastal British Columbia.        \
  MacMillan Bloedel For. Res. Note No. 3. 14 p. + appendix.",
  "The height-age (site index) curves were developed from stem analysis of         \
  undamaged, dominant and co-dominant trees located in approximately 50 stands     \
  throughout Vancouver Island and the mid-coast region of the mainland. The        \
  sample trees ranged in breast-height age from 33 to 285 years and in site index  \
  from 8 to 37 m. Kurucz suggested using this formulation with caution for breast- \
  height ages less than 10 years and for site indexes greater than 37 m.           \
  Note: the formulation was modified in 2003 to move the age,height origin from    \
  0,1.3 to 0.5,1.3.",

  /* SI_BA_KURUCZ82AC */
  "Kurucz, John F. 1982. Report on Project 933-3. Polymorphic site-index curves    \
  for balsam -Abies amabilis- in coastal British Columbia, MacMillan Bloedel Ltd., \
  Resource Economics Section, Woodlands Services, Rep. on Project 933-3. 24 p.     \
  + app. Nanaimo, BC.",
  "The height-age (site index) curves were developed from stem analysis of 199   \
  undamaged, dominant Amabilis fir (Abies amabilis) trees from 50 plots located  \
  throughout the coastal region of British Columbia. Plot ages ranged from 50 to \
  160 years at breast height and site index ranged from 11 to 34 m. The          \
  discontinuity in the height-age curve at age 50 is caused by the adjustment    \
  equation to reduce bias at ages below 50 and is exaggerated by extending the   \
  equation beyond the range of the site index from which it was developed.       \
  Note: the formulation was modified in 2003 to move the age,height origin from  \
  0,1.3 to 0.5,1.3.",

  /* SI_ACT_THROWERAC */
  "J. S. Thrower and Associates Ltd. 1992. Height-age/site-index curves for Black \
  Cottonwood in British Columbia. Ministry of Forests, Inventory Branch. Project  \
  92-07-IB, 21p.",
  "The height-age (site index) curves were developed from 25 stem analysis plots of \
  black cottonwood (Populus balsamifera spp. trichocarpa) located in three          \
  geographic regions of coastal British Columbia. Site index ranged from about 15   \
  to 35 m at 50 years breast-height age and included trees up to 150 years old.     \
  Note: the formulation was modified in 2003 to move the age,height origin from     \
  0,1.3 to 0.5,1.3.",

  /* SI_PY_HANNAC */
  "Hann, D. W. and J. A. Scrivani. 1987. Dominant height growth and site index  \
  equations for Douglas-fir and ponderosa pine in southwest Oregon. Oreg. State \
  Univ. For. Res. Lab., Corvallis Oreg., Res. Bull. 59. 13 p.",
  "The height-age (site index curves) were developed from stem analysis of 41 trees \
  located throughout southwest Oregon. Selected trees came from natural, even-      \
  and uneven-aged, second-growth stands. Site index ranged from 19 to 34 m and      \
  from about 50 to 148 years breast-height age. Most stem analysis trees were       \
  under 120 years.                                                                  \
  Note: the formulation was modified in 2003 to move the age,height origin from     \
  0,1.37 to 0.5,1.37.",

  /* SI_SE_CHENAC */
  "Chen, H.Y.H, and K. Klinka. 2000. Height growth models for high-elevation \
  subalpine fir, Engelmann, spruce, and lodgepole pine in British Columbia.  \
  West. J. Appl. For. 15: 62-69.",
  "The data for these curves come from 87 plots located in the ESSF zone of     \
  British Columbia. The plots were 20 x 20 m (0.04 ha) and the three largest    \
  dbh trees of the target species were felled and stem analyzed. The plots      \
  ranged in age from 50 to 164 years at breast height and ranged in site index  \
  from 5.2 to 25.0 m.                                                           \
  Note: the formulation was modified in 2003 to move the age,height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_SW_GOUDIE_NATAC */
  "Goudie, James W. 1984. Height growth and site index curves for lodgepole pine \
  and white spruce and interim managed stand yield tables for lodgepole pine in  \
  British Columbia. B.C. Min. For., Res. Br. Unpubl. Rep. 75 p.",
  "The height-age (site index) curves were developed from stem analysis of 188  \
  dominant and co-dominant trees located throughout Alberta and Eastern British \
  Columbia. Plots ranged in site index from about 6 to 22 m at 50 years breast  \
  height, and in age from 10 to 150 years.                                      \
  Note: the formulation was modified in 2004 to move the age,height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_PY_NIGH */
  "Nigh, G.D. 2004. A comparison of fitting techniques for ponderosa pine height-age \
  models in British Columbia. Ann. For. Sci. 61: 609-615.                            \
  Nigh, G.D. 2002. Growth intercept, years-to-breast-height, and juvenile height     \
  growth models for ponderosa pine. Res. Br., B.C. Min. For., Victoria, B.C. Tech. Rep. 2.",
  "The hybrid model used herein consists of a Juvenile Height Growth model spliced to a   \
  Site Index model, at breast height. These models were developed from 80 ponderosa pine  \
  stem analysis plots. The plots were distributed across the range of ponderosa pine in   \
  British Columbia, specifically from the BG, PP, IDF, and ICH biogeoclimatic zones. The  \
  site index for these plots ranged from 5.01 m to 24.78 m and the ages ranged from 74 to \
  227 years at breast height.",

  /* SI_PY_NIGHGI */
  "Nigh, G.D. 2002. Growth intercept, years-to-breast-height, and juvenile height \
  growth models for ponderosa pine. Res. Br., B.C. Min. For., Victoria, B.C. Tech. Rep. 2.",
  "",

  /* SI_PLI_NIGHTA2004 */
  "Nigh, G.D. 2004. Juvenile height models for lodgepole pine and   \
  interior spruce: validation of existing models and development of \
  new models. B.C. Min. For., Res. Br., Victoria, B.C. Res. Rep. 25.",
  "New juvenile height models for lodgepole pine were developed with data                   \
  collected from the BWBS, ESSF, ICH, IDF, MS, SBS, and SBPS biogeoclimatic zones. The data \
  included 65 plots. The models extend the                                                  \
  geographic and site index range of the original juvenile height models. These models are  \
  applicable for estimating stands up to total age 15.                                      \
  The site index range is 16.25 to 24.78 m.",

  /* SI_SE_NIGHTA */
  "Nigh, G.D. 2004. Juvenile height models for lodgepole pine and   \
  interior spruce: validation of existing models and development of \
  new models. B.C. Min. For., Res. Br., Victoria, B.C. Res. Rep. 25.",
  "New juvenile height models for interior spruce were developed with data                  \
  collected from the BWBS, ESSF, ICH, IDF, MS, SBS, and SBPS biogeoclimatic zones. The data \
  included 57 plots. The models extend the                                                  \
  geographic and site index range of the original juvenile height models. These models are  \
  applicable for estimating stands up to total age 20.                                      \
  The site index range is 17.01 to 30.48 m.",

  /* SI_SW_NIGHTA2004 */
  "Nigh, G.D. 2004. Juvenile height models for lodgepole pine and   \
  interior spruce: validation of existing models and development of \
  new models. B.C. Min. For., Res. Br., Victoria, B.C. Res. Rep. 25.",
  "New juvenile height models for interior spruce were developed with data                  \
  collected from the BWBS, ESSF, ICH, IDF, MS, SBS, and SBPS biogeoclimatic zones. The data \
  included 57 plots. The models extend the                                                  \
  geographic and site index range of the original juvenile height models. These models are  \
  applicable for estimating stands up to total age 20.                                      \
  The site index range is 17.01 to 30.48 m.",

  /* SI_SW_GOUDIE_PLAAC */
  "Goudie, James W. 1984. Height growth and site index curves for lodgepole pine \
  and white spruce and interim managed stand yield tables for lodgepole pine in  \
  British Columbia. B.C. Min. For., Res. Br. Unpubl. Rep. 75 p.",
  "The height-age (site index) curves were developed from stem analysis of 188   \
  dominant and co-dominant trees located throughout Alberta and Eastern British  \
  Columbia. Plots ranged in site index from about 6 to 22 m at 50 years breast   \
  height, and in age from 10 to 150 years.                                       \
  Note: the formulation was modified in 2004 to move the age, height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_PJ_HUANG */
  "Huang, S. Subregion-based compatible height and site index models for young \
  and mature stands in Alberta: revisions and summaries (Part II).             \
  Alberta Environmental Protection. Land and Forest Service.                   \
  Forest Management Research Note No. 10.",
  "Subregion-based compatible height and site index models expressed in the     \
  form of H=f(SI,age) were developed for major Alberta tree species. All models \
  fitted the data reasonably well across the full range of breast height age    \
  classes. They can be used for growth intercept models for young trees/stands, \
  juvenile height and site index models, and regular height and site index      \
  models for mature trees/stands.",

  /* SI_PJ_HUANGAC */
  "Huang, S. Subregion-based compatible height and site index models for young \
  and mature stands in Alberta: revisions and summaries (Part II).             \
  Alberta Environmental Protection. Land and Forest Service.                   \
  Forest Management Research Note No. 10.",
  "Subregion-based compatible height and site index models expressed in the      \
  form of H=f(SI,age) were developed for major Alberta tree species. All models  \
  fitted the data reasonably well across the full range of breast height age     \
  classes. They can be used for growth intercept models for young trees/stands,  \
  juvenile height and site index models, and regular height and site index       \
  models for mature trees/stands.                                                \
  Note: the formulation was modified in 2004 to move the age, height origin from \
  0,1.3 to 0.5,1.3.",

  /* SI_SW_NIGHGI2004 */
  "Nigh, G.D. 2004. Growth intercept and site series-based estimates of site \
  index for white spruce in the boreal white and black spruce biogeoclimatic \
  zone. B.C. Min. For., Res. Br.,                                            \
  Victoria, B.C. Tech. Rep. 013. 8 p.",
  "",

  /* SI_EP_NIGH */
  "Nigh, G.D., K.D. Thomas, K. Yearsley, and J. Wang.  2009. Site-dependent height-age models for paper birch in British Columbia. Northwest Sci. 83: 253-261.",
  "These height-age index curves were developed from stem analysis of 168 dominant trees in 61 plots located in even-aged stands dominated by paper birch in the SBS, ICH, and IDF biogeoclimatic zones in the interior of British Columbia. The curves were developed from plots ranging in site index from 11 to 26 m and up to 125 years breast-height age.",

  /* SI_BA_NIGHGI */
  "Nigh, G.D. 2009. Amabilis fir height-age and growth intercept models for British Columbia. B.C. Min. For. Range, For. Sci. Prog., Victoria, B.C. Res. Rep. 30. www.for.gov.bc.ca/hfd/pubs/Docs/Rr/Rr30.htm",
  "The height-age (site index) curves were developed from stem analysis of 74 plots of undamaged, dominant amabilis fir (Abies amabilis) located throughout the coastal region of British Columbia. Plot ages ranged from 50 to 220 years at breast height and site index ranged from 11 to 36 m. The data set used to develop these models includes the Kurucz (1982) data and new data collected in 2008.",

  /* SI_BA_NIGH */
  "",
  "",

  /* SI_SW_HU_GARCIA */
  "",
  "",

  /* SI_SE_NIGHGI */
  "Nigh, G.D. (2014). An Errors-in-Variable Model with Correlated Errors: \
  Engelmann Spruce Growth Intercept Models. For. Anal. Inv. Br., B.C.     \
  Min. For., Lands, Nat. Resour. Oper., Victoria, B.C. Tech. Rep. 084.",
  "The growth intercept models were developed from 84 stem analysis plots   \
  located throughout the range of the Engelmann Spruce  Subalpine Fir      \
  (ESSF) biogeoclimatic zone of British Columbia. Plots ranged in site      \
  index from about 6 to 24 m. The models can be used to estimate site index \
  throughout the ESSF zone in British Columbia.",

  /* SI_SE_NIGH */
  "Nigh, G. 2015. Engelmann spruce site index models: a comparison of model functions and parameterizations. PLoS ONE 10(4): e0124079. doi: 10.1371/journal.pone.0124079.",
  "The curves were developed from 84 Engelmann spruce trees located throughout the range of the ESSF biogeoclimatic zone. The age of the sample trees ranged from 70 to 255 years at breast height and their heights ranged from 7.84 to 40.79 m. The range in site index was 5.58 to 24.22 m.",

  /* SI_CWC_NIGH */
  "Nigh, G.D. 2016. Revised site index models for western redcedar for coastal British Columbia. Prov. B.C., Victoria, B.C. Tech. Rep. 105.",
  "The site index models were developed from the stem analysis of 63 trees from 4 sources of data. Pseudo-height/age data were obtained from the Kurucz (1978) site index models and were supplemented with data from a wood quality study, and with data kindly donated by McMillan-Bloedel and Radwan and Harrington. The pseudo-data were generated so that the ages and site indexes corresponded to the original Kurucz data set. The other data were from trees less than 95 years old. The original g-GADA formulation of this model required iterating to estimate one of the model parameters. An ad hoc equation to predict this parameter from site index was developed and implemented.",

  // SI_PLI_NIGH
  "Nigh, G. 2017. Development of a lodgepole pine site index model with the grounded-Generalized Algebraic Difference Approach (g-GADA), B.C. Min. For. Range, For. Sci. Prog., Victoria, B.C. Res. Rep. 31.",
  "The site index model was developed from 174 plots of stem analysis data that were established throughout the interior of British Columbia. The breast height ages of the trees ranged from 46 to 210 years of age and the total heights ranged from 12.47 to 34.32m. Site index ranged from 9.9m to 27.38m.",
  };


#ifdef HOOP
/* SI_PJ_KER */
"", /* same as SI_SB_KER */
"The data for this curve consist of 114 trees taken from 12 m radius plots (3 \
or 4 trees per plot) established in mature and overmature stands in New       \
Brunswick. The trees ranged in age from 51 to 175 years at breast height and  \
ranged in site index from 7.2 m to 21.0 m at 50 years breast height age.",
#endif

static char si_sclist_start[SI_MAX_SPECIES] =
  {
  SI_A_START,
  SI_ABAL_START,
  SI_ABCO_START,
  SI_AC_START,
  SI_ACB_START,
  SI_ACT_START,
  SI_AD_START,
  SI_AH_START,
  SI_AT_START,
  SI_AX_START,
  SI_B_START,
  SI_BA_START,
  SI_BB_START,
  SI_BC_START,
  SI_BG_START,
  SI_BI_START,
  SI_BL_START,
  SI_BM_START,
  SI_BP_START,
  //  SI_BV_START,
  SI_C_START,
  SI_CI_START,
  SI_CP_START,
  SI_CW_START,
  SI_CWC_START,
  SI_CWI_START,
  SI_CY_START,
  SI_D_START,
  SI_DG_START,
  SI_DM_START,
  SI_DR_START,
  SI_E_START,
  SI_EA_START,
  SI_EB_START,
  SI_EE_START,
  SI_EP_START,
  SI_ES_START,
  SI_EW_START,
  SI_EXP_START,
  SI_EY_START,
  SI_FD_START,
  SI_FDC_START,
  SI_FDI_START,
  SI_G_START,
  SI_GP_START,
  SI_GR_START,
  SI_H_START,
  SI_HM_START,
  SI_HW_START,
  SI_HWC_START,
  SI_HWI_START,
  SI_HXM_START,
  SI_IG_START,
  SI_IS_START,
  SI_J_START,
  SI_JR_START,
  SI_JS_START,
  SI_K_START,
  SI_KC_START,
  SI_L_START,
  SI_LA_START,
  SI_LD_START,
  SI_LE_START,
  SI_LS_START,
  SI_LT_START,
  SI_LW_START,
  SI_M_START,
  SI_MB_START,
  SI_ME_START,
  SI_MN_START,
  SI_MR_START,
  SI_MS_START,
  SI_MV_START,
  SI_OA_START,
  SI_OB_START,
  SI_OC_START,
  SI_OD_START,
  SI_OE_START,
  SI_OF_START,
  SI_OG_START,
  SI_OH_START,
  SI_OI_START,
  SI_OJ_START,
  SI_OK_START,
  SI_P_START,
  SI_PA_START,
  SI_PF_START,
  SI_PJ_START,
  SI_PL_START,
  SI_PLC_START,
  SI_PLI_START,
  SI_PM_START,
  SI_PR_START,
  SI_PS_START,
  SI_PW_START,
  SI_PXJ_START,
  SI_PY_START,
  SI_Q_START,
  SI_QE_START,
  SI_QG_START,
  SI_QW_START,
  SI_R_START,
  SI_RA_START,
  SI_S_START,
  SI_SA_START,
  SI_SB_START,
  SI_SE_START,
  SI_SI_START,
  SI_SN_START,
  SI_SS_START,
  SI_SW_START,
  SI_SX_START,
  SI_SXB_START,
  SI_SXE_START,
  SI_SXL_START,
  SI_SXS_START,
  SI_SXW_START,
  SI_SXX_START,
  SI_T_START,
  SI_TW_START,
  SI_U_START,
  SI_UA_START,
  SI_UP_START,
  SI_V_START,
  SI_VB_START,
  SI_VP_START,
  SI_VS_START,
  SI_VV_START,
  SI_W_START,
  SI_WA_START,
  SI_WB_START,
  SI_WD_START,
  SI_WI_START,
  SI_WP_START,
  SI_WS_START,
  SI_WT_START,
  SI_X_START,
  SI_XC_START,
  SI_XH_START,
  SI_Y_START,
  SI_YC_START,
  SI_YP_START,
  SI_Z_START,
  SI_ZC_START,
  SI_ZH_START,
  };


static char si_curve_default[SI_MAX_SPECIES] =
  {
  SI_ERR_NO_ANS,          // A
  SI_ERR_NO_ANS,          // ABAL
  SI_ERR_NO_ANS,          // ABCO
  SI_ERR_NO_ANS,          // AC
  SI_ACB_HUANGAC,         // ACB
  SI_ACT_THROWERAC,       // ACT
  SI_ERR_NO_ANS,          // AD
  SI_ERR_NO_ANS,          // AH
  SI_AT_NIGH,             // AT
  SI_ERR_NO_ANS,          // AX
  SI_ERR_NO_ANS,          // B
  SI_BA_NIGH,             // BA
  SI_ERR_NO_ANS,          // BB
  SI_ERR_NO_ANS,          // BC
  SI_ERR_NO_ANS,          // BG
  SI_ERR_NO_ANS,          // BI
  SI_BL_CHENAC,           // BL
  SI_ERR_NO_ANS,          // BM
  SI_BP_CURTISAC,         // BP
  SI_ERR_NO_ANS,          // C
  SI_ERR_NO_ANS,          // CI
  SI_ERR_NO_ANS,          // CP
  SI_ERR_NO_ANS,          // CW
  SI_CWC_NIGH,            // CWC
  SI_CWI_NIGH,            // CWI
  SI_ERR_NO_ANS,          // CY
  SI_ERR_NO_ANS,          // D
  SI_ERR_NO_ANS,          // DG
  SI_ERR_NO_ANS,          // DM
  SI_DR_NIGH,             // DR
  SI_ERR_NO_ANS,          // E
  SI_ERR_NO_ANS,          // EA
  SI_ERR_NO_ANS,          // EB
  SI_ERR_NO_ANS,          // EE
  SI_EP_NIGH,             // EP
  SI_ERR_NO_ANS,          // ES
  SI_ERR_NO_ANS,          // EW
  SI_ERR_NO_ANS,          // EXP
  SI_ERR_NO_ANS,          // EY
  SI_ERR_NO_ANS,          // FD
  SI_FDC_BRUCEAC,         // FDC
  SI_FDI_THROWERAC,       // FDI
  SI_ERR_NO_ANS,          // G
  SI_ERR_NO_ANS,          // GP
  SI_ERR_NO_ANS,          // GR
  SI_ERR_NO_ANS,          // H
  SI_HM_MEANSAC,          // HM
  SI_ERR_NO_ANS,          // HW
  SI_HWC_WILEYAC,         // HWC
  SI_HWI_NIGH,            // HWI
  SI_ERR_NO_ANS,          // HXM
  SI_ERR_NO_ANS,          // IG
  SI_ERR_NO_ANS,          // IS
  SI_ERR_NO_ANS,          // J
  SI_ERR_NO_ANS,          // JR
  SI_ERR_NO_ANS,          // JS
  SI_ERR_NO_ANS,          // K
  SI_ERR_NO_ANS,          // KC
  SI_ERR_NO_ANS,          // L
  SI_ERR_NO_ANS,          // LA
  SI_ERR_NO_ANS,          // LD
  SI_ERR_NO_ANS,          // LE
  SI_ERR_NO_ANS,          // LS
  SI_ERR_NO_ANS,          // LT
  SI_LW_NIGH,             // LW
  SI_ERR_NO_ANS,          // M
  SI_ERR_NO_ANS,          // MB
  SI_ERR_NO_ANS,          // ME
  SI_ERR_NO_ANS,          // MN
  SI_ERR_NO_ANS,          // MR
  SI_ERR_NO_ANS,          // MS
  SI_ERR_NO_ANS,          // MV
  SI_ERR_NO_ANS,          // OA
  SI_ERR_NO_ANS,          // OB
  SI_ERR_NO_ANS,          // OC
  SI_ERR_NO_ANS,          // OD
  SI_ERR_NO_ANS,          // OE
  SI_ERR_NO_ANS,          // OF
  SI_ERR_NO_ANS,          // OG
  SI_ERR_NO_ANS,          // OH
  SI_ERR_NO_ANS,          // OI
  SI_ERR_NO_ANS,          // OJ
  SI_ERR_NO_ANS,          // OK
  SI_ERR_NO_ANS,          // P
  SI_ERR_NO_ANS,          // PA
  SI_ERR_NO_ANS,          // PF
  SI_PJ_HUANGAC,          // PJ
  SI_ERR_NO_ANS,          // PL
  SI_ERR_NO_ANS,          // PLC
  SI_PLI_THROWER,         // PLI
  SI_ERR_NO_ANS,          // PM
  SI_ERR_NO_ANS,          // PR
  SI_ERR_NO_ANS,          // PS
  SI_PW_CURTISAC,         // PW
  SI_ERR_NO_ANS,          // PXJ
  SI_PY_NIGH,             // PY
  SI_ERR_NO_ANS,          // Q
  SI_ERR_NO_ANS,          // QE
  SI_ERR_NO_ANS,          // QG
  SI_ERR_NO_ANS,          // QW
  SI_ERR_NO_ANS,          // R
  SI_ERR_NO_ANS,          // RA
  SI_ERR_NO_ANS,          // S
  SI_ERR_NO_ANS,          // SA
  SI_SB_NIGH,             // SB
  SI_SE_NIGH,             // SE
  SI_ERR_NO_ANS,          // SI
  SI_ERR_NO_ANS,          // SN
  SI_SS_NIGH,             // SS
  SI_SW_GOUDIE_PLAAC,     // SW
  SI_ERR_NO_ANS,          // SX
  SI_ERR_NO_ANS,          // SXB
  SI_ERR_NO_ANS,          // SXE
  SI_ERR_NO_ANS,          // SXL
  SI_ERR_NO_ANS,          // SXS
  SI_ERR_NO_ANS,          // SXW
  SI_ERR_NO_ANS,          // SXX
  SI_ERR_NO_ANS,          // T
  SI_ERR_NO_ANS,          // TW
  SI_ERR_NO_ANS,          // U
  SI_ERR_NO_ANS,          // UA
  SI_ERR_NO_ANS,          // UP
  SI_ERR_NO_ANS,          // V
  SI_ERR_NO_ANS,          // VB
  SI_ERR_NO_ANS,          // VP
  SI_ERR_NO_ANS,          // VS
  SI_ERR_NO_ANS,          // VV
  SI_ERR_NO_ANS,          // W
  SI_ERR_NO_ANS,          // WA
  SI_ERR_NO_ANS,          // WB
  SI_ERR_NO_ANS,          // WD
  SI_ERR_NO_ANS,          // WI
  SI_ERR_NO_ANS,          // WP
  SI_ERR_NO_ANS,          // WS
  SI_ERR_NO_ANS,          // WT
  SI_ERR_NO_ANS,          // X
  SI_ERR_NO_ANS,          // XC
  SI_ERR_NO_ANS,          // XH
  SI_ERR_NO_ANS,          // Y
  SI_ERR_NO_ANS,          // YC
  SI_ERR_NO_ANS,          // YP
  SI_ERR_NO_ANS,          // Z
  SI_ERR_NO_ANS,          // ZC
  SI_ERR_NO_ANS,          // ZH
  };

static char si_curve_intend[SI_MAX_CURVES] =
  {
  SI_SPEC_ACB, /* SI_ACB_HUANG */
  SI_SPEC_ACT, /* SI_ACT_THROWER */
  SI_SPEC_AT, /* SI_AT_HUANG */
  SI_SPEC_AT, /* SI_AT_CIESZEWSKI */
  SI_SPEC_AT, /* SI_AT_GOUDIE */
  SI_SPEC_BA, /* SI_BA_DILUCCA */
  SI_SPEC_BA,      /* should be SI_SPEC_BB */ /* SI_BB_KER */
  SI_SPEC_BA, /* SI_BA_KURUCZ86 */
  SI_SPEC_BA, /* SI_BA_KURUCZ82 */
  SI_SPEC_BL, /* SI_BL_THROWERGI */
  SI_SPEC_BL, /* SI_BL_KURUCZ82 */
  SI_SPEC_CWC, /* SI_CWC_KURUCZ */
  SI_SPEC_CWC, /* SI_CWC_BARKER */
  SI_SPEC_DR, /* SI_DR_NIGH */
  SI_SPEC_DR, /* SI_DR_HARRING */
  SI_SPEC_FDC, /* SI_FDC_NIGHGI */
  SI_SPEC_FDC, /* SI_FDC_BRUCE */
  SI_SPEC_FDC, /* SI_FDC_COCHRAN */
  SI_SPEC_FDC, /* SI_FDC_KING */
  SI_SPEC_FDI, /* SI_FDI_NIGHGI */
  SI_SPEC_FDI, /* SI_FDI_HUANG_PLA */
  SI_SPEC_FDI, /* SI_FDI_HUANG_NAT */
  SI_SPEC_FDI, /* SI_FDI_MILNER */
  SI_SPEC_FDI, /* SI_FDI_THROWER */
  SI_SPEC_FDI, /* SI_FDI_VDP_MONT */
  SI_SPEC_FDI, /* SI_FDI_VDP_WASH */
  SI_SPEC_FDI, /* SI_FDI_MONS_DF */
  SI_SPEC_FDI, /* SI_FDI_MONS_GF */
  SI_SPEC_FDI, /* SI_FDI_MONS_WRC */
  SI_SPEC_FDI, /* SI_FDI_MONS_WH */
  SI_SPEC_FDI, /* SI_FDI_MONS_SAF */
  SI_SPEC_HWC, /* SI_HWC_NIGHGI */
  SI_SPEC_HWC, /* SI_HWC_FARR */
  SI_SPEC_HWC, /* SI_HWC_BARKER */
  SI_SPEC_HWC, /* SI_HWC_WILEY */
  SI_SPEC_HWC, /* SI_HWC_WILEY_BC */
  SI_SPEC_HWC, /* SI_HWC_WILEY_MB */
  SI_SPEC_HWI, /* SI_HWI_NIGH */
  SI_SPEC_HWI, /* SI_HWI_NIGHGI */
  SI_SPEC_LW, /* SI_LW_MILNER */
  SI_SPEC_PLI, /* SI_PLI_THROWNIGH */
  SI_SPEC_PLI, /* SI_PLI_NIGHTA98 */
  SI_SPEC_PLI, /* SI_PLI_NIGHGI97 */
  SI_SPEC_PLI, /* SI_PLI_HUANG_PLA */
  SI_SPEC_PLI, /* SI_PLI_HUANG_NAT */
  SI_SPEC_PLI, /* SI_PLI_THROWER */
  SI_SPEC_PLI, /* SI_PLI_MILNER */
  SI_SPEC_PLI, /* SI_PLI_CIESZEWSKI */
  SI_SPEC_PLI, /* SI_PLI_GOUDIE_DRY */
  SI_SPEC_PLI, /* SI_PLI_GOUDIE_WET */
  SI_SPEC_PLI, /* SI_PLI_DEMPSTER */
  SI_SPEC_PW, /* SI_PW_CURTIS */
  SI_SPEC_PY, /* SI_PY_MILNER */
  SI_SPEC_PY, /* SI_PY_HANN */
  SI_SPEC_SB, /* SI_SB_HUANG */
  SI_SPEC_SB, /* SI_SB_CIESZEWSKI */
  SI_SPEC_SB, /* SI_SB_KER */
  SI_SPEC_SB, /* SI_SB_DEMPSTER */
  SI_SPEC_SS, /* SI_SS_NIGHGI */
  SI_SPEC_SS, /* SI_SS_NIGH */
  SI_SPEC_SS, /* SI_SS_GOUDIE */
  SI_SPEC_SS, /* SI_SS_FARR */
  SI_SPEC_SS, /* SI_SS_BARKER */
  SI_SPEC_SW, /* SI_SW_NIGHGI */
  SI_SPEC_SW, /* SI_SW_HUANG_PLA */
  SI_SPEC_SW, /* SI_SW_HUANG_NAT */
  SI_SPEC_SW, /* SI_SW_THROWER */
  SI_SPEC_SW, /* SI_SW_CIESZEWSKI */
  SI_SPEC_SW, /* SI_SW_KER_PLA */
  SI_SPEC_SW, /* SI_SW_KER_NAT */
  SI_SPEC_SW, /* SI_SW_GOUDIE_PLA */
  SI_SPEC_SW, /* SI_SW_GOUDIE_NAT */
  SI_SPEC_SW, /* SI_SW_DEMPSTER */
  SI_SPEC_BL, /* SI_BL_CHEN */
  SI_SPEC_AT, /* SI_AT_CHEN */
  SI_SPEC_DR, /* SI_DR_CHEN */
  SI_SPEC_PLI, /* SI_PL_CHEN */
  SI_SPEC_CWI, /* SI_CWI_NIGH */
  SI_SPEC_BP, /* SI_BP_CURTIS */
  SI_SPEC_HWC, /* SI_HWC_NIGHGI99 */
  SI_SPEC_SS, /* SI_SS_NIGHGI99 */
  SI_SPEC_SW, /* SI_SW_NIGHGI99 */
  SI_SPEC_LW, /* SI_LW_NIGHGI */
  SI_SPEC_SW, /* SI_SW_NIGHTA */
  SI_SPEC_CWI, /* SI_CWI_NIGHGI */
  SI_SPEC_SW, /* SI_SW_GOUDNIGH */
  SI_SPEC_HM, /* SI_HM_MEANS */
  SI_SPEC_SE, /* SI_SE_CHEN */
  SI_SPEC_FDC, /* SI_FDC_NIGHTA */
  SI_SPEC_FDC, /* SI_FDC_BRUCENIGH */
  SI_SPEC_LW, /* SI_LW_NIGH */
  SI_SPEC_SB, /* SI_SB_NIGH */
  SI_SPEC_AT, /* SI_AT_NIGH */
  SI_SPEC_BL, /* SI_BL_CHENAC */
  SI_SPEC_BP, /* SI_BP_CURTISAC */
  SI_SPEC_HM, /* SI_HM_MEANSAC */
  SI_SPEC_FDI, /* SI_FDI_THROWERAC */
  SI_SPEC_ACB, /* SI_ACB_HUANGAC */
  SI_SPEC_PW, /* SI_PW_CURTISAC */
  SI_SPEC_HWC, /* SI_HWC_WILEYAC */
  SI_SPEC_FDC, /* SI_FDC_BRUCEAC */
  SI_SPEC_CWC, /* SI_CWC_KURUCZAC */
  SI_SPEC_BA, /* SI_BA_KURUCZ82AC */
  SI_SPEC_ACT, /* SI_ACT_THROWERAC */
  SI_SPEC_PY, /* SI_PY_HANNAC */
  SI_SPEC_SE, /* SI_SE_CHENAC */
  SI_SPEC_SW, /* SI_SW_GOUDIE_NATAC */
  SI_SPEC_PY, /* SI_PY_NIGH */
  SI_SPEC_PY, /* SI_PY_NIGHGI */
  SI_SPEC_PLI, /* SI_PLI_NIGHTA2004 */
  SI_SPEC_SE,  /* SI_NIGHTA */
  SI_SPEC_SW,  /* SI_NIGHTA2004 */
  SI_SPEC_SW,  /* SI_GOUDIE_PLAAC */
  SI_SPEC_PJ,  /* SI_HUANG */
  SI_SPEC_PJ,  /* SI_HUANGAC */
  SI_SPEC_SW,  /* SI_SW_NIGHGI2004 */
  SI_SPEC_EP,  /* SI_EP_NIGH */
  SI_SPEC_BA,  /* SI_BA_NIGHGI */
  SI_SPEC_BA,  /* SI_BA_NIGH */
  SI_SPEC_SW,  /* SI_SW_HU_GARCIA */
  SI_SPEC_SE,  /* SI_SE_NIGHGI */
  SI_SPEC_SE,  /* SI_SE_NIGH */
  SI_SPEC_CWC,  /* SI_CWC_NIGH */
  SI_SPEC_PLI, // SI_PLI_NIGH
  };

// [[Rcpp::export]]
short int Sindex_VersionNumber ()
{
  return 152;
}



/*
 * Enumerate available species
 */
// [[Rcpp::export]]
short int Sindex_FirstSpecies ()
{
  return SI_SPEC_START;
}


// [[Rcpp::export]]
short int Sindex_NextSpecies (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  if (sp_index == SI_SPEC_END)
    return SI_ERR_NO_ANS;

  return sp_index+1;
}

// [[Rcpp::export]]
short int Sindex_SpecUse (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  /*
  *    Code bits are set as follows:
  *         1: BC coast
  *        10: BC interior
  *       100: common in BC
  */
  switch (sp_index)
  {
  case SI_SPEC_A: return 0x00;
  case SI_SPEC_ABAL: return 0x00;
  case SI_SPEC_ABCO: return 0x00;
  case SI_SPEC_AC: return 0x04;
  case SI_SPEC_ACB: return 0x07;
  case SI_SPEC_ACT: return 0x04;
  case SI_SPEC_AD: return 0x00;
  case SI_SPEC_AH: return 0x00;
  case SI_SPEC_AT: return 0x06;
  case SI_SPEC_AX: return 0x00;
  case SI_SPEC_B: return 0x00;
  case SI_SPEC_BA: return 0x05;
  case SI_SPEC_BB: return 0x00;
  case SI_SPEC_BC: return 0x00;
  case SI_SPEC_BG: return 0x00;
  case SI_SPEC_BI: return 0x00;
  case SI_SPEC_BL: return 0x06;
  case SI_SPEC_BM: return 0x00;
  case SI_SPEC_BP: return 0x05;
    //    case SI_SPEC_BV: return 0x00;
  case SI_SPEC_C: return 0x00;
  case SI_SPEC_CI: return 0x00;
  case SI_SPEC_CP: return 0x00;
  case SI_SPEC_CW: return 0x05;
  case SI_SPEC_CWC: return 0x05;
  case SI_SPEC_CWI: return 0x06;
  case SI_SPEC_CY: return 0x01;
  case SI_SPEC_D: return 0x00;
  case SI_SPEC_DG: return 0x00;
  case SI_SPEC_DM: return 0x02;
  case SI_SPEC_DR: return 0x05;
  case SI_SPEC_E: return 0x00;
  case SI_SPEC_EA: return 0x02;
  case SI_SPEC_EB: return 0x02;
  case SI_SPEC_EE: return 0x02;
  case SI_SPEC_EP: return 0x06;
  case SI_SPEC_ES: return 0x02;
  case SI_SPEC_EW: return 0x02;
  case SI_SPEC_EY: return 0x00;
  case SI_SPEC_EXP: return 0x02;
  case SI_SPEC_FD: return 0x05;
  case SI_SPEC_FDC: return 0x05;
  case SI_SPEC_FDI: return 0x06;
  case SI_SPEC_G: return 0x01;
  case SI_SPEC_GP: return 0x01;
  case SI_SPEC_GR: return 0x01;
  case SI_SPEC_H: return 0x00;
  case SI_SPEC_HM: return 0x05;
  case SI_SPEC_HW: return 0x05;
  case SI_SPEC_HWC: return 0x05;
  case SI_SPEC_HWI: return 0x06;
  case SI_SPEC_HXM: return 0x00;
  case SI_SPEC_IG: return 0x00;
  case SI_SPEC_IS: return 0x00;
  case SI_SPEC_J: return 0x02;
  case SI_SPEC_JR: return 0x02;
  case SI_SPEC_JS: return 0x00;
  case SI_SPEC_K: return 0x00;
  case SI_SPEC_KC: return 0x00;
  case SI_SPEC_L: return 0x00;
  case SI_SPEC_LA: return 0x02;
  case SI_SPEC_LD: return 0x00;
  case SI_SPEC_LE: return 0x02;
  case SI_SPEC_LS: return 0x00;
  case SI_SPEC_LT: return 0x02;
  case SI_SPEC_LW: return 0x06;
  case SI_SPEC_M: return 0x00;
  case SI_SPEC_MB: return 0x01;
  case SI_SPEC_ME: return 0x00;
  case SI_SPEC_MN: return 0x00;
  case SI_SPEC_MR: return 0x00;
  case SI_SPEC_MS: return 0x00;
  case SI_SPEC_MV: return 0x00;
  case SI_SPEC_OA: return 0x00;
  case SI_SPEC_OB: return 0x00;
  case SI_SPEC_OC: return 0x00;
  case SI_SPEC_OD: return 0x00;
  case SI_SPEC_OE: return 0x00;
  case SI_SPEC_OF: return 0x00;
  case SI_SPEC_OG: return 0x00;
  case SI_SPEC_OH: return 0x00;
  case SI_SPEC_OI: return 0x00;
  case SI_SPEC_OJ: return 0x00;
  case SI_SPEC_OK: return 0x00;
  case SI_SPEC_P: return 0x02;
  case SI_SPEC_PA: return 0x02;
  case SI_SPEC_PF: return 0x02;
  case SI_SPEC_PJ: return 0x02;
  case SI_SPEC_PL: return 0x06;
  case SI_SPEC_PLC: return 0x01;
  case SI_SPEC_PLI: return 0x06;
  case SI_SPEC_PM: return 0x00;
  case SI_SPEC_PR: return 0x00;
  case SI_SPEC_PS: return 0x00;
  case SI_SPEC_PW: return 0x04;
  case SI_SPEC_PXJ: return 0x02;
  case SI_SPEC_PY: return 0x06;
  case SI_SPEC_Q: return 0x00;
  case SI_SPEC_QE: return 0x00;
  case SI_SPEC_QG: return 0x01;
  case SI_SPEC_QW: return 0x00;
  case SI_SPEC_R: return 0x01;
  case SI_SPEC_RA: return 0x01;
  case SI_SPEC_S: return 0x00;
  case SI_SPEC_SA: return 0x02;
  case SI_SPEC_SB: return 0x06;
  case SI_SPEC_SE: return 0x06;
  case SI_SPEC_SI: return 0x02;
  case SI_SPEC_SN: return 0x02;
  case SI_SPEC_SS: return 0x05;
  case SI_SPEC_SW: return 0x06;
  case SI_SPEC_SX: return 0x06;
  case SI_SPEC_SXB: return 0x02;
  case SI_SPEC_SXE: return 0x01;
  case SI_SPEC_SXL: return 0x01;
  case SI_SPEC_SXS: return 0x01;
  case SI_SPEC_SXW: return 0x02;
  case SI_SPEC_SXX: return 0x02;
  case SI_SPEC_T: return 0x00;
  case SI_SPEC_TW: return 0x00;
  case SI_SPEC_U: return 0x00;
  case SI_SPEC_UA: return 0x00;
  case SI_SPEC_UP: return 0x00;
  case SI_SPEC_V: return 0x00;
  case SI_SPEC_VB: return 0x00;
  case SI_SPEC_VP: return 0x00;
  case SI_SPEC_VS: return 0x00;
  case SI_SPEC_VV: return 0x00;
  case SI_SPEC_W: return 0x00;
  case SI_SPEC_WA: return 0x00;
  case SI_SPEC_WB: return 0x00;
  case SI_SPEC_WD: return 0x00;
  case SI_SPEC_WI: return 0x00;
  case SI_SPEC_WP: return 0x00;
  case SI_SPEC_WS: return 0x00;
  case SI_SPEC_WT: return 0x00;
  case SI_SPEC_X: return 0x00;
  case SI_SPEC_XC: return 0x00;
  case SI_SPEC_XH: return 0x00;
  case SI_SPEC_Y: return 0x00;
  case SI_SPEC_YC: return 0x01;
  case SI_SPEC_YP: return 0x00;
  case SI_SPEC_Z: return 0x00;
  case SI_SPEC_ZC: return 0x00;
  case SI_SPEC_ZH: return 0x00;
  }

  return SI_ERR_SPEC;
}



// [[Rcpp::export]]
short int Sindex_DefCurve (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  return (short int) (si_curve_default[sp_index]);
}



// [[Rcpp::export]]
short int Sindex_DefGICurve (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  switch (sp_index)
  {
  case SI_SPEC_BA: return   SI_BA_NIGHGI;
  case SI_SPEC_BL: return   SI_BL_THROWERGI;
  case SI_SPEC_CWI: return  SI_CWI_NIGHGI;
  case SI_SPEC_FDC: return  SI_FDC_NIGHGI;
  case SI_SPEC_FDI: return  SI_FDI_NIGHGI;
  case SI_SPEC_HWC: return  SI_HWC_NIGHGI99;
  case SI_SPEC_HWI: return  SI_HWI_NIGHGI;
  case SI_SPEC_LW: return   SI_LW_NIGHGI;
  case SI_SPEC_PLI: return  SI_PLI_NIGHGI97;
  case SI_SPEC_PY: return   SI_PY_NIGHGI;
  case SI_SPEC_SE: return   SI_SE_NIGHGI;
  case SI_SPEC_SS: return   SI_SS_NIGHGI99;
  case SI_SPEC_SW: return   SI_SW_NIGHGI2004;
  }
  return SI_ERR_NO_ANS;
}


// [[Rcpp::export]]
short int Sindex_DefCurveEst (short int sp_index, short int estab)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  if (sp_index == SI_SPEC_SW)
  {
    switch (estab)
    {
    case SI_ESTAB_NAT:
      return SI_SW_GOUDIE_NATAC;
      break;
    case SI_ESTAB_PLA:
      return SI_SW_GOUDIE_PLAAC;
      break;
    default:
      return SI_ERR_ESTAB;
    }
  }
  else
    return (short int) (si_curve_default[sp_index]);
}





//' @title
//' Returns first defined curve index for a species.
//' @description
//' Returns first defined curve index for a species.
//' @param sp_index Integer, Specifies species index.
//' @return
//' Integer curve index, for use in other Sindex functions.
//'    May return an error code under the following conditions:
//'
//'    return value    condition
//'    ------------    ---------
//'    SI_ERR_SPEC     input parameter is not a valid species index
//'    SI_ERR_NO_ANS   there are no curves defined for this species
//' @note
//' No assumption should be made about the ordering of the curves.
//' @rdname Sindex_FirstCurve
// [[Rcpp::export]]
short int Sindex_FirstCurve (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  return (short int) (si_sclist_start[sp_index]);
}


//' @title
//' Returns next defined curve index for a species.
//' @description
//' Returns next defined curve index for a species.
//' @param sp_index Integer, Specifies species index.
//' @param cu_index Integer, Specifies curve index.
//' @return
//'   Integer curve index, for use in other Sindex functions.
//'    May return an error code under the following conditions:
//'
//'    return value    condition
//'
//'    ------------    ---------
//'
//'    SI_ERR_SPEC     input species is not a valid species index.
//'
//'    SI_ERR_CURVE    input curve is not a valid curve index for this species.
//'
//'    SI_ERR_NO_ANS   input parameter is last defined index for this species.
//' @note
//'    No assumption should be made about the ordering of the curves.
//' @rdname Sindex_NextCurve
// [[Rcpp::export]]
short int Sindex_NextCurve (
    short int sp_index,
    short int cu_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return SI_ERR_SPEC;

  if (cu_index < 0 || cu_index >= SI_MAX_CURVES)
    return SI_ERR_CURVE;

  if (si_curve_intend[cu_index] != sp_index)
    return SI_ERR_CURVE;

  switch (cu_index)
  {
  case SI_ACB_HUANGAC: cu_index = SI_ACB_HUANG; break;
  case SI_ACB_HUANG: cu_index = SI_ERR_NO_ANS; break;

  case SI_ACT_THROWERAC: cu_index = SI_ACT_THROWER; break;
  case SI_ACT_THROWER: cu_index = SI_ERR_NO_ANS; break;

  case SI_AT_NIGH: cu_index = SI_AT_CHEN; break;
  case SI_AT_CHEN: cu_index = SI_AT_HUANG; break;
  case SI_AT_HUANG: cu_index = SI_AT_CIESZEWSKI; break;
  case SI_AT_CIESZEWSKI: cu_index = SI_AT_GOUDIE; break;
  case SI_AT_GOUDIE: cu_index = SI_ERR_NO_ANS; break;

  case SI_BA_NIGHGI: cu_index = SI_BA_NIGH; break;
  case SI_BA_NIGH: cu_index = SI_BA_KURUCZ82AC; break;
  case SI_BA_KURUCZ82AC: cu_index = SI_BA_DILUCCA; break;
  case SI_BA_DILUCCA: cu_index = SI_BA_KURUCZ86; break;
  case SI_BA_KURUCZ86: cu_index = SI_BA_KURUCZ82; break;
  case SI_BA_KURUCZ82: cu_index = SI_ERR_NO_ANS; break;

  case SI_BL_CHENAC: cu_index = SI_BL_CHEN; break;
  case SI_BL_CHEN: cu_index = SI_BL_THROWERGI; break;
  case SI_BL_THROWERGI: cu_index = SI_BL_KURUCZ82; break;
  case SI_BL_KURUCZ82: cu_index = SI_ERR_NO_ANS; break;

  case SI_BP_CURTISAC: cu_index = SI_BP_CURTIS; break;
  case SI_BP_CURTIS: cu_index = SI_ERR_NO_ANS; break;

  case SI_CWC_NIGH: cu_index = SI_CWC_KURUCZAC; break;
  case SI_CWC_KURUCZAC: cu_index = SI_CWC_KURUCZ; break;
  case SI_CWC_KURUCZ: cu_index = SI_CWC_BARKER; break;
  case SI_CWC_BARKER: cu_index = SI_ERR_NO_ANS; break;

  case SI_CWI_NIGH: cu_index = SI_CWI_NIGHGI; break;
  case SI_CWI_NIGHGI: cu_index = SI_ERR_NO_ANS; break;

  case SI_DR_NIGH: cu_index = SI_DR_HARRING; break;
  case SI_DR_HARRING: cu_index = SI_ERR_NO_ANS; break;

  case SI_EP_NIGH: cu_index = SI_ERR_NO_ANS; break;

  case SI_FDC_BRUCEAC: cu_index = SI_FDC_NIGHTA; break;
  case SI_FDC_NIGHTA: cu_index = SI_FDC_NIGHGI; break;
  case SI_FDC_NIGHGI: cu_index = SI_FDC_BRUCE; break;
  case SI_FDC_BRUCE: cu_index = SI_FDC_COCHRAN; break;
  case SI_FDC_COCHRAN: cu_index = SI_FDC_KING; break;
  case SI_FDC_KING: cu_index = SI_ERR_NO_ANS; break;

  case SI_FDI_THROWERAC: cu_index = SI_FDI_NIGHGI; break;
  case SI_FDI_NIGHGI: cu_index = SI_FDI_HUANG_PLA; break;
  case SI_FDI_HUANG_PLA: cu_index = SI_FDI_HUANG_NAT; break;
  case SI_FDI_HUANG_NAT: cu_index = SI_FDI_MILNER; break;
  case SI_FDI_MILNER: cu_index = SI_FDI_THROWER; break;
  case SI_FDI_THROWER: cu_index = SI_FDI_VDP_MONT; break;
  case SI_FDI_VDP_MONT: cu_index = SI_FDI_VDP_WASH; break;
  case SI_FDI_VDP_WASH: cu_index = SI_FDI_MONS_DF; break;
  case SI_FDI_MONS_DF: cu_index = SI_FDI_MONS_GF; break;
  case SI_FDI_MONS_GF: cu_index = SI_FDI_MONS_WRC; break;
  case SI_FDI_MONS_WRC: cu_index = SI_FDI_MONS_WH; break;
  case SI_FDI_MONS_WH: cu_index = SI_FDI_MONS_SAF; break;
  case SI_FDI_MONS_SAF: cu_index = SI_ERR_NO_ANS; break;

  case SI_HM_MEANSAC: cu_index = SI_HM_MEANS; break;
  case SI_HM_MEANS: cu_index = SI_ERR_NO_ANS; break;

  case SI_HWC_WILEYAC: cu_index = SI_HWC_NIGHGI99; break;
  case SI_HWC_NIGHGI99: cu_index = SI_HWC_FARR; break;
  case SI_HWC_FARR: cu_index = SI_HWC_BARKER; break;
  case SI_HWC_BARKER: cu_index = SI_HWC_WILEY; break;
  case SI_HWC_WILEY: cu_index = SI_HWC_WILEY_BC; break;
  case SI_HWC_WILEY_BC: cu_index = SI_HWC_WILEY_MB; break;
  case SI_HWC_WILEY_MB: cu_index = SI_ERR_NO_ANS; break;

  case SI_HWI_NIGH: cu_index = SI_HWI_NIGHGI; break;
  case SI_HWI_NIGHGI: cu_index = SI_ERR_NO_ANS; break;

  case SI_LW_NIGH: cu_index = SI_LW_NIGHGI; break;
  case SI_LW_NIGHGI: cu_index = SI_LW_MILNER; break;
  case SI_LW_MILNER: cu_index = SI_ERR_NO_ANS; break;

  case SI_PJ_HUANG: cu_index = SI_PJ_HUANGAC; break;
  case SI_PJ_HUANGAC: cu_index = SI_ERR_NO_ANS; break;

  case SI_PL_CHEN: cu_index = SI_PLI_THROWNIGH; break;
  case SI_PLI_THROWNIGH: cu_index = SI_PLI_NIGH; break;
  case SI_PLI_NIGH: cu_index = SI_PLI_NIGHTA98; break;
  case SI_PLI_NIGHTA98: cu_index = SI_PLI_NIGHGI97; break;
  case SI_PLI_NIGHGI97: cu_index = SI_PLI_HUANG_PLA; break;
  case SI_PLI_HUANG_PLA: cu_index = SI_PLI_HUANG_NAT; break;
  case SI_PLI_HUANG_NAT: cu_index = SI_PLI_THROWER; break;
  case SI_PLI_THROWER: cu_index = SI_PLI_MILNER; break;
  case SI_PLI_MILNER: cu_index = SI_PLI_CIESZEWSKI; break;
  case SI_PLI_CIESZEWSKI: cu_index = SI_PLI_GOUDIE_DRY; break;
  case SI_PLI_GOUDIE_DRY: cu_index = SI_PLI_GOUDIE_WET; break;
  case SI_PLI_GOUDIE_WET: cu_index = SI_PLI_DEMPSTER; break;
  case SI_PLI_DEMPSTER: cu_index = SI_ERR_NO_ANS; break;

  case SI_PW_CURTISAC: cu_index = SI_PW_CURTIS; break;
  case SI_PW_CURTIS: cu_index = SI_ERR_NO_ANS; break;

  case SI_PY_NIGH: cu_index = SI_PY_NIGHGI; break;
  case SI_PY_NIGHGI: cu_index = SI_PY_HANNAC; break;
  case SI_PY_HANNAC: cu_index = SI_PY_MILNER; break;
  case SI_PY_MILNER: cu_index = SI_PY_HANN; break;
  case SI_PY_HANN: cu_index = SI_ERR_NO_ANS; break;

  case SI_SB_NIGH: cu_index = SI_SB_HUANG; break;
  case SI_SB_HUANG: cu_index = SI_SB_CIESZEWSKI; break;
  case SI_SB_CIESZEWSKI: cu_index = SI_SB_KER; break;
  case SI_SB_KER: cu_index = SI_SB_DEMPSTER; break;
  case SI_SB_DEMPSTER: cu_index = SI_ERR_NO_ANS; break;

  case SI_SE_CHENAC: cu_index = SI_SE_CHEN; break;
  case SI_SE_CHEN: cu_index = SI_SE_NIGHGI; break;
  case SI_SE_NIGHGI: cu_index = SI_SE_NIGH; break;
  case SI_SE_NIGH: cu_index = SI_ERR_NO_ANS; break;

  case SI_SS_NIGHGI99: cu_index = SI_SS_NIGH; break;
  case SI_SS_NIGH: cu_index = SI_SS_GOUDIE; break;
  case SI_SS_GOUDIE: cu_index = SI_SS_FARR; break;
  case SI_SS_FARR: cu_index = SI_SS_BARKER; break;
  case SI_SS_BARKER: cu_index = SI_ERR_NO_ANS; break;

  case SI_SW_GOUDNIGH:     cu_index = SI_SW_HU_GARCIA; break;
  case SI_SW_HU_GARCIA:    cu_index = SI_SW_NIGHTA; break;
  case SI_SW_NIGHTA:       cu_index = SI_SW_NIGHGI2004; break;
  case SI_SW_NIGHGI2004:   cu_index = SI_SW_HUANG_PLA; break;
  case SI_SW_HUANG_PLA:    cu_index = SI_SW_HUANG_NAT; break;
  case SI_SW_HUANG_NAT:    cu_index = SI_SW_THROWER; break;
  case SI_SW_THROWER:      cu_index = SI_SW_CIESZEWSKI; break;
  case SI_SW_CIESZEWSKI:   cu_index = SI_SW_KER_PLA; break;
  case SI_SW_KER_PLA:      cu_index = SI_SW_KER_NAT; break;
  case SI_SW_KER_NAT:      cu_index = SI_SW_GOUDIE_PLAAC; break;
  case SI_SW_GOUDIE_PLAAC: cu_index = SI_SW_GOUDIE_PLA; break;
  case SI_SW_GOUDIE_PLA:   cu_index = SI_SW_GOUDIE_NATAC; break;
  case SI_SW_GOUDIE_NATAC: cu_index = SI_SW_GOUDIE_NAT; break;
  case SI_SW_GOUDIE_NAT:   cu_index = SI_ERR_NO_ANS; break;
  }

  return cu_index;
}



//' @title
//' Returns a code telling what functions are available for a curve index.
//' @description
//' Returns a code telling what functions are available for a curve index.
//' @param cu_index Integer, Defines curve index.
//'
//' @return
//'    Integer code.
//'    May return an error code under the following conditions:
//'
//'    return value    condition
//'
//'    ------------    ---------
//'
//'    SI_ERR_CURVE    input curve is not a valid curve index.
//' @note
//'      Code bits are set as follows:
//'      0001: ht = fn (si, age)
//'      0010: si = fn (ht, age)
//'      0100: y2bh = fn (si)
//'      1000: si = fn (ht, age) growth intercept
//' @rdname Sindex_CurveUse
// [[Rcpp::export]]
short int Sindex_CurveUse (short int cu_index)
{
  if (cu_index >= 0 && cu_index < SI_MAX_CURVES)
    return (short int) (si_curve_types[cu_index]);

  return SI_ERR_CURVE;
}



//' @title
//' Convert curve index to species index
//' @description
//' Returns species index for a given curve index.
//' @param cu_index Integer, Specifies cuive index.
//' @return
//'    Integer species index, for use in other Sindex functions.
//'    May return an error code under the following conditions:
//'
//'    return value    condition
//'
//'    ------------    ---------
//'
//'    SI_ERR_CURVE    input curve is not a valid curve index for any species.
//' @rdname Sindex_CurveToSpecies
// [[Rcpp::export]]
short int Sindex_CurveToSpecies (
    short int cu_index)
{
  if (cu_index < 0 || cu_index >= SI_MAX_CURVES)
    return SI_ERR_CURVE;

  return si_curve_intend[cu_index];
}




/*
 * site index conversion between species
 */
//' @title
//'  Site index conversion between species
//' @description
//'  Site index conversion between species. This function needs to be revised.
//' @param sp_index1 Integer, Source species index.
//' @param site Numeric, Source species site index.
//' @param sp_index2 Integer, Target species index.
//' @return
//'    Floating point target species site index. (computed)
//'
//'  Return Value
//'  ------------
//'    0, or an error code under the following conditions:
//'
//'    return value    condition
//'    ------------    ---------
//'    SI_ERR_SPEC     source or target species index is not valid.
//'    SI_ERR_NO_ANS   there is no conversion defined.
// [[Rcpp::export]]
short int Sindex_SIToSI (
    short int sp_index1,
    double    site,
    short int sp_index2)
{
  short int i;
  double site2;


  if (sp_index1 < 0 || sp_index1 >= SI_MAX_SPECIES)
  {
    site2 = SI_ERR_SPEC;
    return site2;
  }

  if (sp_index2 < 0 || sp_index2 >= SI_MAX_SPECIES)
  {
    site2 = SI_ERR_SPEC;
    return site2;
  }

  for (i = 0; i < SI_MAX_CONVERT; i++)
  {
    if (si_convert[i][0] == sp_index1)
      if (si_convert[i][1] == sp_index2)
      {
        site2 = si_convert[i][2] + si_convert[i][3] * site;
        return site2;
      }
  }

}



//' @title
//' Returns string containing species code.
//' @description
//' Returns string containing species code.
//' @param sp_index Integer, Specifies species index.
//' @return
//' Pointer to string containing species code.
//'    If input parameter is not a valid species index, the return is the
//'    null pointer.
//' @note
//' Species code string takes the form "Xx" or "Xxx", such as "Sw" or "Fdc".
//' @rdname Sindex_SpecCode
// [[Rcpp::export]]
std::string Sindex_SpecCode (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return NULL;

  return si_spec_code[sp_index];
}



//' @title
//' Returns string containing species name.
//' @description
//' Returns string containing species name.
//' @param sp_index Integer, Specifies species index
//' @return
//'    Pointer to string containing species name.
//'    If input parameter is not a valid species index, the return is the
//'    null pointer.
//'
//' @note
//'    Species name string examples: "Coastal Douglas-fir", "Sitka Spruce".
//' @rdname Sindex_SpecName
// [[Rcpp::export]]
std::string Sindex_SpecName (short int sp_index)
{
  if (sp_index < 0 || sp_index >= SI_MAX_SPECIES)
    return NULL;

  return si_spec_name[sp_index];
}




//' @title
//'    Curve name
//' @description
//'    Returns string containing author and date of curve.
//' @param cu_index Integer, Curve index.
//' @return
//'    A string containing curve author and date.
//'    If input parameter is not a valid curve index, the return is the
//'    \code{null}.
//'
//' @note
//'    Curve name string examples: "Bruce (1981)", "Nigh (1998)".
//' @rdname Sindex_CurveName
// [[Rcpp::export]]
std::string Sindex_CurveName (short int cu_index)
{
  if (cu_index >= 0 && cu_index < SI_MAX_CURVES)
    return si_curve_name[cu_index];

  return NULL;
}




//' @title
//' Curve source
//' @description
//'  Returns string containing publication source.
//' @param cu_index Integer, Curve index.
//' @return
//' String containing publication citation.
//'    If input parameter is not a valid curve index, the return is the
//'    null pointer.
//' @rdname Sindex_CurveSource
// [[Rcpp::export]]
std::string Sindex_CurveSource (short int cu_index)
{
  if (cu_index < 0 || cu_index >= SI_MAX_CURVES)
    return NULL;

  switch (cu_index)
  {
  case SI_BA_NIGH:
    cu_index = SI_BA_NIGHGI;
    break;

  case SI_CWI_NIGHGI:
    cu_index = SI_CWI_NIGH;
    break;

  case SI_AT_HUANG:
  case SI_SB_HUANG:
  case SI_FDI_HUANG_PLA:
  case SI_FDI_HUANG_NAT:
  case SI_PLI_HUANG_PLA:
  case SI_PLI_HUANG_NAT:
  case SI_SW_HUANG_PLA:
  case SI_SW_HUANG_NAT:
    cu_index = SI_ACB_HUANG;
    break;

  case SI_PLI_CIESZEWSKI:
  case SI_SB_CIESZEWSKI:
  case SI_SW_CIESZEWSKI:
    cu_index = SI_AT_CIESZEWSKI;
    break;

  case SI_PLI_DEMPSTER:
  case SI_SB_DEMPSTER:
  case SI_SW_DEMPSTER:
    cu_index = SI_AT_GOUDIE;
    break;

  case SI_SW_KER_PLA:
  case SI_SW_KER_NAT:
    cu_index = SI_SB_KER;
    break;

  case SI_BL_KURUCZ82:
    cu_index = SI_BA_KURUCZ82;
    break;

  case SI_HWC_BARKER:
  case SI_SS_BARKER:
    cu_index = SI_CWC_BARKER;
    break;

  case SI_LW_MILNER:
  case SI_PLI_MILNER:
  case SI_PY_MILNER:
    cu_index = SI_FDI_MILNER;
    break;

  case SI_FDI_VDP_WASH:
    cu_index = SI_FDI_VDP_MONT;
    break;

  case SI_FDI_MONS_GF:
  case SI_FDI_MONS_WRC:
  case SI_FDI_MONS_WH:
  case SI_FDI_MONS_SAF:
    cu_index = SI_FDI_MONS_DF;
    break;

  case SI_SS_FARR:
    cu_index = SI_HWC_FARR;
    break;

  case SI_HWC_WILEY_BC:
  case SI_HWC_WILEY_MB:
    cu_index = SI_HWC_WILEY;
    break;

  case SI_SW_THROWER:
    cu_index = SI_PLI_THROWER;
    break;

  case SI_PLI_GOUDIE_WET:
  case SI_SW_GOUDIE_PLA:
  case SI_SW_GOUDIE_NAT:
    cu_index = SI_PLI_GOUDIE_DRY;
    break;
  }
  return si_curve_notes[cu_index][0];
}


/*
* curve notes
*/
//' @title
//' Curve notes
//' @description
//'    Returns string containing notes on use.
//' @param cu_index Integer, Curve index.
//' @return A string containing notes on use of curve.
//'    If input parameter is not a valid curve index, the return is the
//'    null pointer.
//' @rdname Sindex_CurveNotes
// [[Rcpp::export]]
std::string Sindex_CurveNotes (short int cu_index)
{
  if (cu_index < 0 || cu_index >= SI_MAX_CURVES)
    return NULL;

  switch (cu_index)
  {
  case SI_BA_NIGH:
    cu_index = SI_BA_NIGHGI;
    break;

  case SI_CWI_NIGHGI:
    cu_index = SI_CWI_NIGH;
    break;

  case SI_FDI_HUANG_NAT:
    cu_index = SI_FDI_HUANG_PLA;
    break;

  case SI_PLI_HUANG_NAT:
    cu_index = SI_PLI_HUANG_PLA;
    break;

  case SI_SW_HUANG_NAT:
    cu_index = SI_SW_HUANG_PLA;
    break;

  case SI_SW_KER_NAT:
    cu_index = SI_SW_KER_PLA;
    break;

  case SI_FDI_VDP_WASH:
    cu_index = SI_FDI_VDP_MONT;
    break;

  case SI_FDI_MONS_GF:
  case SI_FDI_MONS_WRC:
  case SI_FDI_MONS_WH:
  case SI_FDI_MONS_SAF:
    cu_index = SI_FDI_MONS_DF;
    break;

  case SI_PLI_GOUDIE_WET:
    cu_index = SI_PLI_GOUDIE_DRY;
    break;

  case SI_SW_GOUDIE_NAT:
    cu_index = SI_SW_GOUDIE_PLA;
    break;

  case SI_PY_NIGHGI:
    cu_index = SI_PY_NIGH;
    break;
  }

  return si_curve_notes[cu_index][1];
}


