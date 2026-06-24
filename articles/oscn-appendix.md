# Appendix: Clean OSCN Charge Descriptions

## ojoRegex and OSCN

The `ojoRegex` package is a collection of regular expressions that can
be used to clean and extract information from court data, in particular
data from the Oklahoma State Court Network (OSCN). This appendix
provides a list of how unique charge descriptions in the OSCN data are
cleaned by the `ojoRegex` package.

The `ojoRegex::ojo_apply_regex()` function begins with a table of regex
strings
([`ojoregex::ojo_regex_flags`](https://openjusticeok.github.io/ojoregex/reference/ojo_regex_flags.md))
that correspond to broad concepts like “marijuana”, “seatbelt”, or
“permit”. The function takes a list of raw charge descriptions, then
checks each one for a match with all the regular expressions in the
table, applying a “flag” for each concept it finds. Next, it classifies
each description with a cleaned charge description / category
([`ojoregex::ojo_regex_cats`](https://openjusticeok.github.io/ojoregex/reference/ojo_regex_cats.md))
based on what concept flags are present.

For example, if the “drugs” flag, the “possess” flag, and the “intent”
flag are all found, the charge will be classified as “CDS Possession
(PWID)”. Alternatively, if the “drugs” flag and “possess” flag are
found, but the “paraphernalia” flag is also present, then “CDS
Paraphernalia Possession / Distribution” will be applied instead.

- The list of flags and cleaned descriptions can be found in [this
  Google
  Sheet.](https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing)

- Note that not every charge in the Oklahoma state statutes has been
  incorporated into the package just yet.

## Cleaned OSCN charge descriptions

This appendix covers charge descriptions from the OSCN data which were
filed in felony, misdemeanor, or traffic cases from 2010 to 2024. There
are 216,729 total unique charge descriptions in the data, but only those
appearing at least 1,000 times from 2010 - 2024 (there are 489 unique
descriptions) are included for brevity. For a full list, please contact
[Andrew Bell at abell@okpolicy.org](mailto:abell@okpolicy.org).

| Original Charge Description | Cleaned Charge Description | Relevant Statutes | Total Filed 2010-2024 |
|----|----|----|----|
| S51, SPEEDING 1 - 10 mph over | Speeding | 47 O.S. 11-801 | 88,222 |
| SB3, Seat Belt - Driver Not Wearing Seat Belt | Seatbelt Violation | 47 O.S. 12-417 | 52,853 |
| DL2, DRIVING UNDER SUSPENSION / DUS | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 43,864 |
| DL5, OPERATING MV WITHOUT BEING LICENSED OR WITH EXPIRED DL | Driving Without Valid License | 47 O.S. 6-112 | 32,636 |
| FR5, FAILURE TO COMPLY WITH COMPULSORY INSURANCE LAW OR FAILURE TO PRODUCE SECURITY VERIFICATION FORM | Driving Without Valid Insurance / Security | NA | 30,739 |
| DR3, POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 29,969 |
| OBS, OBSTRUCTING AN OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 27,076 |
| LP1, OPERATING MV W/O CURRENT LIC PLATE/Taxes Due State/Improper Tag | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 23,511 |
| DR3, UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 22,898 |
| 1\. PUBLIC INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 21,491 |
| LMFR, LARCENY OF MERCHANDISE FROM RETAILER | Larceny (Shoplifting) | 21 O.S. 1731 | 19,683 |
| DR1, PUBLIC INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 18,114 |
| 1\. POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 17,828 |
| BRG2, BURGLARY - SECOND DEGREE | Burglary (Second Degree) | 21 O.S. 1435 | 16,304 |
| 2\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 15,720 |
| S16, SPEEDING 16-20 mph over | Speeding | 47 O.S. 11-801 | 15,376 |
| FR5, FAILURE TO CARRY INSURANCE/SECURITY VERIFICATION FORM | Driving Without Valid Insurance / Security | NA | 13,561 |
| DL5, DRIVING WITHOUT A DRIVER'S LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 13,526 |
| ABDOM, ASSAULT AND BATTERY - DOMESTIC | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 12,020 |
| RCSP, CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 11,881 |
| VPO, VIOLATION OF PROTECTIVE ORDER | Violation of Protective Order (VPO) | NA | 11,690 |
| DR3, POSSESSION OR SELLING OF PARAPHERNALIA WHILE NOT USING A MOTOR VEHICLE | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 11,622 |
| SPEEDING | Speeding | 47 O.S. 11-801 | 11,520 |
| EDUC, VIOLATION OF COMPULSORY EDUCATION ACT | Violation of Compulsory Education Act | 70 O.S. 10-105 | 11,382 |
| RES, RESISTING AN OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 10,810 |
| NO SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 10,732 |
| SPEEDING 1-10 OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 10,643 |
| DI1, DRIVING WHILE UNDER THE INFLUENCE | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 10,445 |
| IL0, UNSAFE LANE USE | Changing Lanes Unsafely | 47 O.S. 11-309 | 9,699 |
| DI1M, DUI ALCOHOL (MUNICIPAL ARREST) | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 9,602 |
| S21, SPEEDING 21-25 mph over | Speeding | 47 O.S. 11-801 | 9,226 |
| DL2, DRIVING WHILE PRIVILEGE SUSPENDED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 8,610 |
| FAIL TO PROPERLY WEAR SEATBELT (DRIVER), in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 8,174 |
| LP1, FAILURE TO PAY TAXES DUE TO STATE | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 7,889 |
| FAILURE TO WEAR SEATBELT | Seatbelt Violation | 47 O.S. 12-417 | 7,882 |
| S11, SPEEDING 11-14 mph over | Speeding | 47 O.S. 11-801 | 7,831 |
| DL1, DRIVING UNDER REVOCATION / DUR | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 7,750 |
| S15, SPEEDING 15 mph over | Speeding | 47 O.S. 11-801 | 7,512 |
| LA3, Unauthorized Use of Vehicle | Unauthorized Use of Vehicle | 47 O.S. 4-102 | 7,459 |
| 1\. DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 7,340 |
| SPEEDING 1-10 MPH OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 7,324 |
| LA4, POSSESSION OF STOLEN VEHICLE | Possession of Stolen Vehicle | 47 O.S. 4-103 | 7,255 |
| DR3MV, UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 6,536 |
| DI1, DUI ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 6,523 |
| SB3, FAIL TO PROPERLY WEAR SEATBELT (DRIVER) | Seatbelt Violation | 47 O.S. 12-417 | 6,507 |
| 3\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 6,443 |
| SB4, Seat Belt - Passenger Not Wearing Seat Belt | Seatbelt Violation | 47 O.S. 12-417 | 6,377 |
| FORG, UTTERING FORGED INSTRUMENT | Fraud (Forgery / Counterfeiting) | 21 O.S. 1561-1628 | 6,352 |
| FAIL TO PROPERLY WEAR SEAT BELT (DRIVER) | Seatbelt Violation | 47 O.S. 12-417 | 6,327 |
| 2\. POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 6,024 |
| 85 IN 75, SPEEDING | Speeding | 47 O.S. 11-801 | 5,930 |
| 1\. DRIVING WITH LICENSE CANCELLED/SUSPENDED/REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 5,918 |
| 75 IN 65, SPEEDING | Speeding | 47 O.S. 11-801 | 5,545 |
| BRG1, BURGLARY - FIRST DEGREE | Burglary (First Degree) | 21 O.S. 1431 | 5,534 |
| FAILURE TO WEAR SEATBELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 5,427 |
| IL1, CHANGING LANES UNSAFELY | Changing Lanes Unsafely | 47 O.S. 11-309 | 5,415 |
| FORG, FORGERY IN THE SECOND DEGREE | Fraud (Forgery / Counterfeiting) | 21 O.S. 1561-1628 | 5,395 |
| ABDGR, ASSAULT WITH A DANGEROUS WEAPON | Assault / Battery (Dangerous Weapon) | 21 O.S. 645 | 5,337 |
| OWPN, POSSESSION OF A FIREARM AFCF | Illegal Possession of a Firearm | 21 O.S. 1272 | 5,309 |
| BRG2, BURGLARY IN THE SECOND DEGREE | Burglary (Second Degree) | 21 O.S. 1435 | 5,280 |
| BH, THREATENING AN ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 5,241 |
| KID, KIDNAPPING | Kidnapping (Simple) | 21 O.S. 25-741 | 5,061 |
| DU8II, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE - METH | CDS Possession (Simple) | 63 O.S. 2-402 | 4,992 |
| ST1, FAIL TO STOP FOR STOP SIGN | Fail to Stop at Sign | 47 O.S. 11-703 | 4,968 |
| FO1, FOLLOWING TOO CLOSELY | Following Too Closely | 47 O.S. 11-310 | 4,934 |
| EMB, EMBEZZLEMENT | Embezzlement | 21 O.S. 59-1451 | 4,933 |
| 1\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 4,916 |
| DL1, DRIVING WHILE PRIVILEGE REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 4,875 |
| 1\. OBSTRUCTING AN OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 4,862 |
| ABDGR, ASSAULT & BATTERY WITH A DANGEROUS WEAPON | Assault / Battery (Dangerous Weapon) | 21 O.S. 645 | 4,716 |
| OWPN, POSSESSION OF AN OFFENSIVE WEAPON WHILE COMMITTING A FELONY | Illegal Possession of a Firearm | 21 O.S. 1272 | 4,693 |
| S26, SPEEDING 26-30 MPH OVER | Speeding | 47 O.S. 11-801 | 4,661 |
| IL1, UNSAFE LANE USE | Changing Lanes Unsafely | 47 O.S. 11-309 | 4,620 |
| SPEEDING 1-10 MPH OVER LIMIT | Speeding | 47 O.S. 11-801 | 4,560 |
| DR1, PUBLIC DRUNK OR INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 4,556 |
| 2\. PUBLIC INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 4,535 |
| DI1, DRIVING UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 4,529 |
| LA3, UNAUTHORIZED USE OF A VEHICLE | Unauthorized Use of Vehicle | 47 O.S. 4-102 | 4,513 |
| LMFR, LARCENY OF MERCHANDISE FROM A RETAILER | Larceny (Shoplifting) | 21 O.S. 1731 | 4,454 |
| 1\. OBTAINING CASH OR MERCHANDISE BY BOGUS CHECK/FALSE PRETENSES | Fraud (Other / Unspecified) | NA | 4,429 |
| 1\. PROTECTIVE ORDER VIOLATION | Violation of Protective Order (VPO) | NA | 4,386 |
| 80 IN 70, SPEEDING | Speeding | 47 O.S. 11-801 | 4,378 |
| DR3MV, POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 4,369 |
| FFP, FALSELY PERSONATE ANOTHER TO CREATE LIABILITY | Fraud (False Personation) | 21 O.S. 1531-1533 | 4,363 |
| LMFR, GRAND LARCENY | Larceny (Grand) | 21 O.S. 1705 | 4,339 |
| 1\. KNOWINGLY CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 4,288 |
| RBDW, ROBBERY WITH A FIREARM | Robbery (With a Dangerous Weapon) | 21 O.S. 801 | 4,201 |
| 1\. DOMESTIC ABUSE - ASSAULT & BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 4,179 |
| ABGEN, Assault and Battery | Assault / Battery (Simple) | 21 O.S. 644 | 4,121 |
| ABDOM, DOMESTIC ABUSE (ASSAULT AND BATTERY) | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 4,118 |
| ST2, FAIL TO STOP FOR RED LIGHT | Fail to Stop at Sign | 47 O.S. 11-703 | 4,115 |
| ABDOM, DOMESTIC ASSAULT & BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 4,107 |
| RCSP, KNOWINGLY RECEIVING/CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 4,105 |
| ABGEN, ASSAULT & BATTERY | Assault / Battery (Simple) | 21 O.S. 644 | 4,016 |
| EDUC, REFUSING AND NEGLECTING TO CAUSE A CHILD UNDER THE AGE OF EIGHTEEN (18) TO ATTEND SCHOOL | Violation of Compulsory Education Act | 70 O.S. 10-105 | 4,012 |
| OWN, MAKING A FALSE DECLARATION OF OWNERSHIP TO A PAWNBROKER | False Declaration of Ownership | 59 O.S. 1512 | 4,007 |
| RK1, RECKLESS DRIVING | Reckless Driving | 47 O.S. 11-901 | 3,956 |
| OBS, OBSTRUCTING OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 3,875 |
| RK2, SPEED NOT REASONABLE AND PROPER | Speeding | 47 O.S. 11-801 | 3,810 |
| 1\. DUS | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 3,796 |
| SPEEDING 16-20 OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 3,769 |
| DR3, Unlawful Possession of Drug Paraphernalia | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 3,763 |
| 1\. PETIT LARCENY | Larceny (Petit) | 21 O.S. 1706 | 3,752 |
| SEATBELT VIOLATION, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 3,710 |
| S51, SPEEDING (1-10 MPH OVER LIMIT) | Speeding | 47 O.S. 11-801 | 3,705 |
| DI1M, DRIVING UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 3,696 |
| DU9II, POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 3,691 |
| DI5A, TRANSPORTING OPEN CONTAINER - BEER | Transporting Open Container | 21 O.S. 1220 | 3,677 |
| LEWD, INDECENT OR LEWD ACTS WITH A CHILD UNDER SIXTEEN (16) | Indecent or Lewd Acts With Child | 21 O.S. 1123 | 3,674 |
| DI1M, DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 3,633 |
| LMFR, PETIT LARCENY | Larceny (Petit) | 21 O.S. 1706 | 3,633 |
| WFEL, POSSESSION OF A FIREARM WHILE IN THE COMMISSION OF A FELONY | Illegal Possession of a Firearm | 21 O.S. 1272 | 3,606 |
| FAIL TO CARRY VALID INS. VERIF | Driving Without Valid Insurance / Security | NA | 3,552 |
| S51, SPEEDING 1 - 10 mph over LIMIT | Speeding | 47 O.S. 11-801 | 3,539 |
| CHN, CHILD NEGLECT | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 3,516 |
| DL3, DRIVING WITH LICENSE CANCELED, SUSPENDED OR REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 3,501 |
| 2\. DRIVING WITH LICENSE CANCELLED/SUSPENDED/REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 3,490 |
| RT2, OPERATING A MV WITH EXPIRED REGISTRATION | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 3,456 |
| SPEEDING (1-10 MPH OVER LIMIT) | Speeding | 47 O.S. 11-801 | 3,440 |
| DR3MV, POSSESSION OR SELLING OF PARAPHERNALIA WHILE USING A MOTOR VEHICLE | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 3,420 |
| DL2, DRIVING WITH LICENSE SUSPENDED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 3,394 |
| DU8II, UNLAWFUL POSSESSION OF CONTROLLED DRUG - METHAMPHETAMINE | CDS Possession (Simple) | 63 O.S. 2-402 | 3,391 |
| TRAFFIC - SPEEDING 1-10 MILES OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 3,367 |
| 1\. POSS OF CDS | CDS Possession (Simple) | 63 O.S. 2-402 | 3,303 |
| BC, BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 3,264 |
| DR1, Public Intoxication | Public Intoxication | 37A O.S. 6-101 | 3,254 |
| ABOFF, ASSAULT & BATTERY UPON A POLICE OFFICER | Assault / Battery (On Official) | 21 O.S. 650 | 3,214 |
| 1\. DUI | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 3,169 |
| 1\. ASSAULT & BATTERY | Assault / Battery (Simple) | 21 O.S. 644 | 3,121 |
| FAIL TO PROPERLY WEAR SEATBELT | Seatbelt Violation | 47 O.S. 12-417 | 3,111 |
| ABDOM, DOMESTIC ABUSE - ASSAULT AND BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 3,092 |
| 1\. PUBLIC INTOXICATION (MISDEMEANOR) | Public Intoxication | 37A O.S. 6-101 | 3,080 |
| MALINJ, MALICIOUS INJURY OR DESTRUCTION OF PROPERTY LESS THAN \$1000 | Malicious Injury to Property | 21 O.S. 1760 | 3,078 |
| MALINJ, MALICIOUS INJURY OR DESTRUCTION OF PROPERTY LESS THAN \$1000 | Malicious Injury to Property | 21 O.S. 22 | 3,078 |
| DU4, POSSESSION OF CONTROLLED DRUG | CDS Possession (Simple) | 63 O.S. 2-402 | 2,998 |
| DR3, POSSESSION OR SELLING OF PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 2,997 |
| SB3, FAILURE TO WEAR SEATBELT | Seatbelt Violation | 47 O.S. 12-417 | 2,993 |
| RES, RESISTING ARREST | Resisting / Eluding Officer | 21 O.S. 19-540 | 2,975 |
| 1\. LARCENY OF MERCHANDISE FROM RETAILER | Larceny (Shoplifting) | 21 O.S. 1731 | 2,964 |
| 1\. DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF DRUGS | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 2,956 |
| ABDOM, DOMESTIC ABUSE BY STRANGULATION | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,940 |
| 1\. BURGLARY IN THE SECOND DEGREE | Burglary (First Degree) | 21 O.S. 1431 | 2,926 |
| ABGEN, ASSAULT AND BATTERY | Assault / Battery (Simple) | 21 O.S. 644 | 2,911 |
| SI1, FAILURE TO SIGNAL | Fail to Signal | 47 O.S. 11-604 | 2,904 |
| IN1, Inattentive Driving resulting in a collision | Inattentive Driving | 47 O.S. 11-901 | 2,892 |
| 75 IN 65, SPEEDING (10 OVER) | Speeding | 47 O.S. 11-801 | 2,860 |
| 1\. FAIL TO PROPERLY WEAR SEATBELT (DRIVER) | Seatbelt Violation | 47 O.S. 12-417 | 2,854 |
| 1\. GRAND LARCENY | Larceny (Grand) | 21 O.S. 1705 | 2,844 |
| DU9II, POSSESSION OF A CDS- METHAMPHETAMINE | CDS Possession (Simple) | 63 O.S. 2-402 | 2,837 |
| DU9, UNLAWFUL POSSESSION OF CONTROLLED DRUG | CDS Possession (Simple) | 63 O.S. 2-402 | 2,831 |
| ODO, UNLAWFUL POSSESSION OF CONTROLLED DRUG WITH INTENT TO DISTRIBUTE | CDS Possession With Intent (PWID) | 63 O.S. 2-401 | 2,828 |
| ES6, ELUDING A POLICE OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 2,815 |
| OWPN, FELON IN POSSESSION OF A FIREARM | Illegal Possession of a Firearm | 21 O.S. 1272 | 2,795 |
| WE3, FELONIOUSLY POINTING FIREARM | Pointing Weapon at Another | 21 O.S. 1279 | 2,789 |
| 2\. OBSTRUCTING AN OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 2,788 |
| FR5, FAILURE TO CARRY INSURANCE/ SECURITY VERIFICATION FORM | Driving Without Valid Insurance / Security | NA | 2,786 |
| ABDOM, DOMESTIC ASSAULT & BATTERY BY STRANGULATION | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,772 |
| ABDOM, DOMESTIC ASSAULT AND BATTERY BY STRANGULATION | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,769 |
| HR2, LEAVING SCENE OF COLLISION INVOLVING PROPERTY DAMAGE | Leaving the Scene of an Accident | 47 O.S. 10-103 | 2,754 |
| FR5, FAILURE TO CARRY VALID SECURITY VERIFICATION | Driving Without Valid Insurance / Security | NA | 2,749 |
| BH, THREATEN TO PERFORM ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 2,714 |
| FAILURE TO PAY TAXES DUE STATE | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 2,713 |
| BH, THREATEN AN ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 2,686 |
| PROCD, POSSESSION OF PROCEEDS IN VIOLATION OF UCDSA | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 2,685 |
| DI7, ACTUAL PHYSICAL CONTROL OF VEHICLE WHILE INTOXICATED | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 2,675 |
| DR3MV, Unlawful Possession of Drug Paraphernalia | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 2,675 |
| DI1, DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 2,671 |
| OWN, FALSE DECLARATION OF OWNERSHIP IN PAWN | False Declaration of Ownership | 59 O.S. 1512 | 2,637 |
| VPO, PROTECTIVE ORDER VIOLATION | Violation of Protective Order (VPO) | NA | 2,587 |
| MALINJ, MALICIOUS INJURY TO PROPERTY | Malicious Injury to Property | 21 O.S. 1760 | 2,584 |
| MALINJ, MALICIOUS INJURY TO PROPERTY | Malicious Injury to Property | 21 O.S. 22 | 2,584 |
| LP1, OPERATING VEHICLE WITH EXPIRED/IMPROPER TAG/DECAL | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 2,576 |
| 1\. DUI (MISDEMEANOR) | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 2,561 |
| 1\. POSSESSION OF CONTROLLED SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 2,534 |
| 1\. EMBEZZLEMENT | Embezzlement | 21 O.S. 59-1451 | 2,532 |
| LA5, LARCENY OF AUTOMOBILE | Larceny (Auto) | 21 O.S. 1720 | 2,510 |
| SPEEDING 1-10 MPH OVER (S51), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 2,510 |
| FAIL TO WEAR SEATBELT (DRIVER) (SB3), in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 2,498 |
| BRG3, BURGLARY- 3RD DEGREE | Burglary (Third Degree) | 21 O.S. 1435 | 2,494 |
| PROCD, ACQUIRE PROCEEDS FROM DRUG ACTIVITY | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 2,438 |
| DI5, TRANSPORTING OPEN CONTAINER - ALCOHOL | Transporting Open Container | 21 O.S. 1220 | 2,423 |
| SPEEDING (1-10 OVER), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 2,421 |
| 2\. RESISTING AN OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 2,418 |
| 1\. DOMESTIC ABUSE - ASSAULT & BATTERY (MISDEMEANOR) | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,414 |
| ABDOM, DOMESTIC ASSAULT AND BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,358 |
| OPERATING MOTOR VEHICLE W/OUT A VALID DRIVERS LICENSE, in violation of 47 OS 6-303(A) | Driving Without Valid License | 47 O.S. 6-112 | 2,333 |
| 1\. UNAUTHORIZED USE OF A VEHICLE | Unauthorized Use of Vehicle | 47 O.S. 4-102 | 2,332 |
| CHEND, CHILD ENDANGERMENT | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 2,319 |
| OP M/V W/O VALID DL | Driving Without Valid License | 47 O.S. 6-112 | 2,313 |
| DU9II, UNLAWFUL POSSESSION OF CDS- METHAMPHETAMINE | CDS Possession (Simple) | 63 O.S. 2-402 | 2,307 |
| RCSP, KNOWINGLY CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 2,306 |
| DMLWT, ACQUIRE PROCEEDS FROM DRUG ACTIVITY | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 2,292 |
| BC, BOGUS CHECK UNDER \$500 | Fraud (Bogus Check) | 21 O.S. 1541 | 2,288 |
| 2\. KNOWINGLY CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 2,266 |
| 1\. DRIVING UNDER SUSPENSION | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 2,265 |
| 1\. DOMESTIC ABUSE - ASSAULT AND BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 2,263 |
| COUNT 2 | ojoRegex Error: COUNT X | NA | 2,260 |
| DU9, POSSESSION OF CONTROLLED DRUG | CDS Possession (Simple) | 63 O.S. 2-402 | 2,245 |
| WE6, POSSESSION OF A FIREARM AFCF | Illegal Possession of a Firearm | 21 O.S. 1272 | 2,239 |
| OWPN, POSSESSION OF A FIREARM | Illegal Possession of a Firearm | 21 O.S. 1272 | 2,236 |
| 1\. TRAFFICKING IN ILLEGAL DRUGS | CDS Trafficking / Distribution | 63 O.S. 2-401 | 2,235 |
| 1\. ASSAULT AND BATTERY | Assault / Battery (Simple) | 21 O.S. 644 | 2,207 |
| PUBLIC INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 2,193 |
| 1\. 75 IN 65, SPEEDING | Speeding | 47 O.S. 11-801 | 2,187 |
| 1\. BURGLARY, SECOND DEGREE | Burglary (First Degree) | 21 O.S. 1431 | 2,180 |
| DL5, DRIVING A MOTOR VEHICLE WITHOUT A LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 2,179 |
| DR3, unlawful poss of drug paraphernalia | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 2,165 |
| DU9, POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 2,165 |
| XDUS | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 2,164 |
| DI5A, TRANSPORTING OPENED CONTAINER OF BEER | Transporting Open Container | 21 O.S. 1220 | 2,157 |
| DI5, TRANSPORTING OPEN CONTAINER OF LIQUOR | Transporting Open Container | 21 O.S. 1220 | 2,152 |
| 3\. PUBLIC INTOXICATION | Public Intoxication | 37A O.S. 6-101 | 2,144 |
| DU8II, POSSESSION OF A CDS - METHAMPHETAMINE | CDS Possession (Simple) | 63 O.S. 2-402 | 2,142 |
| SB1, CHILD RESTRAINT VIOLATIONS | Child Seatbelt Violation | 47 O.S. 11-1112 | 2,132 |
| BC, OBTAINING CASH OR MERCHANDISE BY BOGUS CHECK/FALSE PRETENSES | Fraud (Other / Unspecified) | NA | 2,115 |
| OWPN, FELON IN POSSESSION OF A FIREARM (AFCF) | Illegal Possession of a Firearm | 21 O.S. 1272 | 2,090 |
| DU8I, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE - MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 2,078 |
| LP4, AFFIXING IMPROPER LICENSE PLATE TO MV | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 2,069 |
| 1\. VIOLATION OF COMPULSORY EDUCATION ACT | Violation of Compulsory Education Act | 70 O.S. 10-105 | 2,068 |
| SP 1-10 OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 2,067 |
| DL5, OPERATE VEHICLE WITHOUT A VALID DL | Driving Without Valid License | 47 O.S. 6-112 | 2,061 |
| DU9, POSSESSION OF A CDS- MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 2,047 |
| 1\. THREATEN TO PERFORM ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 2,043 |
| 1\. DRIVING WITH LICENSE SUSPENDED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 2,029 |
| SEAT BELT - DRIVER NOT WEARING SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 2,015 |
| 65 IN 55, SPEEDING | Speeding | 47 O.S. 11-801 | 2,008 |
| UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA, in violation of 63 OS 2-405 | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 2,002 |
| 85 IN 75, SPEEDING, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,994 |
| 1\. DOMESTIC ABUSE - ASSAULT & BATTERY (DISMISSED) | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,990 |
| 1\. OBSTRUCTING OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 1,981 |
| DU9, UNLAWFUL POSSESSION OF CDS- MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 1,973 |
| 1\. POSSESSION OF FIREARM AFTER FORMER FELONY CONVICTION | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,964 |
| 1\. SPEEDING | Speeding | 47 O.S. 11-801 | 1,951 |
| 1\. DOMESTIC ABUSE--ASSAULT & BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,950 |
| OPJ, CONSPIRACY TO COMMIT A FELONY | Conspiracy (Other / Unspecified) | 21 O.S. 11-421 | 1,947 |
| BC, OBTAINING MONEY OR PROPERTY BY MEANS OF A FALSE AND BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 1,944 |
| DL5, NO VALID DRIVER'S LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,939 |
| DU4, POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,935 |
| 4\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,931 |
| 1\. LARCENY OF MERCHANDISE FROM A RETAILER | Larceny (Shoplifting) | 21 O.S. 1731 | 1,928 |
| FE1A, ENDANGERING OTHERS WHILE ELUDING POLICE OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 1,926 |
| PROCD, POSSESSION OF PROCEEDS DERIVED FROM A VIOLATION OF THE UCDSA | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 1,924 |
| MALINJ, MALICIOUS INJURY AND DESTRUCTION OF PROPERTY | Malicious Injury to Property | 21 O.S. 1760 | 1,922 |
| MALINJ, MALICIOUS INJURY AND DESTRUCTION OF PROPERTY | Malicious Injury to Property | 21 O.S. 22 | 1,922 |
| IDAP, ACQUIRE PROCEEDS FROM DRUG ACTIVITY | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 1,910 |
| SWIK, SHOOTING WITH INTENT TO KILL | Shooting With Intent to Kill | 21 O.S. 652 | 1,909 |
| DU9, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE - MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 1,906 |
| FR5, FAILURE TO MAINTAIN INSURANCE OR SECURITY | Driving Without Valid Insurance / Security | NA | 1,894 |
| 1\. FALSELY PERSONATE ANOTHER TO CREATE LIABILITY | Fraud (False Personation) | 21 O.S. 1531-1533 | 1,886 |
| SPEEDING 21-25 OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,878 |
| SPEEDING 15 MPH OVER LIMIT, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,875 |
| 1\. POSSESSION OF STOLEN VEHICLE | Possession of Stolen Vehicle | 47 O.S. 4-103 | 1,874 |
| OWPN, POSSESSION OF FIREARM AFTER FORMER FELONY CONVICTION | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,863 |
| S31, SPEEDING 31-35 MPH OVER | Speeding | 47 O.S. 11-801 | 1,860 |
| 3\. DRIVING WITH LICENSE CANCELLED/SUSPENDED/REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,858 |
| SPEEDING 16-20 MPH OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,858 |
| WE6, POSSESSION OF A FIREARM | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,855 |
| BC, OBTAINING CASH OR MERCHANDISE BY BOGUS CHECK / FALSE PRETENSES | Fraud (Other / Unspecified) | NA | 1,849 |
| 55 IN 45, SPEEDING | Speeding | 47 O.S. 11-801 | 1,844 |
| RT5, OTHER REGISTRATION / TITLE VIOLATIONS | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 1,840 |
| OPERATE VEHICLE WITHOUT A VALID DL | Driving Without Valid License | 47 O.S. 6-112 | 1,835 |
| DU9, MARIJUANA \*MISDEMEANOR\* (NOT USING MV) | CDS (Other / Unspecified) | NA | 1,830 |
| ABDGR, ASSAULT AND BATTERY WITH A DANGEROUS WEAPON | Assault / Battery (Dangerous Weapon) | 21 O.S. 645 | 1,812 |
| PUBSFT, INTERFERENCE WITH EMERGENCY TELEPHONE CALL | Interfering With Emergency Call | 21 O.S. 48-1211 | 1,799 |
| 1\. DRIVING A MV WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,797 |
| RBDW, ROBBERY WITH A DANGEROUS WEAPON | Robbery (With a Dangerous Weapon) | 21 O.S. 801 | 1,795 |
| TRAFFIC - SPEEDING (1 to 10 MPH), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,790 |
| 1\. SPEEDING 1-10 OVER | Speeding | 47 O.S. 11-801 | 1,785 |
| 1\. UNLAWFUL POSSESSION OF CONTROLLED DRUG WITH INTENT TO DISTRIBUTE | CDS Possession With Intent (PWID) | 63 O.S. 2-401 | 1,785 |
| ABDOM, ASSAULT AND BATTERY - DOMESTIC IN THE PRESENCE OF A MINOR CHILD | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,775 |
| DI2D, DUI DRUGS | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,761 |
| 2\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA-MISDEMEANOR | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,746 |
| LC3, DRIVING LEFT OF CENTER | Driving Left of Center | NA | 1,740 |
| RCSP, RECEIVING/CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,739 |
| FR5, FINANCIAL RESPONSIBILITY/SECURITY VERIFICATION | Driving Without Valid Insurance / Security | NA | 1,735 |
| 1\. CHILD NEGLECT | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 1,734 |
| DI5A, UNLAWFUL TRANSPORTATION OF AN OPEN CONTAINER | Transporting Open Container | 21 O.S. 1220 | 1,725 |
| 2\. UNLAWFUL POSS OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,722 |
| MSOS, STALKING | Stalking | 21 O.S. 1173 | 1,718 |
| OBS, OBSTRUCTING AN OFFICER IN THE PERFORMANCE OF HIS OFFICIAL DUTIES | Obstruction of Justice | 21 O.S. 19-540(A) | 1,702 |
| DE0, DEFECTIVE VEHICLE, IMPROPER EQUIPMENT OR VEHICLE WINDSHIELD OR WINDOWS OBSTRUCTED | Defective Vehicle | 47 O.S. 13-101 | 1,701 |
| SEXB, SEXUAL BATTERY | Rape (First Degree) | 21 O.S. 1114 | 1,700 |
| ABDGR, ASSAULT & BATTERY WITH A DEADLY WEAPON | Assault / Battery (Dangerous Weapon) | 21 O.S. 645 | 1,697 |
| 1\. POSSESSION OF CDS (MISDEMEANOR) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,695 |
| DI5A, TRANSPORTING OPEN CONTAINER | Transporting Open Container | 21 O.S. 1220 | 1,692 |
| ANI21, CRUELTY TO ANIMALS | Cruelty to Animals | 21 O.S. 1685-1719 | 1,686 |
| OLAR, PETIT LARCENY | Larceny (Petit) | 21 O.S. 1706 | 1,684 |
| 1\. NO SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 1,683 |
| 74 IN 65, SPEEDING | Speeding | 47 O.S. 11-801 | 1,681 |
| SPEEDING 80 IN 70 | Speeding | 47 O.S. 11-801 | 1,680 |
| SW1, OVERWEIGHT VIOLATION | Overweight Violation | 47 O.S. 14-109 | 1,680 |
| DU4II, POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,675 |
| DI1, DRIVING WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,673 |
| FR5, FAIL TO COMPLY WITH COMPULSORY INSURANCE LAW | Driving Without Valid Insurance / Security | NA | 1,673 |
| INDE, INDECENT EXPOSURE | Indecent Exposure | 21 O.S. 1021 | 1,667 |
| DU2II, UNLAWFUL POSSESSION OF CONTROLLED DRUG - METHAMPHETAMINE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,657 |
| SPEEDING 11-15 OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,656 |
| UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,616 |
| CHSA, SEXUAL ABUSE OF A CHILD | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 1,604 |
| DU8II, POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,597 |
| 1\. POSSESSION OF CDS (FELONY) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,591 |
| ABDOM, DOMESTIC ABUSE ( ASSAULT & BATTERY) | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,589 |
| DMLWT, POSSESSION OF PROCEEDS DERIVED FROM A VIOLATION OF THE UNIFORM CONTROLLED DANGEROUS SUBSTANCE ACT | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 1,579 |
| DU4, UNLAWFUL POSSESSION OF CONTROLLED DRUG | CDS Possession (Simple) | 63 O.S. 2-402 | 1,579 |
| 1\. FAILURE TO COMPLY WITH COMPULSORY INSURANCE LAW | Driving Without Valid Insurance / Security | NA | 1,578 |
| DE0, DEFECTIVE VEHICLE | Defective Vehicle | 47 O.S. 13-101 | 1,577 |
| DI5A, TRANSPORTING OPEN CONTAINER - BEER/INTOXICATING BEVERAGES | Transporting Open Container | 21 O.S. 1220 | 1,574 |
| SEAT BELT - DRIVER NOT WEARING SEAT BELT EFF 7-1-09 | Seatbelt Violation | 47 O.S. 12-417 | 1,571 |
| SPEEDING 1 TO 10 MPH OVER | Speeding | 47 O.S. 11-801 | 1,564 |
| HM11, MURDER - FIRST DEGREE | Murder (First Degree) | 21 O.S. 701.7 | 1,559 |
| FAILURE TO PROPERLY WEAR SEATBELT (DRIVER), in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,554 |
| DL2C, DRIVING UNDER SUSPENSION / DUS (3rd offense) | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,551 |
| FAILURE TO WEAR SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 1,536 |
| NO SEAT BELT (DRIVER), in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,534 |
| FR5, FAILURE TO CARRY INSURANCE /SECURITY VERIFICATION FORM | Driving Without Valid Insurance / Security | NA | 1,531 |
| 1\. DOMESTIC ABUSE-ASSAULT & BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,530 |
| LEWD, LEWD MOLESTATION | Indecent or Lewd Acts With Child | 21 O.S. 1123 | 1,529 |
| OPRP, TRESPASSING AFTER BEING FORBIDDEN | Trespassing After Being Forbidden | 21 O.S. 1835 | 1,521 |
| TRAFFIC - FAILURE TO WEAR SAFETY BELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,518 |
| 3\. POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,515 |
| NO SEAT BELT - DRIVER, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,511 |
| OLAR, GRAND LARCENY | Larceny (Grand) | 21 O.S. 1705 | 1,511 |
| 1\. DRIVING A M/V WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,503 |
| OBS, OBSTRUCTING AN OFFICER IN THE PERFORMANCE OF THEIR OFFICIAL DUTIES | Obstruction of Justice | 21 O.S. 19-540(A) | 1,501 |
| SPEEDING 11-14 MPH OVER LIMIT, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,500 |
| 1 TO 10 MPH OVER, SPEEDING, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,490 |
| FAILURE TO WEAR PROPERLY ADJUSTED SAFETY SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 1,489 |
| 1\. FAIL TO COMPLY WITH COMPULSORY INSURANCE LAW | Driving Without Valid Insurance / Security | NA | 1,484 |
| POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE | CDS Possession (Simple) | 63 O.S. 2-402 | 1,482 |
| ABDOM, ASSAULT AND BATTERY - DOMESTIC ABUSE | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,477 |
| DR3, POSSESSION OF PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,472 |
| IDT, Identity Theft | Fraud (False Personation) | 21 O.S. 1531-1533 | 1,466 |
| FAIL TO PROPERLY WEAR SEATBELT (DRIVER) | Seatbelt Violation | 47 O.S. 12-417 | 1,463 |
| 1\. OBTAINING CASH AND/OR MERCHANDISE BY BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 1,444 |
| DL1, DRIVING WITH LICENSE REVOKED | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,442 |
| 1\. SEATBELT VIOLATION | Seatbelt Violation | 47 O.S. 12-417 | 1,436 |
| DE5, DEFECTIVE VEHICLE | Defective Vehicle | 47 O.S. 13-101 | 1,434 |
| RK2, SPEEDING | Speeding | 47 O.S. 11-801 | 1,421 |
| 1\. OBTAINING CASH OR MERCHANDISE BY BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 1,419 |
| 1\. DOMESTIC ASSAULT AND BATTERY BY STRANGULATION | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,412 |
| ENTB, BREAKING AND ENTERING | Entering with Intent To Commit a Crime | 21 O.S. 1438 | 1,408 |
| DL5, DRIVING WITHOUT A LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,405 |
| NO SEATBELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,405 |
| IL0, IMPROPER USE OF LANE | Changing Lanes Unsafely | 47 O.S. 11-309 | 1,402 |
| 1\. OPERATE VEHICLE WITHOUT A VALID DL | Driving Without Valid License | 47 O.S. 6-112 | 1,401 |
| 1\. UTTERING A FORGED INSTRUMENT | Fraud (Forgery / Counterfeiting) | 21 O.S. 1561-1628 | 1,396 |
| 1\. POSSESSION OF CDS | CDS Possession (Simple) | 63 O.S. 2-402 | 1,383 |
| DU8II, poss of cds | CDS Possession (Simple) | 63 O.S. 2-402 | 1,383 |
| DR1, BEING DRUNK IN A PUBLIC PLACE | Public Intoxication | 37A O.S. 6-101 | 1,367 |
| 3\. OBSTRUCTING AN OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 1,366 |
| OWPN, FELON IN POSSESSION OF A FIREARM/ AFCF | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,366 |
| IL2, FAILURE TO KEEP IN PROPER LANE | Changing Lanes Unsafely | 47 O.S. 11-309 | 1,359 |
| 1\. BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 1,356 |
| 1\. DRIVING WHILE IMPAIRED | Driving While Impaired | 47 O.S. 67-761 | 1,354 |
| 2\. THREATEN TO PERFORM ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 1,350 |
| 1\. FAILURE TO WEAR PROPERLY ADJUSTED SAFETY SEAT BELT | Seatbelt Violation | 47 O.S. 12-417 | 1,348 |
| 2\. UNLAWFUL POSSESSION OF DRUG PARAPHERNILIA (MISD) | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,344 |
| DISDEC, OUTRAGING PUBLIC DECENCY | Outraging Public Decency | NA | 1,338 |
| 1\. FAIL TO PROPERLY WEAR SEATBELT (DRIVER), in violation of 47 O.S. 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,334 |
| LA4, POSSESSION OF A STOLEN VEHICLE | Possession of Stolen Vehicle | 47 O.S. 4-103 | 1,318 |
| LP1, OPERATE VEHICLE ON WHICH ALL TAXES DUE THIS STATE HAVE NOT BEEN PAID | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 1,302 |
| FAILURE TO COMPLY WITH COMPULSORY INS. LAW | Driving Without Valid Insurance / Security | NA | 1,298 |
| SPEEDING 75 IN 65 | Speeding | 47 O.S. 11-801 | 1,297 |
| 1\. DRIVING WITHOUT A DRIVER'S LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,295 |
| ABDOM, DOMESTIC ASSAULT & BATTERY IN THE PRESENCE OF A MINOR CHILD | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,285 |
| S51, SPEEDING IN EXCESS OF LAWFUL MAXIMUM LIMIT | Speeding | 47 O.S. 11-801 | 1,283 |
| DI7, ACTUAL PHYSICAL CONTROL OF VEHICLE UNDER THE INFLUENCE | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,282 |
| ABDOM, DOMESTIC ABUSE-ASSAULT AND BATTERY | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,281 |
| 1\. SPEEDING 1-10 MPH OVER LIMIT | Speeding | 47 O.S. 11-801 | 1,270 |
| CHAB, CHILD ABUSE | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 1,270 |
| 1\. XOBTAINING CASH OR MERCHANDISE BY BOGUS CHECK/FALSE PRETENSES | Fraud (Other / Unspecified) | NA | 1,258 |
| RAPE, RAPE IN THE FIRST DEGREE | Rape (First Degree) | 21 O.S. 1114 | 1,254 |
| DU2I, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE SCHED I- (MARIJUANA) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,249 |
| DL2B, DRIVING UNDER SUSPENSION / DUS (2nd offense) | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,244 |
| DI2DM, DUI DRUGS (MUNICIPAL ARREST) | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,243 |
| SPEEDING 1-10 OVER (TR), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,242 |
| OWPN, POSSESSION OF FIREARM AFTER JUVENILE ADJUDICATION | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,239 |
| 1\. DRIVING WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,238 |
| DL4, VIOLATION OF DL RESTRICTIONS | Driving Without Valid License | 47 O.S. 6-112 | 1,236 |
| 1\. RESISTING AN OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 1,235 |
| BRG1, BURGLARY IN THE FIRST DEGREE | Burglary (First Degree) | 21 O.S. 1431 | 1,235 |
| ES6, ATTEMPTING TO ELUDE A POLICE OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 1,235 |
| DI5, TRANSPORTING OPEN CONTAINER - LIQUOR | Transporting Open Container | 21 O.S. 1220 | 1,230 |
| OWPN, POSSESSION OF A FIREARM WHILE IN THE COMMISSION OF A FELONY | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,229 |
| OPJ, CONSPIRACY | Conspiracy (Other / Unspecified) | 21 O.S. 11-421 | 1,227 |
| SP 1-10 MPH OVER, in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,220 |
| DL5, DRIVING A MV W/OUT A LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,210 |
| ABDOM, DOMESTIC ASSAULT & BATTERY WITH A DANGEROUS WEAPON | Domestic Assault / Battery (Dangerous Weapon) | 21 O.S. 644 | 1,209 |
| DI6AM, DUI ALCOHOL - SECOND OFFENSE (MUNICIPAL ARREST) | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,205 |
| CCV, UNAUTHORIZED USE OF CREDIT/DEBIT CARD | Fraud (Credit Card) | 21 O.S. 1550 | 1,203 |
| FORG, UTTERING FORGED INSTRUMENT UNDER \$1000 | Fraud (Forgery / Counterfeiting) | 21 O.S. 1561-1628 | 1,201 |
| FR5, FAILURE TO CARRY INSURANCE / SECURITY VERIFICATION FORM | Driving Without Valid Insurance / Security | NA | 1,197 |
| DU2I, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE SCHED II- (METH) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,190 |
| 1\. LARCENY OF LOST PROPERTY | Larceny (Other / Unspecified) | NA | 1,185 |
| SPEEDING (1-10 MPH), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,184 |
| DI7, ACTUAL PHYSICAL CONTROL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,183 |
| DI1M, DRIVING WHILE UNDER THE INFLUENCE | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,182 |
| RCSP, KNOWINGLY RECEIVING OR CONCEALING STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,182 |
| FAILURE TO COMPLY WITH COMPULSORY INSURANCE LAW, in violation of 47 OS 7-606(A) | Driving Without Valid Insurance / Security | NA | 1,179 |
| 1\. SPEEDING 1 TO 10 MPH OVER | Speeding | 47 O.S. 11-801 | 1,171 |
| DR3, UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA (M) | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,166 |
| SEATBELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,165 |
| DI1M, DRIVING UNDER THE INFLUENCE OF ALCOHOL (MUNICIPAL ARREST) | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,164 |
| DI8, DRIVING WHILE IMPAIRED | Driving While Impaired | 47 O.S. 67-761 | 1,163 |
| TAXS, POSSESSION OF CONTROLLED DRUGS W/O TAX STAMP AFFIXED | CDS Possession (Tax Stamp) | 68 O.S. 1-450.8 | 1,161 |
| TR SPEEDING (1-10 OVER), in violation of 47 OS 11-801a | Speeding | 47 O.S. 11-801 | 1,159 |
| 1\. BREAKING AND ENTERING DWELLING WITHOUT PERMISSION | Entering with Intent To Commit a Crime | 21 O.S. 1438 | 1,153 |
| 3\. RESISTING AN OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 1,151 |
| PROS, ENGAGING IN PROSTITUTION | Engaging in Sex Work (Simple) | NA | 1,150 |
| PROS, OFFERING TO ENGAGE IN AN ACT OF PROSTITUTION | Engaging in Sex Work (Simple) | NA | 1,150 |
| 2\. OBSTRUCTING OFFICER | Obstruction of Justice | 21 O.S. 19-540(A) | 1,149 |
| 1\. TRESPASSING AFTER BEING FORBIDDEN | Trespassing After Being Forbidden | 21 O.S. 1835 | 1,147 |
| 1\. DUI-DRUGS | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,143 |
| 2\. DUS | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,142 |
| ABDOM, ASSAULT AND BATTERY - DOMESTIC - 2ND OFFENSE | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,142 |
| 1\. ACTUAL PHYSICAL CONTROL OF VEHICLE UNDER THE INFLUENCE | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,140 |
| 1\. DRIVING UNDER SUSPENSION (MISDEMEANOR) | Driving Under Suspension / Revocation | 47 O.S. 6-303 | 1,138 |
| SEXO, FAILURE TO REGISTER AS SEX OFFENDER | Failure to Comply With Sex Offender Registration Act | 57 O.S. 581-587 | 1,137 |
| FAIL TO PROPERLY WEAR SEATBELT (FRONT SEAT PASSENGER), in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,135 |
| 1\. UNLAWFUL POSSESSION OF CONTROLLED DRUG W/INTENT TO DISTRIBUTE | CDS Possession With Intent (PWID) | 63 O.S. 2-401 | 1,134 |
| 2\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA - A MISDEMEANOR | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,134 |
| 1\. ELUDING/ATTEMPTING TO ELUDE POLICE OFFICER | Resisting / Eluding Officer | 21 O.S. 19-540 | 1,127 |
| 2\. POSSESSION OF CDS (MISDEMEANOR) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,127 |
| DL5, DRIVING WITHOUT A VALID DRIVER'S LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,125 |
| RR2, BAIL JUMPING | Bail Jumping | 22 O.S. 1110 | 1,124 |
| 1\. DISTURBING THE PEACE | Disturbing the Peace | 21 O.S. 1361-1363 | 1,122 |
| OWPN, POSSESSION OF A FIREARM (AFCF) | Illegal Possession of a Firearm | 21 O.S. 1272 | 1,122 |
| NO DRIVERS LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,117 |
| OWN, FALSE DECLARATION OF OWNERSHIP IN PAWN SHOP | False Declaration of Ownership | 59 O.S. 1512 | 1,115 |
| 1\. OBTAINING MERCHANDISE BY MEANS OF BOGUS CHECK, A MISDEMEANOR | Fraud (Bogus Check) | 21 O.S. 1541 | 1,114 |
| CWEDJ, POSSESSION OF CONTRABAND IN A PENAL INSTITUTION | NA | NA | 1,114 |
| DU8II, UNLAWFUL POSSESSION OF CONTROLLED DRUG | CDS Possession (Simple) | 63 O.S. 2-402 | 1,114 |
| 1\. POSSESSION OF CONTROLLED DANGEROUS SUBSTANCE (DISMISSED) | CDS Possession (Simple) | 63 O.S. 2-402 | 1,112 |
| INATTENTIVE DRIVING RESULTING IN COLLISION | Inattentive Driving | 47 O.S. 11-901 | 1,109 |
| NO SEAT BELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,109 |
| RCSP, Knowingly Concealing Stolen Property | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,109 |
| 2\. POSSESSION OF DRUG PARAPHERNALIA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,108 |
| RCSP, RECEIVE, POSSESS OR CONCEAL STOLEN PROPERTY | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,107 |
| ODM, GANG RELATED OFFENSE | Gang Related Offense | 21 O.S. 856.3 | 1,102 |
| PROCD, POSSESSION OF PROCEEDS DERIVED FROM A VIOLATION OF THE UNIFORM CONTROLLED DANGEROUS SUBSTANCE ACT | Possession of Proceeds in Violation UCDSA | 63 O.S. 2-503 | 1,098 |
| RCSP, RECEIVE, POSSESS OR CONCEAL STOLEN PROPERTY- UNDER \$1000 | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,096 |
| FAILURE TO COMPLY WITH COMPULSORY INSURANCE LAW | Driving Without Valid Insurance / Security | NA | 1,094 |
| IDT, IDENTITY THEFT | Fraud (False Personation) | 21 O.S. 1531-1533 | 1,093 |
| FSOD, FORCIBLE ORAL SODOMY | Forcible Sodomy | 21 O.S. 888 | 1,092 |
| DU8I, POSSESSION OF A CDS - MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 1,091 |
| 1\. EXPUNGED COUNT | NA | NA | 1,087 |
| 2\. DRIVING WITHOUT A DRIVER'S LICENSE | Driving Without Valid License | 47 O.S. 6-112 | 1,083 |
| 1\. CHILD ENDANGERMENT BY DRIVING UNDER THE INFLUENCE | Child Abuse / Neglect / Sexual Abuse | 21 O.S. 843-846 | 1,080 |
| 1\. OBSTRUCTING AN OFFICER (MISDEMEANOR) | Obstruction of Justice | 21 O.S. 19-540(A) | 1,078 |
| 1\. VIOLATION OF PROTECTIVE ORDER | Violation of Protective Order (VPO) | NA | 1,078 |
| FR5, FAILURE TO PROVIDE PROOF OF SECURITY VERIFICATION TO OFFICER | Driving Without Valid Insurance / Security | NA | 1,077 |
| 2\. UNLAWFUL POSS OF DRUG PARA | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,076 |
| 1\. PUBLIC INTOXICATION - A MISDEMEANOR | Public Intoxication | 37A O.S. 6-101 | 1,074 |
| 1\. DOMESTIC ABUSE - A&B | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,072 |
| FAIL TO COMPLY WITH COMP INS LAW | Driving Without Valid Insurance / Security | NA | 1,070 |
| SEAT BELT - DRIVER NOT (PROPERLY) WEARING SEAT BELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,070 |
| 2\. UNLAWFUL POSSESSION OF DRUG PARAPHERNALIA (DISMISSED) | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,069 |
| DI5, TRANSPORTING OPEN CONTAINER OF ALCOHOLIC BEVERAGE | Transporting Open Container | 21 O.S. 1220 | 1,069 |
| DU9II, UNLAWFUL POSSESSION OF CONTROLLED DRUG - METH | CDS Possession (Simple) | 63 O.S. 2-402 | 1,068 |
| LA4, Possession Of Stolen Vehicle | Possession of Stolen Vehicle | 47 O.S. 4-103 | 1,068 |
| 1\. FAILURE TO REGISTER AS SEX OFFENDER | Failure to Comply With Sex Offender Registration Act | 57 O.S. 581-587 | 1,065 |
| FAILURE TO CARRY SECURITY VERIFICATION, in violation of 47 OS 7-606(A) | Driving Without Valid Insurance / Security | NA | 1,063 |
| FORG, POSSESSION OF FORGED NOTES OR INSTRUMENTS | Fraud (Forgery / Counterfeiting) | 21 O.S. 1561-1628 | 1,062 |
| HDRO, INTERFERENCE WITH EMERGENCY TELEPHONE CALL | Interfering With Emergency Call | 21 O.S. 48-1211 | 1,062 |
| DE6, DEFECTIVE VEHICLE | Defective Vehicle | 47 O.S. 13-101 | 1,061 |
| 1\. FAIL TO PROPERLY WEAR SEATBELT | Seatbelt Violation | 47 O.S. 12-417 | 1,054 |
| DI0, DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,051 |
| 1\. FALSE DECLARATION OF OWNERSHIP IN PAWN | False Declaration of Ownership | 59 O.S. 1512 | 1,044 |
| RW1, FAILURE TO YIELD RIGHT OF WAY TO EMERGENCY VEH | Fail to Yield to Emergency Vehicle | 47 O.S. 11-405 | 1,039 |
| TAXES DUE STATE, in violation of 47 OS 1151(A)(5) | Driving Without Proper Tag / Registration | 47 O.S. 74-1151 | 1,038 |
| BC, OBTAINING CASH OR MERCHANDISE BY BOGUS CHECK | Fraud (Bogus Check) | 21 O.S. 1541 | 1,036 |
| 1\. DUI - ALCOHOL | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,035 |
| DU4, POSSESSION OF A CONTROLLED DANGEROUS SUBSTANCE - MARIJUANA | CDS Possession (Simple) | 63 O.S. 2-402 | 1,030 |
| ABDOM, DOMESTIC ABUSE | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,024 |
| 1\. DISMISSED | ojoRegex Error: DISMISSED | NA | 1,020 |
| SEXO, FAILURE TO COMPLY WITH THE SEX OFFENDER REGISTRATION ACT | Failure to Comply With Sex Offender Registration Act | 57 O.S. 581-587 | 1,019 |
| 1\. PUBLIC INTOXICATION-MISD. | Public Intoxication | 37A O.S. 6-101 | 1,018 |
| RCSP, POSSESSING/RECEIVING/CONCEALING STOLEN PROPERTY - UNDER \$1000 | Receiving / Concealing Stolen Property | 21 O.S. 1713 | 1,018 |
| FR5, FAILURE TO PROVIDE PROOF OF SECURITY VERIFICATION | Driving Without Valid Insurance / Security | NA | 1,013 |
| ENTB, BREAKING AND ENTERING DWELLING WITHOUT PERMISSION | Entering with Intent To Commit a Crime | 21 O.S. 1438 | 1,012 |
| DI2D, DRIVING A MOTOR VEHICLE WHILE UNDER THE INFLUENCE OF DRUGS | Driving Under the Influence / Actual Physical Control | 47 O.S. 11-902 | 1,010 |
| 2\. SPEEDING IN EXCESS OF LAWFUL MAXIMUM LIMIT | Speeding | 47 O.S. 11-801 | 1,009 |
| BH, THREATENING TO PERFORM AN ACT OF VIOLENCE | Threaten or Plan Act of Violence | 21 O.S. 1378 | 1,009 |
| ABDOM, DOMESTIC ASSAULT AND BATTERY IN PRESENCE OF MINOR | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,007 |
| ABDOM, ASSAULT AND BATTERY - DOMESTIC - BY STRANGULATION | Domestic Assault / Battery (Simple) | 21 O.S. 644 | 1,006 |
| NO SEAT BELT (TR) DRIVER NOT WEARING SEAT BELT, in violation of 47 OS 12-417(A) | Seatbelt Violation | 47 O.S. 12-417 | 1,006 |
| 1\. FAILURE TO WEAR SEAT BELT PROPERLY | Seatbelt Violation | 47 O.S. 12-417 | 1,005 |
| 2\. UNLAWFUL POSS DRUG PARAPHERNALIA (MIS) | CDS Paraphernalia Possession / Distribution | 63 O.S. 2-405 | 1,004 |
| Seat Belt - Driver Not Wearing Seat Belt | Seatbelt Violation | 47 O.S. 12-417 | 1,002 |
