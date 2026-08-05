### on\_sato\_cd

<br>**Form**:  
Individual Tax Return  
<br>**Section**:  
Tax offsets  
<br>**Question**:  
Senior Australians (includes age pensioners, service pensioners and
self-funded retirees)  
<br>**Label**:  
Senior Australians tax offset code  
<br>**General Notes**:  
Senior Australians tax offset claim code type for non-veterans. Claim
eligibility is contingent on the taxpayer having reported the
appropriate code at the tax offset item. Veterans claim codes are
captured in on\_sato\_vet\_cd.   

<br>**Latest or Current Conditions**:  
Latest year: 2011-12. This variable refers to the tax offset code used
to calculate the Senior Australian tax offset.  
  
*Code Definition*  
**A:** Single, widowed or separated.  
**B:** Both taxpayer and spouse were eligible for the senior Australians
tax offset and had to live apart due to illness or lived apart because
one of them was in a nursing home.  
**C:**Taxpayer’s spouse was not eligible for the senior Australians tax
offset and taxpayer and spouse had to live apart due to illness or lived
apart because one of them was in a nursing home.  
**D:** Taxpayer and spouse lived together and were both eligible for the
seniors Australians tax offset.  
**E:** Taxpayer and spouse lived together, but spouse was not eligible
for the senior Australians tax offset.  
**Exceptions:**    
**B:** Both A and B applied, and spouse’s rebate income was less than a
given threshold ($18,334).  
**C:** Both A and C applied, and spouse received an Australian
government payment as listed at question 6 Australian Government
pensions and allowances (see i\_gov\_pension) and the spouse’s rebate
income was less than a given threshold ($22,767).  
**D:** Both A and D applied, and the spouse’s rebate income was less
than a given threshold ($12,494).  
**E:** Both A and E applied, and spouse received an Australian
government payment as listed at question 6 Australian Government
pensions and allowances (see i\_gov\_pension) and the spouse’s rebate
income was less than a given threshold ($15,180).  
   

<br>**Latest or Current Calculations**:  
Latest year: 2011-12. Print the Senior Australians tax offset code.   

<br>**Earliest Conditions**:  
For 1996-97, this variable refers to the rebate code used to calculate
the eligible rebate under the low income aged persons rebate.  
  
*Code Definition*  
**R:** Single, widowed, separated or sole parent.  
**T:** Both taxpayer and spouse were eligible for this rebate and ‘had
to live apart due to illness’.  
**C:** Taxpayer’s spouse was not eligible for this rebate and taxpayer
and spouse ‘had to live apart due to illness’.  
**U:** Taxpayer and spouse lived together and were both eligible for
this rebate.  
**K:** Taxpayer and spouse lived together, but spouse was not eligible
for this rebate.  
   

<br>**Earliest Calculations**:  
For 1996-97, print the low income aged persons claim type code.   

<br>**URL Address**:  
NA  
<br>**Type**:  
Numerical  
<br>**Form Location**:  

1991(NA),1992(NA),1993(NA),1994(NA),1995(NA),1996(NA),1997(Pg5\_41N),1998(Pg3\_R3N),1999(Pg3\_R3N),2000(Pg4\_R3N),2001(Pg3\_R3N),2002(Pg3\_T2N),2003(Pg3\_T2N),2004(Pg3\_T2N),2005(Pg3\_T2N),2006(Pg3\_T2N),2007(Pg3\_T2N),2008(Pg4\_T2N),2009(Pg4\_T2N),2010(Pg4\_T2N),2011(Pg4\_T2N),2012(Pg4\_T2N),2013(NA),2014(NA),2015(NA),2016(NA),2017(NA),2018(NA),2019(NA),2020(NA),2021(NA),2022(NA)

<br>**Event time line: Significance, Type of change(s), Description of
change(s)**:  

**1997**: Major; Introduced; New variable.

**2000**: Major; Component; Change in code letter names to A,B,C,D,E
(see notes for definitions). The special rebate for those who had to
live apart due to illness also applies if either the taxpayer or spouse
was in a nursing home at any time.

**2001**: Minor; Component; Removal of “sole parent” from requirements
under code letter A.

**2002**: Minor; Question; Question label changed from ‘low income aged
persons tax offset code’ to ‘senior Australians tax offset code’.

**2004**: Minor; Question; Question label changed from ‘senior
Australians tax offset code’ to ‘Senior Australians (includes age
pensioners, service pensioners and self-funded retirees)’. For service
pensioners tax offset code see on\_sato\_vet\_cd.

**2012**: Major; Variable merged; Tax offset merged with Pensioner tax
offset to become the Senior and pensioner tax offset.

**2013**: Major; Discontinued; Variable discontinued.

------------------------------------------------------------------------
