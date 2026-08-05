### i\_ess

<br>**Form**:  
Individual Tax Return  
<br>**Section**:  
Income  
<br>**Question**:  
Employee Share Schemes: Total assessable discount amount  
<br>**Label**:  
Total assessable discount amount from Employee Share Schemes  
<br>**General Notes**:  
Total assessable income from taxed upfront and deferred tax Employee
Share Schemes less any applicable deductions.  
  
Employee Share Schemes (ESS) give employees:  
- shares in the company they work for at a discounted price  
- the opportunity to buy shares in the company in the future (this is
called a right or option).  
  
The discount you received on the market value for ESS interests needs to
be included in your tax return as assessable income.  

  

<br>**Latest or Current Conditions**:  
This variable refers to the total assessable discount amount on Employee
Share Schemes, which is made up from discounts on ‘Employee Share Scheme
interests’ (ESS interest) that the taxpayer received under a Employee
Share Scheme net any reductions. This may include Discount from tax
upfront schemes - ineligible for reduction (see in\_ess\_disc\_inelgbl)
and/or Discount from tax upfront schemes - eligible for reduction (see
in\_ess\_disc\_elgbl), Discount from deferral schemes (see
in\_ess\_disc\_defer), and Discount on ESS interest acquired pre 1 July
2009 and ‘cessation time’ occurred during financial year (see
in\_ess\_interest\_pre\_2009).  
  
Examples of ESS interests are shares, stapled securities (provided at
least one of the stapled interest is a share in a company, or the rights
to acquire the shares or stapled securities. The ESS interests can be
from an Australian or foreign company, related to employment inside or
outside Australia, or related to a work relationship other than
employment, for example sub-contracting. An ESS interest acquired by the
taxpayer’s associate in respect to their employment is treated as though
the ESS interest was acquired by the taxpayer.  
  
The discount is the difference between the market value of the ESS
interest and the amount paid to acquire them. The taxpayer will be taxed
on the discount in the year in which the interest was acquired. Such
schemes are known as ‘taxed-upfront schemes’. However, if certain
conditions relating to the taxpayer and their scheme is met, the taxing
point is deferred until a later time. These tax-deferred schemes are
known as ‘deferral schemes’.  
  
Changes to Employee Share Schemes took effect on 1 July 2015 and apply
to ESS interests acquired on or after that date. These changes
include:  
- a tax concession through which some discounts on ESS interests in
start-up companies will not be taxed under the Employee Share Scheme
regime, as long as the eligibility criteria are met. Subsequent gains on
the disposal of these ESS interests will be taxed under the capital gain
tax rules  
- changes to the ‘deferred taxing point’.  
  
Taxpayer may report ‘tax file number (TFN) amounts withheld’ if they did
not provide their TFN or ABN to their employer. This is recorded under
tw\_ess.  
  
Excludes any ESS interests given by a start-up company. This amount is
not shown on the Employee share scheme statement.  
If taxpayer qualifies as a temporary resident for tax, special rules may
apply if they acquired ESS interests under pre-1 July 2009 Employee
Share Scheme rules or ESS interests under an Employee Share Scheme  
If taxpayer disposed of any ESS interests due to a corporate restructure
or takeover and received replacement shares, stapled securities or
rights, provisions may apply.  
   

<br>**Latest or Current Calculations**:  
Option 1: if taxpayer does not have any Discount from tax upfront
schemes - eligible for reduction (see in\_ess\_disc\_elgbl), the total
assessable amount is the sum of Discount from tax upfront schemes -
ineligible for reduction (see in\_ess\_disc\_inelgbl), Discount from
deferral schemes (see in\_ess\_disc\_defer), and Discount on ESS
interest acquired pre 1 July 2009 and ‘cessation time’ occurred during
financial year (see in\_ess\_interest\_pre\_2009).  
  
Option 2: if taxpayer reports any Discount from tax upfront schemes -
eligible for reduction (see in\_ess\_disc\_elgbl), they may be entitled
to a reduction of up to $1,000 on the amount they are assessed on.
Eligibility is determined by whether they satisfy the income test based
on their total income. Their total income is the sum of their taxable
income (excluding any potential reductions in Employee Share Scheme
discounts) and the following amounts:   - Total reportable fringe
benefits amounts (sum of it\_rept\_fringe\_benefit\_exe and
it\_rept\_fringe\_benefit\_nexe)  
- Reportable employer superannuation contributions
(it\_rept\_empl\_super\_cont)  
- Net financial investment loss (it\_invest\_loss)  
- Net rental property loss (it\_property\_loss)  
- Deductible personal superannuation contributions
(ds\_pers\_super\_cont).  
  
If their total income calculated is greater than $180,000, they do not
satisfy the income test and are not entitled to a reduction. Therefore,
the total assessable amounts i\_ess is equal to the sum of
in\_ess\_disc\_elgbl, in\_ess\_disc\_inelgbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009.  
  
If their total income calculated is less than or equal to $180,000 they
satisfy the income test and are eligible for the reduction of up to
$1,000. If the amount at in\_ess\_disc\_elgbl:  
- less than or equal to $1,000, i\_ess is the sum of
in\_ess\_disc\_inegbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009.  
- greater than $1,000, i\_ess is the sum of in\_ess\_disc\_elgbl,
in\_ess\_disc\_inegbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009 less $1,000.  
   

<br>**Earliest Conditions**:  
For 2009-10, this variable refers to the total assessable discount
amount on Employee Share Schemes, which is made up from discounts on
‘Employee Share Scheme interests’ (ESS interest) that the taxpayer
received under an employee share scheme net any reductions. This may
include Discount from tax upfront schemes - ineligible for reduction
(see in\_ess\_disc\_inelgbl) and/or Discount from tax upfront schemes -
eligible for reduction (see in\_ess\_disc\_elgbl), Discount from
deferral schemes (see in\_ess\_disc\_defer), and Discount on ESS
interest acquired pre 1 July 2009 and ‘cessation time’ occurred during
financial year (see in\_ess\_interest\_pre\_2009).  
Examples of ESS interests are shares, stapled securities (provided at
least one of the stapled interest is a share in a company, or the rights
to acquire the shares or stapled securities. The ESS interests can be
from an Australian or foreign company, or related to employment inside
or outside Australia. An ESS interest acquired by the taxpayer’s
associate in respect to their employment is treated as though the ESS
interest was acquired by the taxpayer.  
The discount is the difference between the market value of the ESS
interest and the amount paid to acquire them. The taxpayer will be taxed
on the discount in the year in which the interest was acquired. Such
schemes are known as ‘taxed-upfront schemes’. However, if certain
conditions relating to the taxpayer and their scheme is met, the taxing
point is deferred until a later time. These tax-deferred schemes are
known as ‘deferral schemes’.  
Taxpayer may report ‘tax file number (TFN) amounts withheld’ if they did
not provide their TFN or ABN to their employer. This is recorded under
tw\_ess.  
  
If taxpayer qualifies as a temporary resident for tax, special rules may
apply if they acquired ESS interests under pre-1 July 2009 Employee
Share Scheme rules or ESS interests under an Employee Share Scheme.  
If taxpayer disposed of any ESS interests due to a corporate restructure
or takeover and received replacement shares, stapled securities or
rights, provisions may apply.  
   

<br>**Earliest Calculations**:  
For 2009-10:    
Option 1: if taxpayer does not have any Discount from tax upfront
schemes - eligible for reduction (see in\_ess\_disc\_elgbl), the total
assessable amount is the sum of Discount from tax upfront schemes -
ineligible for reduction (see in\_ess\_disc\_inelgbl), Discount from
deferral schemes (see in\_ess\_disc\_defer), and Discount on ESS
interest acquired pre 1 July 2009 and ‘cessation time’ occurred during
financial year (see in\_ess\_interest\_pre\_2009).  
  
Option 2: if taxpayer reports any Discount from tax upfront schemes -
eligible for reduction (see in\_ess\_disc\_elgbl), they may be entitled
to a reduction of up to $1,000 on the amount they are assessed on.
Eligibility is determined by whether they satisfy the income test based
on their total income. Their total income is the sum of their taxable
income (excluding any potential reductions in Employee Share Scheme
discounts) and the following amounts:  
- Total reportable fringe benefits amounts (sum of
it\_rept\_fringe\_benefit\_exe and it\_rept\_fringe\_benefit\_nexe)  
- Reportable employer superannuation contributions
(it\_rept\_empl\_super\_cont)  
- Net financial investment loss (it\_invest\_loss)  
- Net rental property loss (it\_property\_loss)  
- Deductible personal superannuation contributions
(ds\_pers\_super\_cont).  

If their total income calculated is greater than $180,000, they do not
satisfy the income test and are not entitled to a reduction. Therefore,
the total assessable amounts i\_ess is equal to the sum of
in\_ess\_disc\_elgbl, in\_ess\_disc\_inelgbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009.  

If their total income calculated is less than or equal to $180,000 they
satisfy the income test and are eligible for the reduction of up to
$1,000. If the amount at in\_ess\_disc\_elgbl is:  
- less than or equal to $1,000, i\_ess is the sum of
in\_ess\_disc\_inegbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009.  
- greater than $1,000, i\_ess is the sum of in\_ess\_disc\_elgbl,
in\_ess\_disc\_inegbl, in\_ess\_disc\_defer and
in\_ess\_interest\_pre\_2009 less $1,000.  
   

<br>**URL Address**:  
<https://www.ato.gov.au/General/Employee-share-schemes/In-detail/Rollover-relief/ESS---Rollover-relief/>  
<https://www.ato.gov.au/general/employee-share-schemes/in-detail/foreign-residents/ess---foreign-income-exemption-for-australian-residents-and-temporary-residents---employee-share-schemes/>  
<br>**Type**:  
Numerical  
<br>**Form Location**:  

1991(NA),1992(NA),1993(NA),1994(NA),1995(NA),1996(NA),1997(NA),1998(NA),1999(NA),2000(NA),2001(NA),2002(NA),2003(NA),2004(NA),2005(NA),2006(NA),2007(NA),2008(NA),2009(NA),2010(Pg3\_B),2011(Pg3\_B),2012(Pg3\_B),2013(pg3\_12B),2014(pg3\_12B),2015(pg3\_12B),2016(pg3\_12B),2017(Pg3\_12B),2018(Pg3\_12B),2019(Pg3\_12B),2020(Pg3\_12B),2021(Pg3\_12B),2022(Pg3\_12B)

<br>**Event time line: Significance, Type of change(s), Description of
change(s)**:  

**2010**: Major; Introduced; New variable.

**2016**: Major; Component; ESS interests can be related to work
relationship other than employment, for example sub-contracting.
Discounts on eligible ESS interests provided by a start-up company will
not be included on the Employee share scheme statement, and therefore is
excluded.

------------------------------------------------------------------------
