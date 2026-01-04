# Literature Review: Evaluating Place-Based Policing Interventions

## 1. The Identification Problem: Simultaneous Rollout Without Staggering

This is the core methodological challenge you're facing, and it's surprisingly understudied in the policing literature—most experimental and quasi-experimental hot spots studies benefit from randomization or phased rollout.

### Key methodological papers

- **Abadie, A., Diamond, A., & Hainmueller, J. (2010).** Synthetic control methods for comparative case studies: Estimating the effect of California's tobacco control program. *Journal of the American Statistical Association*, 105(490), 493-505.
  - *Relevance*: Foundational synthetic control paper. Your GIVE comparison shows this won't work for NYC aggregate—the parallel trends are too close. But the method could apply within-NYC if you build synthetic controls for zone areas from non-zone areas.

- **Abadie, A. (2021).** Using synthetic controls: Feasibility, data requirements, and methodological aspects. *Journal of Economic Literature*, 59(2), 391-425.
  - *Relevance*: Updated guidance on when synthetic control works and doesn't. Discusses the problem of "perfect fit" in pre-period (which you'd have with GIVE) leaving no treatment effect identifiable.

- **Gobillon, L., & Magnac, T. (2016).** Regional policy evaluation: Interactive fixed effects and synthetic controls. *Review of Economics and Statistics*, 98(3), 535-551.
  - *Relevance*: Extends synthetic control to panel settings; discusses identification when treatment is selected on pre-treatment outcomes.

- **Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).** Synthetic difference-in-differences. *American Economic Review*, 111(12), 4088-4118.
  - *Relevance*: Combines synthetic control with DiD; may be applicable to your within-NYC comparison where you're matching on pre-treatment shooting levels.

### The regression-to-mean problem in crime hot spots

- **Wheeler, A. P., Worden, R. E., & McLean, S. J. (2016).** Replicating group-based trajectory models of crime at micro-places in Albany, NY. *Journal of Quantitative Criminology*, 32(4), 589-612.
  - *Relevance*: Shows that hot spots are unstable over time; high-crime places tend to regress toward mean. This is exactly your identification threat—zones selected on high 2022 shootings would decline anyway.

- **Levin, A., Rosenfeld, R., & Deckard, M. (2017).** The law of crime concentration: An application and recommendations for future research. *Journal of Quantitative Criminology*, 33(3), 635-647.
  - *Relevance*: Examines stability of crime concentration; finds significant year-to-year variation at micro-places, supporting regression-to-mean concerns.

- **Gorr, W., Olligschlaeger, A., & Thompson, Y. (2003).** Short-term forecasting of crime. *International Journal of Forecasting*, 19(4), 579-594.
  - *Relevance*: Early paper on crime prediction showing hot spots "cool off" naturally; useful for arguing that pre-post comparisons overstate intervention effects.

### Geographic regression discontinuity approaches

- **Dell, M. (2010).** The persistent effects of Peru's mining mita. *Econometrica*, 78(6), 1863-1903.
  - *Relevance*: Classic geographic RDD paper. Your zone boundaries create discontinuities—if boundary placement was somewhat arbitrary, you can compare outcomes just inside vs. just outside.

- **Keele, L. J., & Titiunik, R. (2015).** Geographic boundaries as regression discontinuities. *Political Analysis*, 23(1), 127-155.
  - *Relevance*: Methods paper on geographic RDD; discusses when boundary-based identification is valid and threats from sorting/selection at boundaries.

- **MacDonald, J., Fagan, J., & Geller, A. (2016).** The effects of local police surges on crime and arrests in New York City. *PLoS ONE*, 11(6), e0157223.
  - *Relevance*: Directly relevant—uses precinct boundary discontinuities to estimate effects of police surges in NYC. Similar identification strategy to what you might use with zone boundaries.

---

## 2. Hot Spots Policing: Experimental and Quasi-Experimental Evidence

These establish what the "expected" effect of place-based patrol should be under ideal implementation.

### Foundational experiments

- **Sherman, L. W., & Weisburd, D. (1995).** General deterrent effects of police patrol in crime "hot spots": A randomized, controlled trial. *Justice Quarterly*, 12(4), 625-648.
  - *Relevance*: The original hot spots experiment (Minneapolis). Found significant reductions in disorder and crime. But note: this was 110 hot spots randomized, with clear dosage measurement—very different from your 69 zones without randomization.

- **Weisburd, D., & Green, L. (1995).** Policing drug hot spots: The Jersey City drug market analysis experiment. *Justice Quarterly*, 12(4), 711-735.
  - *Relevance*: Early experimental evidence; problem-oriented approach. Useful comparison for "patrol only" vs. "patrol + problem-solving."

- **Braga, A. A., & Bond, B. J. (2008).** Policing crime and disorder hot spots: A randomized controlled trial. *Criminology*, 46(3), 577-607.
  - *Relevance*: Lowell experiment; found effects for disorder but weaker for violent crime. Raises the question of whether patrol deters gun violence specifically.

### Systematic reviews and meta-analyses

- **Braga, A. A., Turchan, B. S., Papachristos, A. V., & Hureau, D. M. (2019).** Hot spots policing and crime reduction: An update of an ongoing systematic review and meta-analysis. *Journal of Experimental Criminology*, 15(3), 289-311.
  - *Relevance*: Most comprehensive meta-analysis. Key finding: mean effect size of about 15% reduction. But most studies are experiments with randomization—your observational setting likely has weaker identification.

- **Weisburd, D., Telep, C. W., Hinkle, J. C., & Eck, J. E. (2010).** Is problem-oriented policing effective in reducing crime and disorder? *Criminology & Public Policy*, 9(1), 139-172.
  - *Relevance*: Problem-oriented policing (more intensive than patrol) shows effects. Raises question of whether NYPD zones were just patrol or included problem-solving components.

### Null and weak findings

- **Ratcliffe, J. H., Taniguchi, T., Groff, E. R., & Wood, J. D. (2011).** The Philadelphia foot patrol experiment: A randomized controlled trial of police patrol effectiveness in violent crime hot spots. *Criminology*, 49(3), 795-831.
  - *Relevance*: **Critical paper.** Randomized foot patrol experiment found 23% reduction in violent crime during patrol hours, but effects disappeared when patrol ended. Also found displacement. Directly relevant to your summer-only deployment.

- **MacDonald, J., & Lattimore, P. K. (2010).** Count models in criminology. In A. R. Piquero & D. Weisburd (Eds.), *Handbook of Quantitative Criminology* (pp. 683-698). Springer.
  - *Relevance*: Methods guidance for rare event outcomes (shootings). You'll need negative binomial or Poisson models given sparse shooting counts at segment level.

---

## 3. Crime Concentration and Optimal Geographic Targeting

This literature directly supports your "targeting efficiency" critique.

### The law of crime concentration

- **Weisburd, D. (2015).** The law of crime concentration and the criminology of place. *Criminology*, 53(2), 133-157.
  - *Relevance*: **Essential.** Establishes that ~5% of street segments account for ~50% of crime across cities. This is your benchmark for evaluating zone efficiency—are they capturing the right 5%?

- **Weisburd, D., Bushway, S., Lum, C., & Yang, S. M. (2004).** Trajectories of crime at places: A longitudinal study of street segments in the city of Seattle. *Criminology*, 42(2), 283-322.
  - *Relevance*: Seattle street segment analysis showing concentration and stability. Methodological model for your segment-level analysis.

- **Bernasco, W., & Steenbeek, W. (2017).** More places than crimes: Implications for evaluating the law of crime concentration at place. *Journal of Quantitative Criminology*, 33(3), 451-467.
  - *Relevance*: Critical examination of concentration metrics; discusses how to measure and interpret concentration at micro-places.

### Optimal targeting and predictive accuracy

- **Mohler, G. O., Short, M. B., Malinowski, S., Johnson, M., Tita, G. E., Bertozzi, A. L., & Brantingham, P. J. (2015).** Randomized controlled field trials of predictive policing. *Journal of the American Statistical Association*, 110(512), 1399-1411.
  - *Relevance*: Tests predictive algorithms vs. analyst-drawn hot spots. Found algorithmic targeting more efficient—supports your critique of NYPD's informal zone selection.

- **Hunt, P., Saunders, J., & Hollywood, J. S. (2014).** Evaluation of the Shreveport predictive policing experiment. *RAND Corporation*.
  - *Relevance*: Predictive policing evaluation; shows that even "data-driven" targeting struggles with rare events like shootings.

- **Chainey, S., Tompson, L., & Uhlig, S. (2008).** The utility of hotspot mapping for predicting spatial patterns of crime. *Security Journal*, 21(1), 4-28.
  - *Relevance*: Compares different hotspot mapping methods; useful for thinking about how NYPD might have (poorly) identified zone locations.

### Zone size and the scale problem

- **Hipp, J. R., & Kim, Y. A. (2017).** Measuring crime concentration across cities of varying sizes: Complications based on the spatial and temporal scale employed. *Journal of Quantitative Criminology*, 33(3), 595-632.
  - *Relevance*: **Directly relevant.** Shows that apparent crime concentration varies dramatically with unit of analysis. Argues for street segment level—supports your LION segment approach.

- **Gerell, M. (2017).** Smallest is better? The spatial distribution of arson and the modifiable areal unit problem. *Journal of Quantitative Criminology*, 33(2), 293-318.
  - *Relevance*: MAUP in crime analysis; shows how zone size affects detection of concentration and intervention effects.

- **Weisburd, D., Groff, E. R., & Yang, S. M. (2012).** *The criminology of place: Street segments and our understanding of the crime problem.* Oxford University Press.
  - *Relevance*: Book-length treatment of why street segments are the right unit. Provides theoretical justification for your segment-level analysis over zone-level or block-group-level.

---

## 4. Displacement and Spillover

Critical for interpreting any zone effects you find (or don't find).

- **Weisburd, D., Wyckoff, L. A., Ready, J., Eck, J. E., Hinkle, J. C., & Gajewski, F. (2006).** Does crime just move around the corner? A controlled study of spatial displacement and diffusion of crime control benefits. *Criminology*, 44(3), 549-592.
  - *Relevance*: Jersey City study finding diffusion of benefits rather than displacement. Suggests checking if out-of-zone areas near zones improved (spillover) vs. worsened (displacement).

- **Bowers, K. J., Johnson, S. D., Guerette, R. T., Summers, L., & Poynton, S. (2011).** Spatial displacement and diffusion of benefits among geographically focused policing initiatives: A meta-analytical review. *Journal of Experimental Criminology*, 7(4), 347-374.
  - *Relevance*: Meta-analysis of displacement studies. Generally finds limited displacement and some diffusion—but for serious violence, evidence is thinner.

- **Telep, C. W., Weisburd, D., Gill, C. E., Vitter, Z., & Teichman, D. (2014).** Displacement of crime and diffusion of crime control benefits in large-scale geographic areas: A systematic review. *Journal of Experimental Criminology*, 10(4), 515-548.
  - *Relevance*: Reviews displacement at larger geographic scales (like your 10 sq mi). Finds less evidence of displacement at larger scales.

---

## 5. Dosage and the Koper Curve

Speaks to your "not enough cops" concern.

- **Koper, C. S. (1995).** Just enough police presence: Reducing crime and disorderly behavior by optimizing patrol time in crime hot spots. *Justice Quarterly*, 12(4), 649-672.
  - *Relevance*: **Classic paper.** Finds 10-15 minutes of patrol presence per hot spot maximizes deterrence, with diminishing returns after. Question: did zone deployment achieve even this threshold?

- **Telep, C. W., Mitchell, R. J., & Weisburd, D. (2014).** How much time should the police spend at crime hot spots? Answers from a police agency directed randomized field trial in Sacramento. *Justice Quarterly*, 31(5), 905-933.
  - *Relevance*: Tests different dosage levels; finds 15-minute visits twice per shift optimal. Useful benchmark for back-of-envelope calculation of whether NYPD zone staffing was sufficient.

- **Williams, S., & Coupe, T. (2017).** Frequency vs. length of hot spots patrol visits: A randomised controlled trial. *Cambridge Journal of Evidence-Based Policing*, 1(1), 5-21.
  - *Relevance*: UK experiment on patrol frequency vs. duration; finds frequency matters more. Relevant to whether thin deployment across many zones is worse than concentrated deployment in fewer zones.

---

## 6. Policing and Gun Violence Specifically

The effect of patrol on gun violence may differ from property crime or disorder.

- **MacDonald, J., Fagan, J., & Geller, A. (2016).** The effects of local police surges on crime and arrests in New York City. *PLoS ONE*, 11(6), e0157223.
  - *Relevance*: **Directly relevant to NYC.** Uses Operation Impact as natural experiment. Found significant effects on homicide and robbery but relied on staggered rollout you don't have.

- **Chalfin, A., Hansen, B., Weisburst, E. K., & Williams, M. C. (2022).** Police force size and civilian race. *American Economic Review: Insights*, 4(2), 139-158.
  - *Relevance*: Large-scale study of police force effects on homicide. Finds significant effects, but at aggregate city level with different identification strategy.

- **Ratcliffe, J. H., & Breen, C. (2011).** Crime diffusion and displacement: Measuring the side effects of police operations. *The Professional Geographer*, 63(2), 230-243.
  - *Relevance*: Philadelphia study of violent crime displacement; methodologically relevant for your spillover tests.

- **Braga, A. A., Hureau, D. M., & Papachristos, A. V. (2014).** Deterring gang-involved gun violence: Measuring the impact of Boston's Operation Ceasefire on street gang behavior. *Journal of Quantitative Criminology*, 30(1), 113-139.
  - *Relevance*: Focused deterrence (different intervention) but addresses gun violence specifically. Uses interrupted time series methods relevant to your analysis.

---

## 7. Methods Papers for Your Specific Analysis

### Difference-in-differences with selection on pre-treatment outcomes

- **Daw, J. R., & Hatfield, L. A. (2018).** Matching and regression to the mean in difference-in-differences analysis. *Health Services Research*, 53(6), 4138-4156.
  - *Relevance*: Directly addresses the regression-to-mean problem in DiD when treatment is selected on baseline outcomes. Recommends matching on pre-treatment trends, not just levels.

- **Ryan, A. M., Kontopantelis, E., Linden, A., & Burgess Jr, J. F. (2019).** Now trending: Coping with non-parallel trends in difference-in-differences analysis. *Statistical Methods in Medical Research*, 28(12), 3697-3711.
  - *Relevance*: Practical guidance for DiD with non-parallel trends; relevant if your pre-treatment trends differ between in-zone and out-of-zone segments.

### Event study designs

- **Freyaldenhoven, S., Hansen, C., Pérez, J. P., & Shapiro, J. M. (2021).** Visualization, identification, and estimation in the linear panel event-study design. *NBER Working Paper No. 29170*.
  - *Relevance*: Best practices for event study plots; essential for showing pre-treatment parallel trends (or lack thereof).

- **Sun, L., & Abraham, S. (2021).** Estimating dynamic treatment effects in event studies with heterogeneous treatment effects. *Journal of Econometrics*, 225(2), 175-199.
  - *Relevance*: Addresses heterogeneous treatment effects in event studies; relevant if zone effects vary across zones.

### Spatial econometrics

- **Gibbons, S., Overman, H. G., & Patacchini, E. (2015).** Spatial methods. In G. Duranton, J. V. Henderson, & W. C. Strange (Eds.), *Handbook of Regional and Urban Economics* (Vol. 5, pp. 115-168). Elsevier.
  - *Relevance*: Overview of spatial methods including spatial lag/error models, relevant for spillover estimation.

---

## 8. Suggested Reading Priority

**For the identification problem (read first):**
1. Daw & Hatfield (2018) - regression to mean in DiD
2. Abadie (2021) - synthetic control feasibility
3. Keele & Titiunik (2015) - geographic RDD

**For the substantive framing:**
4. Weisburd (2015) - law of crime concentration
5. Braga et al. (2019) - hot spots meta-analysis
6. Ratcliffe et al. (2011) - Philadelphia foot patrol (null-ish finding)

**For the targeting critique:**
7. Mohler et al. (2015) - optimal targeting
8. Hipp & Kim (2017) - zone size matters
9. Koper (1995) - dosage requirements

**NYC-specific:**
10. MacDonald, Fagan & Geller (2016) - Operation Impact evaluation
