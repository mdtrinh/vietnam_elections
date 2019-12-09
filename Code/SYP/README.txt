Summary of the code files in this folder

Last updated: 26 Feb 2018

This folder contains the code files that were used in the process of researching for and writing my second year paper (SYP). They complete these following tasks: a) clean data from original sources, b) run main analyses, c) run robustness checks, d) run the analyses and produce the illustrations that eventually go into the paper. There are also some back-end files that create helper functions. Finally, a some files replicate the findings by Malesky et al (2011), which help as a background context for the SYP.

As of 26 Feb 2018, all the files are moved to this folder to make room for a new round of analyses. The code files themselves are going through some changes, most of which involve replacing the customized functions to run regressions with their updated versions inside the vietnamdata package (which is being concurrently developed). So users may encounter some problem running through the entire code, as some parts of the code have not been adapted to using the vietnamdata functions.

The role and function of each of the code files, in order of run, is:

- Functions.R: helper functions to help with cleaning data, (now deprecated) functions to run regressions with the randomization inference framework
- Clean_Budget.R: cleans Finalized and Planned budget
- Clean_Profile.R: cleans profiles of National Assembly candidates
- Clean_Results.R: clean National Assembly results
- Clean_Leaders.R: clean data on provincial leaders (provided by Malesky and Phan)
- Clean_PAPI.R: clean PAPI data
- Clean_PCI.R: clean PCI data

- Merge_All.R: a wrapper to run all the above functions, and merge all different data into a common source
- Merge_All_bound55.R: same as Merge_All.R, except closewin is coded using a 55-percent level (instead of 60) 
- Merge_All_bound65.R: same as Merge_All.R, except closewin is coded using a 65-percent level (instead of 60) (probably not used)
- Merge_All_thres60.R: same as Merge_All.R, except treatment is coded using a 60-percent threshold, such that we can do a Placebo

- Analyze_RI.R: an old file that runs not-very-sophisticated randomization inference analysis; already retired and actually not used in the final paper
- Analyze_NewRI.R: runs most of the main analyses that use (linear fixed effects) regression methods, both final and exploratory ones.
- Analyze_NewRI_Lag.R: runs a robustness check/placebo by repeating most main analyses on a 2-year lag of the main DV
- Analyze_NewRI_Placebo.R: runs a robustness check/placebo by repeating most main analyses with the treatment from one election but outcome from the previous election; should expect null results
- Analyze_NewRI_bound65.R: runs a robustness chec/placebo by repeating most main analyses with 

- Analyze_Synth.R: runs most of the main analyses that use the Synthetic control method, both final and exploratory ones.

- Analyze_Mechanism.R: runs some main analyses on subcomponent of budget to explore causal mechanisms
- Analyze_Leaders.R: runs some analyses on the fate of provincial leaders to rule out an alternative hypothesis

- Illustration_SYP.R: re-run the main analyses that were used in the paper; create graphics and tables to include in the main paper.

- Analyze_Replication.R: replicates the finding by Malesky et al (2011) to substantiate several claims about the background context of Vietnam MA election; this file is copied from an earlier project in my Empirical Methods class.
- Analyze_NewData.R: replicates the finding by Malesky et al (2011) further on additional data from the 2016 election that they did not have access for
- Out_TablesPlots.R: generates tables and plots that match what Malesky et al (2011) produced in their paper; this file is copied from an earlier project in my Empirical Methods class.

- Analyze_DigitTest.R: runs Benford's Digit Test on numbers from election results to substantiate a claim made by Malesky et al (2011) that the government does not engage in widespread fraud.

