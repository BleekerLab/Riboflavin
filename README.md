# Riboflavin paper

<!-- MarkdownTOC autolink="true" -->

- [Storyline](#storyline)
	- [Background](#background)
	- [Unknown](#unknown)
	- [Findings](#findings)
	- [Result statement](#result-statement)
	- [Implications](#implications)
- [List of figures and tables.](#list-of-figures-and-tables)

<!-- /MarkdownTOC -->

# Storyline

## Background
Phloem feeding insects, such as the whitefly Bemisia tabaci, are a major pests in agriculture and cause massive yield losses. The two most promising options for defence against phloem feeders are on the outside of a plant (i.e. trichomes), where the insect can be killed or repelled prior to feeding.  Due to their stealthy feeding style, they penetrate the phloem with their stylet without causing cell damage. This allows them to bypass most of the plants defence mechanisms. Although food quality plays an important role in animal development, research on whitefly resistance in tomato has mainly focused on the trichomes instead of the phloem they feed from. In this paper we focused on developmental effects of whitefly established on its host, tomato, already.

## Unknown
The underlying mechanism of resistance based on reduced whitefly nymphal development, observed in _Solanum chmielewskii_ 

## Findings
-	We identified an accession of Solanum chmielewskii on which whitefly nymphs do not develop
-	The resistance phenotype was studied in detailon grafts of MM on S. chmielewskii: Same phenotype as on non-grafted S. chmielewskii, no development beyond stage ? 
-	Screening of an introgression library points at resistance residing on chromosome 7. 
-	Metabolomics approach comparing KG1955, S. chmielewskii, IL1927 and IL1928 resulted in a list of candidate metabolic features, including riboflavin (elevated levels of the metabolite in the resistant lines).  
-	Because WF carefully feed on the phloem, we set out to identify the presence of this compound in the phloem.
-	PoC: feeding of ribovlavin to the vasculature of KG1955 resulted in reproduction of the WF elevated oviposition, but not the resistance phenotype, of S. chmieleweskii.
- PoC: inhibiting the riboflavin synthase in S. chmielewskii resulted in ? (loss of resistance -> indirect effect of riboflavin on resistance; resistance remains -> no effect of riboflavin on resistance) 


## Result statement
Here we make a case that the presence of a vasculature-based factor in the wild tomato accession Solanum chmielewskii causes Bemisia tabaci nymphs feeding from this host to display hampered development compared to their performance on Solanum lycopersicum cv Moneymaker.

## Implications
This work highlights the importance of phloem composition in the plant-phloem feeder interactions which could be applied in breeding for phloem feeder resistance in tomato, and other crops.


# List of figures and tables. 

- Screen of S. chmielweski Arjen
- Phenotype in detail (foto’s or drawing)
- bioassay on grafts
- Screen of IL library Arjen
- Metabolomics, differential compounds: levels of riboflavin candidate
- Phloem analysis: prescence of riboflavin in the Phloem
- Riboflavin feeding assay
- Riboflavin synthase inhibitor assay

# MRAN checkpoint data
__December 1, 2020__ is set as the checkpoint for all R packages available from the CRAN.

If required, change it everywhere in the scripts (`grep -r "checkpoint"`) and 
it will search through all directories and files for the `checkpoint` function. 

```{r}
library("checkpoint")
checkpoint("2020-12-01")
```