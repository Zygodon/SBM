---
title: 'The River Ouse Project: Meadows Surveys Analysis'
date: "2023-05-09"
output:
  html_document: 
     keep_md: TRUE
     code_folding: hide
     df_print: paged
     fig.width: 3
     fig.height: 2
     bibliography: references.bib
     editor_options: 
       markdown: 
       wrap: 72
---    
<style type="text/css">
   .main-container {max-width: 100%;}
   .row {display: flex;}
   .column {flex: 50%;}
</style>
---
<style type="text/css">
h1, h2 {text-align: center;}
</style>



# 1. Introduction
Without going into details, I have built a model that identifies "latent communities" (LC) in the meadows data. There is no intention to classify sites in the NVC sense. Instead, the idea is that the latent communities may be more or less **expressed** at any site. So what follows is a description of the latent communities, and an analysis of their expression by survey site. 

At a quick look, it appears to me that the LC expressions make at least some sense in terms of what we already know about the sites. If that impression holds up to further scrutiny, then I think that the model will help us to relate the patterns that we see to physical, ecological and historical aspects of the environment. I have tried to encapsulate this idea in Figure 1. Here we have the latent communities on the left and their manifestation at the survey sites on the right. In between, the boxes labelled "Environment" are what is called in the literature a "filtering framework". I quote: "the abiotic conditions define the environmental filters selecting species from a regional species pool ..." [@DosAndDonts]. My scheme agrees with this in the presence of a filtering framework, but the latent communities are NOT the same as a species pool, also known as a Community Pool (CP). Here's the difference:

* Community Pool envisages a set of *species* that may be drawn upon to make up the *community* manifest at a particular site. 
* Latent Community envisages a set of *communities* that may be drawn upon to make up the mix of *species* manifest at a particular site.

The conceptual distinction is significant:

* several latent communities may be manifest at a site.
* latent community expression can calculated conveniently.
* in estimating LC expression, dissociative relationships can be taken into account.
* partial LC membership is included in a natural way.

<center>

![Figure 1. ](Figures/Latent community summary.png){width=50%}
<center>

# 2. Latent communities
According to this scheme, a latent community is a set of *relationships between pairs of plants* such that they occur together in the data more or less frequently than would be expected by chance (p < 0.05). These plant pairs are called dyads, and they may be associative (occur together more frequently than would be expected) or dissociative.

Dyads, not plants, are counted in assessing latent community expression at a site; in order count towards LC expression, both elements of the dyad must be present, and the contribution that they make is determined by the sign (associative/dissociative) of the link between them, see [Calculation of LC expression](#section3) below.

<table class="table table-striped" style="width: auto !important; float: right; margin-left: 10px;">
<caption>Table 1. Model parameters</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> LC1 </th>
   <th style="text-align:left;"> LC2 </th>
   <th style="text-align:left;"> LC3 </th>
   <th style="text-align:left;"> LC4 </th>
   <th style="text-align:left;"> LC5 </th>
   <th style="text-align:left;"> LC6 </th>
   <th style="text-align:left;"> LC7 </th>
   <th style="text-align:left;"> LC8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.858</span> </td>
   <td style="text-align:left;"> 0.517 </td>
   <td style="text-align:left;"> 0.223 </td>
   <td style="text-align:left;"> 0.481 </td>
   <td style="text-align:left;"> 0.071 </td>
   <td style="text-align:left;"> 0.022 </td>
   <td style="text-align:left;"> 0.188 </td>
   <td style="text-align:left;"> 0.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.517 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.596</span> </td>
   <td style="text-align:left;"> 0.169 </td>
   <td style="text-align:left;"> 0.110 </td>
   <td style="text-align:left;"> 0.226 </td>
   <td style="text-align:left;"> 0.055 </td>
   <td style="text-align:left;"> 0.080 </td>
   <td style="text-align:left;"> 0.009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.223 </td>
   <td style="text-align:left;"> 0.169 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.635</span> </td>
   <td style="text-align:left;"> 0.024 </td>
   <td style="text-align:left;"> 0.049 </td>
   <td style="text-align:left;"> 0.254 </td>
   <td style="text-align:left;"> 0.120 </td>
   <td style="text-align:left;"> 0.014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.481 </td>
   <td style="text-align:left;"> 0.110 </td>
   <td style="text-align:left;"> 0.024 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.565</span> </td>
   <td style="text-align:left;"> 0.071 </td>
   <td style="text-align:left;"> 0.085 </td>
   <td style="text-align:left;"> 0.072 </td>
   <td style="text-align:left;"> 0.008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.071 </td>
   <td style="text-align:left;"> 0.226 </td>
   <td style="text-align:left;"> 0.049 </td>
   <td style="text-align:left;"> 0.071 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.260</span> </td>
   <td style="text-align:left;"> 0.079 </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;"> 0.017 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.022 </td>
   <td style="text-align:left;"> 0.055 </td>
   <td style="text-align:left;"> 0.254 </td>
   <td style="text-align:left;"> 0.085 </td>
   <td style="text-align:left;"> 0.079 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.206</span> </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.188 </td>
   <td style="text-align:left;"> 0.080 </td>
   <td style="text-align:left;"> 0.120 </td>
   <td style="text-align:left;"> 0.072 </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.076</span> </td>
   <td style="text-align:left;"> 0.015 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> 0.009 </td>
   <td style="text-align:left;"> 0.014 </td>
   <td style="text-align:left;"> 0.008 </td>
   <td style="text-align:left;"> 0.017 </td>
   <td style="text-align:left;"> 0.030 </td>
   <td style="text-align:left;"> 0.015 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: red !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightgray !important;">0.035</span> </td>
  </tr>
</tbody>
</table>

The dyads are the observed data. The statistical model (Stochastic Block Model) builds a representation of the uncertain nature of the data by assigning probabilities to the links. The probabilities form blocks such that the probability of links between dyads within a block differs from the probability of links between dyads ending in different blocks. Generally, in-block probabilities are greater than out_block probabilities, forming positive groups of dyads. In our case, there are eight blocks. The model is summed up by an 8x8 matrix. The values on the leading diagonal are the in-block probabilities, the off-diagonal values are the probabilities of finding links between the corresponding blocks. The matrix is shown in Table 1. The in_block probabilities in the leading diagonal are shown in bold face, and the text is red when the in_block probability is greater than the out_block probabilities. Latent communities (blocks) 1 - 5 are good, 6 needs to be treated with caution, and the probabilities for 7 and 8 are so low that they may best be ignored (the probabilities are about 1/3 that of LC6, and they contain species that are poorly represented in the data).

I don't want to go into details of the Stochastic Block Model here, but it is worth mentioning that it is not told anything about the associative or dissociative nature of the links it is modelling. That information is added later.

Details of the Latent Communities follow. For each, there is a graph and a table. The graphs are useful in visualising the contribution of associative and dissociative links. The size of the species symbols is an indication of the species frequency in the data. The tables list the species in the dyads of which the LC is composed.

### Latent Community 1.

<div class = "row">
<div class = "column">

![](Figures/Latent community 1.png){width=100%}
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 1 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Anemone_nemorosa </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_caryophyllea </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_flacca </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 22.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lathyrus_montanus </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leontodon_hispidus </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lolium_perenne </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 70.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pimpinella_saxifraga </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Potentilla_erecta </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 17.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Potentilla_sterilis </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pseudoscleropodium_purum </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 14.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rhinanthus_minor </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rhytidiadelphus_squarrosus </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 22.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stachys_betonica </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Succisa_pratensis </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 12.0 </td>
  </tr>
</tbody>
</table>
</div>
</div>


### Latent Community 2.

<div class = "row">
<div class = "column">

![](Figures/Latent community 2.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 2 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Achillea_millefolium </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 44.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ajuga_reptans </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 26.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alopecurus_pratensis </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 49.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Calliergon_cuspidatum </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 31.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Centaurea_nigra </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 44.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Conopodium_majus </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 13.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dactylorhiza_fuchsii </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 17.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Genista_tinctoria </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hypochaeris_radicata </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 31.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leucanthemum_vulgare </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 22.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lotus_corniculatus </td>
   <td style="text-align:right;"> 129 </td>
   <td style="text-align:right;"> 81.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lotus_uliginosus </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 64.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Luzula_campestris </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plantago_lanceolata </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 37.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Prunella_vulgaris </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 55.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_bulbosus </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 19.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_chamaedrys </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 36.7 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 3.
<div class = "row">
<div class = "column">

![](Figures/Latent community 3.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 3 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Achillea_ptarmica </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Angelica_sylvestris </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_panicea </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cirsium_palustre </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 29.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equisetum_arvense </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Galium_palustre </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 10.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hypericum_tetrapterum </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_acutiflorus </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 16.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_articulatus </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_conglomeratus </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 18.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_effusus </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 27.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lychnis_flos-cuculi </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mentha_aquatica </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oenanthe_crocata </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pulicaria_dysenterica </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 12.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_flammula </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 4.
<div class = "row">
<div class = "column">

![](Figures/Latent community 4.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 4 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Agrostis_canina_spp_montana </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 9.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Briza_media </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cerastium_fontanum </td>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 84.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cirsium_arvense </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 59.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Danthonia_decumbens </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equisetum_telmateia </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hieracium_pilosella </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hyacinthoides_non-scripta </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poa_trivialis </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 77.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_acris </td>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:right;"> 88.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_repens </td>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:right;"> 91.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trifolium_repens </td>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:right;"> 88.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_officinalis </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Viola_riviniana </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 5.
<div class = "row">
<div class = "column">

![](Figures/Latent community 5.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 5 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Anthoxanthum_odoratum </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 91.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arrhenatherum_elatius </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 15.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bellis_perennis </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Brachythecium_rutabulum </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 74.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bromus_hordeaceus </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 17.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_hirta </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 30.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cerastium_glomeratum </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cynosurus_cristatus </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 34.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dactylis_glomerata </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 66.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Eurhynchium_praelongum </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 35.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Festuca_rubra </td>
   <td style="text-align:right;"> 106 </td>
   <td style="text-align:right;"> 67.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Geranium_dissectum </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 19.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heracleum_sphondylium </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 16.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lathyrus_pratensis </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 49.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Phleum_pratense </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 53.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plantago_major </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Potentilla_reptans </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 37.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_acetosa </td>
   <td style="text-align:right;"> 130 </td>
   <td style="text-align:right;"> 82.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stellaria_graminea </td>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 75.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Taraxacum_officinale </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 69.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tragopogon_pratensis </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trifolium_dubium </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 21.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trifolium_pratense </td>
   <td style="text-align:right;"> 92 </td>
   <td style="text-align:right;"> 58.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_serpyllifolia </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 36.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vicia_cracca </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vicia_sativa </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vicia_tetrasperma </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 12.0 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 6.
<div class = "row">
<div class = "column">

![](Figures/Latent community 6.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 6 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Agrimonia_eupatoria </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 10.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Calystegia_sepium </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cardamine_flexuosa </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cardamine_pratensis </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 36.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_ovalis </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 13.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cruciata_laevipes </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dactylorhiza_praetermissa </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Deschampsia_cespitosa </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Festuca_arundinacea </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Festuca_pratensis </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 15.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Filipendula_ulmaria </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 7.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Galium_aparine </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Glechoma_hederacea </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 9.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Glyceria_fluitans </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_inflexus </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lapsana_communis </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Myosotis_discolor </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poa_annua </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_ficaria </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rubus_fruticosus </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 10.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_crispus </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 9.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_obtusifolius </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 22.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Senecio_jacobaea </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 17.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stachys_sylvatica </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Urtica_dioica </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_beccabunga </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 7.
<div class = "row">
<div class = "column">

![](Figures/Latent community 7.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 8px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 7 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Agrostis_canina </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 13.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_pilulifera </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Holcus_mollis </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 4.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hypericum_perforatum </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hypericum_pulchrum </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lathyrus_nissolia </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Luzula_multiflora </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mentha_arvensis </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pedicularis_sylvatica </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Primula_veris </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pteridium_aquilinum </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_acetosella </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Silaum_silaus </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stellaria_alsine </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
</tbody>
</table>
</div>
</div>

### Latent Community 8.
<div class = "row">
<div class = "column">

![](Figures/Latent community 8.png)
</div>
<div class = "column">

<table class="table table-striped table-hover table-condensed" style="font-size: 7px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Latent Community 8 species</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> frequency </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Alopecurus_geniculatus </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Atrichum_undulatum </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_laevigata </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Carex_otrubae </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cirsium_vulgare </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Convolvulus_arvensis </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crepis_capillaris </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dactylorhiza_maculata </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dicranella_staphylina </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Elymus_repens </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Epilobium_hirsutum </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Epilobium_tetragonum </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equisetum_fluviatile </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Equisetum_palustre </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Euphrasia_nemorosa </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Festuca_ovina </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fissidens_taxifolius </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Galium_saxatile </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Galium_verum </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Geum_urbanum </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hordeum_secalinum </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_acutiflorus_articulatus </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Juncus_bufonius </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leontodon_autumnalis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Leontodon_taraxacoides </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lysimachia_nummularia </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Malva_moschata </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Molinia_caerulea </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Myosotis_arvensis </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oenanthe_pimpinelloides </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ophioglossum_vulgatum </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poa_pratensis </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 10.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Potentilla_anserina </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Potentilla_hybrid </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ranunculus_sardous </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_conglomeratus </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rumex_sanguineus </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Senecio_aquaticus </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Senecio_erucifolius </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sonchus_oleraceus </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stachys_palustris </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trifolium_medium </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trisetum_flavescens </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_filiformis </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veronica_persica </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vicia_hirsuta </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vulpia_bromoides </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.27 </td>
  </tr>
</tbody>
</table>
</div>
</div>

## Latent Community expression at survey sites.
Here is a summary of all the Latent Communities expressed at all the survey sites. Two features are immediately obvious:

1. Several LC may be expressed at a single site (Wood Field, Bushy Field, Lindfield Bridge).
2. Some sites have very low expression of any LC (Long Mead SE, SW; Pond field W).
We can investigate these sites to find out what is going on.

![](Figures/Site expressions of latent communities.png)
The pages following show the expressions of individual communities by site. The scale on the left hand side is the percentage of the maximum possible expression.

### Latent Community 1.

![](Figures/Site expression of latent community 1.png)

### Latent Community 2.

![](Figures/Site expression of latent community 2.png)

### Latent Community 3.

![](Figures/Site expression of latent community 3.png)

### Latent Community 4.

![](Figures/Site expression of latent community 4.png)

### Latent Community 5.

![](Figures/Site expression of latent community 5.png)

### Latent Community 6.

![](Figures/Site expression of latent community 6.png)

### Latent Community 7.

![](Figures/Site expression of latent community 7.png)

### Latent Community 8.

![](Figures/Site expression of latent community 8.png)



## <a name="section3"></a>3. Calculation of LC expression.


1. Write G~l~ for the graph of Latent Community l, of degree r~l~ (Latent communities are expressed as graphs in section 2 above. The degree of a graph is the number of links between the nodes; in this case the nodes are plant species.)
2. Write G~ls~ for the subgraph of G~l~ represented at site s.
3. Write A~ls~ for the subgraph of G~ls~ with associative links, and D~ls~ for the subgraph of G~ls~ with dissociative links.

Then the expression X~ls~ of Latent Community l at site s is X~ls~ = 100(a~ls~-d~ls~)/r~l~ 

where a~ls~ is the degree (number of links) of A~ls~ and d~ls~ is the degree of D~ls~.

The presence of *associative* dyads increases X~ls~ while *dissociative* dyads decrease X~ls~. An LC with dissociative links can never be 100% represented; another option would be to normalise against the degree of A~l~, the number of associative links in the parent LC. I decided against this because it is conceivable that there could be an LC with only dissociative links. Negative values of X~ls~ are possible.

## 4. Where next?

Can we glean any understanding of community organisation in our data by examining the species composition of the latent communities, and their site-specific expressions? There are hints of some possibilities:

* LC1 could be characterised as dyads with any of several rather interesting plants, but NOT paired with *Lolium perenne*. The meadows where LC1 is most fully expressed are Gravetye East, Gravetye West and Plain Field North. I think I am correct that these are fields with a management history of intentional meadow restoration, and that would explain the presence of interesting plants; to the extent that *L perenne* is present at these sites (and I need to check up whether it is or not), it could be interpreted as a relic from former more intensive management.

* Latent Community 3 contains an assemblage of plants of damp or wet habitats. It is particularly expressed at Bushy Field, Daltons Meadow, Hanging Meadow, The Mead and White Coppice. This makes sense as these are all damp but well managed (from a wildflower meadow perspective).

* LC6 also includes plants from wet habitats, but maybe with a suggestion of higher nutrient level (*Urtica dioica*, *Rumex crispus*, *Rumex acutifolius*). Lindfield Bridge has the strongest expression of LC6; it is a riparine bank bordering a former agricultural field. It was pointed out in Section 2 that LC6 is anomalous in that the in_block probability is less than one of the out_block probabilities; in fact, less than the out_block probability with the other damp community, LC3, suggesting that these communities have a mutually exclusive relationship.

It will be interesting to use this tool to explore the data for further evidence of biotic or abiotic interactions which could further be tested (in principle at least) by field work or reference to literature.

## 5. Bibliography.
