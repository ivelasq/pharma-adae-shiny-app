# Adverse Event Analysis Datasets (ADAE) Shiny App

This is the GitHub repo for the Shiny app described in the Posit Blog post, [The potential for AI-powered Shiny app prototyping with Shiny Assistant](https://posit.co/blog/ai-powered-shiny-app-prototyping/).

See the deployed app (`app.R`) on [Posit Connect Cloud](https://0194a9e0-36d8-f366-dd38-26b34523a0c9.share.connect.posit.cloud/).

<img width="1464" alt="image5" src="https://github.com/user-attachments/assets/6998f631-08d9-4435-8537-70b8c91c56be" />

## Files

In the blog post, we walk through using the [Shiny Assistant](https://gallery.shinyapps.io/assistant/) to create a prototype Shiny app. This repo contains the different iterations of what was produced by the Shiny Assistant.

```
├── 00-generate-datasets.R - create a dummy ADAE dataset using the random.cdisc.data package
├── 01-app.R - first iteration of the app (code produced by Shiny Assistant)
├── 02-app.R - second iteration of the app (code produced by Shiny Assistant)
├── 03-app.R - third iteration of the app (code produced by Shiny Assistant)
├── adae.Rds - stored data generated from `00-generate-datasets.R`
├── app.R - deployed version of the app (adapted from Shiny Assistant code using the generated ADAE data)
└── manifest.json - manifest file for Posit Connect Cloud
```
