# Official repository for MLBA Spring 2024 website
Official repository for the Machine Learning in Business Analytics course taught at [HEC Lausanne](https://hec-si.unil.ch/gide-api/web/syllabus/2736?base_url=https://www.unil.ch/hec/home/menuinst/etudes/cours.html?url=).

## Instructions to get the website up and running
We use `renv` for package management. When running the website for the first time, execute the following from R (at the root of the project with `mlba.Rproj`):

```r
renv::restore()
```

It prompts whether you would like to activate and install the packages, and you must select the option (1) to do so. The command above installs the packages from the `renv.lock` file. Then run the following to start the website from the command line/terminal (unless you're using Rstudio, in which can go to `Build > Render Website` to render everything at once).

```bash
quarto preview
```

## Instructions to update the website

When publishing, first run the following:
```bash
quarto render
```

## Troubleshoot 
In case of memory issues, see [this help page from Quarto](https://quarto.org/docs/troubleshooting/index.html#out-of-memory-issues) and increase your limit

```bash
export QUARTO_DENO_EXTRA_OPTIONS=--v8-flags=--max-old-space-size=16384
```

