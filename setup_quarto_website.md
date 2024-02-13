# Instructions to get the website up and running
For the first time, run the following from R of the root of the project
```R
renv::restore()
```
this installs the packages from the `renv.lock` file. Then run the following to start the website from the command line/terminal (unless you're using Rstudio, in which can press the render button)

```bash
quarto preview
```