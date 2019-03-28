R=r
RSCRIPT=Rscript


# Simple make target that checks that we have all of the required packages
# installed.  If we have all of the packages, we simply touch a file in the
# parent directory to show that this check has been performed.
.RHasRequiredPackages:
	$(RSCRIPT) -e "for (pkg in c('devtools', 'roxygen2')) {if (!(pkg %in% rownames(installed.packages()))) {install.packages(pkg, repos=list(CRAN='https://cran.rstudio.com/'))}}"
	touch .RHasRequiredPackages

.PHONY: doc
doc: .RHasRequiredPackages
	$(RSCRIPT) -e "devtools::document()"

.PHONY: test
test: .RHasRequiredPackages
	$(RSCRIPT) -e "devtools::test()"

.PHONY: clean
clean:
	rm -rf man
	rm -f .RHasRequiredPackages
